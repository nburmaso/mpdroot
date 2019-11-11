////////////////////////////////////////////////////////////////
//                                                            //
//  MpdEmcGeoUtils                                            //
//  Geometrical transformations for EMC                	      //
//  Author List : D.Peresunko, KI, 2019                       //
//                                                            //
////////////////////////////////////////////////////////////////
#include "MpdEmcGeoUtils.h"

#include "FairLogger.h"

#include "TGeoArb8.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TGeoTube.h"
#include "TGeoVolume.h"
#include "MpdEmcSimParams.h"

#include <cstdlib>

// these initialisations are needed for a singleton
MpdEmcGeoUtils* MpdEmcGeoUtils::sGeom = nullptr;

ClassImp(MpdEmcGeoUtils)

  // -----   Default constructor   -------------------------------------------
  MpdEmcGeoUtils::MpdEmcGeoUtils()
  : fNTowsersPerCrate(64),
    fNCratesPerSector1(48),  // Number of crates per large sector
    fNCratesPerSector2(24),  // Number of crates per small sector
    fNSectors1PerChamber(6), // Number of large Sectors per Chamber
    fNSectors2PerChamber(2), // Number of small Sectors per Chamber
    fNChambers(2),           // Number of chambers
    fEcalRmin(0.),           // minimal ECAL radius
    fEcalRmax(0.),           // maximal ECAL radius
    fEcalZmax(0.)            // ECAL half z size
{
  fNTowersPerChamber = fNTowsersPerCrate * fNCratesPerSector1 * fNSectors1PerChamber +
                       fNTowsersPerCrate * fNCratesPerSector2 * fNSectors2PerChamber;

  fFirstCellInSector[0]= 0 ;
  fFirstCellInSector[1]=  fNTowsersPerCrate * fNCratesPerSector1 ;
  fFirstCellInSector[2]=2*fNTowsersPerCrate * fNCratesPerSector1 ;
  fFirstCellInSector[3]=2*fNTowsersPerCrate * fNCratesPerSector1 +   fNTowsersPerCrate * fNCratesPerSector2 ;
  fFirstCellInSector[4]=3*fNTowsersPerCrate * fNCratesPerSector1 +   fNTowsersPerCrate * fNCratesPerSector2 ;
  fFirstCellInSector[5]=4*fNTowsersPerCrate * fNCratesPerSector1 +   fNTowsersPerCrate * fNCratesPerSector2 ;
  fFirstCellInSector[6]=5*fNTowsersPerCrate * fNCratesPerSector1 +   fNTowsersPerCrate * fNCratesPerSector2 ;
  fFirstCellInSector[7]=5*fNTowsersPerCrate * fNCratesPerSector1 + 2*fNTowsersPerCrate * fNCratesPerSector2 ;
}

int MpdEmcGeoUtils::AreNeighbours(int detId1, int detId2) const
{
  // Gives the neighbourness of two digits = 0 are not neighbour but continue searching
  //                                       = 1 are neighbour
  //                                       = 2 are not neighbour but do not continue searching
  // neighbours are defined as digits having at least a common side
  // The order of detId1 and detId2 is important: first (d1) should be a digit already in a cluster
  //                                      which is compared to a digit (d2)  not yet in a cluster

  int ch1, sector1, iphi1, iz1;
  DetIdToRelIndex(detId1, ch1, sector1, iphi1, iz1);
  int ch2, sector2, iphi2, iz2;
  DetIdToRelIndex(detId2, ch2, sector2, iphi2, iz2);

  if (ch1 > ch2){
    return -1;
  }
  if (ch1 < ch2){
    return 2;
  }

  if(MpdEmcSimParams::GetInstance()->AllowMultiSectorClusters()){

    if (sector1 == sector2) { // start looking at digits in next sector, can not create inter-sector clusters

      int phidiff = std::abs(iphi1 - iphi2);
      int zdiff = std::abs(iz1 - iz2);

      if (((phidiff <= 1) && (zdiff == 0)) || ((phidiff == 0) && (zdiff <= 1))) { // At least common side
        return 1;
      } else {
        if ((iphi2 > iphi1 + 1)) {
	        if(iphi1!=0 && iphi1!=MaxPhiSector(sector1) && sector2>sector1){ //not at the edge and current sector alredy scanned
            return 2; //  Difference in iphi numbers is too large to look further
	        }
	        else{ //current digit at the edge, continue looking at other sectors
            return 0 ;
	        }
        }
        return 0; // continue searching
      }
    }
    //May be edge of sector
    if(IsPreviousSector(sector1,sector2)){  // second digit in previous sector
      if(iphi1==0 && iphi2==MaxPhiSector(sector2) && iz1==iz2){
        return 1;
      }
      else{
        return 0;
      }
    }
    else{
      if(IsPreviousSector(sector2,sector1)){ //second digit in next sector
        if(iphi1==MaxPhiSector(sector1)){
          if(iphi2==0 && iz1==iz2){
            return 1;
          }
          else{
            return 0;
          }
        }
        else{
          if(iphi1!=0 && sector2>sector1 ){ //not at the edge and current sector scanned
            return 2; //  Difference in iphi numbers is too large to look further
          }
          else{ //current digit at the edge, continue looking at other sectors
            return 0 ;
          }
        }
      }
      else{ //not adjacent sectors
        if(iphi1!=0 && iphi1!=MaxPhiSector(sector1) && sector2>sector1 ){
          return 2; //  Difference in iphi numbers is too large to look further
        }
        else{ //current digit at the edge, continue looking at other sectors
          return 0 ;
        }
      }
    }
  }
  else{ // only within sector
    if (sector1 > sector2) { // start looking at digits in next sector, can not create inter-sector clusters
      return -1;
    }
    if (sector1 < sector2) {
      return 2;
    }

    int phidiff = std::abs(iphi1 - iphi2);
    int zdiff = std::abs(iz1 - iz2);

    if (((phidiff <= 1) && (zdiff == 0)) || ((phidiff == 0) && (zdiff <= 1))) { // At least common side
      return 1;
    } else {
      if ((iphi2 > iphi1 + 1)) {
        return 2; //  Difference in iphi numbers is too large to look further
      }

      return 0; // continue searching
    }
  }

}
int MpdEmcGeoUtils::AreNeighboursVertex(int detId1, int detId2) const
{
  // Gives the neighbourness of two digits = 0 are not neighbour but continue searching
  //                                       = 1 are neighbour
  // In contrast to previous method, here neighbours are defined as digits having at least a common vertex
  // The order of detId1 and detId2 is important: first (d1) should be a digit already in a cluster
  //                                      which is compared to a digit (d2)  not yet in a cluster

  int ch1, sector1, iphi1, iz1;
  DetIdToRelIndex(detId1, ch1, sector1, iphi1, iz1);
  int ch2, sector2, iphi2, iz2;
  DetIdToRelIndex(detId2, ch2, sector2, iphi2, iz2);

  if (ch1 != ch2)
    return 0;

  if(MpdEmcSimParams::GetInstance()->AllowMultiSectorClusters()){

    if(sector1 == sector2){
      int phidiff = std::abs(iphi1 - iphi2);
      int zdiff = std::abs(iz1 - iz2);

      if (((phidiff <= 1) && (zdiff <= 1))) { // At least common vertex
        return 1;
      } else {
        return 0; // not a neighbour
      }
    }

    //May be edge of sector
    if(IsPreviousSector(sector1,sector2)){  //previous sector
      if(iphi1==0 && iphi2==MaxPhiSector(sector2) && TMath::Abs(iz1-iz2)<=1){
        return 1;
      }
      else{
        return 0;
      }
    }
    if(IsPreviousSector(sector2,sector1)){
      if(iphi1==MaxPhiSector(sector1) && iphi2==0 && TMath::Abs(iz1-iz2)<=1){
        return 1;
      }
    }
    return 0;
  }
  else{ //each sector separately
    if (sector1 != sector2) { // start looking at digits in next sector, can not create inter-sector clusters
      return 0;
    }

    int phidiff = std::abs(iphi1 - iphi2);
    int zdiff = std::abs(iz1 - iz2);

    if (((phidiff <= 1) && (zdiff <= 1))) { // At least common vertex
      return 1;
    } else {
      return 0; // continue searching
    }
  }
}

int MpdEmcGeoUtils::GeantToDetId(int chamberH, int chamber, int sector, int crate, int box) const
{
  // Convert Geant volume indexes to abs ID of a channel
  // Sector numbering: 0,1,2, 3(small),4,5,6,7(small)
  int num =
    box +
    crate * fNTowsersPerCrate; // This convension reduced loop in search of possible neighbours sine in phi (crate) is smaller
  num+=fFirstCellInSector[sector] ;  
      
  num += chamber * fNTowersPerChamber;
  num += chamberH * fNTowersPerChamber * fNChambers;
  return num;
}
void MpdEmcGeoUtils::DetIdToRelIndex(int detId, int& chamber, int& sector, int& iphi, int& iz) const
{
  // Convert detId to iphi,iz indexes within one sector
  // We use consequent numbering of sectors

  int chamberH = detId / (fNTowersPerChamber * fNChambers);
  detId -= chamberH * fNTowersPerChamber * fNChambers;
  chamber = detId / (fNTowersPerChamber);
  detId -= chamber * fNTowersPerChamber;

  sector = detId / (fNTowsersPerCrate * fNCratesPerSector1);
  while((sector<fNSectors1PerChamber+fNSectors2PerChamber-1) && detId>=fFirstCellInSector[sector+1] ){
    sector++ ;
  }
  detId-=fFirstCellInSector[sector] ;
  iz = detId % fNTowsersPerCrate;
  iphi = detId / fNTowsersPerCrate;
}
void MpdEmcGeoUtils::DetIdToGlobalPosition(int detId, double& x, double& y, double& z) const
{
  // calculates senter of front surfase of tower with index detId
  if (!gGeoManager) {
    LOG(ERROR) << "Can not run without constructed geometry";
    x = y = z = 0;
    return;
  }

  const int cellsPerSector1 = fNTowsersPerCrate * fNCratesPerSector1;

  int chamberH = detId / (fNTowersPerChamber * fNChambers);
  detId -= chamberH * fNTowersPerChamber * fNChambers;
  int chamber = detId / (fNTowersPerChamber);
  detId -= chamber * fNTowersPerChamber;
  int sectorA, sectorB;
  int sector = detId / (fNTowsersPerCrate * fNCratesPerSector1);
  while((sector<fNSectors1PerChamber+fNSectors2PerChamber-1) && detId>=fFirstCellInSector[sector+1] ){
    sector++ ;
  }
  if(sector==2 || sector==6){
    sectorA = 2;
    sectorB = sector/4 ;    
  }
  else{
    sectorA = 1;
    sectorB = sector - sector/3 ;        
  }
  detId-=fFirstCellInSector[sector]  ;
  int crate = detId / fNTowsersPerCrate;
  int box = detId % fNTowsersPerCrate;

  // Construct geant pass from detId:
  TString path(Form("cave_1/emc1Chamber1_%d/emc1ChH_%d/emc1Sector%d_%d/emc1Crate_%d/emc1_box%d_%d", chamberH, chamber,
                    sectorA, sectorB, crate, box + 1, box));

  if (gGeoManager->CheckPath(path.Data())) {
    gGeoManager->cd(path.Data());
    TGeoHMatrix* m = gGeoManager->GetCurrentMatrix();
    // check sizes of this tower
    TGeoVolume* v = gGeoManager->GetVolume(Form("emc1_box%d", box + 1));
    TGeoArb8* sh = dynamic_cast<TGeoArb8*>(v->GetShape());
    double towerHalfSizeZ = sh->GetDz();
    double local[3] = { 0 }, global[3] = { 0 };
    if (chamber == 0) { // Why this is different in Chamber0 and Chamber1? Reflection?
      local[2] = -towerHalfSizeZ;
    } else {
      local[2] = towerHalfSizeZ;
    }
    m->LocalToMaster(local, global);
    x = global[0];
    y = global[1];
    z = global[2];
  } else {
    LOG(ERROR) << "Can not find volume " << path;
    x = y = z = 0;
  }
}

int MpdEmcGeoUtils::MaxPhiSector(int sector)const{
  //returns number of crates per sector
  if(sector==2 || sector==6){ 
    return fNCratesPerSector2-1;
  }
  else{  
    return fNCratesPerSector1-1;
  }
}
bool MpdEmcGeoUtils::IsPreviousSector(int sector1, int sector2)const{
  // check if sector1 goes before sector 2
  switch(sector1){
    case 0: return sector2==7 ;
    case 1: return sector2==0 ;
    case 2: return sector2==1 ;
    case 3: return sector2==2 ;
    case 4: return sector2==3 ;
    case 5: return sector2==4 ;
    case 6: return sector2==5 ;
    case 7: return sector2==6 ;
    default : return false;
  }
}
void MpdEmcGeoUtils::GetECALTubeSize(double& rMin, double& rMax, double& zMax)
{
  if (fEcalRmin == 0) {
    TGeoVolume* v = gGeoManager->GetVolume("emc1Chamber1");
    TGeoTube* sh = dynamic_cast<TGeoTube*>(v->GetShape());
    fEcalRmin = sh->GetRmin();
    fEcalRmax = sh->GetRmax();
    fEcalZmax = sh->GetDz();
  }
  rMin = fEcalRmin;
  rMax = fEcalRmax;
  zMax = fEcalZmax;
}
