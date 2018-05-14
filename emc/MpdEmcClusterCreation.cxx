////////////////////////////////////////////////////////////////
//							      //
//  MpdEmcClusterCreation				      //
//  EMC clsuters in MpdEmcCluster, v01  		      //
//  Author List : Martemianov M., ITEP, 2017		      //	
//  							      //	
//////////////////////////////////////////////////////////////// 


#include "TClonesArray.h"
#include "FairRootManager.h"
#include "FairRunAna.h"
#include "FairRunSim.h"
#include "FairEventHeader.h"

#include "MpdEmcHit.h"
#include "MpdEmcCluster.h"
#include "MpdEmcClusterCreation.h"
#include "FairMCTrack.h"

#include "TGeoManager.h"

namespace EMC {
    UInt_t nRows = 336; // number of rows (xy - plane)
    UInt_t nMods = 128; // number of modules in row
    Float_t Distance3DCut = 20.0; // cm
    Float_t HitThreshold = 5.0; // MeV
    UInt_t algorithmNum = 2; // algorithm number
    UInt_t frameRow = 5; // row frame
    UInt_t frameMod = 5; // module frame 
}

using namespace std;
using namespace TMath;


// -----   Default constructor   -------------------------------------------

MpdEmcClusterCreation::MpdEmcClusterCreation() :
	FairTask("EMC cluster creation"), fEnergyThreshold(EMC::HitThreshold), fMaxClusterRadius(EMC::Distance3DCut),
	algoIndex(EMC::algorithmNum), rowFrame(EMC::frameRow), modFrame(EMC::frameMod){
}


// -----   Destructor   ----------------------------------------------------

MpdEmcClusterCreation::~MpdEmcClusterCreation() {
}
// -------------------------------------------------------------------------

// -----   Public method Init   --------------------------------------------

InitStatus MpdEmcClusterCreation::Init() {

    cout << "******************* EMC INIT *********************" << endl;

// Get RootManager


    FairRootManager* ioman = FairRootManager::Instance();
    if (!ioman) {
        cout << "-E- MpdEmcClusterCreation::Init: "
                << "RootManager not instantiated!" << endl;
        return kFATAL;
    }


// Get input array

    fHitArray = (TClonesArray*) ioman->GetObject("MpdEmcHit");
    if (!fHitArray) {
        cout << "-W- MpdEmcHitCreation::Init: " << "No MpdEmcHit array!" << endl;

        return kERROR;
    }
    
    fMcTrackArray = (TClonesArray*) ioman->GetObject("MCTrack");
    if (!fMcTrackArray) {
        cout << "-W- MpdEmcClusterCreation::Init: " << "No MCTrack array!" << endl;
        return kERROR;
    }

    // Create and register output array

    fClusterArray = new TClonesArray("MpdEmcCluster", 100);
    ioman->Register("MpdEmcCluster", "EMC", fClusterArray, kTRUE);
    ioman->Register("MCTrack","EMC",fMcTrackArray, kTRUE);

    cout << "-I- MpdEmcClusterCreation: Intialization successfull" << endl;

    return kSUCCESS;

}

void MpdEmcClusterCreation::Finish() {

    cout << "\n-I- MpdEmcClusterCreation: Finish" << endl;

}

void MpdEmcClusterCreation::Exec(Option_t* opt) {

   cout << "\n-I- MpdEmcClusterCreation: Event No. " << FairRun::Instance()->GetEventHeader()->GetMCEntryNumber() << endl;

// Reset output Array

    if (!fClusterArray) Fatal("MpdEmcClusterCreation::Exec)", "No array of clusters");
    fClusterArray->Delete();

    UInt_t nHits = fHitArray->GetEntriesFast();
	
    MpdEmcCluster *cluster = NULL;
   
    Bool_t addHit, addCut;
    vector<UInt_t> modId, relHits, supHits, outHits;
    vector<vector<UInt_t> > relCluster;
    vector<Float_t> xHit, yHit, zHit;
    vector<Float_t> energy, time;
   
    if (nHits > 0) {

     for (UInt_t iHit = 0; iHit < nHits; ++iHit) { 
      
       MpdEmcHit* hit = (MpdEmcHit*)fHitArray->At(iHit);    
       if (hit->GetE()*1000. > fEnergyThreshold) {
        modId.push_back(1000*(hit->GetRow()+1)+(hit->GetMod()+1));
        xHit.push_back(hit->GetRhoCenter()*cos(hit->GetPhiCenter()*DegToRad()));
        yHit.push_back(hit->GetRhoCenter()*sin(hit->GetPhiCenter()*DegToRad()));
        zHit.push_back(hit->GetZCenter());
        energy.push_back(hit->GetE());
        time.push_back(hit->GetTime());
       }
     }

     UInt_t row, mod;
     vector<Float_t> iterEnergy = energy;     

     while (iterEnergy.size()) {

       relHits.clear(); outHits.clear();
       UInt_t indexMax = distance(iterEnergy.begin(), max_element(iterEnergy.begin(), iterEnergy.end()));
       if (iterEnergy.size() == 1) indexMax = 0; 
       indexMax = distance(energy.begin(),find(energy.begin(),energy.end(),iterEnergy[indexMax]));
       relHits.push_back(indexMax); 
       row = modId[indexMax]/1000; mod = modId[indexMax] - row*1000;
       SearchFrameHits(row, mod, outHits);

        for (UInt_t iHit = 0; iHit < energy.size(); iHit++) {

         Float_t dist = sqrt(pow(xHit[indexMax] - xHit[iHit],2) + pow(yHit[indexMax] - yHit[iHit],2) +
		    pow(zHit[indexMax] - zHit[iHit],2)); 
         addHit = kTRUE; addCut = kFALSE;

         for (UInt_t iAdd = 0; iAdd < supHits.size(); iAdd++) 
				if (supHits[iAdd] == iHit) addHit = kFALSE; 

         if (algoIndex == 1) addCut = (dist < fMaxClusterRadius);

         if (algoIndex == 2) {
	  for (UInt_t iRel = 0; iRel < outHits.size(); iRel++) 
           	if (modId[iHit] == outHits[iRel]) addCut = kTRUE;
         }

         if ( (addCut) && (addHit) ) {
          UInt_t pos = distance(iterEnergy.begin(),find(iterEnergy.begin(),iterEnergy.end(),energy[iHit]));
          if (iterEnergy.size() == 1) pos = 0;
          iterEnergy.erase(iterEnergy.begin()+pos);
          if (indexMax != iHit) relHits.insert(relHits.end(), iHit); 
         }        
        }
        for (UInt_t iRel = 0; iRel < relHits.size(); iRel++) 
			supHits.insert(supHits.end(), relHits[iRel]);
        relCluster.push_back(relHits);	
       }

    }

// Fill cluster information (0,0 - central hit)

    TVector3 fPos; 
    UInt_t numHits; 
    Float_t rho, phi, theta, rhoCenter, thetaCenter, phiCenter;
    Float_t thetaPos, phiPos, rhoPos, zPos, fEnergy, fTime;
    vector<Float_t> eH, xH, yH, zH;

    for (UInt_t iCluster = 0; iCluster < relCluster.size(); iCluster++) {
      fEnergy = 0.; fTime = 0.; phiPos = 0.; thetaPos = 0.;
 
      numHits = relCluster[iCluster].size();
      eH.clear(); xH.clear(); yH.clear(); zH.clear();

      for (UInt_t iHit = 0; iHit < relCluster[iCluster].size(); iHit++) {

        eH.insert(eH.end(), energy[relCluster[iCluster][iHit]]);
        xH.insert(xH.end(),xHit[relCluster[iCluster][iHit]]);
        yH.insert(yH.end(),yHit[relCluster[iCluster][iHit]]); 
        zH.insert(zH.end(),zHit[relCluster[iCluster][iHit]]);
        rho = sqrt(xH[iHit]*xH[iHit]+yH[iHit]*yH[iHit]+zH[iHit]*zH[iHit]);
        theta = ACos(zH[iHit]/rho);
	phi = ATan2(yH[iHit],xH[iHit])*RadToDeg();
        if (phi < 0) phi += 360;
        if (iHit == 0) {
	 phiCenter = phi; rhoCenter = rho; thetaCenter = theta; 
        }

	fEnergy += eH[iHit]; thetaPos += theta*eH[iHit];  
        phiPos += MpdEmcMath::GetPhiDiff(phi, phiCenter)*eH[iHit]; 
        fTime += time[relCluster[iCluster][iHit]]*eH[iHit];
        
      }

       thetaPos = thetaPos/fEnergy;
       phiPos = phiCenter + phiPos/fEnergy;
       fTime = fTime/fEnergy;    
       rhoPos = rhoCenter*sin(thetaPos); 
       zPos = rhoCenter*cos(thetaPos);
       fPos.SetXYZ(phiPos, rhoPos, zPos);   

       cluster = new((*fClusterArray)[fClusterArray->GetEntriesFast()]) MpdEmcCluster(fEnergy, fTime, fPos);
       cluster->SetNHits(numHits); 
       cluster->SetRad(cluster->ComputeClusterRadius(fPos, xH, yH, zH, eH));

    }
  }

// Search relative hits in the definite frame 

void MpdEmcClusterCreation::SearchFrameHits(UInt_t row, UInt_t mod, vector<UInt_t> &outMod) {

      UInt_t ind, curRow; 
      UInt_t nRow = EMC::nRows;
      UInt_t nMod = EMC::nMods;
      vector<Int_t> geoMod;
      Int_t dRowMin = row - rowFrame, dRowMax = row + rowFrame; 
      Int_t dModMin = mod - modFrame, dModMax = mod + modFrame;

      if (dRowMin <= 0) dRowMin = nRow - fabs(dRowMin);       
      if (dRowMax >= nRow) dRowMax = dRowMax - nRow; 
      if (dModMin <= 0) dModMin = 1; 
      if (dModMax >= nMod) dModMax = nMod; 

      curRow = dRowMin;
      for (UInt_t iGeo1 = 0; iGeo1 < 2*rowFrame + 1; iGeo1++) {
       if (curRow == nRow + 1) { curRow = 1;}
       for (UInt_t iGeo2 = dModMin; iGeo2 < dModMax + 1; iGeo2++) 
         geoMod.insert(geoMod.end(), 1000*curRow + iGeo2);
       curRow++;
      }
      
     outMod.clear();
     for (UInt_t iHit = 0; iHit < fHitArray->GetEntriesFast(); iHit++) {
       MpdEmcHit* hit = (MpdEmcHit*)fHitArray->At(iHit);
       ind = 1000*(hit->GetRow()+1) + (hit->GetMod()+1);
       for (UInt_t iGeo = 0; iGeo < geoMod.size(); iGeo++)
        if (ind == geoMod[iGeo]) outMod.push_back(ind);
     }

}

// Search relative hits

void MpdEmcClusterCreation::SearchRelativeHits(UInt_t row, UInt_t mod, vector<Int_t> &outMod) {
  
      UInt_t ind; 
      UInt_t nRow = EMC::nRows; 
      UInt_t nMod = EMC::nMods;
      vector<Int_t> geoMod;
      
      row = row + 1; mod = mod + 1;
      UInt_t rowPrev = row-1, rowNext = row+1;
      if (row == 1) rowPrev = nRow; 
      if (row == nRow) rowNext = 1; 
      geoMod = MpdEmcMath::make_vector<int>() <<
	1000*rowPrev + (mod-1) << 1000*rowPrev + (mod)   << 1000*rowPrev + (mod+1) << 
	1000*row     + (mod-1) << 1000*row     + (mod+1) << 1000*rowNext + (mod-1) <<
        1000*rowNext + (mod)   << 1000*rowNext + (mod+1);

      if (mod - 1 == 0)   geoMod.erase(geoMod.begin()+1,geoMod.begin()+3);
      if (mod + 1 > nMod) geoMod.erase(geoMod.begin()+5,geoMod.begin()+7);

     outMod.clear();
     outMod.insert(outMod.begin(), 1000*(row) + mod);
     for (UInt_t iHit = 0; iHit < fHitArray->GetEntriesFast(); iHit++) {
       MpdEmcHit* hit = (MpdEmcHit*)fHitArray->At(iHit);
       ind = 1000*(hit->GetRow()+1) + (hit->GetMod()+1);
       for (UInt_t iGeo = 0; iGeo < geoMod.size(); iGeo++)
        if (ind == geoMod[iGeo]) outMod.push_back(ind);
     }
   
}

// -------------------------------------------------------------------------

ClassImp(MpdEmcClusterCreation)
