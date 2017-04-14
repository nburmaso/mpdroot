//--------------------------------------------------------------------
//
// Description:
//      MPD EMC Digitizer - takes EmcPoints and makes digits
//
//
// Author List:
//      Alexander Zinchenko LHEP, JINR, Dubna - 8-May-2016
//
//--------------------------------------------------------------------

#include "MpdEmcDigitizer.h"
#include "MpdEmcGeoPar.h"
#include "MpdEmcDigit.h"
#include "MpdEmcPoint.h"

#include "FairMCPoint.h"
#include "FairMCTrack.h"
#include "FairRootManager.h"
#include "FairRunAna.h"

#include <TClonesArray.h>
#include <TGeoBBox.h>
#include <TGeoManager.h>
#include <TMath.h>

using namespace std;
using namespace TMath;

FILE *lun = fopen ("file.txt","w"); //AZ

// -----   Default constructor   -------------------------------------------

MpdEmcDigitizer::MpdEmcDigitizer() :
FairTask("EMC digitizer") {

}

// -----   Destructor   ----------------------------------------------------

MpdEmcDigitizer::~MpdEmcDigitizer() {
}
// -------------------------------------------------------------------------

// -----   Public method Init   --------------------------------------------

InitStatus MpdEmcDigitizer::Init() {

    cout << "******************* EMC Digitizer INIT *********************" << endl;

    // Get RootManager
    FairRootManager* ioman = FairRootManager::Instance();
    if (!ioman) {
        cout << "-E- MpdEmcDigitizer::Init: "
                << "RootManager not instantiated!" << endl;
        return kFATAL;
    }

    fGeoPar = new MpdEmcGeoPar();

    // Get input array
    fPointArray = (TClonesArray*) ioman->GetObject("EmcPoint");
    if (!fPointArray) {
        cout << "-W- MpdEmcDigitizer::Init: " << "No EmcPoint array!" << endl;
        return kERROR;
    }
    
    fMcTrackArray = (TClonesArray*) ioman->GetObject("MCTrack");
    if (!fMcTrackArray) {
        cout << "-W- MpdEmcDigitizer::Init: " << "No MCTrack array!" << endl;
        return kERROR;
    }

    // Create and register output array
    fDigiArray = new TClonesArray("MpdEmcDigit", 100);
    ioman->Register("EmcDigit", "EMC", fDigiArray, kTRUE);

    cout << "-I- MpdEmcDigitizer: Intialization successfull" << endl;

    return kSUCCESS;

}

//__________________________________________________________________________

void MpdEmcDigitizer::Finish() {
    cout << "-I- MpdEmcDigitizer: Finish" << endl;
}

//__________________________________________________________________________

void MpdEmcDigitizer::Exec(Option_t* opt) 
{
  // Main processing engine

  cout << "MpdEmcDigitizer::Exec started" << endl;

  static Double_t dzTower = fGeoPar->GetLengthOfModuleByZ();
  static Double_t dphiTower = TMath::TwoPi() / fGeoPar->GetNsec() / fGeoPar->GetNsupMod() / fGeoPar->GetNModInSuperModByPhi();

  // Reset output Array
  if (!fDigiArray) Fatal("MpdEmcDigitizer::Exec", "No array of digits");
  fDigiArray->Delete();
  fHitMap.clear();
  Int_t nPoints = fPointArray->GetEntriesFast();

  for (Int_t iPnt = 0; iPnt < nPoints; ++iPnt) {
    FairMCPoint* pnt = (FairMCPoint*) fPointArray->UncheckedAt(iPnt);
    Int_t trId = pnt->GetTrackID();
    if (trId < 0) continue; // strange case
    FairMCTrack* tr = (FairMCTrack*) fMcTrackArray->UncheckedAt(trId);
    //        if (tr->GetMotherId() != -1) continue;
    Int_t pdg = tr->GetPdgCode();
    Float_t x = pnt->GetX();
    Float_t y = pnt->GetY();
    Float_t z = pnt->GetZ();
    /*
    UInt_t sec = GetSecId(x, y, z);
    UInt_t row = GetRowId(z);
    UInt_t supMod = GetSupModId(x, y, z, sec);
    UInt_t mod = GetModId(x, y, supMod, sec);
    */
    Float_t e = pnt->GetEnergyLoss();
    //AZ
    gGeoManager->FindNode(x,y,z); 
    TString path(gGeoManager->GetPath());
    //cout << path << endl; 
    //if (!path.Contains("sc")) exit(0); 
    if (!path.Contains("sc")) continue; 
    gGeoManager->CdUp(); // to tower (channel)
    Int_t ip = path.Index("ChH");
    UInt_t ih = TString(path(ip+4,1)).Atoi();
    ip = path.Index("Sector");
    Int_t ip1 = path.Index("/",ip);
    UInt_t isec = TString(path(ip+7,ip1)).Atoi();
    ip = path.Index("Tube");
    ip1 = path.Index("/",ip);
    UInt_t itube = TString(path(ip+5,ip1)).Atoi();
    ip = path.Index("box");
    UInt_t ibox = 1;
    if (ip >= 0) {
      // Tower 4x4 cm
      ibox = TString(path(ip+4,1)).Atoi();
      if (ibox > 3 || !path.Contains("bt_box")) ibox += 3; // central and edge towers
    }
    //cout << ih << " " << isec << " " << itube << " " << ibox << endl;
    fprintf(lun,"%d %d %d %d %f %f\n",ih,isec,itube,ibox,TMath::ATan2(y,x),z);
    // Code channel ID
    TString tower = "c"; // channel
    tower += ibox; 
    tower += "m"; // module
    tower += itube;
    tower += "s"; // sector
    tower += isec;
    tower += "h"; // EMC half (Z <> 0)
    tower += ih;
    //AZ
    
    MpdEmcDigit* hit = SearchHit(tower);
    if (hit == NULL) {
      hit = new((*fDigiArray)[fDigiArray->GetEntriesFast()]) MpdEmcDigit(ih-1, isec-1, itube-1, ibox-1);
      fHitMap[tower] = hit;
      Double_t phi, z;
      FindChanPhiZ(phi,z);
      hit->SetPhiCenter(phi);
      hit->SetZCenter(z);
      Int_t iz = (z / dzTower + 0.5);
      if (iz <= 0) --iz;
      hit->SetChanZId(TMath::Abs(iz));
      Int_t iphi = phi / dphiTower;
      //hit->SetChanPhiId(iphi % (fGeoPar->GetNsupMod() * fGeoPar->GetNModInSuperModByPhi()));
      hit->SetChanPhiId(iphi);
      hit->SetTimeStamp(pnt->GetTime());
    } 
    hit->IncreaseEnergy(e, trId);
    hit->SetTimeStamp(TMath::Min(pnt->GetTime(),hit->GetTimeStamp()));
    hit->SetNumTracks(hit->GetNumTracks() + 1);
    //hit->SetTrackId(trId);
    hit->SetPdg(pdg);
    //AZ hit->SetZCenter(CalcZCenter(sec, row, mod));
    //AZ hit->SetPhiCenter(CalcPhiCenter(sec, supMod, mod));

    // Redo timing - take time from the highest energy deposit
    if (TMath::Abs(e-hit->GetDy()) < 1.e-6) {
      hit->SetTimeStamp(pnt->GetTime()); // Dy - holder of Emax
      hit->SetTrackId(iPnt); // point index
    }
  }
  
  Int_t nHits = fDigiArray->GetEntriesFast();
  for (Int_t iHit = 0; iHit < nHits; ++iHit) {
    MpdEmcDigit* hit = (MpdEmcDigit*) fDigiArray->UncheckedAt(iHit);
    if (hit->GetNumTracks() > 1) {
      hit->SetPdg(0);
      //hit->SetTrackId(-1);
    }
    //hit->Print();
  }    

  // Redo track ID numbering 
  //RedoId();
}

//__________________________________________________________________________

void MpdEmcDigitizer::RedoId(TClonesArray *digis, TClonesArray *mctrs)
{
  // Redo track ID numbering 
  // Take IDs of particles produced outside EMC

  static const Double_t rmin = fGeoPar->GetRmin() / 10, rmax = fGeoPar->GetRmax() / 10;
  TVector3 vert;
  Int_t nDigis = digis->GetEntriesFast();

  for (Int_t iDig = 0; iDig < nDigis; ++iDig) {
    MpdEmcDigit* digi = (MpdEmcDigit*) digis->UncheckedAt(iDig);
    map<Int_t,Float_t> contrib = digi->GetContrib();
    map<Int_t,Float_t> copy(contrib);
    contrib.clear();

    for (map<Int_t,Float_t>::iterator it = copy.begin(); it != copy.end(); ++it) {
      Int_t id = it->first, idm = -1;
      FairMCTrack *mctr = (FairMCTrack*) mctrs->UncheckedAt(id);
      mctr->GetStartVertex(vert);
      Double_t rvert = vert.Pt();
      while (rvert > rmin && rvert < rmax) {
	// Born inside EMC - find ancestor
	idm = mctr->GetMotherId();
	if (idm < 0) break;
	mctr = (FairMCTrack*) mctrs->UncheckedAt(idm);
	mctr->GetStartVertex(vert);
	rvert = vert.Pt();
      }
      if (idm >= 0) id = idm;
      if (contrib.find(id) == contrib.end()) contrib[id] = it->second;
      else contrib[id] += it->second;
    }
  }    
}

//__________________________________________________________________________

UInt_t MpdEmcDigitizer::GetSecId(Float_t x, Float_t y, Float_t z) {
    Float_t ang = ATan2(y, x);
    if (ang < 0) ang += TwoPi();
    ang *= RadToDeg();
    Int_t nSec = fGeoPar->GetNsec();
    if (z > 0.0)
        return UInt_t(ang / 360 * nSec);
    else
        return nSec + UInt_t(ang / 360 * nSec);
}

//__________________________________________________________________________

UInt_t MpdEmcDigitizer::GetSupModId(Float_t x, Float_t y, Float_t z, UInt_t sec) {
    Float_t ang = ATan2(y, x);
    if (ang < 0) ang += TwoPi();
    ang *= RadToDeg();
    Float_t nDegreesInOneSector = fGeoPar->GetAngleOfSector();
    if (z < 0.0)
        sec -= fGeoPar->GetNsec();

    Float_t secStartAng = sec * nDegreesInOneSector;
    Float_t secFinishAng = secStartAng + nDegreesInOneSector;
    Float_t localAng = ang - secStartAng;

    return UInt_t(localAng * fGeoPar->GetNsupMod() / nDegreesInOneSector);
}

//__________________________________________________________________________

UInt_t MpdEmcDigitizer::GetModId(Float_t x, Float_t y, UInt_t supMod, UInt_t sec) {
    Float_t ang = ATan2(y, x);
    if (ang < 0) ang += TwoPi();
    ang *= RadToDeg();
        
    Float_t secStartAng = (sec % fGeoPar->GetNsec()) * fGeoPar->GetAngleOfSector();
    Float_t supModStartAng = secStartAng + supMod * fGeoPar->GetAngleOfSuperModule();
    Float_t localAng = ang - supModStartAng;
    
    return UInt_t(localAng * fGeoPar->GetNmod() / fGeoPar->GetAngleOfSuperModule());
}

//__________________________________________________________________________

UInt_t MpdEmcDigitizer::GetRowId(Float_t z) {
    return UInt_t(Abs(z) / fGeoPar->GetLength() * fGeoPar->GetNrows());
}

//__________________________________________________________________________

void MpdEmcDigitizer::FindChanPhiZ(Double_t &phi, Double_t &z) 
{
  // Compute Z-position and Phi-angle of the tower (channel) center (at outer Z)

  TString path(gGeoManager->GetPath());
  Int_t ip = path.Last('/');
  Int_t ip1 = path.Last('_');
  TString volName = path(ip+1,ip1-ip-1);
  TGeoVolume *tower = gGeoManager->GetVolume(volName);
  TGeoBBox *box = (TGeoBBox*) tower->GetShape();
  Double_t xyzL[3] = {0,box->GetDY(),0}, xyzM[3];
  gGeoManager->LocalToMaster(xyzL,xyzM);
  z = xyzM[2];
  phi = TMath::ATan2 (xyzM[1],xyzM[0]);
  if (phi < 0) phi += TMath::TwoPi();
}

//__________________________________________________________________________

Float_t MpdEmcDigitizer::CalcZCenter(UInt_t sec, UInt_t row, UInt_t mod) 
{
  // Compute Z-position of the tower (channel) center

  /*
  Float_t lengthOfModuleByZ = fGeoPar->GetLengthOfModuleByZ();
  Float_t lengthOfSuperModuleByZ = fGeoPar->GetLengthOfSuperModuleByZ();
  Float_t halfLengthOfModuleByZ = lengthOfModuleByZ / 2.0;
  Float_t leftEdgeOfModuleByZ = row * lengthOfSuperModuleByZ + (mod % fGeoPar->GetNModInSuperModByZ()) * lengthOfModuleByZ;
  Float_t z = leftEdgeOfModuleByZ + halfLengthOfModuleByZ;
  return (sec < fGeoPar->GetNsec()) ? z : -z;
  */
  return 0;
}

//__________________________________________________________________________

Float_t MpdEmcDigitizer::CalcPhiCenter(UInt_t sec, UInt_t supMod, UInt_t mod) 
{
  // Compute Phi-angle of the tower (chanel) center at outer Z position

  /*
  Float_t sectorAngle = fGeoPar->GetAngleOfSector();
  Float_t supModAngle = fGeoPar->GetAngleOfSuperModule();
  Float_t modAngle = fGeoPar->GetAngleOfModule();
  
  Int_t modIdInSupModByPhi = mod / fGeoPar->GetNModInSuperModByPhi(); // 0, 1, 2
  Float_t sectorAngleEdge = sectorAngle * (sec % fGeoPar->GetNsec());
  Float_t supModAngleEdge = supModAngle * supMod;
  Float_t modAngleEdge = modAngle * modIdInSupModByPhi;
  return sectorAngleEdge + supModAngleEdge + modAngleEdge + modAngle / 2.0;
  */
  return 0;
}

//__________________________________________________________________________

MpdEmcDigit* MpdEmcDigitizer::SearchHit(TString tower)
{
  // Check if the hit (tower) exists in the map

  if (fHitMap.find(tower) == fHitMap.end()) return NULL;
  return fHitMap[tower];
}

// -------------------------------------------------------------------------

ClassImp(MpdEmcDigitizer)
