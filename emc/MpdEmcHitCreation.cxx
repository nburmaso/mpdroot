////////////////////////////////////////////////////////////////
//                                                            //
//  MpdEmcHitCreation                                         //
//  Hit production for EMC, v01                        	      //
//  Author List : Martemianov M., ITEP, 2017                  //
//                                                            //
////////////////////////////////////////////////////////////////

#include "TClonesArray.h"
#include "FairRootManager.h"
#include "TMath.h"
#include "TGeoNode.h"
#include "TGeoManager.h"
#include "FairRunAna.h"
#include "FairEventHeader.h"

#include "MpdEmcHitCreation.h"
#include "MpdEmcGeoParams.h"
#include "MpdEmcHit.h"
#include "MpdEmcPoint.h"
#include "FairMCPoint.h"
#include "FairMCTrack.h"

using namespace std;
using namespace TMath;

// -----   Default constructor   -------------------------------------------

MpdEmcHitCreation::MpdEmcHitCreation() :
FairTask("Ideal EMC hit Creation") {

}


// -----   Destructor   ----------------------------------------------------

MpdEmcHitCreation::~MpdEmcHitCreation() {
}
// -------------------------------------------------------------------------



// -----   Public method Init   --------------------------------------------

InitStatus MpdEmcHitCreation::Init() {

    cout << "******************* EMC INIT *********************" << endl;

// Get RootManager
    FairRootManager* ioman = FairRootManager::Instance();
    if (!ioman) {
        cout << "-E- MpdEmcHitCreation::Init: "
                << "RootManager not instantiated!" << endl;
        return kFATAL;
    }


// Get geometry EMC parameters
    fGeoPar = new MpdEmcGeoParams();

    // Get input array
    fPointArray = (TClonesArray*) ioman->GetObject("EmcPoint");
    if (!fPointArray) {
        cout << "-W- MpdEmcHitCreation::Init: " << "No EmcPoint array!" << endl;

        return kERROR;
    }
    
    fMcTrackArray = (TClonesArray*) ioman->GetObject("MCTrack");
    if (!fMcTrackArray) {
        cout << "-W- MpdEmcHitCreation::Init: " << "No MCTrack array!" << endl;
        return kERROR;
    }

    // Create and register output array
    fDigiArray = new TClonesArray("MpdEmcHit", 100);
    ioman->Register("MpdEmcHit", "EMC", fDigiArray, kTRUE);

    ioman->Register("MCTrack","EMC",fMcTrackArray, kTRUE);

    cout << "-I- MpdEmcHitCreation: Intialization successfull" << endl;

    return kSUCCESS;

}

void MpdEmcHitCreation::Finish() {


    cout << "\n-I- MpdEmcHitCreation: Finish" << endl;

}

void MpdEmcHitCreation::Exec(Option_t* opt) {

   cout << "\n-I- MpdEmcHitCreation: Event No. " << FairRun::Instance()->GetEventHeader()->GetMCEntryNumber() << endl;

// Reset output Array

    if (!fDigiArray) Fatal("MpdEmcHitCreation::Exec)", "No array of digits");
    fDigiArray->Delete();

    Double_t phiRow, rhoMod, zMod, thetaMod; 

    for (UInt_t iPnt = 0; iPnt < fPointArray->GetEntriesFast(); ++iPnt) {
        FairMCPoint* pnt = (FairMCPoint*) fPointArray->At(iPnt);
        Int_t trId = pnt->GetTrackID();
        FairMCTrack* tr = (FairMCTrack*) fMcTrackArray->At(trId);
//        if (tr->GetMotherId() != -1) continue;
        Int_t pdg = tr->GetPdgCode();
        Float_t x = pnt->GetX();
        Float_t y = pnt->GetY();
        Float_t z = pnt->GetZ();


// Last part (to be supressed) 

//        TGeoNode* currentNode = gGeoManager->FindNode(x,y,z);
//        TString path(gGeoManager->GetPath());
//        TString nameNode(currentNode->GetName());
//        cout << " Path : " << path.Data() << endl; 
//

// Get sector number of module

        Int_t sec = GetSecId(x, y, z);

// Get row number of module

        Int_t row = GetRowId(x, y, phiRow);

// Get module number in row

        Int_t mod = GetModId(x, y, z, phiRow, rhoMod, zMod, thetaMod);
        Int_t supMod = -1;  
 
        Float_t e = pnt->GetEnergyLoss();
        Float_t timeEnergy = pnt->GetTime()*e;
         
        MpdEmcHit* hit = SearchHit(sec, row, mod);
        if (hit == NULL) {
            hit = new((*fDigiArray)[fDigiArray->GetEntriesFast()]) MpdEmcHit(sec, row, supMod, mod, e, timeEnergy);
        } else {
            hit->IncreaseEnergy(e);
	    hit->IncreaseEnergyTime(timeEnergy);
        }

        hit->SetNumTracks(hit->GetNumTracks() + 1);
        hit->SetTrackId(trId);
        hit->SetPdg(pdg);
        hit->SetPhiCenter(phiRow);
        hit->SetRhoCenter(rhoMod);
        hit->SetZCenter(zMod);
        hit->SetThetaCenter(thetaMod);

    }
    
   for (int iHit = 0; iHit < fDigiArray->GetEntriesFast(); iHit++) {
      MpdEmcHit* hit = (MpdEmcHit*) fDigiArray->At(iHit);
      hit->SetTime(hit->GetTime()/hit->GetE());
      if (hit->GetNumTracks() > 1) {
        hit->SetPdg(0);
        hit->SetTrackId(-1);
      }	
   }
   
}

// Define sector number 

Int_t MpdEmcHitCreation::GetSecId(Double_t x, Double_t y, Double_t z) {

    Int_t index = -1;
    const Int_t nSec = fGeoPar->GetNsec();
    const Double_t phiSectorAngle1 = fGeoPar->GetSizeAngleSector1();
    const Double_t phiSectorAngle2 = fGeoPar->GetSizeAngleSector2();
    vector<double> phiSector = fGeoPar->GetPhiSector();
     
    Double_t phiMinSector, phiMaxSector, phiMinLast;
    Double_t ang = ATan2(y, x)*RadToDeg();
    if (ang < 0) ang += 360.;
    for (int iSector = 0; iSector < 0.5*nSec; iSector++) {
     phiMinSector = phiSector[iSector] - 0.5*phiSectorAngle1; 
     phiMaxSector = phiSector[iSector] + 0.5*phiSectorAngle1; 
     if ( phiMinSector < 0 ) phiMinSector = 0;
     if ( (iSector == 2) || (iSector == 6) ) {
      phiMinSector = phiSector[iSector] - 0.5*phiSectorAngle2;
      phiMaxSector = phiSector[iSector] + 0.5*phiSectorAngle2;
     }      	
      if ( (ang > phiMinSector) && (ang < phiMaxSector) ) index = iSector; 
    }
    if ( (ang > 360.-0.5*phiSectorAngle1) && (ang < 360.) ) index = 0;
    if (z < 0) index += 0.5*nSec;

   return index;
}

// Define row number 

Int_t MpdEmcHitCreation::GetRowId(Double_t x, Double_t y, Double_t &phiRowMod) {

    Int_t index = -1;
    const Int_t nRow = fGeoPar->GetNrows();
    vector<double> phiRow = fGeoPar->GetPhiRow(); 
    vector<double> diffR; 
    Double_t ang = ATan2(y, x)*RadToDeg();
    if (ang < 0) ang += 360.;

    for (int i = 0; i < nRow; i++)  diffR.push_back(fabs(phiRow[i] - ang));    
    index = LocMin(diffR.size(), &diffR[0]);
    phiRowMod =  phiRow[index];
   
    return index;
}

Int_t MpdEmcHitCreation::GetModId(Double_t x, Double_t y, Double_t z, Double_t phiRowMod,  
		Double_t &rhoMod, Double_t &zMod, Double_t &thetaMod) {

    Int_t index = -1; 

// Box parameters
    const Int_t nMod = fGeoPar->GetNmod();
    const Double_t boxSizeLow = fGeoPar->GetSizeLowBox();
    const Double_t boxSizeHigh = fGeoPar->GetSizeHighBox();
    const Double_t boxLength = fGeoPar->GetLengthBox();

// Geometry sets
    vector<double> boxTheta = fGeoPar->GetThetaBox(); 
    vector<double> boxRhoCenter = fGeoPar->GetRhoCenterBox(); 
    vector<double> boxZCenter = fGeoPar->GetZCenterBox();
    vector<double> diffTheta;

    Double_t ang = ATan2(y, x);
    if (ang < 0) ang += TwoPi();
    Double_t cosDeltaPhi = cos(phiRowMod*DegToRad() - ang);

    Double_t rhoMod0, zMod0, rhoPoint, thetaPoint, lenTotal, thetaRad; 
    Double_t dMeanTheta = ATan2(0.5*(boxSizeHigh - boxSizeLow),boxLength)*RadToDeg();
    
    rhoPoint = sqrt(x*x + y*y)*cosDeltaPhi;
 
      for (Int_t iMod = 0.5*nMod; iMod < nMod; iMod++) {

       thetaRad = boxTheta[iMod]*DegToRad();
       lenTotal = boxSizeHigh/tan(dMeanTheta*DegToRad()) - boxLength;
       lenTotal = lenTotal - boxRhoCenter[iMod]/sin(thetaRad);
       rhoMod0 = sin(thetaRad)*lenTotal;
       zMod0 = boxZCenter[iMod] - boxRhoCenter[iMod]/tan(thetaRad) - lenTotal*cos(thetaRad);    
       thetaPoint = ATan2(rhoPoint + rhoMod0, fabs(z) - zMod0)*RadToDeg(); 
       diffTheta.push_back(fabs(boxTheta[iMod] - thetaPoint));
  
     }

     index = LocMin(diffTheta.size(), &diffTheta[0]) + 0.5*nMod;
     if (z < 0) index = nMod - index - 1;
     thetaMod = boxTheta[index]; 
     rhoMod = boxRhoCenter[index]; 
     zMod = boxZCenter[index];
   
    return index;

}

MpdEmcHit* MpdEmcHitCreation::SearchHit(UInt_t sec, UInt_t row, UInt_t mod) {
    MpdEmcHit* foundHit = NULL;
    for (Int_t i = 0; i < fDigiArray->GetEntriesFast(); ++i) {
        MpdEmcHit* hit = (MpdEmcHit*) fDigiArray->At(i);
        if (hit->GetSec() != sec) continue;
        if (hit->GetRow() != row) continue;
        if (hit->GetMod() != mod) continue;
        foundHit = hit;
    }
    return foundHit;
}

// -------------------------------------------------------------------------

ClassImp(MpdEmcHitCreation)
