////////////////////////////////////////////////////////////////
//                                                            //
//  MpdEmcHitCreation                                         //
//  Hit production for EMC, v02                        	      //
//  Author List : Martemianov M., 2019                        //
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
#include "MpdMCTrack.h"

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
    ioman->Register("MCTrack","EMC",fMcTrackArray, kFALSE);
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

    //Double_t phiRow, rhoMod, zMod, thetaMod; 
    for (UInt_t iPnt = 0; iPnt < (UInt_t) fPointArray->GetEntriesFast(); ++iPnt) {

        FairMCPoint* pnt = (FairMCPoint*) fPointArray->At(iPnt);
        Int_t trId = pnt->GetTrackID();
        if (trId < 0) break;  
        MpdMCTrack* tr = (MpdMCTrack*) fMcTrackArray->At(trId);
//        if (tr->GetMotherId() != -1) continue;
        Int_t pdg = tr->GetPdgCode();
        Float_t x = pnt->GetX();
        Float_t y = pnt->GetY();
        Float_t z = pnt->GetZ();

// Path to point 

        //TGeoNode* currentNode = gGeoManager->FindNode(x,y,z);
        //TString path(gGeoManager->GetPath());

// Get sector number of module

        Int_t sec = GetSecId(x, y);

// Get row number of module

        Int_t row = GetRowId(x, y);
         
// Get module number in row

        Double_t xTower, yTower, zTower, phiTower, thetaTower; 
        Int_t tower = GetTowerId(x, y, z, row, xTower, yTower, zTower, phiTower, thetaTower);
        Int_t supMod = -1; 

        Float_t e = pnt->GetEnergyLoss();
        Float_t timeEnergy = pnt->GetTime()*e;
        MpdEmcHit* hit = SearchHit(sec, row, tower);

        if (hit == NULL) {
            hit = new((*fDigiArray)[fDigiArray->GetEntriesFast()]) MpdEmcHit(sec, row, supMod, tower, e, timeEnergy);
        } else {
            hit->IncreaseEnergy(e);
	    hit->IncreaseEnergyTime(timeEnergy);
        }

        hit->SetNumTracks(hit->GetNumTracks() + 1);
        hit->SetTrackId(trId);
        hit->SetPdg(pdg);
        hit->SetRhoCenter(sqrt(xTower*xTower+yTower*yTower));
        hit->SetZCenter(zTower);
        hit->SetPhiCenter(phiTower);
        hit->SetThetaCenter(thetaTower);

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

Int_t MpdEmcHitCreation::GetSecId(Double_t x, Double_t y) {

    Int_t index = -1;
    vector<double> diffSector; 
    const Int_t nSec = fGeoPar->GetNsec();
    vector<double> phiSector = fGeoPar->GetPhiSector();
    Double_t ang = ATan2(y,x)*RadToDeg();
    const Double_t fSectorAngle = fGeoPar->GetSizeAngleSector();
    if (ang < 0) ang += 360.;
    for (int iSector = 0; iSector < nSec; iSector++)
	diffSector.push_back(fabs(phiSector[iSector] - ang));
    
    index = LocMin(diffSector.size(), &diffSector[0]);
    if ( (ang > 0) && (ang < phiSector[0] - 0.5*fSectorAngle) ) index = phiSector.size() - 1; 

    return index; 

  }

// Define row number 

Int_t MpdEmcHitCreation::GetRowId(Double_t x, Double_t y) {

    Int_t index = -1;
    vector<double> diffR; 
    const Int_t nRow = fGeoPar->GetNrows();
    vector<double> phiRow = fGeoPar->GetPhiRow(); 
    vector<double> xRow = fGeoPar->GetXRow(); 
    vector<double> yRow = fGeoPar->GetYRow(); 

    Float_t ang, phiAngle;
    for (int iRow = 0; iRow < nRow; iRow++)  { 
     ang = ATan2(y - yRow[iRow],x - xRow[iRow])*RadToDeg();
     phiAngle = phiRow[iRow]; 
     if ( ( phiAngle == 360.) && ( ang > 0) ) phiAngle = 0.0;
     if (ang < 0) ang += 360.;
     diffR.push_back(fabs(phiAngle - ang));    
    }

    Int_t rowMod = LocMin(diffR.size(), &diffR[0]); 
    ang = ATan2(y - yRow[rowMod],x - xRow[rowMod])*RadToDeg(); 
    phiAngle = phiRow[rowMod];
    if ( (phiAngle == 360.) && (ang > 0) ) phiAngle = 0.0;  
    if (ang < 0) ang += 360.;
    index = 2*rowMod + 1; 
    if (phiAngle - ang > 0) index = index - 1; 
    
    return index;
}

Int_t MpdEmcHitCreation::GetTowerId(Double_t x, Double_t y, Double_t z, Int_t iRow, Double_t &xTower, 
				    Double_t &yTower, Double_t &zTower, Double_t &phiTower, Double_t &thetaTower) {

    Int_t index = -1; 
    vector<double> diffTheta;
    const Int_t nRow = fGeoPar->GetNrows();
    const Int_t nBox = fGeoPar->GetNmod();  
    vector<Double_t> xBox = fGeoPar->GetXBox(); 
    vector<Double_t> yBox = fGeoPar->GetYBox(); 
    vector<Double_t> zBox = fGeoPar->GetZBox(); 
    vector<Double_t> thetaBox = fGeoPar->GetThetaBox();
    vector<Double_t> phiBox = fGeoPar->GetPhiBox();
    Int_t boxStep = nBox/(2*nRow);


    const Double_t boxSizeLow = fGeoPar->GetSizeLowBox();
    const Double_t boxSizeHigh = fGeoPar->GetSizeHighBox();
    const Double_t boxLength = fGeoPar->GetLengthBox();

    Double_t ang = ATan2(y, x);
    if (ang < 0) ang += TwoPi();

    Double_t rhoMod0, zMod0, lenTotal, thetaRad, boxRho; //thetaPoint, 
    Double_t dPolTheta = ATan2(0.5*(boxSizeHigh - boxSizeLow),boxLength)*RadToDeg();

    Int_t iRowStart, iRowNext = iRow*boxStep;//, iRowNum = iRow/2;  
    if ( Odd(iRow) ) {iRowNext = (iRow-1)*boxStep + 1; /*iRowNum = (iRow - 1)/2.;*/}
    iRowStart = iRowNext; 

    for (Int_t iBox = 0; iBox < boxStep; iBox++) {
     boxRho = sqrt(xBox[iRowNext]*xBox[iRowNext]+yBox[iRowNext]*yBox[iRowNext]);
     thetaRad = thetaBox[iRowNext]*DegToRad();
     lenTotal = boxSizeHigh/tan(dPolTheta*DegToRad()) - boxLength - boxRho/sin(thetaRad);
     rhoMod0 = sin(thetaRad)*lenTotal;
     zMod0 = zBox[iRowNext] - boxRho/tan(thetaRad) - lenTotal*cos(thetaRad);    
     diffTheta.push_back(fabs(thetaBox[iRowNext] - 
         ATan2(sqrt(x*x + y*y) + rhoMod0, fabs(z) - zMod0)*RadToDeg()));
     iRowNext += 2; 
    } 

   index = LocMin(diffTheta.size(), &diffTheta[0]);
   iRowNext = iRowStart + 2*index; 

   xTower = xBox[iRowNext]; yTower = yBox[iRowNext]; zTower = zBox[iRowNext]; 
   phiTower = phiBox[iRowNext]; thetaTower = thetaBox[iRowNext]; 

   if (z < 0) {
     index = boxStep - index - 1; 
     zTower = - zTower; 
   }
   else {
     index = boxStep + index; 	
   }

   return index;

}

MpdEmcHit* MpdEmcHitCreation::SearchHit(UInt_t sec, UInt_t row, UInt_t mod) {
    MpdEmcHit* foundHit = NULL;
    for (Int_t i = 0; i < fDigiArray->GetEntriesFast(); ++i) {
        MpdEmcHit* hit = (MpdEmcHit*) fDigiArray->At(i);
        if ((UInt_t)hit->GetSec() != sec) continue;
        if ((UInt_t)hit->GetRow() != row) continue;
        if ((UInt_t)hit->GetMod() != mod) continue;
        foundHit = hit;
    }
    return foundHit;
}

// -------------------------------------------------------------------------

ClassImp(MpdEmcHitCreation)