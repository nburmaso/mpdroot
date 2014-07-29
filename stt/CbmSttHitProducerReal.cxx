/////////////////////////////////////////////////////////////
// CbmSttHitProducerReal
//
// Class for digitalization for STT1
//
// authors: Pablo Genova - Pavia University
//          Lia Lavezzi  - Pavia University
//
/////////////////////////////////////////////////////////////

#include "TClonesArray.h"

#include "FairRootManager.h"
#include "CbmSttHitProducerReal.h"
#include "CbmSttHit.h"
#include "CbmSttHitInfo.h"
#include "CbmSttPoint.h"
#include "TGeoManager.h"
// #include "TGeoVolume.h"
// #include "TGeoNode.h"
// #include "TGeoMatrix.h"
#include "TVector3.h"
#include "TStraw.h"
#include "TRandom.h"

#include <iostream>
#include "TMath.h"

using std::cout;
using std::cerr;
using std::endl;

// -----   Default constructor   -------------------------------------------
CbmSttHitProducerReal::CbmSttHitProducerReal() :
  FairTask("Ideal STT Hit Producer") { }
// -------------------------------------------------------------------------



// -----   Destructor   ----------------------------------------------------
CbmSttHitProducerReal::~CbmSttHitProducerReal() { }
// -------------------------------------------------------------------------



// -----   Public method Init   --------------------------------------------
InitStatus CbmSttHitProducerReal::Init() {
  
  // Get RootManager
  FairRootManager* ioman = FairRootManager::Instance();
  if ( ! ioman ) {
    cout << "-E- CbmSttHitProducerReal::Init: "
	 << "RootManager not instantiated!" << endl;
    return kFATAL;
  }
  
  // Get input array
  fPointArray = (TClonesArray*) ioman->GetObject("STTPoint");
  if ( ! fPointArray ) {
    cout << "-W- CbmSttHitProducerReal::Init: "
	 << "No STTPoint array!" << endl;
    return kERROR;
  }

  // Create and register output array
  fHitArray = new TClonesArray("CbmSttHit");
  ioman->Register("STTHit","STT",fHitArray,kTRUE);
  
 // Create and register output array
  fHitInfoArray = new TClonesArray("CbmSttHitInfo");
  ioman->Register("STTHitInfo", "STT", fHitInfoArray, kTRUE);

  // Geometry loading
  //TFile *tstfile=ioman->GetInFile();
  //TGeoManager *geoMan = (TGeoManager*) tstfile->Get("CBMGeom");
  TFile* f = (TFile*) ioman->GetInChain()->GetListOfFriends()->FindObject("testparams.root");
  if (!f)
  {
     cout<<"\n ---> ERROR: friend's list hasn't testparams.root";
     return kERROR;
  }
  f->Get("FairBaseParSet");

  if(!gGeoManager) // parameter file don't have "FairBaseParSet"
  {
     cout<<"\n ---> ERROR: parameter file don't have \"FairBaseParSet\" folder.";
     f->Close();
     return kERROR;
  }

  fVolumeArray = gGeoManager->GetListOfVolumes();
  
  cout << "-I- CbmSttHitProducerReal: Intialization successfull" << endl;
  
  return kSUCCESS;

}
// -------------------------------------------------------------------------



// -----   Public method Exec   --------------------------------------------
void CbmSttHitProducerReal::Exec(Option_t* opt) {

  //  Int_t evtn=0;	
  //  CbmSttPoint *ptemp =(CbmSttPoint*) fPointArray->At(0);
  //  if(ptemp !=NULL) {
  //    evtn=ptemp->GetEventID();
  //    if(evtn%50==0)cout << "Event Number "<<evtn<<endl;
  //  }
  
  // Reset output array
  if ( ! fHitArray ) Fatal("Exec", "No HitArray");
  
  fHitArray->Clear();
  fHitInfoArray->Clear();

  Int_t detID = 0;    // detectorID
  TVector3 pos, dpos; // position and error vectors

  // Declare some variables
  CbmSttPoint* point  = NULL;
   
  // Loop over SttPoints
  Int_t nPoints = fPointArray->GetEntriesFast();
  for (Int_t iPoint = 0; iPoint < nPoints; iPoint++) {
    point  = (CbmSttPoint*) fPointArray->At(iPoint);
    if (point == NULL) continue;
    double InOut[6];
    memset(InOut, 0, sizeof(InOut));

    InOut[0] = point->GetXInLocal();
    InOut[1] = point->GetYInLocal();
    InOut[2] = point->GetZInLocal();
    InOut[3] = point->GetXOutLocal();
    InOut[4] = point->GetYOutLocal();
    InOut[5] = point->GetZOutLocal();
    
    // single straw tube simulation -----------------------
    TStraw stt;
    
    //setting the single straw tube simulation constants
    // 3 options currently available:
    // TConst(tube radius (cm), gas pressure (bar), Ar%, CO2%)
    // stt.TConst(0.4, 1, 0.9, 0.1); 
    // stt.TConst(0.5, 1, 0.9, 0.1);
     stt.TConst(0.5, 2, 0.8, 0.2);
    
    // wire positioning   
    stt.PutWireXYZ(0.,  0., -75., 0., 0., 75.);

    // get particle momentum
    TVector3 momentum(point->GetPxOut(),point->GetPyOut(),point->GetPzOut()); // GeV/c
      
    Double_t GeV=1.;
    // position in cm (already in cm); momentum in GeV (already in GeV); mass in GeV (already in GeV)
      
    // drift time calculation
    Double_t pulset = stt.PartToTime(point->GetMass()/GeV, momentum.Mag()/GeV, InOut);
        
    // simulated radius (cm)
    double radius = stt.TimnsToDiscm(pulset);
    if(radius < 0.) radius = 0.; // CHECK
      
    // true radius (cm)
    double true_rad = stt.TrueDist(InOut);

    // dE calculation
    //  double depCharge = stt.PartToADC();
      
    // dE/dx calculation
    //TVector3 diff3=point1->Get
    //double distance =diff3.Mag(); //
    //double dedx = 999;
    //if (distance != 0)  dedx = depCharge/(1000000 * distance);  // in arbitrary units
    
    // stt2: detID, pos, dpos, index come from --------------
    // stt2 (FairHit):
    Double_t closestDistanceError = 0.0150; // per adesso (stessa che in Ideal: 
                                            // radialResolution = 0.0150)

    TVector3 position(point->GetX(), point->GetY(), point->GetZ());

    // ----------------
    // stt2, ma cancellati in stt1 (controlla: in stt2 la posizione dell' hit non 
    // corrisponde al centro del tubo (xcentro, ycentro, 35.), ma per il Real deve
    // essere cosi' ??perche' in stt2 non e' cosi'??
    // TVector3 posInLocal(point->GetXInLocal(), point->GetYInLocal(), point->GetZInLocal());
    // TVector3 posOutLocal(point->GetXOutLocal(), point->GetYOutLocal(), point->GetZOutLocal());
    // Double_t zpos = position.Z() + ((posOutLocal.Z() + posInLocal.Z()) / 2.);
    // Double_t zposError;
    // FoldZPosWithResolution(zpos, zposError, posInLocal, posOutLocal);
    //    pos.SetXYZ(position.X(), position.Y(), zpos); // <--- stt2
    // ----------------

    pos.SetXYZ(position.X(), position.Y(), position.Z()); // <--- stt1

    //    dpos.SetXYZ(innerStrawDiameter / 2., innerStrawDiameter / 2., GetLongitudinalResolution(position.Z()));
    dpos.SetXYZ(0.5, 0.5, 3.); // per adesso (stessi che in Ideal:
                               // innerStrawDiameter/2 = 0.5,
                               // longitudinalResolution = 3.)
    //----- end stt2 ------------------------------------------

    // wire direction from stt2 -------------------------------
    TVector3 wireDirection(point->GetXWireDirection(), point->GetYWireDirection(), point->GetZWireDirection());
    // = 0, 0, 1 if only axias tubes
    // --------------------------------------------------------

    // create hit
    AddHit(detID, pos, dpos, iPoint, point->GetTrackID(), pulset, radius, true_rad, closestDistanceError, wireDirection);

    AddHitInfo(0, 0, point->GetTrackID(), iPoint, 0, kFALSE);

  }// Loop over MCPoints
  
  // Event summary
  cout << "-I- CbmSttHitProducerReal: " << nPoints << " SttPoints, "
       << nPoints << " Hits created." << endl;
  
}
// -------------------------------------------------------------------------
void CbmSttHitProducerReal::FoldZPosWithResolution(Double_t &zpos, Double_t &zposError, 
						    TVector3 localInPos, TVector3 localOutPos)
{
  Double_t
    zPosInStrawFrame = (localOutPos.Z() - localInPos.Z()) / 2.;
 
  //  zposError = gRandom->Gaus(0., GetLongitudinalResolution(zPosInStrawFrame));
  zposError = gRandom->Gaus(0., 3.); // per adesso (stesso che in Ideal: 
                                     // longitudinalResolution = 3.)

  zpos += zposError;
}



// -----   Private method AddHit   --------------------------------------------
CbmSttHit* CbmSttHitProducerReal::AddHit(Int_t detID, TVector3& pos, TVector3& dpos, Int_t iPoint, Int_t trackID, Double_t p, Double_t rsim, Double_t rtrue, Double_t closestDistanceError, TVector3 wireDirection){
  // see CbmSttHit for hit description
  TClonesArray& clref = *fHitArray;
  Int_t size = clref.GetEntriesFast();
  //cout << "-I- CbmSttHitProducerReal: Adding Hit: track"<<trackID<<" event: "<<eventID<<" pulse = " << p << ", rsim = " << rsim 
  //     << ", rtrue = " << rtrue <<" name "<<nam<<"center.X "<<center.X()<< endl;
  return new(clref[size]) CbmSttHit(detID, pos, dpos, iPoint, trackID, p, rsim, rtrue, closestDistanceError, wireDirection);
}
// ----

// -----   Private method AddHitInfo   --------------------------------------------
CbmSttHitInfo* CbmSttHitProducerReal::AddHitInfo(Int_t fileNumber, Int_t eventNumber, Int_t trackID, Int_t pointID, Int_t nMerged, Bool_t isFake){
  // see CbmSttHitInfo for hit description
  TClonesArray& clref = *fHitInfoArray;
  Int_t size = clref.GetEntriesFast();
  return new(clref[size])  CbmSttHitInfo(fileNumber, eventNumber, trackID, pointID, nMerged, isFake);
}






ClassImp(CbmSttHitProducerReal)
