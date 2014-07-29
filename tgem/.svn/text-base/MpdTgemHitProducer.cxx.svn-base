//--------------------------------------------------------------------------
//-----                    MpdTgemHitProducer                   -----
//-----                 Created 8/12/09  by A. Zinchenko               -----
//--------------------------------------------------------------------------

/**  MpdTgemHitProducer.cxx
 *@author A.Zinchenko <Alexander.Zinchenko@jinr.ru>
 **
 ** MPD End-Cap Tracker (ECT) hit producer
 **/

#include "MpdTgemHitProducer.h"
#include "MpdTgemHit.h"
#include "MpdTgemPoint.h"
#include "FairRun.h"
#include "FairRuntimeDb.h"
#include "FairRootManager.h"
#include "FairDetector.h"

#include <TClonesArray.h>
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoNode.h"
#include "TGeoMatrix.h"
#include <TMath.h>
#include <TRandom.h>
#include "TVector3.h"
#include <iostream>


// -----   Default constructor   -------------------------------------------
MpdTgemHitProducer::MpdTgemHitProducer(const char* fileGeo) :
  FairTask("Ideal Tgem hit Producer") {
  fFileGeo = fileGeo;
  eneThr = 0.001; // Energy threshold for Tgem
  //= hZ = new TH1F("hZ","This is the Z distribution",100,0,300)
}
// -------------------------------------------------------------------------

// -----   Destructor   ----------------------------------------------------
MpdTgemHitProducer::~MpdTgemHitProducer() { }
// -------------------------------------------------------------------------

// -----   Public method Init   --------------------------------------------
InitStatus MpdTgemHitProducer::Init() {
 
  cout << " INITIALIZATION *********************" << endl;
  
  //FairDetector::Initialize();
  //FairRun* sim = FairRun::Instance();
  //FairRuntimeDb* rtdb=sim->GetRuntimeDb();
  
  // Get RootManager
  FairRootManager* ioman = FairRootManager::Instance();
  if ( ! ioman ) {
    cout << "-E- MpdTgemHitProducer::Init: "
	 << "RootManager not instantiated!" << endl;
    return kFATAL;
  }
  
  // Get input array
  fPointArray = (TClonesArray*) ioman->GetObject("TgemPoint");
  if ( ! fPointArray ) {
    cout << "-W- MpdTgemHitProducer::Init: "
	 << "No TgemPoint array!" << endl;
    return kERROR;
  }
  
  // Create and register output array
  fDigiArray = new TClonesArray("MpdTgemHit");
  
  ioman->Register("TgemHit","TgemHits",fDigiArray,kTRUE);
  CreateStructure();
  
  cout << "-I- MpdTgemHitProducer: Intialization successfull" << endl;
  
  return kSUCCESS;

}
// -------------------------------------------------------------------------

// -----   Public method Exec   --------------------------------------------
void MpdTgemHitProducer::Exec(Option_t* opt) {
 
  //cout << " DIGI EXECUTION *********************" << endl;
  static int eventCounter = 0;    
  cout << " Event " << ++eventCounter << endl;

  // Reset output array
  if ( ! fDigiArray ) Fatal("Exec", "No DigiArray");
  
  fDigiArray->Clear();
  
  /*
  // Declare some variables
  MpdTgemPoint* point  = NULL;
  map<Int_t, Float_t> fTrackEnergy;
  fTrackEnergy.clear();
  map<Int_t, Float_t>::const_iterator p;
  
  // Loop over ZdcPoints
  Int_t nPoints = fPointArray->GetEntriesFast();
  for (Int_t iPoint=0; iPoint<nPoints; iPoint++) {
    point  = (MpdTgemPoint*) fPointArray->At(iPoint);
    fTrackEnergy[point->GetDetectorID()] += point->GetEnergyLoss();
  }
  // Loop over ZdcPoint
  
  // Loop to register ZdcHit
  for(p=fTrackEnergy.begin(); p!=fTrackEnergy.end(); ++p) {
    if ((*p).second>eneThr)
      AddHit(1, (*p).first, (*p).second); 
  }
  */

  Bool_t mirror = kFALSE; //kTRUE; // add mirror hits if TRUE

  Int_t nPoints = fPointArray->GetEntriesFast(), lay = 0, nKH = 0, itube = 0;
//++  cout<< "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! nPoints = "<< nPoints <<endl;
//========================= test =============================

 //=+ MpdTgemPoint *h = (MpdTgemPoint*) fPointArray->UncheckedAt(2);
 //=+ cout<< "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! GetZ = "<< h->GetZ() <<" "<<"h->GetDetectorID()="<<h->GetDetectorID()<<endl;
//============================================================
/* == 
  Double_t phi, r, coord, errR = 0.2, errRphi = 0.02; // 2mm in R, 200um in R-Phi
  //Double_t phi, r, coord, errR = 0.01, errRphi = 0.01; // 100um in R, 100um in R-Phi
  Double_t firstHit[100][1000] = {{0},{0}};
  Int_t indxTube[100][1000];
  for (Int_t i = 0; i < 100; ++i) {
    for (Int_t j = 0; j < 1000; ++j) indxTube[i][j] = -1;
  }
  TVector3 pos, dpos(0.,0.,0.);

  for (Int_t ih = 0; ih < nPoints; ++ih) {
    MpdTgemPoint *h = (MpdTgemPoint*) fPointArray->UncheckedAt(ih);
    if (h->GetZ() < 0) continue; // keep only Z>0
    //if (TMath::Abs(TMath::ATan2(h->GetTrackY(),h->GetTrackX())*TMath::RadToDeg()-97.5) > 7.5) continue; // !!!
    //if (h->GetTrackID() != 1427) continue; // !!!
    phi = h->GetPhi(); // tube Phi
    lay = h->GetDetectorID() / 1000; // + 1; 
    itube = h->GetDetectorID() % 1000; // tube number
    // Extrapolate track to Z = Ztube
    Double_t dZ = h->GetZ() - h->GetTrackZ();
    Double_t dt = dZ / h->GetPz();
    if (TMath::Abs(h->GetPz()) > 1.e-6 && h->GetPz() * h->GetZ() > 0) dt = dZ / h->GetPz();
    Double_t xNew = h->GetTrackX() + dt * h->GetPx();
    Double_t yNew = h->GetTrackY() + dt * h->GetPy();
    Double_t zNew = h->GetTrackZ() + dt * h->GetPz();
    pos.SetXYZ(xNew,yNew,zNew);
    //cout << dZ << " " << h->GetTrackX() << " " << xNew << " " << h->GetTrackY() << " " << yNew << " " << lay << endl;
    // Transform to the rotated coordinate system
    Double_t cosPhi = TMath::Cos(phi);
    Double_t sinPhi = TMath::Sin(phi);
    //Double_t xLoc = h->GetX() * cosPhi + h->GetY() * sinPhi; // cross-check
    //Double_t yLoc = -h->GetX() * sinPhi + h->GetY() * cosPhi;
    Double_t xRot = xNew * cosPhi + yNew * sinPhi;
    Double_t yRot = -xNew * sinPhi + yNew * cosPhi;
    //Double_t xLoc = (xNew - h->GetX()) * cosPhi + (yNew - h->GetY()) * sinPhi;
    //Double_t yLoc = -(xNew - h->GetX()) * sinPhi + (yNew - h->GetY())  * cosPhi;
    //r = xNew * xNew + yNew * yNew;
    //r = TMath::Sqrt (r);
    //r = TMath::Abs(xLoc);
    r = xRot;
    //cout << xRot << " " << yRot << " " << r << " " << h->GetPz() << endl;
    coord = yRot;
    Double_t yWire = -h->GetX() * sinPhi + h->GetY() * cosPhi;
    Double_t dY = TMath::Abs (yWire - coord);

    // Add error                                            
    Double_t dRphi = 0, dR = 0;
    gRandom->Rannor(dRphi,dR);

    MpdTgemHit *hit = new ((*fDigiArray)[nKH]) MpdTgemHit(h->GetDetectorID(), pos, dpos, ih, 0);
    hit->SetMeas(coord+dRphi*errRphi);
    hit->SetError(errRphi);
    hit->SetPhi(phi);
    hit->SetIndex(ih);

    //MpdKalmanHitZ *hit = new ((*fKHits)[nKH]) MpdKalmanHitZ(r+dR*errR, phi, coord+dRphi*errRphi, 
    //						    h->GetTrackZ(), errR, errRphi, lay, ih);
    //hit->SetXY(h->GetX(), h->GetY());
    // Add second measurement - just for test at the moment
    //!!!
    //hit->SetNofDim(2);
    //!!!
    ++nKH;
  }  
*/
//=  fDigiArray->Compress();
 //= fDigiArray->Sort(); // in ascending order in abs(Z)
 //= cout << " Max layer = " << ((MpdTgemHit*)fDigiArray->Last())->GetLayer() << " " << fDigiArray->GetEntriesFast() << endl;
}
// -------------------------------------------------------------------------

// -----   Public method Create Structure   --------------------------------
void MpdTgemHitProducer::CreateStructure() { 
 
  /*
  TString work = getenv("VMCWORKDIR");
  work = work + "/geometry/" + fFileGeo;
  cout << "-I- <MpdTgemHitProducer::CreateStructure> Zdc geometry loaded from: "
       << work << endl;

  Int_t detId = -1;
  MpdTgemReader read(work);
  
  for(Int_t module=1; module<=read.GetMaxModules(); module++) 
    for(Int_t row=1; row<=read.GetMaxRows(module); row++)
      for(Int_t crystal=1; crystal<=read.GetMaxCrystals(module,row); crystal++) {
	DataG4 data = read.GetData(module,row,crystal);
	for(Int_t copy=1; copy<=20; copy++) { 
	  detId =  module*100000000 + row*1000000 + copy*10000 + crystal; 
	  emcX[detId] = data.posX; emcY[detId] = data.posY; emcZ[detId] = data.posZ;
	  emcTheta[detId] = data.theta; emcTau[detId] = data.tau;
	  if (module==3)
	    emcPhi[detId] = fmod(data.phi+90.*(copy-1),360);
	  else
	    emcPhi[detId] = fmod(data.phi+22.5*(copy-1),360);
	}
      }
  */
}

// -----   Private method AddDigi   --------------------------------------------
MpdTgemHit* MpdTgemHitProducer::AddHit(Int_t trackID,Int_t detID, Float_t energy){
  // It fills the MpdTgemHit category
  // cout << "MpdTgemHitProducer: track " << trackID << " evt " << eventID << " sec " << sec << " plane " << pla << " strip " << strip << "box " << box << " tube " << tub << endl;
  TClonesArray& clref = *fDigiArray;
  Int_t size = clref.GetEntriesFast();
  return new(clref[size]) MpdTgemHit(); // FIXME: real hit info needed here
}
// ----


ClassImp(MpdTgemHitProducer)
