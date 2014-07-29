/////////////////////////////////////////////////////////////
//
//  MpdFfdHitProducer
//
//  Filler of MpdFfdHit
//
/////////////////////////////////////////////////////////////// 


#include "TClonesArray.h"

#include "FairRootManager.h"
#include "FairDetector.h"

#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoNode.h"
#include "TGeoMatrix.h"
#include "TVector3.h"
#include "FairRun.h"
#include "FairRuntimeDb.h"
#include "TMath.h"

#include "MpdFfdHitProducer.h"
#include "MpdFfdHit.h"
#include "MpdFfdPoint.h"

// -----   Default constructor   -------------------------------------------
MpdFfdHitProducer::MpdFfdHitProducer(const char* fileGeo) :
  FairTask("Ideal CPC hit Producer") {
  fFileGeo=fileGeo;
  eneThr = 0.001; // Energy threshold for CPC

}


// -----   Destructor   ----------------------------------------------------
MpdFfdHitProducer::~MpdFfdHitProducer() { }
// -------------------------------------------------------------------------



// -----   Public method Init   --------------------------------------------
InitStatus MpdFfdHitProducer::Init() {
 
  cout << "******************* INITIALIZATION *********************" << endl;
  
  //FairDetector::Initialize();
  //FairRun* sim = FairRun::Instance();
  //FairRuntimeDb* rtdb=sim->GetRuntimeDb();
  
  // Get RootManager
  FairRootManager* ioman = FairRootManager::Instance();
  if ( ! ioman ) {
    cout << "-E- MpdFfdHitProducer::Init: "
	 << "RootManager not instantiated!" << endl;
    return kFATAL;
  }
  
  // Get input array
  fPointArray = (TClonesArray*) ioman->GetObject("FfdPoint");
  if ( ! fPointArray ) {
    cout << "-W- MpdFfdHitProducer::Init: "
         << "No FfdPoint array!" << endl;
    return kERROR;
  }
  
  // Create and register output array
  fDigiArray = new TClonesArray("MpdFfdHit");
  
  ioman->Register("CpcHit","Cpc",fDigiArray,kTRUE);

  CreateStructure();

  hlist = new TList();

  MakeHists();  

  cout << "-I- MpdFfdHitProducer: Intialization successfull" << endl;
  
  return kSUCCESS;

}

//__________________________________________________________________
void MpdFfdHitProducer::FinishTask() {
  //---

  cout << "-I- MpdFfdHitProducer: FinishTask" << endl;

}


//__________________________________________________________________
void MpdFfdHitProducer::Finish() {
  //---

  cout << "-I- MpdFfdHitProducer: Finish" << endl;

 // save histograms of corresponding list
  if (hlist!=0) {
    TObject *obj;
    TIter next(hlist);
    while((obj = (TObject*)next())) obj->Write();
  }
}


//__________________________________________________________________
void MpdFfdHitProducer::MakeHists() {
  //---

  fZ    = new TH1F("z","", 1000, -300., 300.);
  hlist->Add(fZ);

  Float_t xbins[5] = {0., 15., 15.5, 16.5, 50.0};


  //fR    = new TH1F("r","", 100, 0., 55.);
  fR    = new TH1F("r","", 4, xbins);
  hlist->Add(fR);

  fXY    = new TH2F("xy","", 100, -30., 30., 100, -30., 30.);
  hlist->Add(fXY);

  fRphi    = new TH2F("rphi","", 100, 0., TMath::TwoPi(), 100, 0., 50.);
  hlist->Add(fRphi);

}
 


// -----   Public method Exec   --------------------------------------------
void MpdFfdHitProducer::Exec(Option_t* opt) {
 
  //cout << " DIGI EXECUTION *********************" << endl;
  // Reset output array
  if ( ! fDigiArray ) Fatal("Exec", "No DigiArray");
  
  fDigiArray->Clear();
  
  // Declare some variables
  MpdFfdPoint* point  = NULL;

  map<Int_t, Float_t> fTrackEnergy;
  fTrackEnergy.clear();
  map<Int_t, Float_t>::const_iterator p;
  
  int oldtrackID = -6;
  // Loop over CpcPoints
  Int_t nPoints = fPointArray->GetEntriesFast();
  for (Int_t iPoint=0; iPoint < nPoints; iPoint++) {
    point  = (MpdFfdPoint*) fPointArray->At(iPoint);
    //    fTrackEnergy[point->GetDetectorID()] += point->GetEnergyLoss();
    int trackID = point->GetTrackID();
    if (oldtrackID != trackID) {

      fZ->Fill(point->GetZ());
      fXY->Fill(point->GetX(), point->GetY());
      TVector2 v = TVector2(point->GetX(), point->GetY());
      fR->Fill(v.Mod());
      fRphi->Fill(v.Phi(), v.Mod());
    }

  }

  
#if 0
  // Loop to register CpcHit
  for(p=fTrackEnergy.begin(); p!=fTrackEnergy.end(); ++p) {
    if ((*p).second>eneThr)
      AddHit(1, (*p).first, (*p).second); 
  }
#endif 
}
// -------------------------------------------------------------------------


// -----   Public method Create Structure   --------------------------------
void MpdFfdHitProducer::CreateStructure() {
 
  /*
  TString work = getenv("VMCWORKDIR");
  work = work + "/geometry/" + fFileGeo;
  cout << "-I- <MpdFfdHitProducer::CreateStructure> Cpc geometry loaded from: "
       << work << endl;

  Int_t detId = -1;
  MpdFfdReader read(work);
  
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
MpdFfdHit* MpdFfdHitProducer::AddHit(Int_t trackID,Int_t detID, Float_t energy){
  // It fills the MpdFfdHit category
  // cout << "MpdFfdHitProducer: track " << trackID << " evt " << eventID << " sec " << sec << " plane " << pla << " strip " << strip << "box " << box << " tube " << tub << endl;

  TClonesArray& clref = *fDigiArray;
  Int_t size = clref.GetEntriesFast();
  return new(clref[size]) MpdFfdHit(); // FIXME: real hit info needed here
}
// ----


ClassImp(MpdFfdHitProducer)
