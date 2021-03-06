/////////////////////////////////////////////////////////////
//
//  MpdCpcHitProducer
//
//  Filler of MpdCpcHit
//
/////////////////////////////////////////////////////////////// 


#include "TClonesArray.h"

#include "FairRootManager.h"
#include "FairDetector.h"
#include "FairBaseParSet.h"
#include "FairGeoNode.h"

#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoNode.h"
#include "TGeoMatrix.h"
#include "TVector3.h"
#include "FairRun.h"
#include "FairRuntimeDb.h"
#include "TMath.h"

#include "FairRunAna.h"

#include "MpdCpcHitProducer.h"
#include "MpdCpcGeoPar.h"
#include "MpdCpcHit.h"
#include "MpdCpcPoint.h"

// -----   Default constructor   -------------------------------------------


MpdCpcHitProducer::MpdCpcHitProducer(const char* fileGeo) :
  FairTask("Ideal CPC hit Producer") {
  fFileGeo=fileGeo;
  eneThr = 0.001; // Energy threshold for CPC

}


// -----   Destructor   ----------------------------------------------------
MpdCpcHitProducer::~MpdCpcHitProducer() { }
// -------------------------------------------------------------------------



// -----   Public method Init   --------------------------------------------
InitStatus MpdCpcHitProducer::Init() {
 
  cout << "******************* INITIALIZATION *********************" << endl;
  
  //FairDetector::Initialize();
  //FairRun* sim = FairRun::Instance();
  //FairRuntimeDb* rtdb=sim->GetRuntimeDb();
  
  // Get RootManager
  FairRootManager* ioman = FairRootManager::Instance();
  if ( ! ioman ) {
    cout << "-E- MpdCpcHitProducer::Init: "
	 << "RootManager not instantiated!" << endl;
    return kFATAL;
  }
  
  // Get input array
  fPointArray = (TClonesArray*) ioman->GetObject("CPCPoint");
  if ( ! fPointArray ) {
    cout << "-W- MpdCpcHitProducer::Init: "
	 << "No CpcPoint array!" << endl;
    return kERROR;
  }
  
  // Create and register output array
  fDigiArray = new TClonesArray("MpdCpcHit");
  
  ioman->Register("CpcHit","Cpc",fDigiArray,kTRUE);

  CreateStructure();

  hlist = new TList();

  MakeHists();  

  cout << "-I- MpdCpcHitProducer: Intialization successfull" << endl;
  
  return kSUCCESS;

}

//__________________________________________________________________
void MpdCpcHitProducer::Finish() {
  //---

  cout << "-I- MpdCpcHitProducer: FinishTask" << endl;

}


//__________________________________________________________________
void MpdCpcHitProducer::FinishTask() {
  //---

  cout << "-I- MpdCpcHitProducer: Finish" << endl;

 // save histograms of corresponding list
  if (hlist!=0) {
    TObject *obj;
    TIter next(hlist);
    while((obj = (TObject*)next())) obj->Write();
  }
}


//__________________________________________________________________
void MpdCpcHitProducer::MakeHists() {
  //---

  fZ    = new TH1F("z","", 500, 150., 250.);
  hlist->Add(fZ);

  Float_t xbins[5] = {0., 15., 20., 30., 50.0};


  fR    = new TH1F("r","", 100, 0., 55.);
  //fR    = new TH1F("r","", 4, xbins);
  hlist->Add(fR);

  fXY    = new TH2F("xy","", 100, -50., 50., 100, -50., 50.);
  hlist->Add(fXY);

  fRphi    = new TH2F("rphi","", 100, 0., TMath::TwoPi(), 100, 0., 50.);
  hlist->Add(fRphi);

}
 
// -----   Private method SetParContainers   -------------------------------
void MpdCpcHitProducer::SetParContainers() {
  // Get run and runtime database
  FairRunAna* run = FairRunAna::Instance();
  if ( ! run ) Fatal("SetParContainers", "No analysis run");

  FairRuntimeDb* db = run->GetRuntimeDb();
  if ( ! db ) Fatal("SetParContainers", "No runtime database");

  // Get Cpc geometry parameter container
  db->getContainer("MpdCpcGeoPar"); 
}

// -----   Public method Exec   --------------------------------------------
void MpdCpcHitProducer::Exec(Option_t* opt) {
 
  //cout << " DIGI EXECUTION *********************" << endl;
  // Reset output array
  if ( ! fDigiArray ) Fatal("Exec", "No DigiArray");

  ///////////////////////////////////////

  FairRuntimeDb* rtdb = FairRun::Instance()->GetRuntimeDb();
  //rtdb->printParamContext();

  MpdCpcGeoPar *geoPar = (MpdCpcGeoPar*) rtdb->getContainer("MpdCpcGeoPar");

  //TString volName = "cpc01l";
  TObjArray* sensNodes = geoPar->GetGeoSensitiveNodes();
  
  Double_t rMin, rMax;
  for (Int_t i=0; i<sensNodes->GetEntriesFast(); i++){
    FairGeoNode* sensVol = (FairGeoNode*) (sensNodes->At(i));
    TArrayD* params = sensVol->getParameters();
    rMin = params->At(0);
    rMax = params->At(1);
    cout << " *** CPC sensitive volume " <<(i+1) <<": " << sensVol->GetName() << " " << rMin << " " << rMax << endl;    
  }


  /////////////////////////////////////

  fDigiArray->Clear();
  
  // Declare some variables
  MpdCpcPoint* point  = NULL;

  map<Int_t, Float_t> fTrackEnergy;
  fTrackEnergy.clear();
  map<Int_t, Float_t>::const_iterator p;
  
  int oldtrackID = -6;
  // Loop over CpcPoints
  Int_t nPoints = fPointArray->GetEntriesFast();
  for (Int_t iPoint=0; iPoint < nPoints; iPoint++) {
    point  = (MpdCpcPoint*) fPointArray->At(iPoint);
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
void MpdCpcHitProducer::CreateStructure() { 
 
  /*
  TString work = getenv("VMCWORKDIR");
  work = work + "/geometry/" + fFileGeo;
  cout << "-I- <MpdCpcHitProducer::CreateStructure> Cpc geometry loaded from: "
       << work << endl;

  Int_t detId = -1;
  MpdCpcReader read(work);
  
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
MpdCpcHit* MpdCpcHitProducer::AddHit(Int_t trackID,Int_t detID, Float_t energy){
  // It fills the MpdCpcHit category
  // cout << "MpdCpcHitProducer: track " << trackID << " evt " << eventID << " sec " << sec << " plane " << pla << " strip " << strip << "box " << box << " tube " << tub << endl;

  TClonesArray& clref = *fDigiArray;
  Int_t size = clref.GetEntriesFast();
  return new(clref[size]) MpdCpcHit(); // FIXME: real hit info needed here
}
// ----

ClassImp(MpdCpcHitProducer)
