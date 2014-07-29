/////////////////////////////////////////////////////////////
//
//  MpdFsaHitProducer
//
//  Filler of MpdFsaHit
//
//  
//
/////////////////////////////////////////////////////////////// 


#include "TClonesArray.h"

#include "FairRootManager.h"
#include "FairDetector.h"
#include "MpdFsaHitProducer.h"
#include "MpdFsaHit.h"
#include "MpdFsaPoint.h"
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoNode.h"
#include "TGeoMatrix.h"
#include "TVector3.h"
#include "FairRun.h"
#include "FairRuntimeDb.h"
#include "FairMCTrack.h"

// -----   Default constructor   -------------------------------------------
MpdFsaHitProducer::MpdFsaHitProducer(const char* fileGeo) :
  FairTask("Ideal FSA hit Producer") {
  fFileGeo=fileGeo;
  eneThr = 0.001; // Energy threshold for FSA
}
// -------------------------------------------------------------------------

// -----   Destructor   ----------------------------------------------------
MpdFsaHitProducer::~MpdFsaHitProducer() { }
// -------------------------------------------------------------------------



// -----   Public method Init   --------------------------------------------
InitStatus MpdFsaHitProducer::Init() {
 
  cout << " INITIALIZATION *********************" << endl;
  
  //FairDetector::Initialize();
  //FairRun* sim = FairRun::Instance();
  //FairRuntimeDb* rtdb=sim->GetRuntimeDb();
  
  // Get RootManager
  FairRootManager* ioman = FairRootManager::Instance();
  if ( ! ioman ) {
    cout << "-E- MpdFsaHitProducer::Init: "
	 << "RootManager not instantiated!" << endl;
    return kFATAL;
  }
  
  // Get input array
  fPointArray = (TClonesArray*) ioman->GetObject("FSAPoint");
  if ( ! fPointArray ) {
    cout << "-W- MpdFsaHitProducer::Init: "
	 << "No FsaPoint array!" << endl;
    return kERROR;
  }

  fListMCtracks = (TClonesArray *)ioman->GetObject("MCTrack");
  
  // Create and register output array
  fDigiArray = new TClonesArray("MpdFsaHit");
  
  ioman->Register("FsaHit","Fsa",fDigiArray,kTRUE);
  CreateStructure();
  
  cout << "-I- MpdFsaHitProducer: Intialization successfull" << endl;
  
  return kSUCCESS;

}
// -------------------------------------------------------------------------

// -----   Public method Exec   --------------------------------------------
void MpdFsaHitProducer::Exec(Option_t* opt) {
 
  //cout << " DIGI EXECUTION *********************" << endl;
  // Reset output array
  if ( ! fDigiArray ) Fatal("Exec", "No DigiArray");
  
  fDigiArray->Clear();
  
  // Declare some variables
  MpdFsaPoint* point  = NULL;
  map<Int_t, Float_t> fTrackEnergy;
  fTrackEnergy.clear();
  map<Int_t, Float_t>::const_iterator p;
  
  // Loop over FsaPoints
  Int_t nPoints = fPointArray->GetEntriesFast();
  Int_t nTracks = 0;
  Int_t iTrack = -1;
  for (Int_t iPoint=0; iPoint<nPoints; iPoint++) {
    point  = (MpdFsaPoint*) fPointArray->At(iPoint);
    fTrackEnergy[point->GetDetectorID()] += point->GetEnergyLoss();
    point->Print("");
    if(iTrack != point->GetTrackID()){
    	nTracks++;
    	iTrack = point->GetTrackID();
    }
    FairMCTrack *gtrack = (FairMCTrack *)fListMCtracks->At(point->GetTrackID());
    cout << "\n PDG " <<  gtrack->GetPdgCode() << "\n\n";

  }
  cout << "\n count tracks " <<  nTracks;
  cout << "\n load " <<  nTracks/9891330.816630875 << " tracks/cm^2";
  // Loop over ZdcPoint
  
  // Loop to register ZdcHit
  for(p=fTrackEnergy.begin(); p!=fTrackEnergy.end(); ++p) {
    if ((*p).second>eneThr)
      AddHit(1, (*p).first, (*p).second); 
  }
 
}
// -------------------------------------------------------------------------


// -----   Public method Create Structure   --------------------------------
void MpdFsaHitProducer::CreateStructure() { 
 
  /*
  TString work = getenv("VMCWORKDIR");
  work = work + "/geometry/" + fFileGeo;
  cout << "-I- <MpdFsaHitProducer::CreateStructure> Zdc geometry loaded from: "
       << work << endl;

  Int_t detId = -1;
  MpdFsaReader read(work);
  
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
MpdFsaHit* MpdFsaHitProducer::AddHit(Int_t trackID,Int_t detID, Float_t energy){
  // It fills the MpdFsaHit category
  // cout << "MpdFsaHitProducer: track " << trackID << " evt " << eventID << " sec " << sec << " plane " << pla << " strip " << strip << "box " << box << " tube " << tub << endl;
  TClonesArray& clref = *fDigiArray;
  Int_t size = clref.GetEntriesFast();
  return new(clref[size]) MpdFsaHit(); // FIXME: real hit info needed here
}
// ----


ClassImp(MpdFsaHitProducer)
