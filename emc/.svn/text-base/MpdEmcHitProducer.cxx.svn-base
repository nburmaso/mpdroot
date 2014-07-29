////////////////////////////////////////
/////////////////////
//
//  MpdEmcHitProducer
//
//  Filler of MpdEmcHit
//
/////////////////////////////////////////////////////////////// 


#include "TClonesArray.h"

#include "FairRootManager.h"
#include "FairDetector.h"
#include "FairBaseParSet.h"
#include "FairGeoNode.h"
//#include "FairTrackPar.h"

#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoNode.h"
#include "TGeoMatrix.h"
#include "TVector3.h"
#include "FairRun.h"
#include "FairRuntimeDb.h"
#include "TMath.h"
//**
#include "TNtuple.h"
//*
#include "FairRunAna.h"

#include "MpdEmcHitProducer.h"
#include "MpdEmcGeoPar.h"
#include "MpdEmcHit.h"
#include "MpdEmcPoint.h"
#include "MpdEmc.h"

// -----   Default constructor   -------------------------------------------


MpdEmcHitProducer::MpdEmcHitProducer(const char* fileGeo) :
  FairTask("Ideal EMC hit Producer") {
  fFileGeo=fileGeo;
  eneThr = 0.001; // Energy threshold for EMC

}


// -----   Destructor   ----------------------------------------------------
MpdEmcHitProducer::~MpdEmcHitProducer() { }
// -------------------------------------------------------------------------



// -----   Public method Init   --------------------------------------------
InitStatus MpdEmcHitProducer::Init() {
 
  cout << "******************* INITIALIZATION *********************" << endl;
  
  //FairDetector::Initialize();
  //FairRun* sim = FairRun::Instance();
  //FairRuntimeDb* rtdb=sim->GetRuntimeDb();
  
  // Get RootManager
  FairRootManager* ioman = FairRootManager::Instance();
  if ( ! ioman ) {
    cout << "-E- MpdEmcHitProducer::Init: "
	 << "RootManager not instantiated!" << endl;
    return kFATAL;
  }
  
  // Get input array
  fPointArray = (TClonesArray*) ioman->GetObject("MpdEmcPoint");
  if ( ! fPointArray ) {
    cout << "-W- MpdEmcHitProducer::Init: "
	 << "No EmcPoint array!" << endl;
    return kERROR;
  }
  
  // Create and register output array
  fDigiArray = new TClonesArray("MpdEmcHit");
  
  ioman->Register("EmcHit","Emc",fDigiArray,kTRUE);

  CreateStructure();

  
  hlist = new TList();

  MakeHists();  

  cout << "-I- MpdEmcHitProducer: Intialization successfull" << endl;
  
  return kSUCCESS;

}

//__________________________________________________________________
void MpdEmcHitProducer::Finish() {
  //---

  cout << "-I- MpdEmcHitProducer: FinishTask" << endl;

}


//__________________________________________________________________
void MpdEmcHitProducer::FinishTask() {
  //---

  cout << "-I- MpdEmcHitProducer: Finish" << endl;

 // save histograms of corresponding list
  if (hlist!=0) {
    TObject *obj;
    TIter next(hlist);
    while((obj = (TObject*)next())) obj->Write();
  }
  nt->Write();
 // ntxyz->Write();
//  nl->Write();
}


//__________________________________________________________________


//_________________________________________________________________
void MpdEmcHitProducer::MakeHists() {


  //---
/*  ntE = new TNtuple("EmcPoint","EmcPoint","ELoss");
  ntx = new TNtuple("EmcPoint","EmcPoint","ntx");
  nty = new TNtuple("EmcPoint","EmcPoint","nty");
  ntz = new TNtuple("EmcPoint","EmcPoint","ntz");
  ntphi = new TNtuple("EmcPoint","EmcPoint","ntphi");
  nttheta = new TNtuple("EmcPoint","EmcPoint","nttheta");
  ntnPoints = new TNtuple("EmcPoint","EmcPoint","nPoints");
*/

  nt= new TNtuple("EmcPoint","EmcPoint","nnPoints:ELoss:phi:theta", 1000000);        
 // ntxyz= new TNtuple("XYZE","XYZE", "x:y:z:energy:Px:Py:Pz:Epxyz:tr",1000000);        
//  nl= new TNtuple("ELossXYZ","ELossXYZ","Px:Py:Pz:ELossXYZ");
     
//  fZ    = new TH1F("z","", 500, -250., 250.);
//  hlist->Add(fZ);
  ffELoss    = new TH1F("ELoss","", 500, 0., 1.);
  hlist->Add(ffELoss);

  Float_t xbins[5] = {0., 15., 20., 30., 50.0};


  fR    = new TH1F("r","", 1000, -250., 250.);
  //fR    = new TH1F("r","", 4, xbins);
  hlist->Add(fR);

  fXYp    = new TH2F("xyp","", 1000, -250., 250., 1000, -250., 250.);
  hlist->Add(fXYp);
  fZYp    = new TH2F("zyp","", 1000, -250.,250.,1000,-250., 250.);
  hlist->Add(fZYp);
  
 // fXYm    = new TH2F("xym","", 1000, -196., 196., 1000, -144., -196.);
//  hlist->Add(fXYm);
//  fZYm    = new TH2F("zym","", 1000, -250., 250., 1000, -144., -196.);
//  hlist->Add(fZYm);
//  fXZm    = new TH2F("xz","", 1000, -196., 196., 1000, -250., 250.);
//  hlist->Add(fXZm);

    fXZ    = new TH2F("xz","", 1000, -250., 250., 1000, -250., 250.);
  hlist->Add(fXZ);

  
  
  
 // fRphi    = new TH2F("rphi","", 100, 0., TMath::TwoPi(), 100, 0., 50.);
// hlist->Add(fRphi);
 
 
 

}
 
// -----   Private method SetParContainers   -------------------------------
void MpdEmcHitProducer::SetParContainers() {
  // Get run and runtime database
  FairRunAna* run = FairRunAna::Instance();
  if ( ! run ) Fatal("SetParContainers", "No analysis run");

  FairRuntimeDb* db = run->GetRuntimeDb();
  if ( ! db ) Fatal("SetParContainers", "No runtime database");

  // Get Emc geometry parameter container
  db->getContainer("MpdEmcGeoPar"); 
}

// -----   Public method Exec   --------------------------------------------
void MpdEmcHitProducer::Exec(Option_t* opt) {
  for(int il=0;il<4;il++)fArgs[il]=0;
  for(int il=0;il<9;il++)fxyz[il]=0;  
//  for(int il=0;il<4;il++)fELossXYZ[il]=0;
  //cout << " DIGI EXECUTION *********************" << endl;
  // Reset output array
  if ( ! fDigiArray ) Fatal("Exec", "No DigiArray");

  ///////////////////////////////////////

  FairRuntimeDb* rtdb = FairRun::Instance()->GetRuntimeDb();
  //rtdb->printParamContext();

  MpdEmcGeoPar *geoPar = (MpdEmcGeoPar*) rtdb->getContainer("MpdEmcGeoPar");

  //TString volName = "emc01l";
  TObjArray* sensNodes = geoPar->GetGeoSensitiveNodes();

  cout << " ***---------- EMC sensitive volume " <<sensNodes->GetEntriesFast() << " " << geoPar->GetGeoPassiveNodes()->GetEntriesFast() << endl;
  

  Double_t rMin, rMax;
  for (Int_t i=0; i<sensNodes->GetEntriesFast(); i++){
    FairGeoNode* sensVol = (FairGeoNode*) (sensNodes->At(i));
    TArrayD* params = sensVol->getParameters();
    rMin = params->At(0);
    rMax = params->At(1);
    cout << " *** EMC sensitive volume "<<(i+1) <<": " << sensVol->GetName() << " " << rMin << " " << rMax << endl;    
  }


  /////////////////////////////////////

  fDigiArray->Clear();
  
  // Declare some variables
  MpdEmcPoint* point  = NULL;

  map<Int_t, Float_t> fTrackEnergy;
  fTrackEnergy.clear();
  map<Int_t, Float_t>::const_iterator p;
  Double_t senergy;
  int oldtrackID = -6;
  // Loop over EmcPoints
  Int_t nPoints = fPointArray->GetEntriesFast();
    //    ffELoss->Fill(fELoss);
  senergy=0;	
  Float_t xv,yv,zv,env,tr;
  Float_t Px,Py,Pz;
  Double_t lxyz;
  
//  cout<<"--------------------   "<<nPoints<<endl;
  for (Int_t iPoint=0; iPoint < nPoints; iPoint++) {
    point  = (MpdEmcPoint*) fPointArray->At(iPoint);
    
       env=point->GetEnergyLoss();

        fTrackEnergy[point->GetDetectorID()] += env;//point->GetEnergyLoss();
	senergy+= env;//point->GetEnergyLoss();

    int trackID = point->GetTrackID();
    if (oldtrackID != trackID) {
      
      xv=point->GetX();
      yv=point->GetY();
      zv=point->GetZ();
      tr=trackID;
       Px=point->GetPx();
       Py=point->GetPy();
       Pz=point->GetPz();
       
       lxyz= sqrt((Px*Px)+(Py*Py)+(Pz*Pz));
          
      
      fXZ->Fill(xv, zv);

     // if (yv>=0.){
//        fZ->Fill(zv);
        fXYp->Fill(xv, yv);
        fZYp->Fill(zv, yv);
     // }
    //  if (yv<0.){
//        fZ->Fill(zv);
    //    fXYm->Fill(xv, yv);
    //    fZYm->Fill(zv, yv);
    //  }
      
      
      fxyz[0]=xv;//point->GetX();
      fxyz[1]=yv;//point->GetY();
      fxyz[2]=zv;//point->GetZ();
      fxyz[3]=env;
      fxyz[4]=Px;
      fxyz[5]=Py;
      fxyz[6]=Pz;
      fxyz[7]=lxyz;
      fxyz[8]=tr;
      
      
      TVector2 v = TVector2(xv,yv);      //point->GetX(), point->GetY());
      
      TVector3 vv = TVector3(xv,yv,zv);//point->GetX(), point->GetY(), point->GetZ());
      
      fR->Fill(v.Mod());
//      fRphi->Fill(v.Phi(), v.Mod());
      if (iPoint==0){
	fArgs[2]=vv.Phi();
	fArgs[3]=vv.Theta();//TMath::ATan(fxyz[1]/fxyz[0]);//v.Theta();
	
      }
    }
//ntxyz->Fill(fxyz);    
//nl->Fill(fELossXYZ);
  }

ffELoss->Fill(senergy);
//****************
nnPoints=1.*nPoints;
 
fArgs[0]=nnPoints;
fArgs[1]=senergy;
//fArgs[2]=Phi();
//fArgs[3]=point->GetTheta();
nt->Fill(fArgs);
// cout<<" =========  "<<fArgs[0]<<"  "<<fArgs[1]<<"   "<<fArgs[2]<<"   "<<fArgs[3]<< endl;
//*********
  
#if 0
  // Loop to register EmcHit
  for(p=fTrackEnergy.begin(); p!=fTrackEnergy.end(); ++p) {
    if ((*p).second>eneThr)
      AddHit(1, (*p).first, (*p).second); 
  }
#endif 
}
// -------------------------------------------------------------------------


// -----   Public method Create Structure   --------------------------------
void MpdEmcHitProducer::CreateStructure() { 
 
  /*
  TString work = getenv("VMCWORKDIR");
  work = work + "/geometry/" + fFileGeo;
  cout << "-I- <MpdEmcHitProducer::CreateStructure> Emc geometry loaded from: "
       << work << endl;

  Int_t detId = -1;
  MpdEmcReader read(work);
  
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
MpdEmcHit* MpdEmcHitProducer::AddHit(Int_t trackID,Int_t detID, Float_t energy){
  // It fills the MpdEmcHit category
  // cout << "MpdEmcHitProducer: track " << trackID << " evt " << eventID << " sec " << sec << " plane " << pla << " strip " << strip << "box " << box << " tube " << tub << endl;

  TClonesArray& clref = *fDigiArray;
  Int_t size = clref.GetEntriesFast();
  return new(clref[size]) MpdEmcHit(); // FIXME: real hit info needed here
}
// ----

ClassImp(MpdEmcHitProducer)
