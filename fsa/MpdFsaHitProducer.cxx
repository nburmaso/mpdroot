/////////////////////////////////////////////////////////////
//
//  MpdFsaHitProducer
//
//  Filler of MpdFsaHit
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

#include "MpdFsaHitProducer.h"
#include "MpdFsaHit.h"
#include "MpdFsaPoint.h"

#include "MpdFsaGeoPar.h"
#include "FairRunAna.h"
#include "TIterator.h"
#include "TObjArray.h"
#include "FairGeoNode.h"

// -----   Default constructor   -------------------------------------------
MpdFsaHitProducer::MpdFsaHitProducer(const char* fileGeo) :
  FairTask("Ideal FSA hit Producer") {
  fFileGeo=fileGeo;
  eneThr = 0.001; // Energy threshold for FSA
  
  Int_t n = 200;
  xt = new Bool_t[n];
  yt = new Bool_t[n];
  for (Int_t i = 0; i < n; i++)
  {
  	xt[i] = false;
  	yt[i] = false;
  }

}


// -----   Destructor   ----------------------------------------------------
MpdFsaHitProducer::~MpdFsaHitProducer() { }
// -------------------------------------------------------------------------



// -----   Public method Init   --------------------------------------------
InitStatus MpdFsaHitProducer::Init() {
 
  cout << "******************* INITIALIZATION *********************" << endl;
  
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
  
  // Create and register output array
  fDigiArray = new TClonesArray("MpdFsaHit");
  
  ioman->Register("FsaHit","Fsa",fDigiArray,kTRUE);

  CreateStructure();

  hlist = new TList();

  MakeHists();  

  cout << "-I- MpdFsaHitProducer: Intialization successfull" << endl;
  
  return kSUCCESS;

}

//__________________________________________________________________
void MpdFsaHitProducer::FinishTask() {
  //---

  cout << "-I- MpdFsaHitProducer: FinishTask" << endl;

 // save histograms of corresponding list
  if (hlist!=0) {
    TObject *obj;
    TIter next(hlist);
    while((obj = (TObject*)next())) obj->Write();
  }
}


//__________________________________________________________________
void MpdFsaHitProducer::Finish() {
  //---

  cout << "-I- MpdFsaHitProducer: Finish" << endl;

}


//__________________________________________________________________
void MpdFsaHitProducer::MakeHists() {
  //---

   fZ    = new TH1F("z","", 1000, 740., 1000.);
   hlist->Add(fZ);
// 
//   fR    = new TH1F("r","", 230, 40., 270.);
//   hlist->Add(fR);
// 
//   fLoadXL1  = new TH1F("loadXLayer1","", 540, -270., 270.);
//   hlist->Add(fLoadXL1);
//   fLoadXL2  = new TH1F("loadXLayer2","", 540, -270., 270.);
//   hlist->Add(fLoadXL2);
//   fLoadXL3  = new TH1F("loadXLayer3","", 540, -270., 270.);
//   hlist->Add(fLoadXL3);
//   fLoadXL4  = new TH1F("loadXLayer4","", 540, -270., 270.);
//   hlist->Add(fLoadXL4);
//   fLoadXL5  = new TH1F("loadXLayer5","", 540, -270., 270.);
//   hlist->Add(fLoadXL5);
//   
//   fLoadYL1  = new TH1F("loadYLayer1","", 540, -270., 270.);
//   hlist->Add(fLoadYL1);
//   fLoadYL2  = new TH1F("loadYLayer2","", 540, -270., 270.);
//   hlist->Add(fLoadYL2);
//   fLoadYL3  = new TH1F("loadYLayer3","", 540, -270., 270.);
//   hlist->Add(fLoadYL3);
//   fLoadYL4  = new TH1F("loadYLayer4","", 540, -270., 270.);
//   hlist->Add(fLoadYL4);
//   fLoadYL5  = new TH1F("loadYLayer5","", 540, -270., 270.);
//   hlist->Add(fLoadYL5);
// 
//   fTime = new TH1F("Time","",500,0.0,500.0);
//   hlist->Add(fTime);
// 
//   fXY    = new TH2F("xy","", 540, -270., 270., 540, -270., 270.);
//   hlist->Add(fXY);
// 
//   fRphi    = new TH2F("rphi","", 360, 0., TMath::TwoPi(), 230, 40., 270.);
//   hlist->Add(fRphi);
//   
//   fLoadXYL1 = new TH2F("LoadXYLayer1","", 540, -270., 270., 540, -270., 270.);
//   hlist->Add(fLoadXYL1);
  
  ///////////////////////////
  
  fLoadX1  = new TH1F("loadXLayer1","", 540, -270., 270.);
  hlist->Add(fLoadX1);
  fLoadX1u  = new TH1F("loadXLayer1u","", 540, -270., 270.);
  hlist->Add(fLoadX1u);
  fLoadX1d  = new TH1F("loadXLayer1d","", 540, -270., 270.);
  hlist->Add(fLoadX1d);
  fLoadY1  = new TH1F("loadYLayer1","", 540, -270., 270.);
  hlist->Add(fLoadY1);
  fLoadY1u  = new TH1F("loadYLayer1u","", 540, -270., 270.);
  hlist->Add(fLoadY1u);
  fLoadY1d  = new TH1F("loadYLayer1d","", 540, -270., 270.);
  hlist->Add(fLoadY1d);
  
  fLoadX2  = new TH1F("loadXLayer2","", 540, -270., 270.);
  hlist->Add(fLoadX2);
  fLoadX2u  = new TH1F("loadXLayer2u","", 540, -270., 270.);
  hlist->Add(fLoadX2u);
  fLoadX2d  = new TH1F("loadXLayer2d","", 540, -270., 270.);
  hlist->Add(fLoadX2d);
  fLoadY2  = new TH1F("loadYLayer2","", 540, -270., 270.);
  hlist->Add(fLoadY2);
  fLoadY2u  = new TH1F("loadYLayer2u","", 540, -270., 270.);
  hlist->Add(fLoadY2u);
  fLoadY2d  = new TH1F("loadYLayer2d","", 540, -270., 270.);
  hlist->Add(fLoadY2d);
  
  fLoadX3  = new TH1F("loadXLayer3","", 540, -270., 270.);
  hlist->Add(fLoadX3);
  fLoadX3u  = new TH1F("loadXLayer3u","", 540, -270., 270.);
  hlist->Add(fLoadX3u);
  fLoadX3d  = new TH1F("loadXLayer3d","", 540, -270., 270.);
  hlist->Add(fLoadX3d);
  fLoadY3  = new TH1F("loadYLayer3","", 540, -270., 270.);
  hlist->Add(fLoadY3);
  fLoadY3u  = new TH1F("loadYLayer3u","", 540, -270., 270.);
  hlist->Add(fLoadY3u);
  fLoadY3d  = new TH1F("loadYLayer3d","", 540, -270., 270.);
  hlist->Add(fLoadY3d);
  
  fLoadX4  = new TH1F("loadXLayer4","", 540, -270., 270.);
  hlist->Add(fLoadX4);
  fLoadX4u  = new TH1F("loadXLayer4u","", 540, -270., 270.);
  hlist->Add(fLoadX4u);
  fLoadX4d  = new TH1F("loadXLayer4d","", 540, -270., 270.);
  hlist->Add(fLoadX4d);
  fLoadY4  = new TH1F("loadYLayer4","", 540, -270., 270.);
  hlist->Add(fLoadY4);
  fLoadY4u  = new TH1F("loadYLayer4u","", 540, -270., 270.);
  hlist->Add(fLoadY4u);
  fLoadY4d  = new TH1F("loadYLayer4d","", 540, -270., 270.);
  hlist->Add(fLoadY4d);
  
  fLoadX5  = new TH1F("loadXLayer5","", 540, -270., 270.);
  hlist->Add(fLoadX5);
  fLoadX5u  = new TH1F("loadXLayer5u","", 540, -270., 270.);
  hlist->Add(fLoadX5u);
  fLoadX5d  = new TH1F("loadXLayer5d","", 540, -270., 270.);
  hlist->Add(fLoadX5d);
  fLoadY5  = new TH1F("loadYLayer5","", 540, -270., 270.);
  hlist->Add(fLoadY5);
  fLoadY5u  = new TH1F("loadYLayer5u","", 540, -270., 270.);
  hlist->Add(fLoadY5u);
  fLoadY5d  = new TH1F("loadYLayer5d","", 540, -270., 270.);
  hlist->Add(fLoadY5d);
  
  fLoadXY1 = new TH2F("LoadXYLayer1","", 540, -270., 270., 540, -270., 270.);
  hlist->Add(fLoadXY1);
  fRealXY1 = new TH2F("RealXYLayer1","", 540, -270., 270., 540, -270., 270.);
  hlist->Add(fRealXY1);
}
 


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
  for (Int_t iPoint=0; iPoint < nPoints; iPoint++) {
    point  = (MpdFsaPoint*) fPointArray->At(iPoint);
    //    fTrackEnergy[point->GetDetectorID()] += point->GetEnergyLoss();

     fZ->Fill(point->GetZ());
//     fXY->Fill(point->GetX(), point->GetY());
//     TVector2 v = TVector2(point->GetX(), point->GetY());
//     fR->Fill(v.Mod());
//     fRphi->Fill(v.Phi(), v.Mod());
//     
//     if(point->GetZ()< 761.0 && point->GetZ()> 760.0){
//       fLoadXL1->Fill(point->GetX());
//       fLoadXYL1->Fill(point->GetX(), point->GetY());
//       fTime->Fill(point->GetTime());
//     }
//     if(point->GetZ()< 815.5 && point->GetZ()> 814.5)
//       fLoadXL2->Fill(point->GetX());
//     if(point->GetZ()< 870.0 && point->GetZ()> 869.0)
//       fLoadXL3->Fill(point->GetX());
//     if(point->GetZ()< 924.5 && point->GetZ()> 923.5)
//       fLoadXL4->Fill(point->GetX());
//     if(point->GetZ()< 979.0 && point->GetZ()> 978.0)
//       fLoadXL5->Fill(point->GetX());
//       
//     if(point->GetZ()< 762.0 && point->GetZ()> 761.0)
//       fLoadYL1->Fill(point->GetY());
//     if(point->GetZ()< 816.5 && point->GetZ()> 815.5)
//       fLoadYL2->Fill(point->GetY());
//     if(point->GetZ()< 871.0 && point->GetZ()> 870.0)
//       fLoadYL3->Fill(point->GetY());
//     if(point->GetZ()< 925.5 && point->GetZ()> 924.5)
//       fLoadYL4->Fill(point->GetY());
//     if(point->GetZ()< 980.0 && point->GetZ()> 979.0)
//       fLoadYL5->Fill(point->GetY());

	/////////////////////
	//layer1
	if(point->GetZ()< 761.0 && point->GetZ()> 760.0){
		fLoadX1->Fill(point->GetX());
		if(point->GetY()>=0.0) fLoadX1u->Fill(point->GetX());
		if(point->GetY()<0.0) fLoadX1d->Fill(point->GetX());
	}
	if(point->GetZ()< 762.0 && point->GetZ()> 761.0){
		fLoadY1->Fill(point->GetY());
		if(point->GetX()>=0.0) fLoadY1u->Fill(point->GetY());
		if(point->GetX()<0.0) fLoadY1d->Fill(point->GetY());
	}
	//layer2
	if(point->GetZ()< 815.5 && point->GetZ()> 814.5){
		fLoadX2->Fill(point->GetX());
		if(point->GetY()>=0.0) fLoadX2u->Fill(point->GetX());
		if(point->GetY()<0.0) fLoadX2d->Fill(point->GetX());
	}
	if(point->GetZ()< 816.5 && point->GetZ()> 815.5){
		fLoadY2->Fill(point->GetY());
		if(point->GetX()>=0.0) fLoadY2u->Fill(point->GetY());
		if(point->GetX()<0.0) fLoadY2d->Fill(point->GetY());
	}
	//layer3
	if(point->GetZ()< 870.0 && point->GetZ()> 869.0){
		fLoadX3->Fill(point->GetX());
		if(point->GetY()>=0.0) fLoadX3u->Fill(point->GetX());
		if(point->GetY()<0.0) fLoadX3d->Fill(point->GetX());
	}
	if(point->GetZ()< 871.0 && point->GetZ()> 870.0){
		fLoadY3->Fill(point->GetY());
		if(point->GetX()>=0.0) fLoadY3u->Fill(point->GetY());
		if(point->GetX()<0.0) fLoadY3d->Fill(point->GetY());
	}
	//layer4
	if(point->GetZ()< 924.5 && point->GetZ()> 923.5){
		fLoadX4->Fill(point->GetX());
		if(point->GetY()>=0.0) fLoadX4u->Fill(point->GetX());
		if(point->GetY()<0.0) fLoadX4d->Fill(point->GetX());
	}
	if(point->GetZ()< 925.5 && point->GetZ()> 924.5){
		fLoadY4->Fill(point->GetY());
		if(point->GetX()>=0.0) fLoadY4u->Fill(point->GetY());
		if(point->GetX()<0.0) fLoadY4d->Fill(point->GetY());
	}
	//layer5
	if(point->GetZ()< 979.0 && point->GetZ()> 978.0){
		fLoadX5->Fill(point->GetX());
		if(point->GetY()>=0.0) fLoadX5u->Fill(point->GetX());
		if(point->GetY()<0.0) fLoadX5d->Fill(point->GetX());
	}
	if(point->GetZ()< 980.0 && point->GetZ()> 979.0){
		fLoadY5->Fill(point->GetY());
		if(point->GetX()>=0.0) fLoadY5u->Fill(point->GetY());
		if(point->GetX()<0.0) fLoadY5d->Fill(point->GetY());
	}
	//layer1xy
	if(point->GetZ()< 761.0 && point->GetZ()> 760.0)
	{
		Int_t tx = (Int_t)floor(point->GetX());
		Int_t ty = (Int_t)floor(point->GetY());
		if (tx >= 0 && ty >= 0)
		{
			const Double_t initDistMin = 7600.0;
			const Double_t initDistMax = 9800.0;
			const Double_t layerThickness = 20.0;
			const Int_t numLayers = 5;
			Double_t distBetLayers = (initDistMax - initDistMin - layerThickness) / (numLayers - 1.0);
			const Double_t angleRMax = 14.0;
			const Double_t angleRMin = 5.0;
			const Double_t tubeRadius = 5.0;
			Double_t outerRadius = (initDistMin + 0 /*1 layer*/ * distBetLayers) * TMath::Tan(angleRMax * TMath::DegToRad());
			Double_t innerRadius = (initDistMin + 0 * distBetLayers) * TMath::Tan(angleRMin * TMath::DegToRad());
			Int_t numStrawsPerLayer = (Int_t)floor(outerRadius / (2 * tubeRadius));
			Double_t outR2 = outerRadius * outerRadius;
			Double_t inR2 = innerRadius * innerRadius;
			Double_t tubeDiameter = 2 * tubeRadius;
			if (xt[tx] == false)
			{
				//xt[tx] = true;
				Int_t curYu = (Int_t)floor((TMath::Sqrt(outR2 - TMath::Power(tubeDiameter * (tx + 1), 2))) / 10.0);
				Int_t curYd = 0;
				if (tubeDiameter * tx < innerRadius)
				{
					curYd = (Int_t)ceil((TMath::Sqrt(inR2 - TMath::Power(tubeDiameter * tx, 2))) / 10.0);
				}
				//Double_t movX = tubeDiameter * tx + tubeRadius;
				//cout << tx << "  " << curYd << " " << curYu;
				for (Int_t i = curYd; i < curYu; i++)
				{
					if(yt[i] == true)
					{
						fLoadXY1->Fill(tx + 0.5, i + 0.5);
						//cout << i << " ";
					}
				}
				//cout << endl;
			}
			if (yt[ty] == false)
			{
				//yt[ty] = true;
				Int_t curXu = (Int_t)floor((TMath::Sqrt(outR2 - TMath::Power(tubeDiameter * (ty + 1), 2))) / 10.0);
				Int_t curXd = 0;
				if (tubeDiameter * ty < innerRadius)
				{
					curXd = (Int_t)ceil((TMath::Sqrt(inR2 - TMath::Power(tubeDiameter * ty, 2))) / 10.0);
				}
				//Double_t movX = tubeDiameter * tx + tubeRadius;
				for (Int_t i = curXd; i < curXu; i++)
				{
					if(xt[i] == true) fLoadXY1->Fill(i + 0.5, ty + 0.5);
				}
			}
			xt[tx] = true;
			yt[ty] = true;
			fLoadXY1->Fill(tx + 0.5, ty + 0.5);
			fRealXY1->Fill(tx + 0.5, ty + 0.5);
		}
	}
	
	/*FairRuntimeDb* rtdb = FairRun::Instance()->GetRuntimeDb();
	MpdFsaGeoPar *geoPar = (MpdFsaGeoPar*) rtdb->getContainer("MpdFsaGeoPar");
	TObjArray* sensNodes = geoPar->GetGeoSensitiveNodes();
	cout << sensNodes->LowerBound() << endl;
	TIterator* it = sensNodes->MakeIterator();
	Int_t k = 0;
	while (it->Next() != 0)
	{
		//TIterator* tmp = it;
		cout << k++ << ((FairGeoNode*)&it)->getName() << endl;
	}*/
	
	/*Int_t j = 0; //layer 1
	const Double_t initDistMin = 7600.0;
	const Double_t initDistMax = 9800.0;
	const Double_t layerThickness = 20.0;
	const Int_t numLayers = 5;
	Double_t distBetLayers = (initDistMax - initDistMin - layerThickness) / (numLayers - 1.0);
	const Double_t angleRMax = 14.0;
	const Double_t tubeRadius = 5.0;
	Int_t outerRadius = (initDistMin + j * distBetLayers) * TMath::Tan(angleRMax * TMath::DegToRad());
	Int_t numStrawsPerLayer = floor(outerRadius / (2 * tubeRadius));
	for (Int_t i = 0; i < numStrawsPerLayer; i++)
	{
		TString volName = "fsa01gas";
		volName += itoa(j);
		volName += itoa(i);
		volName += "#1";
		FairGeoNode* sensVol = (FairGeoNode*) (sensNodes->FindObject(volName));
	}*/
  }

  
#if 0
  // Loop to register FsaHit
  for(p=fTrackEnergy.begin(); p!=fTrackEnergy.end(); ++p) {
    if ((*p).second>eneThr)
      AddHit(1, (*p).first, (*p).second); 
  }
#endif 
}
// -------------------------------------------------------------------------


// -----   Public method Create Structure   --------------------------------
void MpdFsaHitProducer::CreateStructure() { 
 
  /*
  TString work = getenv("VMCWORKDIR");
  work = work + "/geometry/" + fFileGeo;
  cout << "-I- <MpdFsaHitProducer::CreateStructure> Fsa geometry loaded from: "
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
