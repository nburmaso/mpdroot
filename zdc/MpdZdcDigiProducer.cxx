/*************************************************************************************
 *
 *            MpdZdcDigiProducer 
 *    Class to create digital data taken from MpdZdc detector 
 *         
 *  Author:   Elena Litvinenko
 *  e-mail:   litvin@nf.jinr.ru
 *  Version:  18-Apr-2008
 *
 ************************************************************************************/


#include <iostream>
#include "TClonesArray.h"

#include "FairRootManager.h"
#include "FairRun.h"
#include "FairRunAna.h"
#include "FairRuntimeDb.h"

#include "MpdZdcDigiProducer.h"
#include "MpdZdcDigi.h"
#include "MpdZdcPoint.h"

#include "TROOT.h"
#include "TVectorT.h"


// -----   Default constructor   -------------------------------------------
MpdZdcDigiProducer::MpdZdcDigiProducer(const char* name) :
  FairTask(name) {
  fPointArray=0;
  fDigiArray=0;
  fGeoPar=0;
  fHistZdc1En=0;
  fHistZdc2En=0;
  fELossZdc1Value = NULL, fELossZdc2Value = NULL, fELossZdc1Histo = NULL, fELossZdc2Histo = NULL;
}
// -------------------------------------------------------------------------

// -----   Destructor   ----------------------------------------------------
MpdZdcDigiProducer::~MpdZdcDigiProducer() { }
// -------------------------------------------------------------------------

// -------------------------------------------------------------------------
void MpdZdcDigiProducer::SetParContainers() 
{
   cout << "-I- MpdZdcDigiProducer: SetParContainers started..." << endl;

   //   Get run and runtime database
   FairRunAna* run = FairRunAna::Instance();
   if ( ! run ) Fatal("FairMuchDigitize::SetParContainers", "No analysis run");

   FairRuntimeDb* rtdb = run->GetRuntimeDb();
   if ( ! rtdb ) Fatal("FairMuchDigitize::SetParContainers", "No runtime database");
  
   cout << "-I- MpdZdcDigiProducer: SetParContainers continued..." << endl;

   rtdb->activateParIo(rtdb->getFirstInput());
   //   fGeoPar=( MpdZdcGeoPar*) rtdb->getContainer("MpdZdcGeoPar");
   fGeoPar=( MpdZdcGeoPar*) gROOT->FindObject("MpdZdcGeoPar");
   fGeoPar->print();

   cout << "-I- MpdZdcDigiProducer: SetParContainers finished." << endl;
}

// -------------------------------------------------------------------------
// -----   Public method Init   --------------------------------------------
InitStatus MpdZdcDigiProducer::Init() {
 

  cout << "-I- MpdZdcDigiProducer: Init started..." << endl;

  // Get RootManager
  FairRootManager* ioman = FairRootManager::Instance();
  if ( ! ioman ) {
    cout << "-E- MpdZdcDigiProducer::Init: "
	 << "RootManager not instantiated!" << endl;
    return kFATAL;
  }
  
  // Get input array
  fPointArray = (TClonesArray*) ioman->GetObject("ZdcPoint");
  if ( ! fPointArray ) {
    cout << "-W- MpdZdcDigiProducer::Init: "
	 << "No ZdcPoint array!" << endl;
    return kERROR;
  }
  
  // Create and register output array
  fDigiArray = new TClonesArray("MpdZdcDigi");  
  ioman->Register("ZdcDigi","Zdc",fDigiArray,kTRUE);


  fELossZdc1Value = new TClonesArray("TParameter<double>");
  ioman->Register("ELossZdc1Value","Zdc",fELossZdc1Value,kTRUE);

  fELossZdc2Value = new TClonesArray("TParameter<double>");
  ioman->Register("ELossZdc2Value","Zdc",fELossZdc2Value,kTRUE);

  fELossZdc1Histo = new TClonesArray("TVectorT<float>");
  ioman->Register("ELossZdc1Histo","Zdc",fELossZdc1Histo,kTRUE);

  fELossZdc2Histo = new TClonesArray("TVectorT<float>");
  ioman->Register("ELossZdc2Histo","Zdc",fELossZdc2Histo,kTRUE);  

  MpdZdcDigiScheme *fDigiScheme  = MpdZdcDigiScheme::Instance();
  fDigiScheme->Init(fGeoPar,0,kTRUE,2);

  cout << "-I- MpdZdcDigiProducer: Intialization successfull" << endl;
  
  return kSUCCESS;

}
// -------------------------------------------------------------------------
void MpdZdcDigiProducer::CreateHistograms ( MpdZdcDigiId_t *pDigiID)
{
  Int_t nx,ny, nz;
  Double_t dx, dy, dz;

  MpdZdcDigiScheme *fDigiScheme  = MpdZdcDigiScheme::Instance();
  fDigiScheme->GetZdcDimensions (nx,ny,nz);
  fDigiScheme->GetVolDxDyDz (pDigiID,dx, dy, dz);
  
  Int_t Nx=nx+2;
  Double_t Dx=dx*Nx; 
  Int_t Ny=ny+2;
  Double_t Dy=dy*Ny; 

  fHistZdc1En = new TH2F ("HistZdc1En","HistZdc1Energy",Nx,-Dx,Dx,Ny,-Dy,Dy);
  fHistZdc2En = new TH2F ("HistZdc2En","HistZdc2Energy",Nx,-Dx,Dx,Ny,-Dy,Dy);

  if ((!fHistZdc1En)||(!fHistZdc2En)) 
    cout << "-E- MpdZdcDigiProducer: HistZdc1En or HistZdc2En Histograms not created !!" << endl;
  else {
    FairRootManager* ioman = FairRootManager::Instance();
    fHistZdc1En->SetDirectory((TFile*)ioman->GetOutFile());
    fHistZdc2En->SetDirectory((TFile*)ioman->GetOutFile());
    fHistZdc1En->Write();
    fHistZdc2En->Write();
}

}

// -----   Public method Exec   --------------------------------------------
void MpdZdcDigiProducer::Exec(Option_t* opt) {
 

  //#define EDEBUG
#ifdef EDEBUG
  static Int_t lEDEBUGcounter=0;
  cout << "EDEBUG-- MpdZdcDigiProducer::Exec() started... " << endl;;
#endif

  if ( ! fDigiArray ) Fatal("Exec", "No DigiArray");
  
  fDigiArray->Clear();

  MpdZdcDigiScheme *pDigiScheme  = MpdZdcDigiScheme::Instance();

  if (!pDigiScheme) 
    Fatal("MpdZdcDigiProducer::Exec", "No DigiScheme");

  Int_t detID, modID, chanID;
  MpdZdcDigiId_t digiID;

  //marina
  Double_t dEdepSectEv[90][10];  

    for(Int_t i=0; i<90; i++) {// mod    
      for(Int_t ii=0;ii<10;ii++) { // section 
	dEdepSectEv[i][ii] = 0.;
      }
    }
    //end marina
  
  MpdZdcPoint* point  = NULL;

  map<MpdZdcDigiId_t, Float_t> fDigiIdEnergy;
  fDigiIdEnergy.clear();
  map<MpdZdcDigiId_t, Float_t>::const_iterator p;
  
  Int_t nPoints = fPointArray->GetEntriesFast();
  Double_t e1=0, e2=0;

  if (fHistZdc1En) {
    fHistZdc1En->Reset();
    fHistZdc2En->Reset();
  }
  TH2F* hist1=fHistZdc1En;
  TH2F* hist2=fHistZdc2En;

  Bool_t flag_of_not_created=1;
  //cout <<"marina " <<nPoints <<endl;
  for (Int_t iPoint=0; iPoint<nPoints; iPoint++) {
    //marina
    point  = (MpdZdcPoint*) fPointArray->At(iPoint);
    //cout <<"marina 1 " <<point->GetCopyZdc() <<" " <<point->GetCopyMother() <<" " <<point->GetCopy() <<endl;

    detID = point->GetCopyZdc();//==1 (z>0), ==2 (z<0)
    //modID  = point->GetCopyMother(); // modules 1-45
    if(detID==1) modID  = point->GetCopyMother(); // modules 1-45
    else modID = point->GetCopyMother() + 45;//modules 46-90
    chanID = (Int_t)((point->GetCopy()-1)/6); //sections 0-9

//    cout <<"marina 1 " <<detID <<" " <<modID <<" " <<point->GetCopy() <<' ' <<chanID <<endl;

    dEdepSectEv[modID-1][chanID]+=point->GetEnergyLoss();
    //end marina

    Int_t pMMcopy=1*(point->GetZ()>0)+2*(point->GetZ()<0);
    digiID = pDigiScheme->GetDigiIdFromVolumeData  (point->GetDetectorID(), point->GetCopy(), point->GetCopyMother(),pMMcopy);

    if ((digiID[0]!=-1)&&(digiID[1]!=-1)) {


      if (!fHistZdc1En) {

	CreateHistograms(&digiID);

	hist1=fHistZdc1En;
	hist2=fHistZdc2En;
      }

      if (fDigiIdEnergy.find(digiID)==fDigiIdEnergy.end())
	fDigiIdEnergy[digiID] = point->GetEnergyLoss();
      else
	fDigiIdEnergy[digiID] += point->GetEnergyLoss();

      if (pMMcopy==1) {
	e1 += point->GetEnergyLoss();
        hist1->Fill(point->GetX(),point->GetY(),point->GetEnergyLoss());
      }
      else {
	e2 += point->GetEnergyLoss();
        hist2->Fill(point->GetX(),point->GetY(),point->GetEnergyLoss());
      }

    }//if ((digiID[0]!=-1)&&(digiID[1]!=-1))

#ifdef EDEBUG
    else {
      if (lEDEBUGcounter<100) {
	cout << "EDEBUG-- MpdZdcDigiProducer::Exec:  Boundary point? : "; point->Print("");
	lEDEBUGcounter++;
      }
    }
#endif

  }//for (Int_t iPoint=0; iPoint<nPoints; iPoint++)

  //cout <<"marina 2 " <<endl;
  TClonesArray& clref1 = *fELossZdc1Value;
  new(clref1[0]) TParameter<double>("ELossZdc1",e1);
  TClonesArray& clref2 = *fELossZdc2Value;
  new(clref2[0]) TParameter<double>("ELossZdc2",e2); 

  if (fHistZdc1En) {
    TClonesArray& clref1e = *fELossZdc1Histo;
    new(clref1e[0]) TVectorT<float>(fHistZdc1En->GetSize(),fHistZdc1En->GetArray());
  }

  if (fHistZdc2En) {
    TClonesArray& clref2e = *fELossZdc2Histo;
    new(clref2e[0]) TVectorT<float>(fHistZdc2En->GetSize(),fHistZdc2En->GetArray());
  }
  
  //cout <<"marina 3 " <<endl;
    for(Int_t i=0; i<90; i++) {// mod    
      for(Int_t ii=0;ii<10;ii++) { // section 
	//cout <<"dEdepSectEv " <<i <<" " <<ii <<" " <<dEdepSectEv[i][ii] <<endl;
	if(dEdepSectEv[i][ii]>0) {
	  if(i<=44) detID = 1;
	  else detID = 2;
	  if(detID==1) {
	    MpdZdcDigi* digi = AddHit(detID, i+1, ii+1, dEdepSectEv[i][ii]); 
	    digi->ConvertSim();
	  }
	  else {
	    MpdZdcDigi* digi = AddHit(detID, i-45+1, ii+1, dEdepSectEv[i][ii]);
	    digi->ConvertSim();
	  }
	}
      }
    }//for(Int_t i=0; i<90; i++)

    /*
  for(p=fDigiIdEnergy.begin(); p!=fDigiIdEnergy.end(); ++p) {

    pDigiScheme->SplitDigiID((*p).first, detID, modID, chanID);


    if ((detID!=-1)&&(chanID!=-1)) {
      MpdZdcDigi* digi = AddHit(detID, modID, chanID, (*p).second); 
      digi->ConvertSim();
#ifdef EDEBUG
      if (lEDEBUGcounter<20) {
	cout << "EDEBUG-- MpdZdcDigiProducer::Exec: "<< detID<< " " << chanID << "   " << 
	  (*p).second << "     " << lEDEBUGcounter << endl;
	lEDEBUGcounter++;
      }
#endif
    }

  }
    */
 
#undef EDEBUG
}
// -------------------------------------------------------------------------



// -----   Private method AddDigi   --------------------------------------------
MpdZdcDigi* MpdZdcDigiProducer::AddHit(Int_t detID, Int_t modID, Int_t chanID,Float_t energy)
{
  TClonesArray& clref = *fDigiArray;
  Int_t size = clref.GetEntriesFast();
  //cout <<"size " <<size <<endl;
  MpdZdcDigi* result = new(clref[size]) MpdZdcDigi(detID,modID,chanID,energy);
  //cout <<"result " <<result <<endl;
  return result;
}
// ----


ClassImp(MpdZdcDigiProducer)
