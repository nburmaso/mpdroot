/*************************************************************************************
 *
 *            BmdDigiProducer
 *    Class to create digital data taken from Bmd detector
 *
 *  Author:   RO
 *  e-mail:   rogachevsky@jinr.ru
 *  Version:  19-May-2019
 *
 ************************************************************************************/

#include "TClonesArray.h"
#include <iostream>

#include "FairRootManager.h"
#include "FairRun.h"
#include "FairRunAna.h"
#include "FairRuntimeDb.h"

#include "CpcDigi.h"
#include "CpcDigiProducer.h"
#include "MpdCpcPoint.h"

#include "TROOT.h"
#include "TVectorT.h"

using namespace std;

// -----   Default constructor   -------------------------------------------
CpcDigiProducer::CpcDigiProducer(const char *name) : FairTask(name) {
  fPointArray = 0;
  fDigiArray = 0;
  fGeoPar = 0;
  fHistCpc1En = 0;
  fHistCpc2En = 0;
  fELossCpc1Value = NULL, fELossCpc2Value = NULL, fELossCpc1Histo = NULL,
  fELossCpc2Histo = NULL;
}
// -------------------------------------------------------------------------

// -----   Destructor   ----------------------------------------------------
CpcDigiProducer::~CpcDigiProducer() {}
// -------------------------------------------------------------------------

// -------------------------------------------------------------------------
void CpcDigiProducer::SetParContainers() {
  cout << "-I- CpcDigiProducer: SetParContainers started..." << endl;

  //   Get run and runtime database
  FairRunAna *run = FairRunAna::Instance();
  if (!run)
    Fatal("FairMuchDigitize::SetParContainers", "No analysis run");

  FairRuntimeDb *rtdb = run->GetRuntimeDb();
  if (!rtdb)
    Fatal("FairMuchDigitize::SetParContainers", "No runtime database");

  cout << "-I- BmdDigiProducer: SetParContainers continued..." << endl;

  rtdb->activateParIo(rtdb->getFirstInput());
  //   fGeoPar=( MpdCpcGeoPar*) rtdb->getContainer("MpdCpcGeoPar");
  fGeoPar = (MpdCpcGeoPar *)gROOT->FindObject("MpdCpcGeoPar");
  fGeoPar->print();

  cout << "-I- CpcDigiProducer: SetParContainers finished." << endl;
}

// -------------------------------------------------------------------------
// -----   Public method Init   --------------------------------------------
InitStatus CpcDigiProducer::Init() {

  cout << "-I- CpcDigiProducer: Init started..." << endl;

  // Get RootManager
  FairRootManager *ioman = FairRootManager::Instance();
  if (!ioman) {
    cout << "-E- CpcDigiProducer::Init: "
         << "RootManager not instantiated!" << endl;
    return kFATAL;
  }

  // Get input array
  fPointArray = (TClonesArray *)ioman->GetObject("MpdCpcPoint");
  if (!fPointArray) {
    cout << "-W- CpcDigiProducer::Init: "
         << "No MpdCpcPoint array!" << endl;
    return kERROR;
  }

  // Create and register output array
  fDigiArray = new TClonesArray("CpcDigi");
  ioman->Register("CpcDigi", "Cpc", fDigiArray, kTRUE);

  fELossCpc1Value = new TClonesArray("TParameter<double>");
  ioman->Register("ELossCpc1Value", "Cpc", fELossCpc1Value, kTRUE);

  fELossCpc2Value = new TClonesArray("TParameter<double>");
  ioman->Register("ELossBmd2Value", "Cpc", fELossCpc2Value, kTRUE);

  fELossCpc1Histo = new TClonesArray("TVectorT<float>");
  ioman->Register("ELossCpc1Histo", "Cpc", fELossCpc1Histo, kTRUE);

  fELossCpc2Histo = new TClonesArray("TVectorT<float>");
  ioman->Register("ELossCpc2Histo", "Cpc", fELossCpc2Histo, kTRUE);

  CpcDigiScheme *fDigiScheme = CpcDigiScheme::Instance();
  fDigiScheme->Init(fGeoPar, 0, kTRUE, 2);

  cout << "-I- CpcDigiProducer: Intialization successfull" << endl;

  return kSUCCESS;
}
// -------------------------------------------------------------------------
void CpcDigiProducer::CreateHistograms(CpcDigiId_t *pDigiID) {
  Int_t nx, ny, nz;
  Double_t dx, dy, dz;

  CpcDigiScheme *fDigiScheme = CpcDigiScheme::Instance();
  fDigiScheme->GetCpcDimensions(nx, ny, nz);
  fDigiScheme->GetVolDxDyDz(pDigiID, dx, dy, dz);

  Int_t Nx = nx + 2;
  Double_t Dx = dx * Nx;
  Int_t Ny = ny + 2;
  Double_t Dy = dy * Ny;

  fHistCpc1En =
      new TH2F("HistCpc1En", "HistCpc1Energy", Nx, -Dx, Dx, Ny, -Dy, Dy);
  fHistCpc2En =
      new TH2F("HistCpc2En", "HistCpc2Energy", Nx, -Dx, Dx, Ny, -Dy, Dy);

  if ((!fHistCpc1En) || (!fHistCpc2En))
    cout << "-E- CpcDigiProducer: HistCpc1En or HistCpc2En Histograms not "
            "created !!"
         << endl;
  else {
    FairRootManager *ioman = FairRootManager::Instance();
    fHistCpc1En->SetDirectory((TFile *)ioman->GetOutFile());
    fHistCpc2En->SetDirectory((TFile *)ioman->GetOutFile());
    fHistCpc1En->Write();
    fHistCpc2En->Write();
  }
}

// -----   Public method Exec   --------------------------------------------
void CpcDigiProducer::Exec(Option_t *opt) {

  //#define EDEBUG
#ifdef EDEBUG
  static Int_t lEDEBUGcounter = 0;
  cout << "EDEBUG-- CpcDigiProducer::Exec() started... " << endl;
  ;
#endif

  if (!fDigiArray)
    Fatal("Exec", "No DigiArray");

  fDigiArray->Clear();

  CpcDigiScheme *pDigiScheme = CpcDigiScheme::Instance();

  if (!pDigiScheme)
    Fatal("CpcDigiProducer::Exec", "No DigiScheme");

  Int_t detID, modID, chanID;
  CpcDigiId_t digiID;

  // marina
  Double_t dEdepSectEv[90][10];

  for (Int_t i = 0; i < 90; i++) {      // mod
    for (Int_t ii = 0; ii < 10; ii++) { // section
      dEdepSectEv[i][ii] = 0.;
    }
  }
  // end marina

  MpdCpcPoint *point = NULL;

  map<CpcDigiId_t, Float_t> fDigiIdEnergy;
  fDigiIdEnergy.clear();
  map<CpcDigiId_t, Float_t>::const_iterator p;

  Int_t nPoints = fPointArray->GetEntriesFast();
  Double_t e1 = 0, e2 = 0;

  if (fHistCpc1En) {
    fHistCpc1En->Reset();
    fHistCpc2En->Reset();
  }
  TH2F *hist1 = fHistCpc1En;
  TH2F *hist2 = fHistCpc2En;

  Bool_t flag_of_not_created = 1;
  // cout <<"marina " <<nPoints <<endl;
  for (Int_t iPoint = 0; iPoint < nPoints; iPoint++) {
    // marina
    point = (MpdCpcPoint *)fPointArray->At(iPoint);
    // cout <<"marina 1 " <<point->GetCopyBmd() <<" " <<point->GetCopyMother()
    // <<" " <<point->GetCopy() <<endl;

    //    detID = point->GetCopyBmd();//==1 (z>0), ==2 (z<0)
    // modID  = point->GetCopyMother(); // modules 1-45
    //    if(detID==1) modID  = point->GetCopyMother(); // modules 1-45
    //    else modID = point->GetCopyMother() + 45;//modules 46-90
    //    chanID = (Int_t)((point->GetCopy()-1)/6); //sections 0-9

    //    cout <<"marina 1 " <<detID <<" " <<modID <<" " <<point->GetCopy() <<'
    //    ' <<chanID <<endl;

    dEdepSectEv[modID - 1][chanID] += point->GetEnergyLoss();
    // end marina

    Int_t pMMcopy = 1 * (point->GetZ() > 0) + 2 * (point->GetZ() < 0);
    //    digiID = pDigiScheme->GetDigiIdFromVolumeData (point->GetDetectorID(),
    //    point->GetCopy(), point->GetCopyMother(),pMMcopy);

    if ((digiID[0] != -1) && (digiID[1] != -1)) {

      if (!fHistCpc1En) {

        CreateHistograms(&digiID);

        hist1 = fHistCpc1En;
        hist2 = fHistCpc2En;
      }

      if (fDigiIdEnergy.find(digiID) == fDigiIdEnergy.end())
        fDigiIdEnergy[digiID] = point->GetEnergyLoss();
      else
        fDigiIdEnergy[digiID] += point->GetEnergyLoss();

      if (pMMcopy == 1) {
        e1 += point->GetEnergyLoss();
        hist1->Fill(point->GetX(), point->GetY(), point->GetEnergyLoss());
      } else {
        e2 += point->GetEnergyLoss();
        hist2->Fill(point->GetX(), point->GetY(), point->GetEnergyLoss());
      }

    } // if ((digiID[0]!=-1)&&(digiID[1]!=-1))

#ifdef EDEBUG
    else {
      if (lEDEBUGcounter < 100) {
        cout << "EDEBUG-- CpcDigiProducer::Exec:  Boundary point? : ";
        point->Print("");
        lEDEBUGcounter++;
      }
    }
#endif

  } // for (Int_t iPoint=0; iPoint<nPoints; iPoint++)

  // cout <<"marina 2 " <<endl;
  TClonesArray &clref1 = *fELossCpc1Value;
  new (clref1[0]) TParameter<double>("ELossCpc1", e1);
  TClonesArray &clref2 = *fELossCpc2Value;
  new (clref2[0]) TParameter<double>("ELossCpc2", e2);

  if (fHistCpc1En) {
    TClonesArray &clref1e = *fELossCpc1Histo;
    new (clref1e[0])
        TVectorT<float>(fHistCpc1En->GetSize(), fHistCpc1En->GetArray());
  }

  if (fHistCpc2En) {
    TClonesArray &clref2e = *fELossCpc2Histo;
    new (clref2e[0])
        TVectorT<float>(fHistCpc2En->GetSize(), fHistCpc2En->GetArray());
  }

  // cout <<"marina 3 " <<endl;
  for (Int_t i = 0; i < 90; i++) {      // mod
    for (Int_t ii = 0; ii < 10; ii++) { // section
      // cout <<"dEdepSectEv " <<i <<" " <<ii <<" " <<dEdepSectEv[i][ii] <<endl;
      if (dEdepSectEv[i][ii] > 0) {
        if (i <= 44)
          detID = 1;
        else
          detID = 2;
        if (detID == 1) {
          CpcDigi *digi = AddHit(detID, i + 1, ii + 1, dEdepSectEv[i][ii]);
          digi->ConvertSim();
        } else {
          CpcDigi *digi = AddHit(detID, i - 45 + 1, ii + 1, dEdepSectEv[i][ii]);
          digi->ConvertSim();
        }
      }
    }
  } // for(Int_t i=0; i<90; i++)

  /*
for(p=fDigiIdEnergy.begin(); p!=fDigiIdEnergy.end(); ++p) {

  pDigiScheme->SplitDigiID((*p).first, detID, modID, chanID);


  if ((detID!=-1)&&(chanID!=-1)) {
    BmdDigi* digi = AddHit(detID, modID, chanID, (*p).second);
    digi->ConvertSim();
#ifdef EDEBUG
    if (lEDEBUGcounter<20) {
      cout << "EDEBUG-- BmdDigiProducer::Exec: "<< detID<< " " << chanID << " "
<<
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
CpcDigi *CpcDigiProducer::AddHit(Int_t detID, Int_t modID, Int_t chanID,
                                 Float_t energy) {
  TClonesArray &clref = *fDigiArray;
  Int_t size = clref.GetEntriesFast();
  // cout <<"size " <<size <<endl;
  CpcDigi *result = new (clref[size]) CpcDigi(detID, modID, chanID, energy);
  // cout <<"result " <<result <<endl;
  return result;
}
// ----

ClassImp(CpcDigiProducer)
