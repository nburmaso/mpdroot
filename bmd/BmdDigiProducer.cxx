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

#include "BmdDigi.h"
#include "BmdDigiProducer.h"
#include "BmdPoint.h"

#include "TROOT.h"
#include "TVectorT.h"

using namespace std;

// -----   Default constructor   -------------------------------------------
BmdDigiProducer::BmdDigiProducer(const char *name) : FairTask(name) {
  fPointArray = 0;
  fDigiArray = 0;
  fGeoPar = 0;
  fHistBmd1En = 0;
  fHistBmd2En = 0;
  fELossBmd1Value = NULL, fELossBmd2Value = NULL, fELossBmd1Histo = NULL,
  fELossBmd2Histo = NULL;
}
// -------------------------------------------------------------------------

// -----   Destructor   ----------------------------------------------------
BmdDigiProducer::~BmdDigiProducer() {}
// -------------------------------------------------------------------------

// -------------------------------------------------------------------------
void BmdDigiProducer::SetParContainers() {
  cout << "-I- BmdDigiProducer: SetParContainers started..." << endl;

  //   Get run and runtime database
  FairRunAna *run = FairRunAna::Instance();
  if (!run)
    Fatal("FairMuchDigitize::SetParContainers", "No analysis run");

  FairRuntimeDb *rtdb = run->GetRuntimeDb();
  if (!rtdb)
    Fatal("FairMuchDigitize::SetParContainers", "No runtime database");

  cout << "-I- BmdDigiProducer: SetParContainers continued..." << endl;

  rtdb->activateParIo(rtdb->getFirstInput());
  //   fGeoPar=( BmdGeoPar*) rtdb->getContainer("BmdGeoPar");
  fGeoPar = (BmdGeoPar *)gROOT->FindObject("BmdGeoPar");
  fGeoPar->print();

  cout << "-I- BmdDigiProducer: SetParContainers finished." << endl;
}

// -------------------------------------------------------------------------
// -----   Public method Init   --------------------------------------------
InitStatus BmdDigiProducer::Init() {

  cout << "-I- BmdDigiProducer: Init started..." << endl;

  // Get RootManager
  FairRootManager *ioman = FairRootManager::Instance();
  if (!ioman) {
    cout << "-E- BmdDigiProducer::Init: "
         << "RootManager not instantiated!" << endl;
    return kFATAL;
  }

  // Get input array
  fPointArray = (TClonesArray *)ioman->GetObject("BmdPoint");
  if (!fPointArray) {
    cout << "-W- BmdDigiProducer::Init: "
         << "No BmdPoint array!" << endl;
    return kERROR;
  }

  // Create and register output array
  fDigiArray = new TClonesArray("BmdDigi");
  ioman->Register("BmdDigi", "Bmd", fDigiArray, kTRUE);

  fELossBmd1Value = new TClonesArray("TParameter<double>");
  ioman->Register("ELossBmd1Value", "Bmd", fELossBmd1Value, kTRUE);

  fELossBmd2Value = new TClonesArray("TParameter<double>");
  ioman->Register("ELossBmd2Value", "Bmd", fELossBmd2Value, kTRUE);

  fELossBmd1Histo = new TClonesArray("TVectorT<float>");
  ioman->Register("ELossBmd1Histo", "Bmd", fELossBmd1Histo, kTRUE);

  fELossBmd2Histo = new TClonesArray("TVectorT<float>");
  ioman->Register("ELossBmd2Histo", "Bmd", fELossBmd2Histo, kTRUE);

  BmdDigiScheme *fDigiScheme = BmdDigiScheme::Instance();
  fDigiScheme->Init(fGeoPar, 0, kTRUE, 2);

  cout << "-I- BmdDigiProducer: Intialization successfull" << endl;

  return kSUCCESS;
}
// -------------------------------------------------------------------------
void BmdDigiProducer::CreateHistograms(BmdDigiId_t *pDigiID) {
  Int_t nx, ny, nz;
  Double_t dx, dy, dz;

  BmdDigiScheme *fDigiScheme = BmdDigiScheme::Instance();
  fDigiScheme->GetBmdDimensions(nx, ny, nz);
  fDigiScheme->GetVolDxDyDz(pDigiID, dx, dy, dz);

  Int_t Nx = nx + 2;
  Double_t Dx = dx * Nx;
  Int_t Ny = ny + 2;
  Double_t Dy = dy * Ny;

  fHistBmd1En =
      new TH2F("HistBmd1En", "HistBmd1Energy", Nx, -Dx, Dx, Ny, -Dy, Dy);
  fHistBmd2En =
      new TH2F("HistBmd2En", "HistBmd2Energy", Nx, -Dx, Dx, Ny, -Dy, Dy);

  if ((!fHistBmd1En) || (!fHistBmd2En))
    cout << "-E- BmdDigiProducer: HistBmd1En or HistBmd2En Histograms not "
            "created !!"
         << endl;
  else {
    FairRootManager *ioman = FairRootManager::Instance();
    fHistBmd1En->SetDirectory((TFile *)ioman->GetOutFile());
    fHistBmd2En->SetDirectory((TFile *)ioman->GetOutFile());
    fHistBmd1En->Write();
    fHistBmd2En->Write();
  }
}

// -----   Public method Exec   --------------------------------------------
void BmdDigiProducer::Exec(Option_t *opt) {

  //#define EDEBUG
#ifdef EDEBUG
  static Int_t lEDEBUGcounter = 0;
  cout << "EDEBUG-- BmdDigiProducer::Exec() started... " << endl;
  ;
#endif

  if (!fDigiArray)
    Fatal("Exec", "No DigiArray");

  fDigiArray->Clear();

  BmdDigiScheme *pDigiScheme = BmdDigiScheme::Instance();

  if (!pDigiScheme)
    Fatal("BmdDigiProducer::Exec", "No DigiScheme");

  Int_t detID, modID, chanID;
  BmdDigiId_t digiID;

  // marina
  Double_t dEdepSectEv[90][10];

  for (Int_t i = 0; i < 90; i++) {      // mod
    for (Int_t ii = 0; ii < 10; ii++) { // section
      dEdepSectEv[i][ii] = 0.;
    }
  }
  // end marina

  BmdPoint *point = NULL;

  map<BmdDigiId_t, Float_t> fDigiIdEnergy;
  fDigiIdEnergy.clear();
  map<BmdDigiId_t, Float_t>::const_iterator p;

  Int_t nPoints = fPointArray->GetEntriesFast();
  Double_t e1 = 0, e2 = 0;

  if (fHistBmd1En) {
    fHistBmd1En->Reset();
    fHistBmd2En->Reset();
  }
  TH2F *hist1 = fHistBmd1En;
  TH2F *hist2 = fHistBmd2En;

  Bool_t flag_of_not_created = 1;
  // cout <<"marina " <<nPoints <<endl;
  for (Int_t iPoint = 0; iPoint < nPoints; iPoint++) {
    // marina
    point = (BmdPoint *)fPointArray->At(iPoint);
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

      if (!fHistBmd1En) {

        CreateHistograms(&digiID);

        hist1 = fHistBmd1En;
        hist2 = fHistBmd2En;
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
        cout << "EDEBUG-- BmdDigiProducer::Exec:  Boundary point? : ";
        point->Print("");
        lEDEBUGcounter++;
      }
    }
#endif

  } // for (Int_t iPoint=0; iPoint<nPoints; iPoint++)

  // cout <<"marina 2 " <<endl;
  TClonesArray &clref1 = *fELossBmd1Value;
  new (clref1[0]) TParameter<double>("ELossBmd1", e1);
  TClonesArray &clref2 = *fELossBmd2Value;
  new (clref2[0]) TParameter<double>("ELossBmd2", e2);

  if (fHistBmd1En) {
    TClonesArray &clref1e = *fELossBmd1Histo;
    new (clref1e[0])
        TVectorT<float>(fHistBmd1En->GetSize(), fHistBmd1En->GetArray());
  }

  if (fHistBmd2En) {
    TClonesArray &clref2e = *fELossBmd2Histo;
    new (clref2e[0])
        TVectorT<float>(fHistBmd2En->GetSize(), fHistBmd2En->GetArray());
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
          BmdDigi *digi = AddHit(detID, i + 1, ii + 1, dEdepSectEv[i][ii]);
          digi->ConvertSim();
        } else {
          BmdDigi *digi = AddHit(detID, i - 45 + 1, ii + 1, dEdepSectEv[i][ii]);
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
BmdDigi *BmdDigiProducer::AddHit(Int_t detID, Int_t modID, Int_t chanID,
                                 Float_t energy) {
  TClonesArray &clref = *fDigiArray;
  Int_t size = clref.GetEntriesFast();
  // cout <<"size " <<size <<endl;
  BmdDigi *result = new (clref[size]) BmdDigi(detID, modID, chanID, energy);
  // cout <<"result " <<result <<endl;
  return result;
}
// ----

ClassImp(BmdDigiProducer)
