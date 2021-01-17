/*************************************************************************************
 *
 *         Class BmdTstSim
 *
 *  Author:   RO
 *  e-mail:   rogachevsky@jinr.ru
 *  Version:  19-May-2019
 *
 ************************************************************************************/

#ifndef MPDBMDTSTSIM_H
#define MPDBMDTSTSIM_H

#include "TClonesArray.h"
#include "TH3F.h"
#include "TNtuple.h"
#include "TNtupleD.h"

#include "BmdGeoPar.h"
#include "FairMCEventHeader.h"
#include "FairTask.h"

class BmdTstSim : public FairTask {

public:
  BmdTstSim();
  BmdTstSim(const char *name, const char *title, Int_t verbose);
  BmdTstSim(const char *name, const char *title, Int_t verbose, Int_t flag);
  virtual ~BmdTstSim();

  virtual InitStatus Init();
  virtual InitStatus ReInit();

  virtual void Exec(Option_t *option = "0");
  virtual void Finish();

  void CreateMyTree();

private:
  Int_t nevents;

  TClonesArray *fMCTrackArray;       //!
  TClonesArray *fMCBmdPointArray;    //!
  TClonesArray *fMCTpcPointArray;    //!
  FairMCEventHeader *fMCEventHeader; //!

  TNtuple *fTree;         //
  TNtupleD *fTreeSummary; //
  TH3F *fH3;              //

  //  BmdGeoPar *fPar;           //  !

  ClassDef(BmdTstSim, 1)
};

#endif
