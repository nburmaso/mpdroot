/*************************************************************************************
 *
 *         Class BmdTstSim
 *
 *  Author:   RO
 *  e-mail:   rogachevsky@jinr.ru
 *  Version:  19-May-2019
 *
 ************************************************************************************/

#ifndef MPDCPCTSTSIM_H
#define MPDCPCTSTSIM_H

#include "TClonesArray.h"
#include "TH3F.h"
#include "TNtuple.h"
#include "TNtupleD.h"

#include "MpdCpcGeoPar.h"
#include "FairMCEventHeader.h"
#include "FairTask.h"

class CpcTstSim : public FairTask {

public:
  CpcTstSim();
  CpcTstSim(const char *name, const char *title, Int_t verbose);
  CpcTstSim(const char *name, const char *title, Int_t verbose, Int_t flag);
  virtual ~CpcTstSim();

  virtual InitStatus Init();
  virtual InitStatus ReInit();

  virtual void Exec(Option_t *option = "0");
  virtual void Finish();

  void CreateMyTree();

private:
  Int_t nevents;

  TClonesArray *fMCTrackArray;       //!
  TClonesArray *fMCCpcPointArray;    //!
  TClonesArray *fMCTpcPointArray;    //!
  FairMCEventHeader *fMCEventHeader; //!

  TNtuple *fTree;         //
  TNtupleD *fTreeSummary; //
  TH3F *fH3;              //

  //  BmdGeoPar *fPar;           //  !

  ClassDef(CpcTstSim, 1)
};

#endif
