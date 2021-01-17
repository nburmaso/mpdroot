// -------------------------------------------------------------------------
// -----                 BmdHitproducer header file                 -----
// -----                 Created 14/08/06  by S.Spataro                -----
// -------------------------------------------------------------------------

#ifndef MPDBMDDIGIPRODUCER_H
#define MPDBMDDIGIPRODUCER_H 1

#include "BmdDigi.h"
#include "BmdDigiScheme.h"
#include "BmdGeoPar.h"
#include "FairTask.h"
#include "TClonesArray.h"
#include <map>

#include "TH2F.h"
#include "TParameter.h"

class BmdDigiProducer : public FairTask {

public:
  /** Default constructor **/
  BmdDigiProducer(const char *name = "Bmd Digi Producer");

  /** Destructor **/
  ~BmdDigiProducer();

  /** Virtual method Init **/
  virtual InitStatus Init();

  /** Virtual method Exec **/
  virtual void Exec(Option_t *opt);

  BmdDigi *AddHit(Int_t detID, Int_t modID, Int_t chanID, Float_t energy);

  void CreateHistograms(BmdDigiId_t *pDigiID);

private:
  virtual void SetParContainers();

private:
  /** Input array of BmdPoints **/
  TClonesArray *fPointArray;

  /** Output array of BmdDigi **/
  TClonesArray *fDigiArray;

  TClonesArray *fELossBmd1Value;
  TClonesArray *fELossBmd2Value;

  TClonesArray *fELossBmd1Histo;
  TClonesArray *fELossBmd2Histo;

  /** Input geometry parameters container**/
  BmdGeoPar *fGeoPar;

  /** Output Histograms of X-Y energy map **/
  TH2F *fHistBmd1En;
  TH2F *fHistBmd2En;

  ClassDef(BmdDigiProducer, 1);
};

#endif
