// -------------------------------------------------------------------------
// -----                 BmdHitproducer header file                 -----
// -----                 Created 14/08/06  by S.Spataro                -----
// -------------------------------------------------------------------------

#ifndef MPDCPCDIGIPRODUCER_H
#define MPDCPCDIGIPRODUCER_H 1

#include "CpcDigi.h"
#include "CpcDigiScheme.h"
#include "MpdCpcGeoPar.h"
#include "FairTask.h"
#include "TClonesArray.h"
#include <map>

#include "TH2F.h"
#include "TParameter.h"

class CpcDigiProducer : public FairTask {

public:
  /** Default constructor **/
  CpcDigiProducer(const char *name = "Cpc Digi Producer");

  /** Destructor **/
  ~CpcDigiProducer();

  /** Virtual method Init **/
  virtual InitStatus Init();

  /** Virtual method Exec **/
  virtual void Exec(Option_t *opt);

  CpcDigi *AddHit(Int_t detID, Int_t modID, Int_t chanID, Float_t energy);

  void CreateHistograms(CpcDigiId_t *pDigiID);

private:
  virtual void SetParContainers();

private:
  /** Input array of CpcPoints **/
  TClonesArray *fPointArray;

  /** Output array of CpcDigi **/
  TClonesArray *fDigiArray;

  TClonesArray *fELossCpc1Value;
  TClonesArray *fELossCpc2Value;

  TClonesArray *fELossCpc1Histo;
  TClonesArray *fELossCpc2Histo;

  /** Input geometry parameters container**/
  MpdCpcGeoPar *fGeoPar;

  /** Output Histograms of X-Y energy map **/
  TH2F *fHistCpc1En;
  TH2F *fHistCpc2En;

  ClassDef(CpcDigiProducer, 1);
};

#endif
