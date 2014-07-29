// -------------------------------------------------------------------------
// -----                 MpdZdcHitproducer header file                 -----
// -----                 Created 14/08/06  by S.Spataro                -----
// -------------------------------------------------------------------------

#ifndef MPDZDCDIGIPRODUCER_H
#define MPDZDCDIGIPRODUCER_H 1


#include <map>
#include "FairTask.h"
#include "TClonesArray.h"
#include "MpdZdcDigi.h"
#include "MpdZdcGeoPar.h"
#include "MpdZdcDigiScheme.h"

#include "TParameter.h"
#include "TH2F.h"


class MpdZdcDigiProducer : public FairTask
{

 public:

  /** Default constructor **/  
  MpdZdcDigiProducer(const char* name="MpdZdc Digi Producer");


  /** Destructor **/
  ~MpdZdcDigiProducer();


  /** Virtual method Init **/
  virtual InitStatus Init();


  /** Virtual method Exec **/
  virtual void Exec(Option_t* opt);

  MpdZdcDigi* AddHit(Int_t detID, Int_t modID, Int_t chanID,Float_t energy);

  void CreateHistograms ( MpdZdcDigiId_t *pDigiID);

 private: 
   
  virtual void SetParContainers(); 
 

 private: 
   
  /** Input array of MpdZdcPoints **/
  TClonesArray* fPointArray;

  /** Output array of MpdZdcDigi **/
  TClonesArray* fDigiArray; 

  TClonesArray* fELossZdc1Value;
  TClonesArray* fELossZdc2Value;

  TClonesArray* fELossZdc1Histo;
  TClonesArray* fELossZdc2Histo;

  /** Input geometry parameters container**/
  MpdZdcGeoPar* fGeoPar;

  /** Output Histograms of X-Y energy map **/
  TH2F *fHistZdc1En;
  TH2F *fHistZdc2En;

  
  ClassDef(MpdZdcDigiProducer,1);
  
};

#endif
