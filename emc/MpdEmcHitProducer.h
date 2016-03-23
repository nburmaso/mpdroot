// -------------------------------------------------------------------------
// -----                 MpdEmcHitproducer header file                 -----
// -------------------------------------------------------------------------

#ifndef CBMHYPHITPRODUCER_H
#define CBMHYPHITPRODUCER_H 1

#include <iostream>
#include "FairTask.h"
#include "MpdEmcHit.h"
#include "MpdEmcGeoPar.h"


class TClonesArray;

class MpdEmcHitProducer : public FairTask
{

 public:

  /** Default constructor **/  
  MpdEmcHitProducer();


  /** Destructor **/
  ~MpdEmcHitProducer();


  /** Virtual method Init **/
  virtual InitStatus Init();


  /** Virtual method Exec **/
  virtual void Exec(Option_t* opt);
  void virtual Finish();  

 private:

  /** Input array of MpdEmcPoints **/
  TClonesArray* fPointArray;

  /** Output array of MpdEmcHit **/
  TClonesArray* fDigiArray;  
  
  UInt_t GetSecId(Float_t x, Float_t y, Float_t z);
  UInt_t GetRowId(Float_t z);
  UInt_t GetModId(Float_t x, Float_t y, Float_t z, UInt_t modId);
  
  MpdEmcHit* SearchHit(UInt_t mod, UInt_t row, UInt_t dig);
  
  MpdEmcGeoPar* fGeoPar;
  
  ClassDef(MpdEmcHitProducer, 2);
  
};

#endif
