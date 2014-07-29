// -------------------------------------------------------------------------
// -----                 MpdTgemHitproducer header file                 -----
// -------------------------------------------------------------------------

#ifndef CBMHYPHITPRODUCER_H
#define CBMHYPHITPRODUCER_H 1


#include "FairTask.h"
#include "TH1F.h"
#include "TFile.h"
#include "MpdTgemHit.h"
#include "TVector3.h"

class TClonesArray;
class TObjectArray;

class MpdTgemHitProducer : public FairTask
{

 public:

  /** Default constructor **/  
  MpdTgemHitProducer(const char* fileGeo);


  /** Destructor **/
  ~MpdTgemHitProducer();


  /** Virtual method Init **/
  virtual InitStatus Init();


  /** Virtual method Exec **/
  virtual void Exec(Option_t* opt);

  MpdTgemHit* AddHit(Int_t trackID, Int_t detID, Float_t energy);
  void CreateStructure();
  //=TH1F* GethZ();

 private: 
   
  /** Input array of MpdTgemPoints **/
  TClonesArray* fPointArray;

  /** Output array of MpdTgemHit **/
  TClonesArray* fDigiArray;  

  TObjArray *fVolumeArray;
 
  /** Geo file to use **/
  TString fFileGeo; 
  Float_t eneThr;
  
  //=TH1F *hZ;
    
 // map<Int_t, Float_t> emcX;
 // map<Int_t, Float_t> emcY;
 // map<Int_t, Float_t> emcZ;
 // map<Int_t, Float_t> emcTheta;
 // map<Int_t, Float_t> emcPhi;
 // map<Int_t, Float_t> emcTau;
  
  ClassDef(MpdTgemHitProducer,1);
  
};

#endif
