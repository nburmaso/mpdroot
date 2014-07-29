// -------------------------------------------------------------------------
// -----                 MpdStrawendcapHitproducer header file                 -----
// -------------------------------------------------------------------------

#ifndef CBMHYPHITPRODUCER_H
#define CBMHYPHITPRODUCER_H 1


#include "FairTask.h"
#include "TH1F.h"
#include "TFile.h"
#include "MpdStrawendcapHit.h"
#include "TVector3.h"

class TClonesArray;
class TObjectArray;

class MpdStrawendcapHitProducer : public FairTask
{

 public:

  /** Default constructor **/  
  MpdStrawendcapHitProducer(const char* fileGeo);


  /** Destructor **/
  ~MpdStrawendcapHitProducer();


  /** Virtual method Init **/
  virtual InitStatus Init();


  /** Virtual method Exec **/
  virtual void Exec(Option_t* opt);

  MpdStrawendcapHit* AddHit(Int_t trackID, Int_t detID, Float_t energy);
  void CreateStructure();
  

 private: 
   
  /** Input array of MpdStrawendcapPoints **/
  TClonesArray* fPointArray;

  /** Output array of MpdStrawendcapHit **/
  TClonesArray* fDigiArray;  

  TObjArray *fVolumeArray;
 
  /** Geo file to use **/
  TString fFileGeo; 
  Float_t eneThr;
  
 // map<Int_t, Float_t> emcX;
 // map<Int_t, Float_t> emcY;
 // map<Int_t, Float_t> emcZ;
 // map<Int_t, Float_t> emcTheta;
 // map<Int_t, Float_t> emcPhi;
 // map<Int_t, Float_t> emcTau;
  
  ClassDef(MpdStrawendcapHitProducer,1);
  
};

#endif
