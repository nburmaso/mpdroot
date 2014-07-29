// -------------------------------------------------------------------------
// -----                 MpdFsaHitproducer header file                 -----
// -------------------------------------------------------------------------

#ifndef CBMHYPHITPRODUCER_H
#define CBMHYPHITPRODUCER_H 1


#include <map>
#include <iostream>
#include "FairTask.h"
#include "TH1F.h"
#include "TFile.h"
#include "MpdFsaHit.h"
#include "TVector3.h"

class TClonesArray;
class TObjectArray;

class MpdFsaHitProducer : public FairTask
{

 public:

  /** Default constructor **/  
  MpdFsaHitProducer(const char* fileGeo);


  /** Destructor **/
  ~MpdFsaHitProducer();


  /** Virtual method Init **/
  virtual InitStatus Init();


  /** Virtual method Exec **/
  virtual void Exec(Option_t* opt);

  MpdFsaHit* AddHit(Int_t trackID, Int_t detID, Float_t energy);
  void CreateStructure();
  

 private: 
   
  /** Input array of MpdFsaPoints **/
  TClonesArray* fPointArray;

  /** Output array of MpdFsaHit **/
  TClonesArray* fDigiArray;  
  TClonesArray* fListMCtracks;

  TObjArray *fVolumeArray;
 
  /** Geo file to use **/
  TString fFileGeo; 
  Float_t eneThr;

  typedef std::map<Int_t, Float_t> mapper;
  
  mapper emcX, emcY, emcZ, emcTheta, emcPhi, emcTau;
/*   map<Int_t, Float_t> emcX; */
/*   map<Int_t, Float_t> emcY; */
/*   map<Int_t, Float_t> emcZ; */
/*   map<Int_t, Float_t> emcTheta; */
/*   map<Int_t, Float_t> emcPhi; */
/*   map<Int_t, Float_t> emcTau; */
  
  ClassDef(MpdFsaHitProducer,1);
  
};

#endif
