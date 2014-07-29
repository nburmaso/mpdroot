// -------------------------------------------------------------------------
// -----                 MpdFsaHitproducer header file                 -----
// -------------------------------------------------------------------------

#ifndef CBMHYPHITPRODUCER_H
#define CBMHYPHITPRODUCER_H 1


#include <map>
#include <iostream>
#include "FairTask.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TList.h"

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


  void virtual FinishTask();
  void virtual Finish();
  void MakeHists();
  

 private: 
   
  /** Input array of MpdFsaPoints **/
  TClonesArray* fPointArray;

  /** Output array of MpdFsaHit **/
  TClonesArray* fDigiArray;  

  TObjArray *fVolumeArray;
 
  /** Geo file to use **/
  TString fFileGeo; 
  Float_t eneThr;


  //_____ Histograms_____________ 
  TList *hlist;

   TH1F *fZ;
//   TH1F *fR;
//   
//   TH1F *fLoadXL1;
//   TH1F *fLoadXL2;
//   TH1F *fLoadXL3;
//   TH1F *fLoadXL4;
//   TH1F *fLoadXL5;
//   
//   TH1F *fLoadYL1;
//   TH1F *fLoadYL2;
//   TH1F *fLoadYL3;
//   TH1F *fLoadYL4;
//   TH1F *fLoadYL5;
//   
//   TH1F *fTime;
// 
//   TH2F *fXY;
//   TH2F *fRphi;
// 
//   TH2F *fLoadXYL1;
  
  /////////////////////////
  
  TH1F *fLoadX1;
  TH1F *fLoadX1u;
  TH1F *fLoadX1d;
  TH1F *fLoadY1;
  TH1F *fLoadY1u;
  TH1F *fLoadY1d;
  
  TH1F *fLoadX2;
  TH1F *fLoadX2u;
  TH1F *fLoadX2d;
  TH1F *fLoadY2;
  TH1F *fLoadY2u;
  TH1F *fLoadY2d;
  
  TH1F *fLoadX3;
  TH1F *fLoadX3u;
  TH1F *fLoadX3d;
  TH1F *fLoadY3;
  TH1F *fLoadY3u;
  TH1F *fLoadY3d;
  
  TH1F *fLoadX4;
  TH1F *fLoadX4u;
  TH1F *fLoadX4d;
  TH1F *fLoadY4;
  TH1F *fLoadY4u;
  TH1F *fLoadY4d;
  
  TH1F *fLoadX5;
  TH1F *fLoadX5u;
  TH1F *fLoadX5d;
  TH1F *fLoadY5;
  TH1F *fLoadY5u;
  TH1F *fLoadY5d;
  
  TH2F *fLoadXY1;
  TH2F *fRealXY1;
  Bool_t *xt;
  Bool_t *yt;

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
