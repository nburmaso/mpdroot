#ifndef CBMSTTHITPRODUCERREAL_H
#define CBMSTTHITPRODUCERREAL_H 1


#include "FairTask.h"
//#include "CbmSttDigi.h"
#include "CbmSttHit.h"
#include "CbmSttHitInfo.h"
#include "TVector3.h"

class TClonesArray;
class TObjectArray;

class CbmSttHitProducerReal : public FairTask
{

 public:

  /** Default constructor **/  
  CbmSttHitProducerReal();


  /** Destructor **/
  ~CbmSttHitProducerReal();


  /** Virtual method Init **/
  virtual InitStatus Init();


  /** Virtual method Exec **/
  virtual void Exec(Option_t* opt);

  CbmSttHit* AddHit(Int_t detID, TVector3& pos, TVector3& dpos, Int_t iPoint, Int_t trackID, Double_t p, Double_t rsim, Double_t rtrue, Double_t closestDistanceError, TVector3 wireDirection);

  CbmSttHitInfo* AddHitInfo(Int_t fileNumber, Int_t eventNumber, Int_t trackID, Int_t pointID, Int_t nMerged, Bool_t isFake);

  void FoldZPosWithResolution(Double_t &zpos, Double_t &zposError, TVector3 localInPos, TVector3 localOutPos);

 private: 

  /** Input array of CbmSttPoints **/
  TClonesArray* fPointArray;

  /** Output array of CbmSttHits **/
  TClonesArray* fHitArray;  

  TObjArray *fVolumeArray;

   /** Output array of CbmSttHitInfo **/
  TClonesArray* fHitInfoArray;

  ClassDef(CbmSttHitProducerReal,1);

};

#endif
