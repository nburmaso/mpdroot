/** CbmSttTrackFitter
 *@author R.Castelijns <r.castelijns@fz-juelich.de>
 **
 ** Abstract base class for concrete STT track fitting algorithm.
 ** Each derived class must implement the method DoFit. This has
 ** to operate on an object of type CbmSttTrack and fill the
 ** parameters fPidHypo, fParamFirst, fParamLast, fFlag and fChi2.
 **/

#ifndef CBMSTTHELIXTRACKFITTER
#define CBMSTTHELIXTRACKFITTER 1

#include "CbmSttTrack.h"
#include "CbmSttHit.h"
//#include "CbmSttHOT.h"
#include "CbmSttPoint.h"
#include "FairTrackParam.h"
#include "CbmSttTrackFitter.h"
//#include "CbmSttHoughAccumulatorNew.h"
#include "TH2F.h"
#include "TCanvas.h"
#include "TList.h"

class CbmSttTrackFitter;


void fcnHelix(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);

class CbmSttHelixTrackFitter : public CbmSttTrackFitter
{
 private:
    Int_t fEventCounter;

  CbmSttTrack* fTrack;
  CbmSttTrack currentTrack;

  TClonesArray* fHitArray;
  TClonesArray* fPointArray;
  TClonesArray *fHotArray;
  TObjArray *ZPointsArray;
  TCanvas *eventCanvas;
  TCanvas *eventCanvas2;
  TH2F *h1;
  TH2F *h2;
  Bool_t rootoutput;
 Int_t fVerbose;

 /** Private method AddHOT
  **
  ** Adds a CbmSttHOT the HOTCollection
  **/
/*  CbmSttHOT* AddHOT(Double_t x, Double_t y, Double_t x, Int_t hitindex, Int_t pointindex, Int_t trackindex); */
 
 public:
 CbmSttHelixTrackFitter();
  CbmSttHelixTrackFitter(Int_t verbose);
  ~CbmSttHelixTrackFitter();
  void Init();


 
  Bool_t IntersectionFinder(CbmSttTrack *pTrack, FairTrackParam *par);
  Bool_t IntersectionFinder4b(CbmSttTrack *pTrack, FairTrackParam *par);
 
  Bool_t ZFinder(CbmSttTrack* pTrack, Int_t pidHypo);
  Bool_t ZFinder2(CbmSttTrack* pTrack, Int_t pidHypo);
  Int_t Zfit(CbmSttTrack* pTrack, Int_t pidHypo);
  Int_t Zfit2(CbmSttTrack* pTrack, Int_t pidHypo);
 
  
  Int_t Fit4(CbmSttTrack* pTrack, Int_t pidHypo); 
  Int_t Fit4b(CbmSttTrack* pTrack, Int_t pidHypo);
 
  Int_t MinuitFit(CbmSttTrack* pTrack, Int_t pidHypo);


  Int_t DoFit(CbmSttTrack* pTrack, Int_t pidHypo = 211);

  /*   Int_t AddHitOnTrack(CbmSttTrack *pTrack); */

  // ========================
  // ========================
 
  CbmSttHit* GetHitFromCollections(Int_t hitCounter) const;
 
  // ========================
  // ========================
  TList fHitCollectionList; 
  virtual void AddHitCollection(TClonesArray* mHitArray) {fHitCollectionList.Add(mHitArray); }

  virtual void Extrapolate( CbmSttTrack* track, Double_t r, 
                            FairTrackParam *param );
  void ResetMArray();
 
  CbmSttTrack* GetTrack() const { return fTrack; };
  TClonesArray* GetHitArray() const { return fHitArray; };
/*   TClonesArray* GetHOTCollection() const { return fHotArray; }; */

  ClassDef(CbmSttHelixTrackFitter,1);
};

#endif
