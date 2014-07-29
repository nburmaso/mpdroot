// -------------------------------------------------------------------------
// -----                      CbmSttTrack header file                  -----
// -----                  Created 28/03/06  by R. Castelijns           -----
// -------------------------------------------------------------------------


/**  CbmSttTrack.h
 *@author R.Castelijns <r.castelijns@fz-juelich.de>
 **
 ** STT local track. Holds lists of CbmSttHits and the fitted
 ** track parameters. The fit parameters are of type FairTrackParam
 ** and can only be accesssed and modified via this class.
 **/

#ifndef CBMSTTTRACK_H
#define CBMSTTTRACK_H 

#include "TArrayI.h"
#include "TObject.h"
#include "FairTrackParam.h"
#include "TClonesArray.h"
#include <map>

using namespace std;

class CbmSttHit;

class CbmSttTrack : public TObject
{
 public:

  /** Default constructor **/
  CbmSttTrack();


  /** Destructor **/
  virtual ~CbmSttTrack();


  /** Public methods AddHit
   ** Adds the hit index to the index array
   **/
  void AddHit(Int_t hitID, CbmSttHit* mHit);

  /** Public method Print
   ** Output to screen 
   **/
  void Print();


  /** Public method SortHits
   ** Sorts the hits in each array in downstream direction
   ** and writes the hit indizes into the member TArrayI
   **/
  void SortHits();


  /** Accessors  **/
  Int_t GetNofHits()                  const { return fHits.GetSize(); };
  Int_t GetNHits()                    const;
  Int_t GetHitIndex(Int_t iHit)       const { return fHits.At(iHit); };
  Int_t GetPidHypo()                  const { return fPidHypo; };
  Int_t GetFlag()                     const { return fFlag; };
  Double_t GetChi2Long()              const { return fChi2Long; };
  Double_t GetChi2Rad()               const { return fChi2Rad; };
  Int_t GetNDF()                      const { return fNDF; };

  // stt1
  //  TClonesArray * GetHOT() const {return fHotArray;}

  FairTrackParam* GetParamFirst() { return &fParamFirst; };
  FairTrackParam* GetParamLast()  { return &fParamLast ; };
  Bool_t AlreadyHasHit(Int_t iHit);
 
  /** Modifiers  **/
  void SetPidHypo(Int_t pid)                { fPidHypo    = pid;  };
  void SetParamFirst(FairTrackParam& par)    { fParamFirst = par;  };
  void SetParamLast(FairTrackParam& par)     { fParamLast  = par;  };
  void SetFlag(Int_t flag)                  { fFlag       = flag; };
  void SetChi2Long(Double_t chi2)           { fChi2Long   = chi2; };
  void SetChi2Rad(Double_t chi2)            { fChi2Rad    = chi2; };
  void SetNDF(Int_t ndf)                    { fNDF        = ndf;  };
  // stt1
  //  void SetHOT(TClonesArray *hotarray) {fHotArray = hotarray;}

 private:
  Double_t fRefAngle;

  /** Arrays containg the indices of the hits attached to the track **/
  TArrayI fHits;

  /** PID hypothesis used by the track fitter **/
  Int_t fPidHypo;

  /** Track parameters at first and last fitted hit **/
  FairTrackParam fParamFirst;
  FairTrackParam fParamLast;

  /** Quality flag **/
  Int_t fFlag;

  /** RMS deviation of hit coordinates to track **/
  Double32_t fChi2Long;
  Int_t fNDF;
  Double32_t fChi2Rad;

  /** Maps from hit z position to hit index. STL map is used because it
   ** is automatically sorted. Temporary only; not for storgage.
   ** The Hit index arrys will be filled by the method SortHits.
   **/
  map<Double_t, Int_t> fHitMap;            //!

  ClassDef(CbmSttTrack,1);
};


inline Int_t CbmSttTrack::GetNHits() const 
{
  return GetNofHits(); 
}



#endif
