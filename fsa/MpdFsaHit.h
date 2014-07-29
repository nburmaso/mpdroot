#ifndef MPDFSAHIT_H
#define MPDFSAHIT_H

#include "FairHit.h"

class MpdFsaHit : public FairHit
{

 public:

  /** Default constructor **/
  MpdFsaHit();

  /** Constructor with hit parameters (1)**/
  MpdFsaHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex, Int_t flag);

  /** Constructor with hit parameters (2) [not the flag]**/
  MpdFsaHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex);

  /** Destructor **/
  virtual ~MpdFsaHit();

  void Print(const Option_t* opt = 0) const;

  /** Accessors **/
  Int_t    GetFlag()       const { return fFlag; }; 

  /** Modifiers **/
  void SetFlag(Int_t flag)           { fFlag = flag; };
  
 protected:

  Int_t fTrackID;
  Int_t      fFlag;              // Flag for general purposes [TDC, event tagging...]

  ClassDef(MpdFsaHit,1)

};


#endif
