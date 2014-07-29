#ifndef MPDEMCHIT_H
#define MPDEMCHIT_H

#include "FairHit.h"

class MpdEmcHit : public FairHit
{

 public:

  /** Default constructor **/
  MpdEmcHit();

  /** Constructor with hit parameters (1)**/
  MpdEmcHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex, Int_t flag);

  /** Constructor with hit parameters (2) [not the flag]**/
  MpdEmcHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex);

  /** Destructor **/
  virtual ~MpdEmcHit();

  void Print(const Option_t* opt = 0) const;

  /** Accessors **/
  Int_t    GetFlag()       const { return fFlag; }; 

  /** Modifiers **/
  void SetFlag(Int_t flag)           { fFlag = flag; };
  
 protected:

  Int_t fTrackID;
  Int_t      fFlag;              // Flag for general purposes [TDC, event tagging...]

  ClassDef(MpdEmcHit,1)

};


#endif
