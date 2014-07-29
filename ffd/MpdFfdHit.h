#ifndef MPDFFDHIT_H
#define MPDFFDHIT_H

#include "FairHit.h"

class MpdFfdHit : public FairHit
{

 public:

  /** Default constructor **/
  MpdFfdHit();

  /** Constructor with hit parameters (1)**/
  MpdFfdHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex, Int_t flag);

  /** Constructor with hit parameters (2) [not the flag]**/
  MpdFfdHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex);

  /** Destructor **/
  virtual ~MpdFfdHit();

  void Print(const Option_t* opt = 0) const;

  /** Accessors **/
  Int_t    GetFlag()       const { return fFlag; }; 

  /** Modifiers **/
  void SetFlag(Int_t flag)           { fFlag = flag; };
  
 protected:

  Int_t fTrackID;
  Int_t      fFlag;              // Flag for general purposes [TDC, event tagging...]

  ClassDef(MpdFfdHit,1)

};


#endif
