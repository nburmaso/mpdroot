//MpdSftHit
//Author E. Cordier
//Modified by D. Gonzalez-Diaz 06/09/06
//From MpdSftPoint to smeared hits

#ifndef MPDSFTHIT_H
#define MPDSFTHIT_H

#include "FairHit.h"

class MpdSftHit : public FairHit
{

 public:

  /** Default constructor **/
  MpdSftHit();

  /** Constructor with hit parameters (1)**/
  MpdSftHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex, Double_t tof, Int_t flag);

  /** Constructor with hit parameters (2) [not the flag]**/
  MpdSftHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex, Double_t tof);

  /** Destructor **/
  virtual ~MpdSftHit();

  void Print(const Option_t* opt = 0) const;

  /** Accessors **/
  Double_t GetTime()       const { return fTime; };
  Int_t    GetFlag()       const { return fFlag; }; 

  Int_t GetCell()   const   {return ((fDetectorID>>4) & 1023);};
  Int_t GetModule() const   {return ((fDetectorID>>14) & 1023);};
  Int_t GetRegion() const   {return fDetectorID>>24;};
 

  /** Modifiers **/
  void SetTime(Double_t time)        { fTime = time; };
  void SetFlag(Int_t flag)           { fFlag = flag; };

  

 protected:

  Double32_t fTime;              // Time since event start [ns]
  Int_t      fFlag;              // Flag for general purposes [TDC, event tagging...]

  ClassDef(MpdSftHit,1)

};


#endif
