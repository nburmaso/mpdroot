//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_ETOF_HIT_H
#define __MPD_ETOF_HIT_H 1

#include "FairHit.h"
//------------------------------------------------------------------------------------------------------------------------
class MpdEtofHit : public FairHit
{

public:

	MpdEtofHit();
	MpdEtofHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex, Double_t tof, Int_t flag);
	MpdEtofHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex, Double_t tof);
	virtual ~MpdEtofHit();

	void Print(const Option_t* opt = 0) const;

	Double_t GetTime()       const { return fTime; };
	Int_t    GetFlag()       const { return fFlag; }; 

	Int_t GetCell()   const   {return ((fDetectorID>>4) & 1023);};
	Int_t GetModule() const   {return ((fDetectorID>>14) & 1023);};
	Int_t GetRegion() const   {return fDetectorID>>24;};
 

	void SetTime(Double_t time)        { fTime = time; };
	void SetFlag(Int_t flag)           { fFlag = flag; };

  	bool			CheckTrackID(Int_t uid);

protected:

	Double32_t 	fTime;              // Time since event start [ns]
	Int_t      	fFlag;              // Flag for general purposes [TDC, event tagging...]

  ClassDef(MpdEtofHit,1)

};

//------------------------------------------------------------------------------------------------------------------------
#endif
