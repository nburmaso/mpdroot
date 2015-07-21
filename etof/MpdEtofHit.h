//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_ETOF_HIT_H
#define __MPD_ETOF_HIT_H 1

#include "FairHit.h"
//------------------------------------------------------------------------------------------------------------------------
class MpdEtofHit : public FairHit
{
protected:
	Double32_t 	fTime;              // Time since event start [ns]
	Int_t      	fFlag;              // Flag for general purposes [TDC, event tagging...]
	
public:
	MpdEtofHit();
	MpdEtofHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex, Double_t tof, Int_t flag = 0);
	virtual ~MpdEtofHit();

	Double_t 	GetTime()       const { return fTime; };
	Int_t    	GetFlag()       const { return fFlag; };
	void 		SetTime(Double_t time){ fTime = time; };
	void 		SetFlag(Int_t flag)   { fFlag = flag; };
	
	void 		Print(const char* comment = nullptr) const;	
  	bool		CheckTrackID(Int_t uid);

ClassDef(MpdEtofHit,1)
};
//------------------------------------------------------------------------------------------------------------------------
#endif
