//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_HIT_H
#define __MPD_TOF_HIT_H 1

#include "FairHit.h"
//------------------------------------------------------------------------------------------------------------------------
class MpdTofHit : public FairHit
{
protected:
	Double32_t		fTime;              // Time since event start [ns]
	Int_t			fFlag;              // Flag for general purposes [TDC, event tagging...]

//	Double32_t		fADC[25];
//	Double32_t		fQAmplitude;

public:
	MpdTofHit();
	MpdTofHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex, Double_t tof, Int_t flag = 0);
	virtual ~MpdTofHit();

	void			Print(const char* comment = nullptr) const;

	Double_t		GetTime() const { return fTime; };
	Int_t			GetFlag() const { return fFlag; }; 

	void			SetTime(Double_t time) { fTime = time; };
	void			SetFlag(Int_t flag) { fFlag = flag; };
	void			AddFlag(Int_t flag) { fFlag =  fFlag | flag; };
	
	bool			CheckVolumeUID(Int_t uid);
	bool			CheckTrackID(Int_t uid);

ClassDef(MpdTofHit,1)
};
//------------------------------------------------------------------------------------------------------------------------
#endif
