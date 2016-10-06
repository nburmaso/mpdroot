//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_HIT_H
#define __MPD_TOF_HIT_H 1

#include "FairHit.h"
//------------------------------------------------------------------------------------------------------------------------
class MpdTofHit : public FairHit
{
	static const double nan;
	
protected:
	Double_t		fTime;              // Time since event start [ns]
	Int_t			fFlag;              // Flag for general purposes [TDC, event tagging...]

public:
	MpdTofHit();
	MpdTofHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex, Double_t tof, Int_t flag = 0);

	void			Print(const char* comment = nullptr) const;

	Double_t		GetTime() const { return fTime; };
	Int_t			GetFlag() const { return fFlag; }; 

	void			SetTime(Double_t time) { fTime = time; };
	void			SetFlag(Int_t flag) { fFlag = flag; };
	void			AddFlag(Int_t flag) { fFlag =  fFlag | flag; };
	
	bool			CheckVolumeUID(Int_t uid);
	bool			CheckTrackID(Int_t uid);

	inline void 		GetXYZ(Double_t *carray) const { carray[0] = fX; carray[1] = fY; carray[2] = fZ;}

ClassDef(MpdTofHit,2)
};
//------------------------------------------------------------------------------------------------------------------------
#endif
