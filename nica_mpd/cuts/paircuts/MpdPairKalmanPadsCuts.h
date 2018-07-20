/*
 * MpdPairKalmanPadsCuts.h
 *
 *  Created on: 19 lip 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_NICA_MPD_CUTS_PAIRCUTS_MPDPAIRKALMANPADSCUTS_H_
#define MPDROOT_NICA_MPD_CUTS_PAIRCUTS_MPDPAIRKALMANPADSCUTS_H_

#include "NicaTwoTrackCut.h"

class MpdPairKalmanPadsCuts : public NicaTwoTrackCut{
public:
	MpdPairKalmanPadsCuts();
	virtual Bool_t Pass(NicaTwoTrack *pair);
	Bool_t Init(Int_t task_id);
	static Int_t AverageSep(){return 0;};
	static Int_t SharedPads(){return 1;};
	static Int_t MinDeltaPhiStar(){return 2;};
	static Int_t TPCEntranceDist(){return 3;};
	static Int_t MinTPCSep(){return 4;};
	static Int_t HitQuality(){return 5;}
	static Int_t HitShared(){return 6;};
	virtual ~MpdPairKalmanPadsCuts();
	ClassDef(MpdPairKalmanPadsCuts,1)
};

#endif /* MPDROOT_NICA_MPD_CUTS_PAIRCUTS_MPDPAIRKALMANPADSCUTS_H_ */
