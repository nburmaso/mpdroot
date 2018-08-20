/*
 * MpdPairPadsCuts.h
 *
 *  Created on: 2 lip 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_NICA_MPD_CUTS_PAIRCUTS_MPDPAIRPADSCUTS_H_
#define MPDROOT_NICA_MPD_CUTS_PAIRCUTS_MPDPAIRPADSCUTS_H_
#include "NicaTwoTrackCut.h"
/**
 * class for pair cuts that base on pads, you have to use format that support such data
 */
class MpdPairPadsCuts : public NicaTwoTrackCut{
public:
	MpdPairPadsCuts();
	virtual Bool_t Pass(NicaTwoTrack *pair);
	Bool_t Init(Int_t task_id);
	static Int_t AverageSep(){return 0;};
	static Int_t SharedPads(){return 1;};
	static Int_t MinDeltaPhiStar(){return 2;};
	static Int_t TPCEntranceDist(){return 3;};
	static Int_t MinTPCSep(){return 4;};
	virtual ~MpdPairPadsCuts();
	ClassDef(MpdPairPadsCuts,1)
};

#endif /* MPDROOT_NICA_MPD_CUTS_PAIRCUTS_MPDPAIRPADSCUTS_H_ */
