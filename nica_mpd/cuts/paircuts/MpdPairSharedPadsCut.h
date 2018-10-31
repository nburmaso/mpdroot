/*
 * MpdSharedPadsCut.h
 *
 *  Created on: 30 paź 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_NICA_MPD_CUTS_PAIRCUTS_PAD_CUTS_MPDSHAREDPADSCUT_H_
#define MPDROOT_NICA_MPD_CUTS_PAIRCUTS_PAD_CUTS_MPDSHAREDPADSCUT_H_

#include "NicaTwoTrackCut.h"
/**
 * calculate fraction of shared pads
 */
class MpdPairSharedPadsCut  : public NicaTwoTrackCut{
public:
	MpdPairSharedPadsCut();
	virtual Bool_t Pass(NicaTwoTrack *pair);
	static Int_t OverlappedPads(){return 0;}
	static Int_t SharedPads(){return 1;};
	virtual ~MpdPairSharedPadsCut();
	ClassDef(MpdPairSharedPadsCut,1)
};

#endif /* MPDROOT_NICA_MPD_CUTS_PAIRCUTS_PAD_CUTS_MPDSHAREDPADSCUT_H_ */
