/*
 * MpdTwoTrackShareCut.h
 *
 *  Created on: 7 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef INTERFACES_MPDROOT_CUTS_PAIRCUTS_MPDTWOTRACKSHAREDQUALITYCUT_H_
#define INTERFACES_MPDROOT_CUTS_PAIRCUTS_MPDTWOTRACKSHAREDQUALITYCUT_H_

#include "NicaTwoTrackCut.h"

class MpdTwoTrackSharedQualityCut  : public NicaTwoTrackCut{
public:
	MpdTwoTrackSharedQualityCut();
	virtual Bool_t Pass(NicaTwoTrack *pair);
	virtual Bool_t PassRotated(NicaTwoTrack *pair){return Pass(pair);};
	virtual Bool_t PassHemisphere(NicaTwoTrack *pair){return Pass(pair);};
	virtual Bool_t Init(Int_t format_id);
	static Int_t Quality(){return 1;};
	static Int_t Shared(){return 0;};
	virtual ~MpdTwoTrackSharedQualityCut();
	ClassDef(MpdTwoTrackSharedQualityCut,1)

};

#endif /* INTERFACES_MPDROOT_CUTS_PAIRCUTS_MPDTWOTRACKSHAREDQUALITYCUT_H_ */
