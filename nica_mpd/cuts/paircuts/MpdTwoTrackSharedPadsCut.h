/*
 * MptTwoTrackSharedPadsCut.h
 *
 *  Created on: 28 gru 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef INTERFACES_MPDROOT_CUTS_PAIRCUTS_MPDTWOTRACKSHAREDPADSCUT_H_
#define INTERFACES_MPDROOT_CUTS_PAIRCUTS_MPDTWOTRACKSHAREDPADSCUT_H_

#include "NicaTwoTrackCut.h"
#include  "NicaHelix.h"
#include "MpdTpcSectorGeo.h"

class MpdTwoTrackSharedPadsCut : public NicaTwoTrackCut{
	MpdTpcSectorGeo *fSec;
	NicaHelix *fHelix1;
	NicaHelix *fHelix2;
	void Overlap(NicaHelix *helix, Double_t R, Double_t &x, Double_t &y);
public:
	MpdTwoTrackSharedPadsCut();
	virtual Bool_t Pass(NicaTwoTrack *pair);
	virtual ~MpdTwoTrackSharedPadsCut();
	CutDef(MpdTwoTrackSharedPadsCut);
	ClassDef(MpdTwoTrackSharedPadsCut,1)
};

#endif /* INTERFACES_MPDROOT_CUTS_PAIRCUTS_MPDTWOTRACKSHAREDPADSCUT_H_ */
