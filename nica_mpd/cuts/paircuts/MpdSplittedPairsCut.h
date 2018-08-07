/*
 * MpdRejectSPlittedPairsCut.h
 *
 *  Created on: 28 lip 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_NICA_MPD_CUTS_PAIRCUTS_MPDSPLITTEDPAIRSCUT_H_
#define MPDROOT_NICA_MPD_CUTS_PAIRCUTS_MPDSPLITTEDPAIRSCUT_H_
#include "NicaTwoTrackCut.h"
#include "NicaComplexTrack.h"

class MpdSplittedPairsCut : public NicaTwoTrackCut{
	Bool_t fReject;
public:
	MpdSplittedPairsCut();
	virtual Bool_t Pass(NicaTwoTrack *pair);
	void AcceptSplitted(){fReject = kFALSE;};
	virtual ~MpdSplittedPairsCut();
	ClassDef(MpdSplittedPairsCut,1)
};

#endif /* MPDROOT_NICA_MPD_CUTS_PAIRCUTS_MPDSPLITTEDPAIRSCUT_H_ */
