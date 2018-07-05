/*
 * MpdDeltaEtaDeltaPhiStarCut.h
 *
 *  Created on: 4 lip 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_NICA_MPD_CUTS_MPDDELTAETADELTAPHISTARCUT_H_
#define MPDROOT_NICA_MPD_CUTS_MPDDELTAETADELTAPHISTARCUT_H_

#include "NicaTwoTrackCut.h"

class MpdDeltaEtaDeltaPhiStarCut: public NicaTwoTrackCut {
public:
	MpdDeltaEtaDeltaPhiStarCut();
	virtual Bool_t Pass(NicaTwoTrack *pair);
	Bool_t Init(Int_t task_id);
	static Int_t DeltaPhiStar(){return 0;};
	static Int_t DeltaEta(){return 1;};
	static Int_t DeltaPhiStarMin(){return 2;};
	virtual ~MpdDeltaEtaDeltaPhiStarCut();
	ClassDef(MpdDeltaEtaDeltaPhiStarCut,1)
};

#endif /* MPDROOT_NICA_MPD_CUTS_MPDDELTAETADELTAPHISTARCUT_H_ */
