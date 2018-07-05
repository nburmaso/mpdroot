/*
 * NicaMpdDstKalmanTrackInterface.h
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef INTERFACES_MPDROOT_FORMAT_KALMANDST_NICAMPDDSTKALMANTRACKINTERFACE_H_
#define INTERFACES_MPDROOT_FORMAT_KALMANDST_NICAMPDDSTKALMANTRACKINTERFACE_H_

#include "NicaTrackInterface.h"

class NicaMpdDstKalmanTrackInterface : public NicaTrackInterface{
	TObject *fKalman;
public:
	NicaMpdDstKalmanTrackInterface();
	void SetRawTrack(TObject *prim_track,TObject *kalman){fRawObject = prim_track; fKalman = kalman;};
	virtual ~NicaMpdDstKalmanTrackInterface();
	ClassDef(NicaMpdDstKalmanTrackInterface,1)
};

#endif /* INTERFACES_MPDROOT_FORMAT_KALMANDST_NICAMPDDSTKALMANTRACKINTERFACE_H_ */
