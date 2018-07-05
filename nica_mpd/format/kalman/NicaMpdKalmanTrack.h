/*
 * NicaMpdKalmanTrack.h
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef NICAMPDKALMANTRACK_H_
#define NICAMPDKALMANTRACK_H_

#include "NicaTrack.h"
#include "MpdKalmanHit.h"
class NicaMpdKalmanTrack : public NicaTrack{
public:
	NicaMpdKalmanTrack();
	Int_t GetNHits()const;
	MpdKalmanHit* GetHit(Int_t hit_id)const;
	virtual ~NicaMpdKalmanTrack();
	ClassDef(NicaMpdKalmanTrack,1)
};

#endif /* INTERFACES_MPDROOT_FORMAT_KALMAN_NICAMPDKALMANTRACK_H_ */
