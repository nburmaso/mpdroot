/*
 * NicaKalmanTrackInterface.h
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef NICAMPDKALMANTRACKINTERFACE_H_
#define NICAMPDKALMANTRACKINTERFACE_H_
#include "NicaTrackInterface.h"
#include "MpdKalmanTrack.h"
#include "NicaMpdKalmanTrack.h"
class NicaMpdKalmanTrack;
class NicaMpdKalmanTrackInterface : public NicaTrackInterface{
friend class NicaMpdKalmanTrack;
public:
	NicaMpdKalmanTrackInterface();
	virtual Double_t GetPx()const;
	virtual Double_t GetPy()const;
	virtual Double_t GetPz()const;
	virtual Double_t GetCharge()const;
	virtual ~NicaMpdKalmanTrackInterface();
	ClassDef(NicaMpdKalmanTrackInterface,1)
};

#endif /* NICAMPDKALMANTRACKINTERFACE_H_ */
