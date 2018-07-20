/*
 * NicaMpdKalmanEvent.h
 *
 *  Created on: 6 lip 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef INTERFACES_MPDROOT_FORMAT_NICAMPDKALMANEVENT_H_
#define INTERFACES_MPDROOT_FORMAT_NICAMPDKALMANEVENT_H_
#include "NicaExpEvent.h"

class NicaMpdKalmanEvent : public NicaEvent {
protected:
	void CreateSource();
	virtual void ShallowCopyTracks(NicaEvent *event);
public:
	NicaMpdKalmanEvent();
	NicaMpdKalmanEvent(const NicaMpdKalmanEvent &other);
	virtual void Update();
	virtual void Clear(Option_t *opt=" ");
	virtual TString GetFormatName()const {return "MpdKalman";};
	virtual ~NicaMpdKalmanEvent();
	ClassDef(NicaMpdKalmanEvent,1)
};

#endif /* INTERFACES_MPDROOT_FORMAT_NICAMPDKALMANEVENT_H_ */
