/*
 * NicaMpdDstKalman.h
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef INTERFACES_MPDROOT_FORMAT_NICAMPDDSTKALMANEVENT_H_
#define INTERFACES_MPDROOT_FORMAT_NICAMPDDSTKALMANEVENT_H_

#include "NicaExpEvent.h"

class NicaMpdDstKalmanEvent : public NicaExpEvent {
	enum kTrackType{
		kAllTracks,
		kPrimaryTracks,
		kGlobalTracks
	};
	kTrackType fMode;
protected:
	void CreateSource();
public:
	NicaMpdDstKalmanEvent();
	NicaMpdDstKalmanEvent(const NicaMpdDstKalmanEvent &other);
	void OnlyPrimary();
	void OnlyGlobal();
	virtual void Update();
	virtual NicaTrack *GetNewTrack() const;
	virtual NicaEvent *GetNewEvent() const{return new NicaMpdDstKalmanEvent(*this);};
	virtual TString GetFormatName()const{return "MpdDstKalman";};
	virtual ~NicaMpdDstKalmanEvent();
	ClassDef(NicaMpdDstKalmanEvent,1)
};

#endif /* INTERFACES_MPDROOT_FORMAT_NICAMPDDSTKALMANEVENT_H_ */
