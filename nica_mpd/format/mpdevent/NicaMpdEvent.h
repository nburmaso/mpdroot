/*
 * NicaMpdEvent.h
 *
 *  Created on: 28 mar 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef INTERFACES_MPDROOT_NICAMPDEVENT_H_
#define INTERFACES_MPDROOT_NICAMPDEVENT_H_
#include "NicaMpdEvent.h"

#include "NicaExpEvent.h"
#include "MpdEvent.h"
#include "NicaMpdTrack.h"
class NicaMpdEvent : public NicaExpEvent{
	enum kTrackType{
		kAllTracks,
		kPrimaryTracks,
		kGlobalTracks
	};
	kTrackType fMode;
protected:
	virtual void ShallowCopyTracks(NicaEvent *event);
	virtual void ShallowCopyEvent(NicaEvent *event);
	void CreateSource();
public:
	NicaMpdEvent();
	NicaMpdEvent(const 	NicaMpdEvent &other);
	void Update();
	void OnlyPrimary();
	void OnlyGlobal();
	virtual TString GetFormatName()const;
	virtual ~NicaMpdEvent();
	ClassDef(NicaMpdEvent,1)
};

#endif /* INTERFACES_MPDROOT_NICAMPDEVENT_H_ */

