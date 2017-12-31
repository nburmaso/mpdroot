/*
 * NicaMpdDstMCEvent.h
 *
 *  Created on: 1 kwi 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef INTERFACES_MPDROOT_NICAMPDDSTMCEVENT_H_
#define INTERFACES_MPDROOT_NICAMPDDSTMCEVENT_H_

#include "NicaComplexEvent.h"
#include "NicaMCEvent.h"
#include "NicaMpdEvent.h"
#include "NicaMpdTrack.h"

class NicaMpdDstMCEvent : public NicaComplexEvent{
public:
	NicaMpdDstMCEvent();
	void OnlyPrimary();
	void OnlyGlobal();
	void Update();
	virtual NicaTrack *GetNewTrack() const{return new NicaComplexTrack();};
	virtual NicaEvent *GetNewEvent() const{return new NicaMpdDstMCEvent(*this);};
	virtual TString GetFormatName()const{return "NicaMpdDstMCEvent";};
	virtual ~NicaMpdDstMCEvent();
	ClassDef(NicaMpdDstMCEvent,1)
};

#endif /* INTERFACES_MPDROOT_NICAMPDDSTMCEVENT_H_ */
