/*
 * NicaMpdFullEvent.h
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef INTERFACES_MPDROOT_FORMAT_COMBINED_NICAMPDFULLEVENT_H_
#define INTERFACES_MPDROOT_FORMAT_COMBINED_NICAMPDFULLEVENT_H_

#include "NicaComplexEvent.h"

class NicaMpdFullEvent  : public NicaComplexEvent{
public:
	NicaMpdFullEvent();
	void OnlyPrimary();
	void OnlyGlobal();
	void Update();
	virtual TString GetFormatName()const{return "MpdFullEvent";};
	virtual ~NicaMpdFullEvent();
	ClassDef(NicaMpdFullEvent,1)
};

#endif /* INTERFACES_MPDROOT_FORMAT_COMBINED_NICAMPDFULLEVENT_H_ */
