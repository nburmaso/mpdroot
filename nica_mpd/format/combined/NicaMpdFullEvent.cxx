/*
 * NicaMpdFullEvent.cxx
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdFullEvent.h"
#include "NicaMpdDstKalmanEvent.h"
#include "NicaFairEvent.h"

NicaMpdFullEvent::NicaMpdFullEvent() : NicaComplexEvent(new NicaMpdDstKalmanEvent(), new NicaFairEvent()){
}

void NicaMpdFullEvent::OnlyPrimary() {
	((NicaMpdDstKalmanEvent*)fRealEvent)->OnlyPrimary();
}

void NicaMpdFullEvent::OnlyGlobal() {
	((NicaMpdDstKalmanEvent*)fRealEvent)->OnlyGlobal();
}

NicaMpdFullEvent::~NicaMpdFullEvent() {
	// TODO Auto-generated destructor stub
}

