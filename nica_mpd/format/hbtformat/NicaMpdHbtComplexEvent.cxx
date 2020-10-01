/*
 * NicaMpdHbtComplexEvent.cxx
 *
 *  Created on: 1 paź 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdHbtComplexEvent.h"
#include "NicaMpdDstMCEvent.h"
#include "NicaMpdHbtEvent.h"

NicaMpdHbtComplexEvent::NicaMpdHbtComplexEvent()
    : NicaComplexEvent(new NicaMpdHbtEvent(), new NicaMpdDstMCEvent()) {}

NicaMpdHbtComplexEvent::~NicaMpdHbtComplexEvent() {
  // TODO Auto-generated destructor stub
}

NicaMpdHbtComplexEvent::NicaMpdHbtComplexEvent(
    const NicaMpdHbtComplexEvent &other) {
  // TODO Auto-generated constructor stub
}
