/*
 * MpdHbtEvent.cxx
 *
 *  Created on: 1 paź 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdHbtEvent.h"

#include "NicaPackage.h"
#include "NicaParameter.h"

NicaMpdHbtEvent::NicaMpdHbtEvent()
    : NicaExpEvent("NicaMpdHbtTrack"), fR(50.0) {}

Bool_t NicaMpdHbtEvent::AreCompatible(const NicaEvent* buffered) const {
  if (buffered->InheritsFrom("NicaMpdMiniDstEvent")) return kTRUE;
  return kFALSE;
}

NicaPackage* NicaMpdHbtEvent::Report() const {
  NicaPackage* rep = NicaEvent::Report();
  rep->AddObject(new NicaParameterDouble("R", fR));
  return rep;
}

NicaMpdHbtEvent::NicaMpdHbtEvent(const NicaMpdHbtEvent& other)
    : NicaExpEvent(other), fR(other.fR) {}

NicaMpdHbtEvent& NicaMpdHbtEvent::operator=(const NicaMpdHbtEvent& other) {
  if (this == &other) return *this;
  NicaExpEvent::operator=(other);
  fR = other.fR;
  return *this;
}

void NicaMpdHbtEvent::CreateSource() {
  NicaCout::PrintInfo("Cannot create source for NicaMpdHbtEvent", kLessError);
}

NicaMpdHbtEvent::~NicaMpdHbtEvent() {}
