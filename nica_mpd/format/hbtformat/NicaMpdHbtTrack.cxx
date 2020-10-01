/*
 * MpdHbtTrack.cxx
 *
 *  Created on: 1 paź 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdHbtTrack.h"

#include "NicaMpdConst.h"
#include "NicaMpdHbtEvent.h"
#include "NicaTrackTpcPads.h"

NicaMpdHbtTrack::NicaMpdHbtTrack() : fPads(new NicaTrackTpcPads()) {}

void NicaMpdHbtTrack::CopyData(NicaTrack *track) {
  NicaExpTrack::CopyData(track);
  NicaMpdHbtEvent *event = (NicaMpdHbtEvent *)GetEvent();
  // calculate pads
  GetPadsInfo()->Calculate(GetHelix(), event->GetVertex());
}

NicaMpdHbtTrack::NicaMpdHbtTrack(const NicaMpdHbtTrack &other)
    : NicaExpTrack(other), fPads(new NicaTrackTpcPads(*other.fPads)) {}

NicaMpdHbtTrack &NicaMpdHbtTrack::operator=(const NicaMpdHbtTrack &other) {
  if (this == &other) return *this;
  NicaExpTrack::operator=(other);
  *fPads = *other.fPads;
  return *this;
}

NicaMpdHbtTrack::~NicaMpdHbtTrack() { delete fPads; }
