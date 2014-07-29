// Author: Emelyanov D.
// Update: 2009-09-17 17:29:46+0400
// Copyright: 2009 (C) MPD coll.
//

#ifndef ROOT_MpdEvent
#include "MpdEvent.h"
#endif

// -----   Default constructor ---------------------------------------
MpdEvent::MpdEvent():
  TNamed("MpdEvent","Global"),
  fEventInfoNofPrimaryTracks(0),
  fEventInfoNofGlobalTracks(0),
  fPrimaryTracks(new TClonesArray("MpdTrack")),
  fGlobalTracks(new TClonesArray("MpdTrack"))
{}

// -------------------------------------------------------------------
MpdEvent::~MpdEvent()
{
  delete fPrimaryTracks;
  delete fGlobalTracks;
}

void MpdEvent::Reset(){
  fEventInfoNofPrimaryTracks = 0;
  fEventInfoNofGlobalTracks = 0;
  fPrimaryTracks->Delete();
  fGlobalTracks->Delete();
}

// -------------------------------------------------------------------
MpdTrack *MpdEvent::AddPrimaryTrack() 
{
  return new((*fPrimaryTracks)[fEventInfoNofPrimaryTracks++]) MpdTrack();
}

// -------------------------------------------------------------------
MpdTrack *MpdEvent::AddGlobalTrack()
{
  return new((*fGlobalTracks)[fEventInfoNofGlobalTracks++]) MpdTrack();
}
// -------------------------------------------------------------------
ClassImp(MpdEvent);
