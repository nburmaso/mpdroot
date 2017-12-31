/*
 * NicaMpdDstKalman.cxx
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdDstKalmanEventInterface.h"
#include "NicaMpdDstKalmanEvent.h"
#include "NicaMpdDstKalmanTrack.h"
#include "MpdTpcKalmanTrack.h"
NicaMpdDstKalmanEvent::NicaMpdDstKalmanEvent() : NicaExpEvent("NicaMpdDstKalmanTrack"), fMode(kAllTracks){
	fSource = new NicaMpdDstKalmanEventInterface();
}

void NicaMpdDstKalmanEvent::Update() {
	fTracks->Clear();
	NicaMpdDstKalmanEventInterface *interface = (NicaMpdDstKalmanEventInterface*)fSource;
	MpdEvent *event =(MpdEvent*) interface->GetRawEventPointer();
	fVertex->SetXYZT(event->GetPrimaryVerticesX(), event->GetPrimaryVerticesY(),event->GetPrimaryVerticesZ(),0);
	fVertexError->SetXYZT(event->GetPrimaryVerticesXerr(), event->GetPrimaryVerticesYerr(),event->GetPrimaryVerticesZerr(),0);
	fTotalTracksNo = interface->GetTotalTrackNo();
	fRunInfoId = event->GetRunInfoRunId();
	fMagField->SetXYZ(0,0,event->GetRunInfoMagneticFieldZ());
	TClonesArray *prim = event->GetPrimaryTracks();
	TClonesArray *glob = event->GetGlobalTracks();
	TClonesArray *kalm = interface->GetKalmans();
	switch(fMode){
	case kAllTracks:{
		fTotalTracksNo  = event->GetEventInfoNofPrimaryTracks()
		+event->GetEventInfoNofGlobalTracks();
		fTracks->ExpandCreateFast(fTotalTracksNo);
		int count = 0;
		for(int i=0;i<event->GetEventInfoNofPrimaryTracks();i++){
			NicaMpdDstKalmanTrack *mpd_track = (NicaMpdDstKalmanTrack*)fTracks->ConstructedAt(count++);
			mpd_track->Update((MpdTrack*)prim->UncheckedAt(i),NULL);
			mpd_track->SetEvent(this);
		}
		for(int i=0;i<event->GetEventInfoNofGlobalTracks();i++){
			NicaMpdDstKalmanTrack *mpd_track = (NicaMpdDstKalmanTrack*)fTracks->ConstructedAt(count++);
			mpd_track->Update((MpdTrack*)glob->UncheckedAt(i),NULL);
			mpd_track->SetEvent(this);
		}

	}break;
	case kPrimaryTracks:{
		fTotalTracksNo  = event->GetEventInfoNofPrimaryTracks();
		fTracks->ExpandCreateFast(fTotalTracksNo);
		for(int i=0;i<event->GetEventInfoNofPrimaryTracks();i++){
			NicaMpdDstKalmanTrack *mpd_track = (NicaMpdDstKalmanTrack*)fTracks->ConstructedAt(i);
			mpd_track->Update((MpdTrack*)prim->UncheckedAt(i),NULL);
			mpd_track->SetEvent(this);
		}
	}break;
	case kGlobalTracks:{
		fTotalTracksNo  = event->GetEventInfoNofGlobalTracks();
		fTracks->ExpandCreateFast(fTotalTracksNo);
		for(int i=0;i<event->GetEventInfoNofGlobalTracks();i++){
			NicaMpdDstKalmanTrack *mpd_track = (NicaMpdDstKalmanTrack*)fTracks->ConstructedAt(i);
			MpdTpcKalmanTrack *kalman_track = (MpdTpcKalmanTrack*)kalm->UncheckedAt(i);
			mpd_track->Update((MpdTrack*)glob->UncheckedAt(i), kalman_track);
			mpd_track->SetEvent(this);
		}
	}break;
	}
	fTracks->Compress();
}

void NicaMpdDstKalmanEvent::OnlyPrimary() {
	fMode = kPrimaryTracks;
}

void NicaMpdDstKalmanEvent::OnlyGlobal() {
	fMode = kGlobalTracks;
}

NicaTrack* NicaMpdDstKalmanEvent::GetNewTrack() const {
	return new NicaMpdDstKalmanTrack();
}

NicaMpdDstKalmanEvent::~NicaMpdDstKalmanEvent() {
	// TODO Auto-generated destructor stub
}

NicaMpdDstKalmanEvent::NicaMpdDstKalmanEvent(
		const NicaMpdDstKalmanEvent& other) :NicaExpEvent(other){
	if(Source()) CreateSource();
	fMode = other.fMode;
}

void NicaMpdDstKalmanEvent::CreateSource() {
	fSource = new NicaMpdDstKalmanEventInterface();
}
