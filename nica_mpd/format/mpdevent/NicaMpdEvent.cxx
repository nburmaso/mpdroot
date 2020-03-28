/*
 * NicaMpdEvent.cxx
 *
 *  Created on: 28 mar 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdTrack.h"
#include "NicaMpdEvent.h"
#include "NicaMpdTrack.h"
#include "NicaMpdEventInterface.h"
#include "FairRootManager.h"

NicaMpdEvent::NicaMpdEvent(const NicaMpdEvent& other) :NicaExpEventHelix(other){
	if(other.fSource) CreateSource();
	fMode = other.fMode;
}

void NicaMpdEvent::Update() {
	fTracks->Clear();
	NicaMpdEventInterface *interface = (NicaMpdEventInterface*)fSource;
	MpdEvent *event =(MpdEvent*) interface->GetRawEventPointer();
	fVertex->SetXYZT(event->GetPrimaryVerticesX(), event->GetPrimaryVerticesY(),event->GetPrimaryVerticesZ(),0);
	fVertexError->SetXYZT(event->GetPrimaryVerticesXerr(), event->GetPrimaryVerticesYerr(),event->GetPrimaryVerticesZerr(),0);
	fTotalTracksNo = interface->GetTotalTrackNo();
	fRunInfoId = event->GetRunInfoRunId();
	fMagField->SetXYZ(0,0,event->GetRunInfoMagneticFieldZ());
	TClonesArray *prim = event->GetPrimaryTracks();
	TClonesArray *glob = event->GetGlobalTracks();
	switch(fMode){
	case kAllTracks:{
		fTotalTracksNo  = event->GetEventInfoNofPrimaryTracks()
		+event->GetEventInfoNofGlobalTracks();
		fTracks->ExpandCreateFast(fTotalTracksNo);
		int count = 0;
		for(int i=0;i<event->GetEventInfoNofPrimaryTracks();i++){
			NicaMpdTrack *mpd_track = (NicaMpdTrack*)fTracks->ConstructedAt(count++);
			mpd_track->Update((MpdTrack*)prim->UncheckedAt(i));
			mpd_track->SetEvent(this);
			mpd_track->GetLink()->Clear();
			mpd_track->GetLink()->SetLink(0, i);
			mpd_track->SetID(count-1);
		}
		for(int i=0;i<event->GetEventInfoNofGlobalTracks();i++){
			NicaMpdTrack *mpd_track = (NicaMpdTrack*)fTracks->ConstructedAt(count++);
			mpd_track->Update((MpdTrack*)glob->UncheckedAt(i));
			mpd_track->SetEvent(this);
			mpd_track->GetLink()->Clear();
			mpd_track->GetLink()->SetLink(0, i+event->GetEventInfoNofPrimaryTracks());
			mpd_track->SetID(count-1);
		}

	}break;
	case kPrimaryTracks:{
		fTotalTracksNo  = event->GetEventInfoNofPrimaryTracks();
		fTracks->ExpandCreateFast(fTotalTracksNo);
		for(int i=0;i<event->GetEventInfoNofPrimaryTracks();i++){
			NicaMpdTrack *mpd_track = (NicaMpdTrack*)fTracks->ConstructedAt(i);
			mpd_track->Update((MpdTrack*)prim->UncheckedAt(i));
			mpd_track->SetEvent(this);
			mpd_track->GetLink()->Clear();
			mpd_track->GetLink()->SetLink(0, i);
			mpd_track->SetID(i);
		}
	}break;
	case kGlobalTracks:{
		fTotalTracksNo  = event->GetEventInfoNofGlobalTracks();
		fTracks->ExpandCreateFast(fTotalTracksNo);
		for(int i=0;i<event->GetEventInfoNofGlobalTracks();i++){
			NicaMpdTrack *mpd_track = (NicaMpdTrack*)fTracks->ConstructedAt(i);
			mpd_track->Update((MpdTrack*)glob->UncheckedAt(i));
			mpd_track->SetEvent(this);
			mpd_track->GetLink()->Clear();
			mpd_track->GetLink()->SetLink(0, i);
			mpd_track->SetID(i);
		}
	}break;
	}
	fTracks->Compress();
}

void NicaMpdEvent::CreateSource() {
	fSource = new NicaMpdEventInterface();
}

TString NicaMpdEvent::GetFormatName() const{
	switch(fMode){
	case kAllTracks:
		return "MpdEvent";
		break;
	case kPrimaryTracks:
		return "MpdEvent+PrimaryTracks";
		break;
	case kGlobalTracks:
		return "MpdEvent+GlobalTracks";
		break;
	default:
		return "";
		break;
	}
}

void NicaMpdEvent::ShallowCopyEvent(NicaEvent* event) {
	NicaExpEventHelix::ShallowCopyEvent(event);
}

void NicaMpdEvent::OnlyPrimary() {
	fMode = kPrimaryTracks;
	if(fSource){
		((NicaMpdEventInterface*)fSource)->OnlyPrimary();
	}
}

void NicaMpdEvent::OnlyGlobal() {
	fMode = kGlobalTracks;
	if(fSource){
		((NicaMpdEventInterface*)fSource)->OnlyGlobal();
	}
}
Bool_t NicaMpdEvent::ExistInTree() const {
	FairRootManager *manager = FairRootManager::Instance();
	if(manager->CheckBranch("MPDEvent.")){
		return kTRUE;
	}
	return kFALSE;
}

NicaMpdEvent::~NicaMpdEvent() {
}

NicaMpdEvent::NicaMpdEvent() :NicaExpEventHelix("NicaMpdTrack"){
	fSource = new NicaMpdEventInterface();
	fMode = kAllTracks;
}

NicaMpdEvent::NicaMpdEvent(TString trackname) :NicaExpEventHelix(trackname){
	fSource = new NicaMpdEventInterface();
	fMode = kAllTracks;
}
