/*
 * NicaMpdDstMCEventTpcPads.cxx
 *
 *  Created on: 1 kwi 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdDstMCEventTpcPads.h"
#include "NicaFairEvent.h"
#include "FairMCTrack.h"
#include "NicaTpcTrack.h"

NicaMpdDstMCEventTpcPads::NicaMpdDstMCEventTpcPads() :NicaComplexEvent(new NicaMpdEventTpcPads(), new NicaFairEvent()){
}

void NicaMpdDstMCEventTpcPads::OnlyPrimary() {
	((NicaMpdEventTpcPads*)fRealEvent)->OnlyPrimary();
}

void NicaMpdDstMCEventTpcPads::OnlyGlobal() {
	((NicaMpdEventTpcPads*)fRealEvent)->OnlyGlobal();
}

void NicaMpdDstMCEventTpcPads::Update() {
	fImgEvent->Update();
	fRealEvent->Update();
	NicaEvent::ShallowCopyEvent(fRealEvent);
	fTracks->Clear();
	fTotalTracksNo = fRealEvent->GetTotalTrackNo();
	fTracks->ExpandCreateFast(fTotalTracksNo);
	for(int i=0;i<fTotalTracksNo;i++){
		NicaComplexTrack *track = (NicaComplexTrack*)fTracks->UncheckedAt(i);
		track->SetRealTrack(fRealEvent->GetTrack(i));
		MpdTrack *mpd_track = (MpdTrack*)track->GetRealTrack()->GetTrackPointer();
		Int_t parent_id = mpd_track->GetID();
		track->NicaTrack::CopyData(fRealEvent->GetTrack(i));
		track->SetMatchID(parent_id);
		if(parent_id>=0){
			track->SetImgTrack(fImgEvent->GetTrack(parent_id));
		}else{
			track->SetImgTrack(NULL);
		}
	}
}

NicaMpdDstMCEventTpcPads::~NicaMpdDstMCEventTpcPads() {
}

