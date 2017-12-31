/*
 * NicaMpdKalmanEventInterface.cxx
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdKalmanEventInterface.h"
#include "NicaMpdKalmanTrackInterface.h"
#include "MpdKalmanTrack.h"
#include "FairRootManager.h"

NicaMpdKalmanEventInterface::NicaMpdKalmanEventInterface() {
	fTracks = new TClonesArray("MpdKalmanTrack",1000);
}

void NicaMpdKalmanEventInterface::Compress(Int_t* map, Int_t map_size) {
	Int_t track_pos = 0;
	for(int i=0;i<map_size;i++){
		Int_t good_track = map[i];
		for(int j=track_pos;j<good_track;track_pos++){
			fTracks->RemoveAt(i);
		}
		track_pos = good_track+1;
	}
}

void NicaMpdKalmanEventInterface::Copy(NicaEventInterface* s) {
	NicaMpdKalmanEventInterface *ev = (NicaMpdKalmanEventInterface*)s;
	Int_t total = ev->fTracks->GetEntriesFast();
	fTracks->Clear();
	fTracks->ExpandCreateFast(total);
	for(int i=0;i<total;i++){
		MpdKalmanTrack *from = (MpdKalmanTrack*)ev->fTracks->UncheckedAt(i);
		MpdKalmanTrack *to = (MpdKalmanTrack*)fTracks->UncheckedAt(i);
		*to = *from;
	}
}

void NicaMpdKalmanEventInterface::CopyAndCompress(NicaEventInterface* s,
		Int_t* map, Int_t map_size) {
	NicaMpdKalmanEventInterface *ev = (NicaMpdKalmanEventInterface*)s;
	for(int i=0;i<map_size;i++){
		MpdKalmanTrack *from = (MpdKalmanTrack*)ev->fTracks->UncheckedAt(map[i]);
		MpdKalmanTrack *to = (MpdKalmanTrack*)fTracks->UncheckedAt(i);
		*to = *from;
	}
}

void NicaMpdKalmanEventInterface::FastCopy(NicaEventInterface* s) {
	fTracks = ((NicaMpdKalmanEventInterface*)s)->fTracks;
}

void NicaMpdKalmanEventInterface::Register(Bool_t write) {
	FairRootManager *manager = FairRootManager::Instance();
	manager->Register("TpcKalmanTrack","TpcKalmanTrack",fTracks,write);
}

void NicaMpdKalmanEventInterface::ConnectToTree() {
	FairRootManager *manager = FairRootManager::Instance();
	//if(fTracks) delete fTracks;
	fTracks = (TClonesArray*)manager->GetObject("TpcKalmanTrack");
}

void NicaMpdKalmanEventInterface::FillTrackInterface(NicaTrackInterface* track,
		Int_t index) {
	track->SetRawTrack(fTracks->UncheckedAt(index));
}

Bool_t NicaMpdKalmanEventInterface::ExistInTree() const {
	FairRootManager *manager = FairRootManager::Instance();
	if(manager->CheckBranch("TpcKalmanTrack")){
		return kTRUE;
	}
	return kFALSE;
}

Int_t NicaMpdKalmanEventInterface::GetTotalTrackNo() const {
	return fTracks->GetEntriesFast();
}

NicaTrackInterface* NicaMpdKalmanEventInterface::GetTrackInterface() const {
	return new NicaMpdKalmanTrackInterface();
}

TObject* NicaMpdKalmanEventInterface::GetRawTrackPointer(Int_t index) const {
	return fTracks->UncheckedAt(index);
}

TObject* NicaMpdKalmanEventInterface::GetRawEventPointer() const {
	return fTracks;
}

NicaMpdKalmanEventInterface::NicaMpdKalmanEventInterface(
		const NicaMpdKalmanEventInterface& other) :NicaEventInterface(other) {
	fTracks = new TClonesArray("MpdKalmanTrack",1000);
}

NicaMpdKalmanEventInterface::~NicaMpdKalmanEventInterface() {
	if(CanDeleteEvent())
	if(fTracks)
		delete fTracks;
}

