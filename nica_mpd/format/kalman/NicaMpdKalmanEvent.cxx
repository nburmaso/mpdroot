/*
 * NicaMpdKalmanEvent.cxx
 *
 *  Created on: 6 lip 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdKalmanEvent.h"
#include "NicaMpdKalmanEventInterface.h"
#include "MpdKalmanTrack.h"
#include "NicaMpdKalmanTrack.h"

NicaMpdKalmanEvent::NicaMpdKalmanEvent():NicaEvent("NicaMpdKalmanTrack") {
	fSource = new NicaMpdKalmanEventInterface();
}

void NicaMpdKalmanEvent::Update() {
	fTotalTracksNo = fSource->GetTotalTrackNo();
	TClonesArray *kalmans = ((NicaMpdKalmanEventInterface*)fSource)->fTracks;
	fTracks->ExpandCreateFast(fTotalTracksNo);
	for(int i=0;i<fTotalTracksNo;i++){
		NicaTrack *tr = GetTrack(i);
		tr->SetTrackPointer(fTracks->UncheckedAt(i));
		MpdKalmanTrack *kalman = (MpdKalmanTrack*)kalmans->UncheckedAt(i);
		TLorentzVector *mom = tr->GetMomentum();
		Double_t pt = kalman->Pt();
		Double_t phi = kalman->Phi();
		Double_t theta = kalman->Theta();
		mom->SetPxPyPzE(pt*TMath::Cos(phi),pt*TMath::Sin(phi),pt*TMath::ATan(theta),0);
		tr->SetID(kalman->GetTrackID());
	}
}

void NicaMpdKalmanEvent::Clear(Option_t* opt) {
	NicaEvent::Clear(opt);
	if(Source())
		fSource->Clear(opt);
}

NicaTrack* NicaMpdKalmanEvent::GetNewTrack() const {
	return new NicaMpdKalmanTrack();
}

NicaEvent* NicaMpdKalmanEvent::GetNewEvent() const {
	return new NicaMpdKalmanEvent();
}

NicaMpdKalmanEvent::NicaMpdKalmanEvent(const NicaMpdKalmanEvent& other) {
	if(Source()) CreateSource();
}

void NicaMpdKalmanEvent::CreateSource() {
	fSource = new NicaMpdKalmanEventInterface();
}

NicaMpdKalmanEvent::~NicaMpdKalmanEvent() {
}

