/*
 * NicaEventFair.cxx
 *
 *  Created on: 05-07-2014
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaFairExtendedEvent.h"
#include <TRandom.h>
#include "FairRootManager.h"
#include "NicaFairExtendedEventInterface.h"
#include "NicaFairExtendedTrack.h"

NicaFairExtendedEvent::NicaFairExtendedEvent(): NicaFairEvent("NicaFairExtendedTrack"){
}

void NicaFairExtendedEvent::Update() {
	NicaFairEvent::Update();
	TClonesArray *freez = ((NicaFairExtendedEventInterface*)fSource)->fFreezouts;
	for(int i=0;i<freez->GetEntriesFast();i++){
		TLorentzVector *fr_from = (TLorentzVector*)freez->UncheckedAt(i);
		NicaMCTrack *mc = (NicaMCTrack*)GetTrack(i);
		mc->GetLink()->Clear();
		mc->GetLink()->SetLink(0,i);
		TLorentzVector *fr_to = mc->GetFreezoutPosition();
		*fr_to = *fr_from;
	}
	for(int i=freez->GetEntriesFast();i<fTotalTracksNo;i++){
		NicaMCTrack *mc = (NicaMCTrack*)GetTrack(i);
		TLorentzVector *fr_to = mc->GetFreezoutPosition();
		fr_to->SetXYZT(gRandom->Gaus(0,1E+6),gRandom->Gaus(0,1E+6),gRandom->Gaus(0,1E+6),gRandom->Gaus(0,1E+6));
	}
}

void NicaFairExtendedEvent::LinkTracks() {
/*	for(int i=0;i<fTotalTracksNo;i++){
		NicaFairExtendedTrack *track = (NicaFairExtendedTrack*)GetTrack(i);
	}
	*/
}

NicaFairExtendedEvent::NicaFairExtendedEvent(
		const NicaFairExtendedEvent& other) :NicaFairEvent(other){
}

NicaFairExtendedEvent::~NicaFairExtendedEvent() {
}

TString NicaFairExtendedEvent::GetFormatName() const{
	return "FairMCExtendedFormat";
}

void NicaFairExtendedEvent::CreateSource() {
	fSource = new NicaFairExtendedEventInterface();
}

Bool_t NicaFairExtendedEvent::ExistInTree() const {
	FairRootManager *manager = FairRootManager::Instance();
	if(manager->CheckBranch("Freezouts.")&&NicaFairEvent::ExistInTree()){
		return kTRUE;
	}
	return kFALSE;
}
