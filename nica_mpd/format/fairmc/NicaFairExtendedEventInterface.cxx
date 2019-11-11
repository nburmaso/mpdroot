/*
 * NicaFairSource.cxx
 *
 *  Created on: 2 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaFairExtendedEventInterface.h"
#include "FairRootManager.h"
#include "NicaFairExtendedTrackInterface.h"

NicaFairExtendedEventInterface::NicaFairExtendedEventInterface():NicaFairEventInterface(),fFreezouts(NULL) {
	fFreezouts = new TClonesArray("TLorentzVector");
}

void NicaFairExtendedEventInterface::Clear(Option_t* opt) {
	NicaFairEventInterface::Clear(opt);
	fFreezouts->Clear(opt);
}

void NicaFairExtendedEventInterface::ConnectToTree() {
	NicaFairEventInterface::ConnectToTree();
	FairRootManager *manager  = FairRootManager::Instance();
	if(fFreezouts) delete fFreezouts;
	fFreezouts = (TClonesArray*)manager->GetObject("Freezouts.");
}

void NicaFairExtendedEventInterface::Compress(Int_t* map, Int_t map_size) {
	NicaFairEventInterface::Compress(map,map_size);
	Int_t track_pos=0;
	for(int i=0;i<map_size;i++){
		Int_t good_track = map[i];
		for(int j=track_pos;j<good_track;track_pos++){
			fFreezouts->RemoveAt(j);
		}
		track_pos = good_track+1;
	}
	fFreezouts->Compress();
}

void NicaFairExtendedEventInterface::CopyData(NicaEventInterface* s) {
	NicaFairEventInterface::CopyData(s);
	NicaFairExtendedEventInterface *ex = (NicaFairExtendedEventInterface*)s;
	fFreezouts->Clear();
	fFreezouts->ExpandCreateFast(ex->fFreezouts->GetEntriesFast());
	for(int i =0;i<ex->fFreezouts->GetEntriesFast();i++){
		TLorentzVector* from = (TLorentzVector*)((NicaFairExtendedEventInterface*)s)->fFreezouts->UncheckedAt(i);
		TLorentzVector *to = (TLorentzVector*)fFreezouts->UncheckedAt(i);
		*to = *from;
	}
}

void NicaFairExtendedEventInterface::CopyAndCompress(NicaEventInterface* s, Int_t* map,
		Int_t map_size) {
	NicaFairEventInterface::CopyAndCompress(s,map,map_size);
	fFreezouts->Clear();
	for(int i =0;i<map_size;i++){
		TLorentzVector* from = (TLorentzVector*)((NicaFairExtendedEventInterface*)s)->fFreezouts->UncheckedAt(map[i]);
		TLorentzVector *to = (TLorentzVector*)fFreezouts->ConstructedAt(i);
		*to = *from;
	}
}

void NicaFairExtendedEventInterface::Register(Bool_t write) {
	NicaFairEventInterface::Register(write);
	if(fFreezouts==NULL){
		fFreezouts = new TClonesArray("TLorentzVector",1000);
	}
	FairRootManager *manager = FairRootManager::Instance();
	manager->Register("Freezouts.","Freezouts",fFreezouts,write);
}

NicaFairExtendedEventInterface::~NicaFairExtendedEventInterface() {
	if(CanDeleteEvent()){
		if(fFreezouts) delete fFreezouts;
	}
}

void NicaFairExtendedEventInterface::FillTrackInterface(
		NicaTrackInterface* track, Int_t index) {
	NicaFairExtendedTrackInterface *tr = (NicaFairExtendedTrackInterface*)track;
	tr->SetRawTrack(GetRawTrackPointer(index),GetRawFreezPointer(index));
}
