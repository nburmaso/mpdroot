/*
 * NicaFairSource.cxx
 *
 *  Created on: 2 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaFairEventInterface.h"
#include "FairRootManager.h"
#include "NicaFairTrackInterface.h"

NicaFairEventInterface::NicaFairEventInterface():fEvent(NULL),fFairTracks(NULL) {
	fEvent = new FairMCEventHeader();
	fFairTracks = new NicaTrackClones("FairMCTrack","MCTrack","MC");
}

void NicaFairEventInterface::Clear(Option_t* opt) {
	fFairTracks->GetArray()->Clear(opt);
}

void NicaFairEventInterface::ConnectToTree() {
	FairRootManager *manager  = FairRootManager::Instance();
	if(CanDeleteEvent()){
		if(fEvent) delete fEvent;
		fFairTracks->DeleteClones();
	}
	fEvent = (FairMCEventHeader*)manager->GetObject("MCEventHeader.");
	if(fEvent==NULL)
		fEvent = (FairMCEventHeader*)manager->GetObject("EventHeader.");
	fFairTracks->GetFromTree();
}

void NicaFairEventInterface::Compress(Int_t* map, Int_t map_size) {
	fFairTracks->Compress(map, map_size);
}

void NicaFairEventInterface::CopyData(NicaEventInterface* s) {
	FairMCEventHeader *header = (FairMCEventHeader*)((NicaFairEventInterface*)s)->fEvent;
	NicaTrackClones *tracks = (NicaTrackClones*)((NicaFairEventInterface*)s)->fFairTracks;
	fEvent->SetRunID(header->GetRunID());
	fEvent->SetEventID(header->GetEventID());
	fEvent->SetVertex(header->GetX(),header->GetY(),header->GetZ());
	fEvent->SetRotX(header->GetRotX());
	fEvent->SetRotY(header->GetRotY());
	fEvent->SetRotZ(header->GetRotZ());
	fEvent->SetB(header->GetB());
	fEvent->SetNPrim(header->GetNPrim());
	fFairTracks->CopyFrom<FairMCTrack>(tracks->GetArray());
}

void NicaFairEventInterface::CopyAndCompress(NicaEventInterface* s, Int_t* map,
		Int_t map_size) {
	FairMCEventHeader *header = (FairMCEventHeader*)((NicaFairEventInterface*)s)->fEvent;

	fEvent->SetRunID(header->GetRunID());
	fEvent->SetEventID(header->GetEventID());
	fEvent->SetVertex(header->GetX(),header->GetY(),header->GetZ());
	fEvent->SetRotX(header->GetRotX());
	fEvent->SetRotY(header->GetRotY());
	fEvent->SetRotZ(header->GetRotZ());
	fEvent->SetB(header->GetB());
	fEvent->SetNPrim(header->GetNPrim());
	fFairTracks->CopyCompress<FairMCTrack>(((NicaFairEventInterface*)s)->fFairTracks->GetArray(),
			map, map_size);
}

void NicaFairEventInterface::Register(Bool_t write) {
	if(fEvent==NULL){
		fEvent = new FairMCEventHeader();
		fFairTracks = new NicaTrackClones("FairMCTrack","MCTrack","MC");
	}
	FairRootManager *manager = FairRootManager::Instance();
	TString event_name = "MCEventHeader.";
	TString track_name = "MCTrack.";
	manager->Register(event_name,"MCEventHeader",fEvent,write);
	fFairTracks->Register(write);
}

NicaTrackInterface* NicaFairEventInterface::GetTrackInterface() const {
	return new NicaFairTrackInterface();
}

void NicaFairEventInterface::FillTrackInterface(NicaTrackInterface* track,
		Int_t index) {
	track->SetRawTrack(fFairTracks->UncheckedAt(index));
}

NicaFairEventInterface::~NicaFairEventInterface() {
	if(CanDeleteEvent()){
		if(fEvent) delete fEvent;
		if(fFairTracks) delete fFairTracks;
	}
}

TLorentzVector NicaFairEventInterface::GetVertex() const {
	TLorentzVector vec;
	TVector3 vec3d;
	fEvent->GetVertex(vec3d);
	vec.SetXYZT(vec3d.X(),vec3d.Y(),vec3d.Z(),0);
	return vec;
}
