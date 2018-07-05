/*
 * NicaMpdDstKalmanInterface.cxx
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdDstKalmanEventInterface.h"
#include "NicaMpdDstKalmanTrackInterface.h"
#include "NicaMpdKalmanEventInterface.h"
#include "MpdKalmanTrack.h"
#include "NicaMpdTrackInterface.h"
#include "FairRootManager.h"

NicaMpdDstKalmanEventInterface::NicaMpdDstKalmanEventInterface() :fEvent(NULL),fKalmanTracks(NULL){
	fMpdInterface = new NicaMpdEventInterface();
	fKalmanInterface = new NicaMpdKalmanEventInterface();
	fMode = kAllTracks;
	fEvent = fMpdInterface->fEvent;
	fKalmanTracks = fKalmanInterface->fTracks;
}

void NicaMpdDstKalmanEventInterface::Copy(NicaEventInterface* s) {
	NicaMpdDstKalmanEventInterface *ev = (NicaMpdDstKalmanEventInterface*)s;
	fMpdInterface->Copy(ev->fMpdInterface);
	fKalmanInterface->Copy(ev->fKalmanInterface);
}

void NicaMpdDstKalmanEventInterface::Register(Bool_t write) {
	fMpdInterface->Register(write);
	fKalmanInterface->Register(write);
}

void NicaMpdDstKalmanEventInterface::FastCopy(NicaEventInterface* s) {
	NicaMpdDstKalmanEventInterface *ev = (NicaMpdDstKalmanEventInterface*)s;
	fMpdInterface->SetSource(ev->fMpdInterface);
	fKalmanInterface->SetSource(ev->fKalmanInterface);
}

void NicaMpdDstKalmanEventInterface::Compress(Int_t* map, Int_t map_size) {
	switch(fMode){
		case kAllTracks:{
			// TODO watch for upgrades !
			fMpdInterface->Compress(map,map_size);
			fKalmanInterface->Compress(map,map_size);
		}break;
		case kPrimaryTracks:{
			//TODO Watch for upgrades !
		}break;
		case kGlobalTracks:{
			fMpdInterface->Compress(map,map_size);
			fKalmanInterface->Compress(map,map_size);
		}break;
		}
}

void NicaMpdDstKalmanEventInterface::CopyAndCompress(NicaEventInterface* s,
		Int_t* map, Int_t map_size) {
	NicaMpdDstKalmanEventInterface *ev = (NicaMpdDstKalmanEventInterface*)s;
	fMpdInterface->CopyAndCompress(ev->fMpdInterface,map,map_size);
	fKalmanInterface->CopyAndCompress(ev->fKalmanInterface,map,map_size);
}

void NicaMpdDstKalmanEventInterface::ConnectToTree() {
	fMpdInterface->LinkWithTree(NicaEventInterface::ModeRead());
	fKalmanInterface->LinkWithTree(NicaEventInterface::ModeRead());
	fEvent = fMpdInterface->fEvent;
	fKalmanTracks = fKalmanInterface->fTracks;
}

void NicaMpdDstKalmanEventInterface::FillTrackInterface(NicaTrackInterface* track,
		Int_t index) {
	NicaMpdDstKalmanTrackInterface *tr = (NicaMpdDstKalmanTrackInterface*)track;
	switch(fMode){
	case kAllTracks:{
		if(index<fEvent->GetEventInfoNofPrimaryTracks()){
			tr->SetRawTrack( fEvent->GetPrimaryTracks()->UncheckedAt(index),NULL);
		}else{
			index = index -fEvent->GetEventInfoNofPrimaryTracks();
			tr->SetRawTrack( fEvent->GetPrimaryTracks()->UncheckedAt(index),fKalmanTracks->UncheckedAt(index));
		}
	}break;
	case kPrimaryTracks:
		tr->SetRawTrack( fEvent->GetPrimaryTracks()->UncheckedAt(index),NULL);
		break;
	case kGlobalTracks:
		tr->SetRawTrack(fEvent->GetGlobalTracks()->UncheckedAt(index),fKalmanTracks->UncheckedAt(index));
		break;
	}
}

Bool_t NicaMpdDstKalmanEventInterface::ExistInTree() const {
	if(fMpdInterface->ExistInTree()&&fKalmanInterface->ExistInTree()){
		return kTRUE;
	}
	return kFALSE;
}

Int_t NicaMpdDstKalmanEventInterface::GetTotalTrackNo() const {
	return fMpdInterface->GetTotalTrackNo();
}

NicaTrackInterface* NicaMpdDstKalmanEventInterface::GetTrackInterface() const {
	return new NicaMpdDstKalmanTrackInterface();
}

TObject* NicaMpdDstKalmanEventInterface::GetRawTrackPointer(Int_t index) const {
	return fMpdInterface->GetRawTrackPointer(index);
}

void NicaMpdDstKalmanEventInterface::SetRunInfoId(Int_t i) {
	fMpdInterface->SetRunInfoId(i);
}

Int_t NicaMpdDstKalmanEventInterface::GetRunInfoId() const {
	return fEvent->GetRunInfoRunId();
}

void NicaMpdDstKalmanEventInterface::SetMagneticField(TVector3 mag) {
	fEvent->SetRunInfoMagneticFieldZ(mag.Z());
}

TVector3 NicaMpdDstKalmanEventInterface::GetMagneticField() const {
	Double_t mz =  fEvent->GetRunInfoMagneticFieldZ();
	return TVector3(0,0,mz);
}

TLorentzVector NicaMpdDstKalmanEventInterface::GetVertexError() const {
	TLorentzVector vx;
	vx.SetXYZT(fEvent->GetPrimaryVerticesXerr(),fEvent->GetPrimaryVerticesYerr(),fEvent->GetPrimaryVerticesZerr(),0);
	return vx;
}

TLorentzVector NicaMpdDstKalmanEventInterface::GetVertex() const {
	TLorentzVector v;
	v.SetXYZT(fEvent->GetPrimaryVerticesX(), fEvent->GetPrimaryVerticesY(), fEvent->GetPrimaryVerticesZ(),0);
	return v;
}

void NicaMpdDstKalmanEventInterface::OnlyPrimary() {
	fMode = kPrimaryTracks;
	fMpdInterface->OnlyPrimary();
}

void NicaMpdDstKalmanEventInterface::OnlyGlobal() {
	fMode = kGlobalTracks;
	fMpdInterface->OnlyGlobal();
}

NicaMpdDstKalmanEventInterface::~NicaMpdDstKalmanEventInterface() {
	delete fMpdInterface;
	delete fKalmanInterface;
}

