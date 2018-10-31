/*
 * NicaMpdTrackTpcPads.cxx
 *
 *  Created on: 2 lip 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdTrackTpcPads.h"
#include "TLorentzVector.h"
#include "NicaEvent.h"

NicaMpdTrackTpcPads::NicaMpdTrackTpcPads() {
	fSec = NicaTpcSectorGeo::Instance();
	fPadsNo[0] = -2;
	fPadsNo[1] = -2;
	for(int i=0;i<GetMaxPadsNo();i++){
		fPaths[i] =0;
		fPadID[i] =0;
	}
}

void NicaMpdTrackTpcPads::Update(MpdTrack* track) {
	NicaMpdTrack::Update(track);
#ifdef NICAMPDTRACKTPCPADS_CALCULATEMPDPAD_ON_DEMAND
	fPadsNo[0] = -2;
	fPadsNo[1] = -2;
#else
	fPadsNo[0] = -2;
	fPadsNo[1] = -2;
	CalculatePads();
#endif
}

NicaMpdTrackTpcPads::~NicaMpdTrackTpcPads() {
	fSec = NULL;
}

NicaMpdTrackTpcPads::NicaMpdTrackTpcPads(const NicaMpdTrackTpcPads& other):
	NicaMpdTrack(other)
		{
	fPadsNo[0] = other.fPadsNo[0];
	fPadsNo[1] = other.fPadsNo[1];
	fSec = other.fSec;
	for(int i=fPadsNo[0];i<fPadsNo[1];i++){
		fPaths[i] = other.fPaths[i];
		fPadID[i] = other.fPadID[i];
	}
}

NicaMpdTrackTpcPads& NicaMpdTrackTpcPads::operator =(
		const NicaMpdTrackTpcPads& other) {
	if(this!=&other){
		NicaMpdTrack::operator=(other);
		fPadsNo[0] = other.fPadsNo[0];
		fPadsNo[1] = other.fPadsNo[1];
		for(int i=fPadsNo[0];i<fPadsNo[1];i++){
			fPaths[i] = other.fPaths[i];
			fPadID[i] = other.fPadID[i];
		}
	}
	return *this;
}

Float_t NicaMpdTrackTpcPads::GetR(Int_t lay) const {
	Double_t R  =0;
	Double_t L = lay;
	if(lay<fSec->NofRowsReg(0)){
		return  fSec->GetMinY()+fSec->PadHeight(0)*(L+0.5);
	}else{
		return fSec->GetRocY(1) +fSec->PadHeight(1)*(L-fSec->NofRowsReg(0)+0.5);
	}
}

void NicaMpdTrackTpcPads::CopyData(NicaTrack* other) {
	NicaMpdTrack::CopyData(other);
	NicaMpdTrackTpcPads *track = (NicaMpdTrackTpcPads*)other;
	fPadsNo[0] = track->fPadsNo[0];
	fPadsNo[1] = track->fPadsNo[1];
	for(int i=fPadsNo[0];i<fPadsNo[1];i++){
		fPaths[i] = track->fPaths[i];
		fPadID[i] = track->fPadID[i];
	}
}

Float_t NicaMpdTrackTpcPads::GetPhi(Int_t lay) const {
	return GetHelix()->Evaluate(fPaths[lay]).Phi();
}

Float_t NicaMpdTrackTpcPads::GetZ(Int_t lay) const {
	return GetHelix()->Evaluate(fPaths[lay]).Z();
}

void NicaMpdTrackTpcPads::CalculatePads() {
	if(PadsCalculated())
		return;//dont repeat calculations if pads calculated
	 fSec->CalculatePads(this->GetHelix(), fPaths,fPadsNo);
	 for(int i=fPadsNo[0];i<fPadsNo[1];i++){
		TVector3 glob = GetHelix()->Evaluate(fPaths[i]);
		TVector3 loc;
		fPadID[i] =  fSec->Global2Local(glob,loc,-1);
	}
	for(int i=0;i<fPadsNo[0];i++){// fill other layers
		fPadID[i] = -1;
	}
	for(int i=fPadsNo[1];i<53;i++){
		fPadID[i] = -1;
	}
}

Bool_t NicaMpdTrackTpcPads::PadsCalculated() const {
	if(fPadsNo[0]==-2) return kFALSE;
	return kTRUE;
}
