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
	fMaxPads = 53;
	fSec = NULL;
	fPadsNo = 0;
	for(int i=0;i<fMaxPads;i++){
		fPaths[i] =0;
		fPadID[i] =0;
	}
}

void NicaMpdTrackTpcPads::Update(MpdTrack* track) {
	NicaMpdTrack::Update(track);
	if(fSec==NULL){
		fSec = NicaTpcSectorGeo::Instance();
	}
	/* old version (cylindircal)
	Double_t R;
	Double_t R_min = fSec->GetMinY();
	Double_t padH1 = fSec->PadHeight(0);
	Double_t padH2 = fSec->PadHeight(1);
	Double_t s1,s2, s;
	Int_t lay = 0;
	fPadsNo = 0;
	TVector3 glob,loc;
	for(double i=0;i<fSec->NofRowsReg(0);i++){
		R = R_min + i*padH1+0.5*padH1;
		GetHelix()->PathLength(R, s1, s2);
		s = TMath::Min(s1,s2);
		if(s<0){
			s = TMath::Max(s1,s2);
		}
		if(s==NicaHelix::MaxPath()){
			fPadID[lay] = -1;
			fPadsNo = lay;
			s =0;
			return;
		}
		glob = GetHelix()->Evaluate(s);
		fPadID[lay] =  fSec->Global2Local(glob,loc,-1);
		fPaths[lay] = s;
		lay++;
	}
	R_min = fSec->GetRocY(1);
	for(double i =0;i<fSec->NofRowsReg(1);i++){
		R = R_min + i*padH2+0.5*padH2;
		GetHelix()->PathLength(R, s1, s2);
		s = TMath::Min(s1,s2);
		if(s<0)
			s = TMath::Max(s1,s2);
		if(s==NicaHelix::MaxPath()){
			fPadID[lay] = -1;
			fPadsNo = lay+1;
			return;
		}
		glob = GetHelix()->Evaluate(s);
		fPadID[lay] =  fSec->Global2Local(glob,loc,-1);
		fPaths[lay] = s;
		lay++;
	}
	fPadsNo = lay;
	*/
	// new modular geometry
	fPadsNo = fSec->CalculatePads(this->GetHelix(), fPaths);
	for(int i=0;i<fPadsNo;i++){
		TVector3 glob = GetHelix()->Evaluate(fPaths[i]);
		TVector3 loc;
		fPadID[i] =  fSec->Global2Local(glob,loc,-1);
	}
}

NicaMpdTrackTpcPads::~NicaMpdTrackTpcPads() {
	fSec = NULL;
}

NicaMpdTrackTpcPads::NicaMpdTrackTpcPads(const NicaMpdTrackTpcPads& other):
	NicaMpdTrack(other)
		{
	fPadsNo = other.fPadsNo;
	for(int i=0;i<fPadsNo;i++){
		fPaths[i] = other.fPaths[i];
		fPadID[i] = other.fPadID[i];
		fMaxPads = other.fMaxPads;
		fSec = other.fSec;
	}
}

NicaMpdTrackTpcPads& NicaMpdTrackTpcPads::operator =(
		const NicaMpdTrackTpcPads& other) {
	if(this!=&other){
		NicaMpdTrack::operator=(other);
		fPadsNo = other.fPadsNo;
		for(int i=0;i<fPadsNo;i++){
			fPaths[i] = other.fPaths[i];
			fPadID[i] = other.fPadID[i];
			fMaxPads = other.fMaxPads;
			fSec = other.fSec;
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
	fPadsNo = track->fPadsNo;
	for(int i=0;i<fPadsNo;i++){
		fPaths[i] = track->fPaths[i];
		fPadID[i] = track->fPadID[i];
		if(track->fSec)
			fSec = track->fSec;
	}
}

Float_t NicaMpdTrackTpcPads::GetPhi(Int_t lay) const {
	return GetHelix()->Evaluate(fPaths[lay]).Phi();
}

Float_t NicaMpdTrackTpcPads::GetZ(Int_t lay) const {
	return GetHelix()->Evaluate(fPaths[lay]).Z();
}
