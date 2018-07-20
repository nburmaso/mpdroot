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
		fPhiPads[i] =0;
		fZPads[i] =0;
	}
}

void NicaMpdTrackTpcPads::Update(MpdTrack* track) {
	NicaMpdTrack::Update(track);
	if(fSec==NULL){
		fSec = NicaTpcSectorGeo::Instance();
	}
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
			return;
		}
		glob = GetHelix()->Evaluate(s);
		fZPads[lay] = glob.Z();
		glob.SetZ(0);
		fPadID[lay] =  fSec->Global2Local(glob,loc,-1);
		fPhiPads[lay] = glob.Phi();
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
		fZPads[lay] = glob.Z();
		glob.SetZ(0);
		fPadID[lay] =  fSec->Global2Local(glob,loc,-1);
		fPhiPads[lay] = glob.Phi();
		lay++;
	}
	fPadsNo = lay;
}

NicaMpdTrackTpcPads::~NicaMpdTrackTpcPads() {
	fSec = NULL;
}

NicaMpdTrackTpcPads::NicaMpdTrackTpcPads(const NicaMpdTrackTpcPads& other):
	NicaMpdTrack(other)
		{
	fPadsNo = other.fPadsNo;
	for(int i=0;i<fPadsNo;i++){
		fPhiPads[i] = other.fPhiPads[i];
		fZPads[i] = other.fZPads[i];
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
			fPhiPads[i] = other.fPhiPads[i];
			fPadID[i] = other.fPadID[i];
			fZPads[i] = other.fZPads[i];
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
		fPhiPads[i] = track->fPhiPads[i];
		fZPads[i] = track->fZPads[i];
		fPadID[i] = track->fPadID[i];
		if(track->fSec)
			fSec = track->fSec;
	}
}
