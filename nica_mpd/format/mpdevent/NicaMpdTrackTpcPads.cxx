/*
 * NicaMpdTrackTpcPads.cxx
 *
 *  Created on: 2 lip 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdTrackTpcPads.h"

NicaMpdTrackTpcPads::NicaMpdTrackTpcPads() {
	fMaxPads = 53;
	fSec = NULL;
	fPadsNo = 0;
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
		if(s==NicaHelix::MaxPath()){
			fPadID[lay] = -1;
			fPadsNo = lay;
			return;
		}
		glob = GetHelix()->Evaluate(s);
		glob.SetZ(0);
		fPadID[lay] =  fSec->Global2Local(glob,loc,-1);
		fRPads[lay] = R;
		fPhiPads[lay] = glob.Phi();
		lay++;
	}
	R_min = fSec->GetRocY(1);
	for(double i =0;i<fSec->NofRowsReg(1);i++){
		R = R_min + i*padH2+0.5*padH2;
		GetHelix()->PathLength(R, s1, s2);
		s = TMath::Min(s1,s2);
		if(s==NicaHelix::MaxPath()){
			fPadID[lay] = -1;
			fPadsNo = lay+1;
			return;
		}
		glob = GetHelix()->Evaluate(s);
		glob.SetZ(0);
		fPadID[lay] =  fSec->Global2Local(glob,loc,-1);
		fRPads[lay] = R;
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
		fRPads[i] = other.fRPads[i];
		fPhiPads[i] = other.fPhiPads[i];
		fPadID[i] = other.fPadID[i];
		fLayersNo[i] = other.fLayersNo[i];
		fMaxPads = other.fMaxPads;
		fSec = other.fSec;
	}
}
