/*
 * NicaMpdTrack.cxx
 *
 *  Created on: 28 mar 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdTrack.h"
#include "NicaDetectorID.h"
#include <iostream>
#include <Rtypes.h>
NicaMpdTrack::NicaMpdTrack() :NicaExpTrack(){
	fFirstPoint = new TVector3();
	fLastPoint = new TVector3();
	fTpcTrack = new NicaTpcTrack();
	fToFTrack = new NicaToFTrack();
	fHitsMap = 0;
}

NicaMpdTrack::~NicaMpdTrack() {
	delete fFirstPoint;
	delete fLastPoint;
	delete fTpcTrack;
	delete fToFTrack;
}

void NicaMpdTrack::Update(MpdTrack* track) {
	GetMomentum()->SetPxPyPzE(track->GetPx(),track->GetPy(),track->GetPz(),0);
	SetID(track->GetID());
	if(track->GetPt()>0)
		SetCharge(-1);
	else
		SetCharge(1);
	SetNHits(track->GetNofHits());
	fTpcTrack->SetNHits(track->GetNofHits());//! FIXME
	fTpcTrack->SetPidProb(track->GetTPCPidProbPion(),track->GetTPCPidProbKaon(),track->GetTPCPidProbProton(),track->GetTPCPidProbElectron());
	SetPidProb(track->GetPidProbPion(),track->GetPidProbKaon(),track->GetPidProbProton(),track->GetPidProbProton());
	fToFTrack->SetBeta(track->GetTofBeta());
	fToFTrack->SetMass2(track->GetTofMass2());
	fTpcTrack->SetDeDx(track->GetdEdXTPC());
	fToFTrack->SetFlag(track->GetTofFlag());
	fTpcTrack->SetSigma(track->GetNSigmaPion(),track->GetNSigmaKaon(),track->GetNSigmaProton(),track->GetNSigmaProton());
	SetChi2(track->GetChi2());
	GetDCA()->SetXYZ(track->GetDCAGlobalX(), track->GetDCAGlobalY(),track->GetDCAGlobalZ());
	fFirstPoint->SetXYZ(track->GetFirstPointX(),track->GetFirstPointY(), track->GetFirstPointZ());
	fLastPoint->SetXYZ(track->GetLastPointX(),track->GetLastPointY(),track->GetLastPointZ());
	NicaHelix *helix = GetHelix();
	TVector3 mom = GetMomentum()->Vect();
	helix->SetParams(*fFirstPoint,mom, fCharge);
}

void NicaMpdTrack::CopyData(NicaTrack* other) {
	NicaMpdTrack *track = (NicaMpdTrack*)other;
	*fFirstPoint = *track->fFirstPoint;
	*fLastPoint = *track->fLastPoint;
	*fTpcTrack = *track->fTpcTrack;
	*fToFTrack = *track->fToFTrack;
	NicaExpTrack::CopyData(track);
}

NicaMpdTrack::NicaMpdTrack(const NicaMpdTrack& other) :NicaExpTrack(other){
	fFirstPoint = new TVector3(*other.fFirstPoint);
	fLastPoint  = new TVector3(*other.fLastPoint);
	fTpcTrack = new NicaTpcTrack(*other.fTpcTrack);
	fToFTrack = new NicaToFTrack(*other.fToFTrack);
	fHitsMap = other.fHitsMap;
}

NicaMpdTrack& NicaMpdTrack::operator =(const NicaMpdTrack& other) {
	if(this==&other) return *this;
	NicaExpTrack::operator=(other);
	*fFirstPoint = *other.fFirstPoint;
	*fLastPoint  = *other.fLastPoint;
	*fTpcTrack = *other.fTpcTrack;
	*fToFTrack = *other.fToFTrack;
	return *this;
}

void NicaMpdTrack::PrintHitMaps() const {
	for(int i=0;i<52;i++){
		Bool_t bit = TESTBIT(fHitsMap,i);
		if(bit){
			std::cout<<'1';
		}else{
			std::cout<<'0';
		}
	}
	std::cout<<std::endl;
}

TObject* NicaMpdTrack::GetDetTrack(const UInt_t detID) const {
	switch(detID){
	case NicaDetectorID::kTPC:{
		return fTpcTrack;
	}break;
	case NicaDetectorID::kTOF:{
		return fToFTrack;
	}break;
	default:
		return NULL;
		break;
	}
}
