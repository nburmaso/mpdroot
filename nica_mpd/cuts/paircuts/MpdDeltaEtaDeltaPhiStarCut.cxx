/*
 * MpdDeltaEtaDeltaPhiStarCut.cxx
 *
 *  Created on: 4 lip 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdDeltaEtaDeltaPhiStarCut.h"
#include "NicaMpdTrackTpcPads.h"

MpdDeltaEtaDeltaPhiStarCut::MpdDeltaEtaDeltaPhiStarCut() :NicaTwoTrackCut(3){
	SetUnitName("#Delta#eta", DeltaEta());
	SetUnitName("#Delta#phi^{*}",DeltaPhiStar());
	SetUnitName("#Delta#phi^{*}_{min}",DeltaPhiStarMin());
}

Bool_t MpdDeltaEtaDeltaPhiStarCut::Pass(NicaTwoTrack* pair) {
	NicaMpdTrackTpcPads *track1 = (NicaMpdTrackTpcPads*)pair->GetTrack1();
	NicaMpdTrackTpcPads *track2 = (NicaMpdTrackTpcPads*)pair->GetTrack2();
	const Double_t hits1 = track1->GetPadsNo();
	const Double_t hits2 = track2->GetPadsNo();
	Double_t minHits = TMath::Min(hits1,hits2);
	Double_t maxHits = TMath::Max(hits1,hits2);
	Double_t phi1, phi2;
	Double_t phi_star, phi_star_min;
	TLorentzVector *mom1 = track1->GetMomentum();
	SetValue(track1->GetMomentum()->Eta()-track2->GetMomentum()->Eta(),DeltaEta());
	phi_star_min = 1E+9;
	phi_star=0;
	for(int i=0;i<minHits;i++){
		if(track1->GetPadID(i)==-1) continue;
		if(track2->GetPadID(i)==-1) continue;
		phi1 = track1->GetPhi(i);
		phi2 = track2->GetPhi(i);
		Double_t dphi = TVector2::Phi_mpi_pi(phi1-phi2);
		if(TMath::Abs(dphi)<TMath::Abs(phi_star_min))
			phi_star_min = dphi;
		phi_star+=dphi;
	}
	SetValue(phi_star/minHits,DeltaPhiStar());
	SetValue(phi_star_min,DeltaPhiStarMin());
	if(InLimits(DeltaEta())&&InLimits(DeltaPhiStar())&&InLimits(DeltaPhiStarMin())){
		return ForcedUpdate(kFALSE);
	}
	return ForcedUpdate(kTRUE);
}

Bool_t MpdDeltaEtaDeltaPhiStarCut::Init(Int_t task_id) {
	return FormatEquals("NicaMpdEventTpcPads", task_id);
}

MpdDeltaEtaDeltaPhiStarCut::~MpdDeltaEtaDeltaPhiStarCut() {
	// TODO Auto-generated destructor stub
}

