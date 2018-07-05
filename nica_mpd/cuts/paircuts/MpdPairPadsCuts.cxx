/*
 * MpdPairPadsCuts.cxx
 *
 *  Created on: 2 lip 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdPairPadsCuts.h"
#include "NicaMpdTrackTpcPads.h"

MpdPairPadsCuts::MpdPairPadsCuts():NicaTwoTrackCut(5) {
	SetUnitName("Av TPC Sep [cm]",AverageSep());
	SetUnitName("Shared Pads[%]",SharedPads());
	SetUnitName("Min#Delta#Phi^* [cm]",MinDeltaPhiStar());
	SetUnitName("TPC Entrace Dist [cm]",TPCEntranceDist());
	SetUnitName("Min TPC dist Cut [cm]",MinTPCSep());
}

Bool_t MpdPairPadsCuts::Pass(NicaTwoTrack* pair) {
	Double_t av_sep = 0;
	NicaMpdTrackTpcPads *track1 = (NicaMpdTrackTpcPads*)pair->GetTrack1();
	NicaMpdTrackTpcPads *track2 = (NicaMpdTrackTpcPads*)pair->GetTrack2();
	const Double_t hits1 = track1->GetPadsNo();
	const Double_t hits2 = track2->GetPadsNo();
	Double_t minHits = TMath::Min(hits1,hits2);
	Double_t maxHits = TMath::Max(hits1,hits2);
	Double_t sharedPads =0;
	Double_t entrance =0;
	Double_t minDeltaPhi = 1E+9;
	Double_t minDist = 1E+9;
	for(int i=0;i<minHits;i++){
		Double_t phi1 = track1->GetPhi(i);
		Double_t phi2 = track2->GetPhi(i);
		Double_t phi = TVector2::Phi_mpi_pi(phi1-phi2);
		Double_t aphi = TMath::Abs(phi);
		Double_t R = track1->GetR(i);
		Double_t dst = R*aphi;
		if(i==0)
			entrance = dst;
		av_sep+= dst;
		minDist = TMath::Min(minDist,dst);
		if(track1->GetPadID(i)==track2->GetPadID(i)&&track1->GetPadID(i)!=-1)
			sharedPads++;
		if(aphi<TMath::Abs(minDeltaPhi)){
			minDeltaPhi = phi;
		}
	}

	SetValue(av_sep/minHits,AverageSep());
	SetValue(sharedPads/minHits,SharedPads());
	SetValue(minDeltaPhi,MinDeltaPhiStar());
	SetValue(entrance,TPCEntranceDist());
	SetValue(minDist,MinTPCSep());
	//std::cout<<minHits<<" "<<maxHits<<std::endl;
	//for(int i=0;i<GetCutSize();i++)
	//		std::cout<<"\t"<<GetValue(i)<<std::endl;
	if(!InLimits(AverageSep()))return ForcedUpdate(kFALSE);
	if(!InLimits(SharedPads()))return ForcedUpdate(kFALSE);
	if(!OutLimits(MinDeltaPhiStar()))return ForcedUpdate(kFALSE);
	if(!InLimits(TPCEntranceDist()))return ForcedUpdate(kFALSE);
	if(!InLimits(MinTPCSep()))return ForcedUpdate(kFALSE);
	return ForcedUpdate(kTRUE);
}

Bool_t MpdPairPadsCuts::Init(Int_t task_id) {
	return FormatEquals("NicaMpdEventTpcPads", task_id);
}

MpdPairPadsCuts::~MpdPairPadsCuts() {
	// TODO Auto-generated destructor stub
}

