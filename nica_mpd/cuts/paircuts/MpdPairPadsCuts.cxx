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
	Double_t Z1  = track1->GetEvent()->GetVertex()->Z();
	Double_t Z2 = track1->GetEvent()->GetVertex()->Z();
	for(int i=0;i<minHits;i++){
		Double_t phi1 = track1->GetPhi(i);
		Double_t phi2 = track2->GetPhi(i);
		Double_t phi = TVector2::Phi_mpi_pi(phi1-phi2);
		Double_t aphi = TMath::Abs(phi);
		Double_t R = track1->GetR(i);
		Double_t x1 = R*TMath::Cos(phi1);
		Double_t y1 = R*TMath::Sin(phi1);
		Double_t x2 = R*TMath::Cos(phi2);
		Double_t y2 = R*TMath::Sin(phi2);
		Double_t z1 = track1->GetZ(i)-Z1;
		Double_t z2 = track2->GetZ(i)-Z2;
		Double_t dst2 =(x1-x2)*(x1-x2)+
				(y1-y2)*(y1-y2) + (z1-z2)*(z1-z2);
		if(i==0)
			entrance = dst2;
		av_sep+= dst2;
		minDist = TMath::Min(minDist,dst2);
		if(track1->GetPadID(i)==track2->GetPadID(i)&&track1->GetPadID(i)!=-1)
			sharedPads++;
		if(aphi<TMath::Abs(minDeltaPhi)){
			minDeltaPhi = phi;
		}
	}
	SetValue(av_sep/minHits,AverageSep());
	SetValue(sharedPads/minHits,SharedPads());
	SetValue(minDeltaPhi,MinDeltaPhiStar());
	SetValue(TMath::Sqrt(entrance),TPCEntranceDist());
	SetValue(TMath::Sqrt(minDist),MinTPCSep());
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

