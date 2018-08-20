/*
 * MpdPairKalmanPadsCuts.cxx
 *
 *  Created on: 19 lip 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdPairKalmanPadsCuts.h"
#include "NicaMpdDstKalmanTrack.h"
#include <bitset>
MpdPairKalmanPadsCuts::MpdPairKalmanPadsCuts():NicaTwoTrackCut(7) {
	SetUnitName("Av TPC Sep [cm]",AverageSep());
	SetUnitName("Shared Pads[%]",SharedPads());
	SetUnitName("Min#Delta#Phi^* [cm]",MinDeltaPhiStar());
	SetUnitName("TPC Entrace Dist [cm]",TPCEntranceDist());
	SetUnitName("Min TPC dist Cut [cm]",MinTPCSep());
	SetUnitName("Pair Qualityt [AU]",HitQuality());
	SetUnitName("Hits shared [AU]",HitShared());
	SetMinMax(0, 1E+4, AverageSep());
	SetMinMax(0,1,SharedPads());
	SetMinMax(0.1,-0.1,MinDeltaPhiStar());
	SetMinMax(0,1E+6,TPCEntranceDist());
	SetMinMax(0,1E+4,MinTPCSep());
	SetMinMax(-2,2,HitQuality());
	SetMinMax(0,1, HitShared());
}

Bool_t MpdPairKalmanPadsCuts::Pass(NicaTwoTrack* pair) {
	Double_t av_sep = 0;
	NicaMpdDstKalmanTrack *track1 = (NicaMpdDstKalmanTrack*)pair->GetTrack1();
	NicaMpdDstKalmanTrack *track2 = (NicaMpdDstKalmanTrack*)pair->GetTrack2();
	const Double_t hits1 = track1->GetPadsNo();
	const Double_t hits2 = track2->GetPadsNo();
	Double_t minHits = TMath::Min(hits1,hits2);
	Double_t maxHits = TMath::Max(hits1,hits2);
	Double_t sharedPads =0;
	Double_t entrance =0;
	Double_t minDeltaPhi = 1E+9;
	Double_t minDist = 1E+9;
	Double_t Z1  = track1->GetEvent()->GetVertex()->Z();
	Double_t Z2 = track2->GetEvent()->GetVertex()->Z();
	Double_t mCosDipAngle1 = TMath::Cos(track1->GetHelix()->GetDipAngle());
	Double_t mCosDipAngle2 = TMath::Cos(track2->GetHelix()->GetDipAngle());
	Double_t mSinDipAngle1 = TMath::Sin(track1->GetHelix()->GetDipAngle());
	Double_t mSinDipAngle2 = TMath::Sin(track2->GetHelix()->GetDipAngle());
	Double_t mCosPhase1 = TMath::Cos(track1->GetHelix()->GetPhi0());
	Double_t mCosPhase2 = TMath::Cos(track2->GetHelix()->GetPhi0());
	Double_t mSinPhase1 = TMath::Sin(track1->GetHelix()->GetPhi0());
	Double_t mSinPhase2 = TMath::Sin(track2->GetHelix()->GetPhi0());
	NicaHelix *helix1 = track1->GetHelix();
	NicaHelix *helix2 = track2->GetHelix();

	for(int i=0;i<minHits;i++){
		Double_t s1 = track1->GetPathAt(i);
		/* manual calcuations instead of helix, speed */
		Double_t x1 = helix1->GetStartX()
				+ (TMath::Cos(helix1->GetPhi0() + s1 * helix1->GetH() * helix1->GetCurv() * mCosDipAngle1)
						- mCosPhase1) / helix1->GetCurv();
		Double_t y1 = helix1->GetStartY()
				+ (TMath::Sin(helix1->GetPhi0() + s1 * helix1->GetH() *  helix1->GetCurv() * mCosDipAngle1)
						- mSinPhase1) /  helix1->GetCurv();
		Double_t z1 =  helix1->GetStartZ() + s1 * mSinDipAngle1;
		Double_t s2 = track2->GetPathAt(i);
		Double_t x2 = helix2->GetStartX()
				+ (TMath::Cos(helix2->GetPhi0() + s2 * helix2->GetH() * helix2->GetCurv() * mCosDipAngle2)
						- mCosPhase2) / helix2->GetCurv();
		Double_t y2 = helix2->GetStartY()
				+ (TMath::Sin(helix2->GetPhi0() + s2 * helix2->GetH() *  helix2->GetCurv() * mCosDipAngle2)
						- mSinPhase2) /  helix2->GetCurv();
		Double_t z2 =  helix2->GetStartZ() + s2 * mSinDipAngle2;
		//-------
		Double_t phi1 = TMath::ATan2(y1, x1);
		Double_t phi2 = TMath::ATan2(y2, x2);

		Double_t phi = TVector2::Phi_mpi_pi(phi1-phi2);
		Double_t aphi = TMath::Abs(phi);
		Double_t R = track1->GetR(i);
		z1 = z1-Z1;
		z2 = z2-Z2;
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
	Double_t quality =0;
	Int_t raw_hits = track1->GetKalmanHits()+track2->GetKalmanHits();
	Double_t shared_hits = 0;
	ULong64_t bits1 = track1->GetBitMap();
	ULong64_t bits2 = track2->GetBitMap();
//	std::cout<<std::bitset<32>(bits1)<<" "<<std::bitset<32>(bits2)<<std::endl;
	shared_hits = 0;
	ULong64_t bit_shared1 = track1->GetBitMapSHared();
	ULong64_t bit_shared2 = track2->GetBitMapSHared();
	for(int i=0;i<53;i++){
		Int_t hit_1 = track1->LayerStatus(i);
		Int_t hit_2 = track2->LayerStatus(i);
		Int_t sum = hit_1+hit_2;
		Int_t shared = track1->LayerShared(i)*track2->LayerShared(i);
		if(sum==2){
			if(track1->GetPadID(i)==track2->GetPadID(i)&&track1->GetPadID(i)!=-1){
				sum = 1;
				shared_hits++;
			}
		}
	//	shared_hits +=  shared;
		if(sum==2){
			//shared pads
			if(track1->GetPadID(i)==track2->GetPadID(i)){
				shared_hits++;
				sum = 1;
			}

		}
		switch(sum){
		case 0:
			// nothing
			break;
		case 1:
			quality++;
			break;
		case 2:
			quality--;
			break;
		}
	}
//	std::cout<<shared_hits/53<<std::endl;
	SetValue(TMath::Sqrt(av_sep)/minHits,AverageSep());
	SetValue(sharedPads/maxHits,SharedPads());
	SetValue(minDeltaPhi,MinDeltaPhiStar());
	SetValue(TMath::Sqrt(entrance),TPCEntranceDist());
	SetValue(TMath::Sqrt(minDist),MinTPCSep());
	SetValue(quality/53, HitQuality());
	SetValue(shared_hits/53,HitShared());
//	for(int i=0;i<GetCutSize();i++)
//			std::cout<<"\t"<<GetValue(i)<<std::endl;
	if(!InLimits(AverageSep()))return ForcedUpdate(kFALSE);
	if(!InLimits(SharedPads()))return ForcedUpdate(kFALSE);
	if(!OutLimits(MinDeltaPhiStar()))return ForcedUpdate(kFALSE);
	if(!InLimits(TPCEntranceDist()))return ForcedUpdate(kFALSE);
	if(!InLimits(MinTPCSep()))return ForcedUpdate(kFALSE);
	if(!InLimits(HitQuality())) return ForcedUpdate(kFALSE);
	if(!InLimits(HitShared())) return ForcedUpdate(kFALSE);
	return ForcedUpdate(kTRUE);
}

Bool_t MpdPairKalmanPadsCuts::Init(Int_t task_id) {
	return FormatEquals("NicaMpdDstKalmanEvent", task_id);
}

MpdPairKalmanPadsCuts::~MpdPairKalmanPadsCuts() {
	// TODO Auto-generated destructor stub
}

