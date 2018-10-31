/*
 * MpdPairKalmanPadsCuts.cxx
 *
 *  Created on: 19 lip 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdPairKalmanPadsCuts.h"
#include "NicaMpdTrackTpcPads.h"
#include "NicaTpcTrack.h"
#include <bitset>
MpdPairKalmanPadsCuts::MpdPairKalmanPadsCuts():NicaTwoTrackCut(9) {
	SetUnitName("Av TPC Sep [cm]",AverageSep());
	SetUnitName("Shared Pads[%]",SharedPads());
	SetUnitName("Min#Delta#Phi^* [cm]",MinDeltaPhiStar());
	SetUnitName("TPC Entrace Dist [cm]",TPCEntranceDist());
	SetUnitName("Min TPC dist Cut [cm]",MinTPCSep());
	SetUnitName("Pair Quality [AU]",HitQuality());
	SetUnitName("Hits shared [AU]",HitShared());
	SetUnitName("HitQualityDist [%]",HitQualityDistance());
	SetUnitName("HitQualityLayer [%]", HitQualityLayer());
	SetMinMax(0, 1E+6, AverageSep());
	SetMinMax(0,1E+6,SharedPads());
	SetMinMax(0.1,-0.1,MinDeltaPhiStar());
	SetMinMax(0,1E+6,TPCEntranceDist());
	SetMinMax(0,1E+4,MinTPCSep());
	SetMinMax(-2,2,HitQuality());
	SetMinMax(0,1, HitShared());
	SetMinMax(-1,1,HitQualityDistance());
	SetMinMax(-1,1,HitQualityLayer());
	fQualityThreshold2 = 0;
}

Bool_t MpdPairKalmanPadsCuts::Pass(NicaTwoTrack* pair) {
	Double_t av_sep = 0;
	NicaMpdTrackTpcPads *track1 = (NicaMpdTrackTpcPads*)pair->GetTrack1();
	NicaMpdTrackTpcPads *track2 = (NicaMpdTrackTpcPads*)pair->GetTrack2();
	track1->CalculatePads();
	track2->CalculatePads();
	const Double_t hits1 = ((NicaTpcTrack*)track1->GetDetTrack(NicaDetectorID::kTPC))->GetNHits();
	const Double_t hits2 = ((NicaTpcTrack*)track2->GetDetTrack(NicaDetectorID::kTPC))->GetNHits();
	const Double_t pads1 = track1->GetPadsNo();
	const Double_t pads2 = track2->GetPadsNo();

	const Int_t start1_pad = track1->GetFirstGoodPad();
	const Int_t start2_pad = track2->GetFirstGoodPad();

	const Int_t	end1_pad = track1->GetFirstBadPad();
	const Int_t end2_pad = track2->GetFirstBadPad();

	Int_t first_common_pad = TMath::Max(start1_pad,start2_pad);
	Int_t last_common_pad = TMath::Min(end1_pad,end2_pad);
	if(last_common_pad==-1){// no pads
		SetValue(1E+9,AverageSep()); //very large av sep
		SetValue(0,SharedPads());// no shared pads
		SetValue(TMath::Pi(),MinDeltaPhiStar());//180 degree
		SetValue(1E+9,TPCEntranceDist());//max tpc entrance
		SetValue(1E+0,MinTPCSep());//max min
		Double_t tot_hits = hits1+hits2;
		SetValue(0, HitQuality());
		SetValue(0, HitQualityDistance());
		SetValue(0, HitQualityLayer());
		SetValue(0,HitShared());
		return ForcedUpdate(kFALSE);// reject one of this track is not real tpc track
	}
	Double_t maxPads  = TMath::Max(end1_pad,end2_pad)-TMath::Min(start1_pad,start2_pad);

	Int_t raw_hits = hits1+hits2;
	Double_t shared_hits = 0;
	ULong64_t layer1 = track1->GetHitMap();
	ULong64_t layer2 = track2->GetHitMap();


	Double_t commonPads = last_common_pad -first_common_pad;
	Double_t sharedPads =0;
	Double_t entrance =0;
	ULong64_t bit_shared1 = track1->GetSharedHitMap();
	ULong64_t bit_shared2 = track2->GetSharedHitMap();

	Double_t minDeltaPhi = 1E+9;
	Double_t minDist = 1E+9;

	Double_t X1 = track1->GetEvent()->GetVertex()->X();
	Double_t X2 = track2->GetEvent()->GetVertex()->X();
	Double_t Y1 = track1->GetEvent()->GetVertex()->Y();
	Double_t Y2 = track2->GetEvent()->GetVertex()->Y();
	Double_t Z1 = track1->GetEvent()->GetVertex()->Z();
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



	Double_t quality[3] ={0,0,0};
	for(int i=first_common_pad;i<last_common_pad;i++){

		Int_t hit_1 =  TESTBIT(layer1,i);
		Int_t hit_2 = TESTBIT(layer2,i);
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

//		Double_t real_dst = (x1-x2)*(x1-x2)+
//				(y1-y2)*(y1-y2) + (z1-z2)*(z1-z2);
		x1 = x1-X1;
		x2 = x2-X2;
		y1 = y1-Y1;
		y2 = y2-Y2;
		z1 = z1-Z1;
		z2 = z2-Z2;

		Double_t phi1 = TMath::ATan2(y1, x1);
		Double_t phi2 = TMath::ATan2(y2, x2);
		Double_t phi = TVector2::Phi_mpi_pi(phi1-phi2);
	//	Double_t aphi = TMath::Abs(phi);

		Double_t dst = TMath::Sqrt((x1-x2)*(x1-x2)+
				(y1-y2)*(y1-y2) + (z1-z2)*(z1-z2));
		if(i==first_common_pad){
			entrance = dst;
		}
		Bool_t shared_pad = kFALSE;
		if(track1->GetPadID(i)==track2->GetPadID(i)&&track1->GetPadID(i)!=-1)
			shared_pad = kTRUE;
		if(shared_pad)
			sharedPads++;
		/* calcuate values for threshod quality cut*/
		Int_t sum1 = hit_1 + hit_2;// shared hit = shared pad
		Int_t sum2 = sum1; //shared hit = small distance


		av_sep+= dst;
		minDist = TMath::Min(minDist,dst);

		if(TMath::Abs(phi)<TMath::Abs(minDeltaPhi)){
			minDeltaPhi = phi;
		}

		if(sum1==2){//two separate hits
			if(dst<fQualityThreshold2)
				sum2  = 1; // hit is shared
			if(shared_pad){
				sum1 = 1;
				shared_hits++;
			}
		}
		switch(sum1){
		case 1:
			++quality[0];
			break;
		case 2:
			--quality[0];
			break;
		}

		switch(sum2){
		case 1:
			++quality[1];
			break;
		case 2:
			--quality[1];
			break;
		}

	}
	for(int i=0;i<53;i++){
		Int_t hit_1 =  TESTBIT(layer1,i);
		Int_t hit_2 = TESTBIT(layer2,i);
		Int_t sum3 = hit_1+hit_2;//shared hit = shared layer
		switch(sum3){
		case 1:
			++quality[2];
			break;
		case 2:
			--quality[2];
			break;
		}
	}


//	std::cout<<shared_hits/53<<std::endl;
	SetValue(av_sep/commonPads,AverageSep());
	SetValue(sharedPads/maxPads,SharedPads());
	SetValue(minDeltaPhi,MinDeltaPhiStar());
	SetValue(entrance,TPCEntranceDist());
	SetValue(minDist,MinTPCSep());
	Double_t tot_hits = hits1+hits2;
	SetValue(quality[0]/commonPads, HitQuality());
	SetValue(quality[1]/commonPads, HitQualityDistance());
	SetValue(quality[2]/tot_hits, HitQualityLayer());
	SetValue(shared_hits/tot_hits,HitShared());
	if(!InLimits(AverageSep()))return ForcedUpdate(kFALSE);
	if(!InLimits(SharedPads()))return ForcedUpdate(kFALSE);
	if(!OutLimits(MinDeltaPhiStar()))return ForcedUpdate(kFALSE);
	if(!InLimits(TPCEntranceDist()))return ForcedUpdate(kFALSE);
	if(!InLimits(MinTPCSep()))return ForcedUpdate(kFALSE);
	if(!InLimits(HitQuality())) return ForcedUpdate(kFALSE);
	if(!InLimits(HitShared())) return ForcedUpdate(kFALSE);
	if(!InLimits(HitQualityDistance())) return ForcedUpdate(kFALSE);
	if(!InLimits(HitQualityLayer())) return ForcedUpdate(kFALSE);
	return ForcedUpdate(kTRUE);
}

Bool_t MpdPairKalmanPadsCuts::Init(Int_t task_id) {
	return kTRUE;
	return FormatEquals("NicaMpdDstKalmanEvent", task_id);
}

MpdPairKalmanPadsCuts::~MpdPairKalmanPadsCuts() {
}

MpdPairKalmanPadsCuts::MpdPairKalmanPadsCuts(
		const MpdPairKalmanPadsCuts& other) :NicaTwoTrackCut(other),
				fQualityThreshold2(other.fQualityThreshold2){
}

MpdPairKalmanPadsCuts& MpdPairKalmanPadsCuts::operator =(
		const MpdPairKalmanPadsCuts& other) {
	if(this !=&other){
		NicaTwoTrackCut::operator=(other);
		fQualityThreshold2 = other.fQualityThreshold2;
	}
	return *this;
}

NicaPackage* MpdPairKalmanPadsCuts::Report() const {
	NicaPackage *report=  NicaTwoTrackCut::Report();
	report->AddObject(new NicaParameterDouble("Hit distance",TMath::Sqrt(fQualityThreshold2)));
	return report;
}
