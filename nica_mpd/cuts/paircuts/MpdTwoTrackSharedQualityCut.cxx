/*
 * MpdTwoTrackShareCut.cxx
 *
 *  Created on: 7 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdTwoTrackSharedQualityCut.h"
#include "NicaDataFormatManager.h"
#include "NicaComplexTrack.h"
#include "NicaMpdDstKalmanTrack.h"


MpdTwoTrackSharedQualityCut::MpdTwoTrackSharedQualityCut() : NicaTwoTrackCut(2){
	SetUnitName("Shared fraction [%]",0);
	SetUnitName("Quality [AU]",1);
}

Bool_t MpdTwoTrackSharedQualityCut::Init(Int_t format_id) {
	const NicaEvent *ev  = NicaDataFormatManager::Instance()->GetEventFromTree(format_id);
	if(ev->GetFormatName()=="MpdDstKalman") return kTRUE;
	if(ev->GetFormatName()=="MpdFullEvent") return kTRUE;
	return kFALSE;
 }

Bool_t MpdTwoTrackSharedQualityCut::Pass(NicaTwoTrack* pair) {
	NicaComplexTrack *tr1 = (NicaComplexTrack*)pair->GetTrack1();
	NicaComplexTrack *tr2 = (NicaComplexTrack*)pair->GetTrack2();
	NicaMpdDstKalmanTrack *track1 = (NicaMpdDstKalmanTrack*)tr1->GetRealTrack();
	NicaMpdDstKalmanTrack *track2 = (NicaMpdDstKalmanTrack*)tr2->GetRealTrack();
	//fMaxHits = track1->GetNHits()+track2->GetNHits();
	Int_t hits1 = track1->GetKalmanHits();
	Int_t hits2 = track2->GetKalmanHits();
	Double_t total = hits1+hits2;
	Double_t shared = 0;
	// calucalte shared fraction
#ifdef OLD_SHARED
	for(int i=0;i<hits1;i++){
		Int_t hit_index1 = track1->GetHitId(i);
		for(int j=0;j<hits2;j++){
			Int_t hit_index2 = track2->GetHitId(j);
		//	std::cout<<hit_index1<<" "<<hit_index2<<std::endl;
			if(hit_index1 == hit_index2)
				shared++;
		}
	}
#endif

	for(int i=0;i<hits1;i++){
		Int_t hit_index1 = track1->GetHitId(i);
		Int_t hit_index2 = track2->GetHitId(i);
		if(hit_index1==-1) continue;
		if(hit_index2==-1) continue;
		if(hit_index1==hit_index2)
			shared++;
	}
	//calculate quality
	Double_t quality = 0.0;
	for(int i =0;i<MPD_TPC_LAYERS;i++){
		Int_t lay1 = track1->LayerStatus(i);
		Int_t lay2 = track2->LayerStatus(i);
		Int_t sum  = lay1 + lay2;
		switch(sum){
		case 1:
			++quality;
			break;
		case 2:
			--quality;
			break;
		}
	}
	SetValue(shared/total,0);
	SetValue(quality/total,1);
//	std::cout<<track1->GetId()<<" " <<track2->GetId()<<std::endl;
	//std::cout<<shared<<"      "<<total<<std::endl;
//	if(shared!=0)
//	std::cout<<"S\t"<<GetValue(0)<<"\tQ\t"<<GetValue(1)<<std::endl;
	return Validate();
}

MpdTwoTrackSharedQualityCut::~MpdTwoTrackSharedQualityCut() {
	// TODO Auto-generated destructor stub
}

