/*
 * NicaMpdDstKalmanTrack.cxx
 *
 *  Created on: 4 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdDstKalmanTrack.h"
#include "MpdKalmanHit.h"
#include "TObjArray.h"
NicaMpdDstKalmanTrack::NicaMpdDstKalmanTrack():fNKalmanhits(0),fSize(MPD_TPC_LAYERS),fLayers(NULL),fIndex(NULL){
	fLayers = new Int_t[fSize];
	fIndex = new Int_t[fSize];
	fLayerMap = new Int_t[fSize];
}

void NicaMpdDstKalmanTrack::Update(MpdTrack* track, MpdTpcKalmanTrack* kalman) {
	NicaMpdTrack::Update(track);
	if(kalman!=NULL){
		fNKalmanhits = kalman->GetNofTrHits();
		for(int i =0;i<fNKalmanhits;i++){
			fLayers[i]=0;
			fIndex[i]=-1;
		}
		for(int i=0;i<fSize;i++)
			fLayerMap[i]=0;
		TObjArray *khits = kalman->GetTrHits();
		if(khits){
			GetTpcTrack()->SetNHits(khits->GetEntriesFast());
		}
		Int_t tpchits = GetTpcTrack()->GetNHits();
		for(int i=0;i<tpchits;i++){
			MpdKalmanHit *hit = (MpdKalmanHit*)khits->UncheckedAt(i);
			Int_t layer = hit->GetLayer();
			fLayerMap[layer] =1;
			fLayers[layer] = 1;
			fIndex[i] = hit->GetIndex(0);
		}
	}else{
		fNKalmanhits = 0;
	}
}

NicaMpdDstKalmanTrack::~NicaMpdDstKalmanTrack() {
	delete []fLayers;
	delete []fIndex;
	delete []fLayerMap;
}

void NicaMpdDstKalmanTrack::CopyData(NicaTrack* other) {
	NicaMpdTrack::CopyData((NicaMpdTrack*)other);
	NicaMpdDstKalmanTrack *tr = (NicaMpdDstKalmanTrack*)other;
	fNKalmanhits = tr->fNKalmanhits;
	for(int i =0;i<fNKalmanhits;i++){
		fLayers[i]=tr->fLayers[i];
		fIndex[i]=tr->fIndex[i];
	}
	for(int i=0;i<fSize;i++)
		fLayerMap[i]=tr->fLayerMap[i];
}
