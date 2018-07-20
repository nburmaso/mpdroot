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
	fLayers = new Short_t[fSize];
	fIndex = new Int_t[fSize];
	fLayerMap = new Short_t[fSize];
}

void NicaMpdDstKalmanTrack::Update(MpdTrack* track, MpdTpcKalmanTrack* kalman) {
	NicaMpdTrackTpcPads::Update(track);
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
	NicaMpdTrackTpcPads::CopyData((NicaMpdTrackTpcPads*)other);
	NicaMpdDstKalmanTrack *tr = (NicaMpdDstKalmanTrack*)other;
	fNKalmanhits = tr->fNKalmanhits;
	for(int i =0;i<fSize;i++){
		fLayers[i]=tr->fLayers[i];
		fIndex[i]=tr->fIndex[i];
		fLayerMap[i]=tr->fLayerMap[i];
	}
}

NicaMpdDstKalmanTrack::NicaMpdDstKalmanTrack(
		const NicaMpdDstKalmanTrack& other):NicaMpdTrackTpcPads(other),fSize(MPD_TPC_LAYERS) {
	fNKalmanhits = other.fNKalmanhits;
	fLayers = new Short_t[fSize];
	fIndex = new Int_t[fSize];
	fLayerMap = new Short_t[fSize];
	for(int i=0;i<fSize;i++){
		fLayers[i] = other.fLayers[i];
		fLayerMap[i]  = other.fLayerMap[i];
		fIndex[i] = other.fIndex[i];
	}
}

NicaMpdDstKalmanTrack& NicaMpdDstKalmanTrack::operator =(
		const NicaMpdDstKalmanTrack& other) {
	if(this!=&other){
		NicaMpdTrackTpcPads::operator=(other);
		fNKalmanhits = other.fNKalmanhits;
		for(int i=0;i<fSize;i++){
			fLayers[i] = other.fLayers[i];
			fLayerMap[i]  = other.fLayerMap[i];
			fIndex[i] = other.fIndex[i];
		}
	}
	return *this;
}
