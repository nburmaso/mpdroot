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
#include "MpdTpcHit.h"
#include <bitset>
NicaMpdDstKalmanTrack::NicaMpdDstKalmanTrack():fNKalmanhits(0),fHitMap(0),fSharedHitMap(0){
}

void NicaMpdDstKalmanTrack::Update(MpdTrack* track, MpdTpcKalmanTrack* kalman) {
	NicaMpdTrackTpcPads::Update(track);
	if(kalman!=NULL){
		fNKalmanhits = kalman->GetNofTrHits();
		fHitMap = 0;
		fSharedHitMap = 0;
		TObjArray *khits = kalman->GetTrHits();
		if(khits){
			GetTpcTrack()->SetNHits(khits->GetEntriesFast());
		}
		Int_t tpchits = GetTpcTrack()->GetNHits();
		for(int i=0;i<tpchits;i++){
			MpdKalmanHit *hit = (MpdKalmanHit*)khits->UncheckedAt(i);
			Int_t layer = hit->GetLayer();
			if(TESTBIT(fHitMap,layer)){
				SETBIT(fSharedHitMap,layer);
			}
			SETBIT(fHitMap,layer);
		}
	}else{
		fNKalmanhits = 0;
	}
//	std::cout<<std::bitset<32>(fHitMap)<<std::endl;
}

NicaMpdDstKalmanTrack::~NicaMpdDstKalmanTrack() {

}

void NicaMpdDstKalmanTrack::CopyData(NicaTrack* other) {
	NicaMpdTrackTpcPads::CopyData((NicaMpdTrackTpcPads*)other);
	NicaMpdDstKalmanTrack *tr = (NicaMpdDstKalmanTrack*)other;
	fNKalmanhits = tr->fNKalmanhits;
	fHitMap = tr->fHitMap;
}

NicaMpdDstKalmanTrack::NicaMpdDstKalmanTrack(
		const NicaMpdDstKalmanTrack& other):NicaMpdTrackTpcPads(other) {
	fNKalmanhits = other.fNKalmanhits;
	fHitMap = other.fHitMap;
	fSharedHitMap = other.fSharedHitMap;
}

NicaMpdDstKalmanTrack& NicaMpdDstKalmanTrack::operator =(
		const NicaMpdDstKalmanTrack& other) {
	if(this!=&other){
		NicaMpdTrackTpcPads::operator=(other);
		fNKalmanhits = other.fNKalmanhits;
		fHitMap = other.fHitMap;
		fSharedHitMap = other.fSharedHitMap;
	}
	return *this;
}
