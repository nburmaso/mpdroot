/*
 * NicaMpdDstKalmanTrack.h
 *
 *  Created on: 4 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef INTERFACES_MPDROOT_FORMAT_KALMANDST_NICAMPDDSTKALMANTRACK_H_
#define INTERFACES_MPDROOT_FORMAT_KALMANDST_NICAMPDDSTKALMANTRACK_H_

#include "NicaMpdTrackTpcPads.h"
#include "MpdTpcKalmanTrack.h"

#define MPD_TPC_LAYERS 53
class NicaMpdDstKalmanTrack : public NicaMpdTrackTpcPads{
	Int_t fNKalmanhits;
	ULong64_t fHitMap;
	ULong64_t fSharedHitMap;
public:
	NicaMpdDstKalmanTrack();
	NicaMpdDstKalmanTrack(const NicaMpdDstKalmanTrack &other);
	NicaMpdDstKalmanTrack &operator=(const NicaMpdDstKalmanTrack &other);
	/**
	 *
	 * @return number of hits in kalman track
	 */
	inline Int_t GetKalmanHits()const{return fNKalmanhits;};
	/**
	 *
	 * @param layer layer number (0-55)
	 * @return 1 if track has hit in given layer, 0 otherwise
	 */
	inline Int_t LayerStatus(Int_t layer)const {return TESTBIT(fHitMap,layer);};
	inline Int_t LayerShared(Int_t layer)const {return TESTBIT(fSharedHitMap,layer);};
	ULong64_t GetBitMap()const{return fHitMap;};
	ULong64_t GetBitMapSHared()const{return fSharedHitMap;};
	void Update(MpdTrack *track,MpdTpcKalmanTrack *kalman);
	virtual void CopyData(NicaTrack *other);
	virtual ~NicaMpdDstKalmanTrack();
	ClassDef(NicaMpdDstKalmanTrack,1)
};

#endif /* INTERFACES_MPDROOT_FORMAT_KALMANDST_NICAMPDDSTKALMANTRACK_H_ */
