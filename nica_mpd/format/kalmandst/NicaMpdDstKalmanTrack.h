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

#include "NicaMpdTrack.h"
#include "MpdTpcKalmanTrack.h"

#define MPD_TPC_LAYERS 55
class NicaMpdDstKalmanTrack : public NicaMpdTrack{
	Int_t fNKalmanhits;
	const Int_t fSize;
	Int_t *fLayers;//[fSize]
	Int_t *fIndex;//[fSize]
	Int_t *fLayerMap;//[fSize]
public:
	NicaMpdDstKalmanTrack();
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
	inline Int_t LayerStatus(Int_t layer)const {return fLayerMap[layer];};
	/**
	 *
	 * @param n hit number
	 * @return layer number of hit
	 */
	inline Int_t GetHitLayer(Int_t n)const {return fLayers[n];}
	/**
	 *
	 * @param n hit number
	 * @return id of hit
	 */
	inline Int_t GetHitId(Int_t n)const {return fIndex[n];};
	void Update(MpdTrack *track,MpdTpcKalmanTrack *kalman);
	virtual void CopyData(NicaTrack *other);
	virtual ~NicaMpdDstKalmanTrack();
	ClassDef(NicaMpdDstKalmanTrack,1)
};

#endif /* INTERFACES_MPDROOT_FORMAT_KALMANDST_NICAMPDDSTKALMANTRACK_H_ */
