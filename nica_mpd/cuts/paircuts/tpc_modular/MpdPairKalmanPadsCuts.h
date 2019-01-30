/*
 * MpdPairKalmanPadsCuts.h
 *
 *  Created on: 19 lip 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_NICA_MPD_CUTS_PAIRCUTS_MPDPAIRKALMANPADSCUTS_H_
#define MPDROOT_NICA_MPD_CUTS_PAIRCUTS_MPDPAIRKALMANPADSCUTS_H_

#include "MpdModularTpcPairCut.h"
// TODO Fix this messy cut
/**
 * OBSOLETE CUT!
 * old cut for checking all TPC cuts,
 * AverageSep = MpdTpcSeparation::TpcAverage
 * SharedPads =MpdPairSharedPadsCut::OverlapPads
 * TpcEntranceDist = MpdTpcSeparation::Entrance
 * HitQuality = ~MpdAlicePairQuality
 * HitShared = MpdPairSharedPadsCut::SharedPads
 * HitQualityDistance =
 * HitQualityLayer = MpdStarPairQualityCut
 */
class MpdPairKalmanPadsCuts : public MpdModularTpcPairCut{
	Double_t fQualityThreshold2;
public:
	MpdPairKalmanPadsCuts();
	MpdPairKalmanPadsCuts(const MpdPairKalmanPadsCuts &other);
	MpdPairKalmanPadsCuts& operator=(const MpdPairKalmanPadsCuts &other);
	virtual Bool_t Pass(NicaTwoTrack *pair);
	void SetSharingThreshold(Double_t thres){fQualityThreshold2 = thres;};
	Bool_t Init(Int_t task_id);
	static Int_t AverageSep(){return 0;};
	static Int_t SharedPads(){return 1;};
	static Int_t MinDeltaPhiStar(){return 2;};
	static Int_t TPCEntranceDist(){return 3;};
	static Int_t MinTPCSep(){return 4;};
	static Int_t HitQuality(){return 5;}
	static Int_t HitShared(){return 6;};
	static Int_t HitQualityDistance(){return 7;};
	static Int_t HitQualityLayer(){return 8;};
	NicaPackage *Report()const;
	virtual ~MpdPairKalmanPadsCuts();
	ClassDef(MpdPairKalmanPadsCuts,1)
};

#endif /* MPDROOT_NICA_MPD_CUTS_PAIRCUTS_MPDPAIRKALMANPADSCUTS_H_ */
