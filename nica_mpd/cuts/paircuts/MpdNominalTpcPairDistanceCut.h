/*
 * MpdTpcDistanceCut.h
 *
 *  Created on: 30 paź 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_NICA_MPD_CUTS_PAIRCUTS_TPC_DIST_MPDTPCDISTANCECUT_H_
#define MPDROOT_NICA_MPD_CUTS_PAIRCUTS_TPC_DIST_MPDTPCDISTANCECUT_H_

#include "NicaTwoTrackCut.h"
#include "NicaHelix.h"
#include "NicaMpdTrackTpcPads.h"
/**
 * basic  abstractu cut that calculates values based on distances between tracks in active TPC area
 */
class MpdNominalTpcPairDistanceCut : public NicaTwoTrackCut {
protected:
	Float_t fCosDipAngle1;
	Float_t fCosDipAngle2;
	Float_t fSinDipAngle1;
	Float_t fSinDipAngle2;
	Float_t fCosPhase1;
	Float_t fCosPhase2;
	Float_t fSinPhase1;
	Float_t fSinPhase2;
	Int_t fFirstCommonPad;
	Int_t fLastCommonPad;
	NicaMpdTrackTpcPads *fTrack1;//!
	NicaMpdTrackTpcPads *fTrack2;//!
	Float_t fdX, fdY,fdZ;
	/**
	 * needed at begin of pass to fill proper fields
	 * @param pair
	 */
	void InitPass(NicaTwoTrack *pair);
	/**
	 *
	 * @param lay layer number
	 * @return distance between two helices at given layer
	 */
	Double_t GetDistance(Int_t lay) const;
public:
	MpdNominalTpcPairDistanceCut(Int_t size=1);
	MpdNominalTpcPairDistanceCut(const MpdNominalTpcPairDistanceCut &other);
	MpdNominalTpcPairDistanceCut& operator=(const MpdNominalTpcPairDistanceCut &other);
	virtual ~MpdNominalTpcPairDistanceCut();
	ClassDef(MpdNominalTpcPairDistanceCut,1)
};

#endif /* MPDROOT_NICA_MPD_CUTS_PAIRCUTS_TPC_DIST_MPDTPCDISTANCECUT_H_ */
