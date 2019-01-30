/*
 * MpdTpcEntranceCut.h
 *
 *  Created on: 30 paź 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_NICA_MPD_CUTS_PAIRCUTS_TPC_DIST_MPDTPCENTRANCECUT_H_
#define MPDROOT_NICA_MPD_CUTS_PAIRCUTS_TPC_DIST_MPDTPCENTRANCECUT_H_

#include "MpdNominalTpcPairDistanceCut.h"
#include "NicaTwoTrackCut.h"
/**
 * cut that calculate distance between particles at entrance to ACTIVE TPC area
 */
class MpdPairTpcEntranceCut  : public MpdNominalTpcPairDistanceCut{
public:
	MpdPairTpcEntranceCut();
	virtual Bool_t Pass(NicaTwoTrack *pair);
	virtual ~MpdPairTpcEntranceCut();
	ClassDef(MpdPairTpcEntranceCut,1)
};

class MpdPairTpcEntranceCut2D  : public MpdNominalTpcPairDistanceCut{
public:
	MpdPairTpcEntranceCut2D();
	virtual Bool_t Pass(NicaTwoTrack *pair);
	static Int_t XY(){return 0;};
	static Int_t Z(){return 1;}
	virtual ~MpdPairTpcEntranceCut2D();
	ClassDef(MpdPairTpcEntranceCut2D,1)
};


#endif /* MPDROOT_NICA_MPD_CUTS_PAIRCUTS_TPC_DIST_MPDTPCENTRANCECUT_H_ */
