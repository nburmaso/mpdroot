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

#include "NicaTwoTrackCut.h"
#include "MpdNominalTpcPairDistanceCut.h"
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

#endif /* MPDROOT_NICA_MPD_CUTS_PAIRCUTS_TPC_DIST_MPDTPCENTRANCECUT_H_ */
