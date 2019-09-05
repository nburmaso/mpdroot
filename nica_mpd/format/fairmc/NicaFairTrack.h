/*
 * NicaFairTrack.h
 *
 *  Created on: 05-07-2014
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef NICAFAIRTRACK_H_
#define NICAFAIRTRACK_H_

#include "FairMCTrack.h"

#include "NicaMCTrack.h"
/**
 * class used by FairEvent that holds FairMCTracks
 */
class NicaFairTrack: public NicaMCTrack {
public:
	NicaFairTrack();
	virtual ~NicaFairTrack();
	ClassDef(NicaFairTrack,1)
};

#endif /* NICAFAIRTRACK_H_ */
