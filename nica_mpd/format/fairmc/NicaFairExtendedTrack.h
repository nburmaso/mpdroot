/*
 * NicaFairExtendedTrack.h
 *
 *  Created on: 05-07-2014
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef NICAFAIREXTENDEDTRACK_H_
#define NICAFAIREXTENDEDTRACK_H_

#include "NicaFairTrack.h"

/**
 * class used by FairEvent that holds FairMCTracks
 */
class NicaFairExtendedTrack: public NicaFairTrack {
	TLorentzVector *fFreezVec;
public:
	NicaFairExtendedTrack();
	/**
	 * clear this object
	 * @param opt clear option
	 */
	void Clear(Option_t *opt = " ");
	TObject *GetTrackPointer() const;
	virtual ~NicaFairExtendedTrack();
	ClassDef(NicaFairExtendedTrack,1)
};

#endif /* NICAFAIREXTENDEDTRACK_H_ */
