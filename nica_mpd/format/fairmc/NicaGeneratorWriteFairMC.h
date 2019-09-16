/*
 * NicaGeneratorWriteFairMC.h
 *
 *  Created on: 14 sie 2015
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef NICAGENERATORWRITEFAIRMC_H_
#define NICAGENERATORWRITEFAIRMC_H_

#include <TClonesArray.h>
#include "FairMCEventHeader.h"
#include "FairMCTrack.h"
#include "NicaGeneratorWrite.h"
/**
 * convert data into FairMCTrack format
 */
class NicaGeneratorWriteFairMC: public NicaGeneratorWrite {
	FairMCEventHeader *fEventHeader;
	TClonesArray *fTracks;
protected:
	Int_t fTrackCounter;
	NicaGeneratorWriteFairMC(Int_t event_par_i, Int_t event_par_d, Int_t event_par_s,Int_t event_par_u,Int_t track_par_i,Int_t track_par_d, Int_t track_par_s, Int_t track_par_u);
public:
	/**
	 * default constructor
	 */
	NicaGeneratorWriteFairMC();
	virtual InitStatus Init();
	virtual void ClearEvent();
	void AddEvent();
	void AddParticle();
	virtual ~NicaGeneratorWriteFairMC();
	ClassDef(NicaGeneratorWriteFairMC,1)
};

#endif /* NICAGENERATORWRITEFAIRMC_H_ */
