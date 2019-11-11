/*
 * NicaFairEvent.h
 *
 *  Created on: 05-07-2014
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef NICAFAIREVENT_H_
#define NICAFAIREVENT_H_
#include "FairMCEventHeader.h"
#include "FairMCTrack.h"
#include <TClonesArray.h>
#include <TLorentzVector.h>
#include <TRegexp.h>

#include "NicaFairTrack.h"
#include "NicaMCEvent.h"
#include "NicaTrack.h"
/**
 * basic  class for reading FairMCEventHeader and FairMCTracks
 */
class NicaFairEvent : public NicaMCEvent{
protected:
	NicaFairEvent(TString trackname);
public:
	/**
	 * default constructor
	 */
	NicaFairEvent();
	NicaFairEvent(const NicaFairEvent &other);
	virtual void CreateSource();
	virtual void Update();
	virtual void Clear(Option_t *opt);
	virtual void Print();
	virtual Bool_t ExistInTree()const;
	virtual TString GetFormatName()const;
	virtual ~NicaFairEvent();
	ClassDef(NicaFairEvent,1)
};

#endif /* NICAFAIREVENT_H_ */
