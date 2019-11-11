/*
 * NicaFairExtendedEvent.h
 *
 *  Created on: 05-07-2014
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef NICAFAIREXTENDEDEVENT_H_
#define NICAFAIREXTENDEDEVENT_H_
#include "NicaFairEvent.h"
/**
 * basic  class for reading FairMCEventHeader and FairMCTracks
 */
class NicaFairExtendedEvent : public NicaFairEvent{
protected:
	virtual void LinkTracks();
public:
	/**
	 * default constructor
	 */
	NicaFairExtendedEvent();
	NicaFairExtendedEvent(const NicaFairExtendedEvent &other);
	void CreateSource();
	virtual void Update();
	virtual Bool_t ExistInTree()const;
	virtual TString GetFormatName()const;
	virtual ~NicaFairExtendedEvent();
	ClassDef(NicaFairExtendedEvent,1)
};

#endif /* NICAFAIREXTENDEDEVENT_H_ */
