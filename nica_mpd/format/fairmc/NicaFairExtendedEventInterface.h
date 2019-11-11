/*
 * NicaFairSource.h
 *
 *  Created on: 2 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef NICAFAIREXTENDEDEVENTINTERFACE_H_
#define NICAFAIREXTENDEDEVENTINTERFACE_H_

#include "NicaFairEventInterface.h"
/**

 * interface to FairMC data event.
 */
class NicaFairExtendedEvent;
class NicaFairExtendedEventInterface : public NicaFairEventInterface {
	friend class NicaFairExtendedEvent;
	TClonesArray *fFreezouts;
protected:
	virtual void ConnectToTree();
public:
	NicaFairExtendedEventInterface();
	virtual void Register(Bool_t write);
	virtual void Clear(Option_t *opt="");
	virtual void Compress(Int_t *map, Int_t map_size);
	virtual void CopyData(NicaEventInterface *s);
	virtual void CopyAndCompress(NicaEventInterface *s, Int_t *map, Int_t map_size);
	virtual void FillTrackInterface(NicaTrackInterface *track, Int_t index) ;
	virtual TObject *GetRawFreezPointer(Int_t index)const {return fFreezouts->UncheckedAt(index);};
	virtual ~NicaFairExtendedEventInterface();
	ClassDef(NicaFairExtendedEventInterface,1)
};

#endif /* NICAFAIREXTENDEDEVENTINTERFACE_H_ */
