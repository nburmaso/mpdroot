/*
 * NicaFairTrackInterface.h
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef NICAFAIREXTENDEDTRACKINTERFACE_H_
#define NICAFAIREXTENDEDTRACKINTERFACE_H_

#include "NicaFairTrackInterface.h"
#include <TLorentzVector.h>

class NicaFairExtendedTrackInterface : public NicaFairTrackInterface{
	TLorentzVector *fRawFreez;
public:
	NicaFairExtendedTrackInterface();
	virtual Double_t GetFreezX()const{return fRawFreez->X();};
	virtual Double_t GetFreezY()const{return fRawFreez->Y();};
	virtual Double_t GetFreezZ()const{return fRawFreez->Z();};
	virtual Double_t GetFreezT()const{return fRawFreez->T();};
	virtual void SetRawTrack(TObject *track, TObject *freez);
	virtual void SetFreez(Double_t x, Double_t y, Double_t z, Double_t t);
	virtual ~NicaFairExtendedTrackInterface();
	ClassDef(NicaFairExtendedTrackInterface,1)
};

#endif /* NICAROOT_DATAFORMAT_FORMATS_FAIR_NICAFAIRTRACKINTERFACE_H_ */
