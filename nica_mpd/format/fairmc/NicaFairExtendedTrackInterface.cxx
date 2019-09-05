/*
 * NicaFairTrackInterface.cxx
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaFairExtendedTrackInterface.h"


NicaFairExtendedTrackInterface::NicaFairExtendedTrackInterface() :NicaFairTrackInterface(),fRawFreez(NULL){
}

void NicaFairExtendedTrackInterface::SetRawTrack(TObject* track,
		TObject* freez) {
	NicaFairTrackInterface::SetRawTrack(track);
	fRawFreez = (TLorentzVector*)freez;
}

void NicaFairExtendedTrackInterface::SetFreez(Double_t x, Double_t y,
		Double_t z, Double_t t) {
	fRawFreez->SetXYZT(x,y,z,t);
}

NicaFairExtendedTrackInterface::~NicaFairExtendedTrackInterface() {
}
