/*
 * NicaFairExtendedTrack.cpp
 *
 *  Created on: 05-07-2014
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaFairExtendedTrack.h"

NicaFairExtendedTrack::NicaFairExtendedTrack() :fFreezVec(NULL){

}

void NicaFairExtendedTrack::Clear(Option_t* opt) {
	NicaFairTrack::Clear(opt);
	fFreezVec->Clear();
}

NicaFairExtendedTrack::~NicaFairExtendedTrack() {
}

TObject* NicaFairExtendedTrack::GetTrackPointer() const{
	return NicaFairExtendedTrack::GetTrackPointer();
}

