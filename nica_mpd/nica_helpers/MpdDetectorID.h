/*
 * MpdDetectorsID.h
 *
 *  Created on: 6 wrz 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDDETECTORID_H_
#define MPDDETECTORID_H_

#include "NicaDetectorID.h"

namespace MpdDetectorID {
	const UInt_t kTPC = NicaDetectorID::kTPC;
	const UInt_t kTOF = NicaDetectorID::kTOF;
	const UInt_t kSTS = NicaDetectorID::kSTS;
	const UInt_t kZDC = 45840;
	const UInt_t kEMC = 18948;
	const UInt_t kCPC = 16464;
	const UInt_t kETOF = 691647;
	const UInt_t kFFD = 19993;
};

#endif /* MPDDETECTORID_H_ */
