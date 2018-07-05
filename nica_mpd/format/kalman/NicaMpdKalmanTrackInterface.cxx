/*
 * NicaKalmanTrackInterface.cxx
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdKalmanTrackInterface.h"

NicaMpdKalmanTrackInterface::NicaMpdKalmanTrackInterface() {
	// TODO Auto-generated constructor stub

}

Double_t NicaMpdKalmanTrackInterface::GetPx() const {
	Double_t pt = ((MpdKalmanTrack*)fRawObject)->Pt();
	Double_t phi = ((MpdKalmanTrack*)fRawObject)->Phi();
	return pt*TMath::Cos(phi);
}

Double_t NicaMpdKalmanTrackInterface::GetPy() const {
	Double_t pt = ((MpdKalmanTrack*)fRawObject)->Pt();
	Double_t phi = ((MpdKalmanTrack*)fRawObject)->Phi();
	return pt*TMath::Sin(phi);
}

Double_t NicaMpdKalmanTrackInterface::GetPz() const {
	Double_t pt = ((MpdKalmanTrack*)fRawObject)->Momentum();
	Double_t phi = ((MpdKalmanTrack*)fRawObject)->Theta();
	return pt*TMath::Sin(phi);
}

Double_t NicaMpdKalmanTrackInterface::GetCharge() const {
	return ((MpdKalmanTrack*)fRawObject)->Charge();
}

NicaMpdKalmanTrackInterface::~NicaMpdKalmanTrackInterface() {
	// TODO Auto-generated destructor stub
}

