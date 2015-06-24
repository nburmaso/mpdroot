//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                      MpdStrawECTPoint source file                  -----
// -------------------------------------------------------------------------


#include <iostream>
#include "MpdStrawECTPoint.h"


//------------------------------------------------------------------------------------------------------------------------
MpdStrawECTPoint::MpdStrawECTPoint() : FairMCPoint() {
    fModule = -1;
    fSubmodule = -1;
    fLayer = -1;
    fLayerType = "";
    fStraw = -1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdStrawECTPoint::MpdStrawECTPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t tof,
			Double_t length, Double_t eLoss)
                        : FairMCPoint(trackID, detID, pos, mom, tof, length, eLoss) {
    fModule = -1;
    fSubmodule = -1;
    fLayer = -1;
    fLayerType = "";
    fStraw = -1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdStrawECTPoint::~MpdStrawECTPoint() { }
//------------------------------------------------------------------------------------------------------------------------

void MpdStrawECTPoint::Print(const Option_t* opt) const
{
	cout 	<< "-I- MpdStrawECTPoint: StrawECT point for track " << fTrackID
       		<< " in detector " << fDetectorID << endl;
	cout 	<< "    Position (" << fX << ", " << fY << ", " << fZ
       		<< ") cm" << endl;
	cout 	<< "    Momentum (" << fPx << ", " << fPy << ", " << fPz
       		<< ") GeV" << endl;
	cout 	<< "    Time " << fTime << " ns,  Length " << fLength
       		<< " cm,  Energy loss " << fELoss*1.0e06 << " keV" << endl;
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdStrawECTPoint)
