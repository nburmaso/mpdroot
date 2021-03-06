//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                      MpdFfdPoint source file                  -----
// -------------------------------------------------------------------------


#include <iostream>
#include "MpdFfdPoint.h"


//------------------------------------------------------------------------------------------------------------------------
MpdFfdPoint::MpdFfdPoint() : FairMCPoint() { }
//------------------------------------------------------------------------------------------------------------------------
MpdFfdPoint::MpdFfdPoint(Int_t trackID, Int_t detID, TVector3 pos,
			 	TVector3 mom, Double_t tof, Double_t length, Double_t eLoss)
 : FairMCPoint(trackID, detID, pos, mom, tof, length, eLoss)
{ }
//------------------------------------------------------------------------------------------------------------------------
MpdFfdPoint::~MpdFfdPoint() { }
//------------------------------------------------------------------------------------------------------------------------
void MpdFfdPoint::Print(const Option_t* opt) const
{
        cout 	<< "-I- MpdFfdPoint: FFD point for track " << fTrackID
       		<< " in detector " << fDetectorID << endl;
	cout 	<< "    Position (" << fX << ", " << fY << ", " << fZ
       		<< ") cm" << endl;
	cout 	<< "    Momentum (" << fPx << ", " << fPy << ", " << fPz
       		<< ") GeV" << endl;
	cout 	<< "    Time " << fTime << " ns,  Length " << fLength 
       		<< " cm,  Energy loss " << fELoss*1.0e06 << " keV" << endl;
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdFfdPoint)
