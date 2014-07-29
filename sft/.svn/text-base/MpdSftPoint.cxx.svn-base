//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                      MpdSftPoint source file                  -----
// -----                   Created  28/07/04  by V. Friese             -----
// -----                   Modified 30/08/06  by D. Gonzalez           -----
// -------------------------------------------------------------------------


#include <iostream>
#include "MpdSftPoint.h"


//------------------------------------------------------------------------------------------------------------------------
MpdSftPoint::MpdSftPoint() : FairMCPoint() { }
//------------------------------------------------------------------------------------------------------------------------
MpdSftPoint::MpdSftPoint(Int_t trackID, Int_t detID, TVector3 pos, 
			 TVector3 mom, Double_t tof, Double_t length, Double_t eLoss)
 : FairMCPoint(trackID, detID, pos, mom, tof, length, eLoss)
{ }
//------------------------------------------------------------------------------------------------------------------------
MpdSftPoint::~MpdSftPoint() { }
//------------------------------------------------------------------------------------------------------------------------
void MpdSftPoint::Print(const Option_t* opt) const 
{
	cout 	<< "-I- MpdSftPoint: SFT point for track " << fTrackID 
       		<< " in detector " << fDetectorID << endl;
	cout 	<< "    Position (" << fX << ", " << fY << ", " << fZ
       		<< ") cm" << endl;
	cout 	<< "    Momentum (" << fPx << ", " << fPy << ", " << fPz
       		<< ") GeV" << endl;
	cout 	<< "    Time " << fTime << " ns,  Length " << fLength 
       		<< " cm,  Energy loss " << fELoss*1.0e06 << " keV" << endl;
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdSftPoint)
