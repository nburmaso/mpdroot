//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                      MpdCpcPoint source file                  -----
// -------------------------------------------------------------------------


#include <iostream>
#include "MpdCpcPoint.h"


//------------------------------------------------------------------------------------------------------------------------
MpdCpcPoint::MpdCpcPoint() : FairMCPoint() { }
//------------------------------------------------------------------------------------------------------------------------
MpdCpcPoint::MpdCpcPoint(Int_t trackID, Int_t detID, TVector3 pos,
			 	TVector3 mom, Double_t tof, Double_t length, Double_t eLoss, Int_t CpcID, Int_t RingID, Int_t CellID)
 : FairMCPoint(trackID, detID, pos, mom, tof, length, eLoss)
{
	fDetectorID = detID;

 	fCpcID = CpcID;
	fRingID = RingID;
	fCellID = CellID;
	fELoss = eLoss;
}
//------------------------------------------------------------------------------------------------------------------------
MpdCpcPoint::~MpdCpcPoint() { }
//------------------------------------------------------------------------------------------------------------------------
void MpdCpcPoint::Print(const Option_t* opt) const
{
	cout 	<< "-I- MpdCpcPoint: CPC point for track " << fTrackID
       		<< " in detector " << fDetectorID << endl;
	cout 	<< "    Position (" << fX << ", " << fY << ", " << fZ
       		<< ") cm" << endl;
	cout 	<< "    Momentum (" << fPx << ", " << fPy << ", " << fPz
       		<< ") GeV" << endl;
	cout 	<< "    Time " << fTime << " ns,  Length " << fLength
       		<< " cm,  Energy loss " << fELoss*1.0e06 << " keV" << endl;
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdCpcPoint)
