//------------------------------------------------------------------------------------------------------------------------

#include <iostream>

#include "MpdNDetPoint.h"

//------------------------------------------------------------------------------------------------------------------------
MpdNDetPoint::MpdNDetPoint() : FairMCPoint() { }
//------------------------------------------------------------------------------------------------------------------------
MpdNDetPoint::MpdNDetPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t tof, Double_t length, Double_t eLoss)
 : FairMCPoint(trackID, detID, pos, mom, tof, length, eLoss)
{ }
//------------------------------------------------------------------------------------------------------------------------
MpdNDetPoint::~MpdNDetPoint() { }
//------------------------------------------------------------------------------------------------------------------------
void MpdNDetPoint::Print(const Option_t* opt) const 
{
	cout 	<<"\n-I- MpdNDetPoint: NDET point for track "<< fTrackID <<" in detector "<< fDetectorID 
	 	<<"\n    Position ("<< fX <<", "<< fY <<", "<< fZ << ") cm" 
	 	<<"\n    Momentum ("<< fPx <<", "<< fPy <<", "<< fPz <<") GeV"
	 	<<"\n    Time "<< fTime <<" ns,  Length "<< fLength <<" cm,  Energy loss "<< fELoss*1.e06 <<" keV.";
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdNDetPoint)
