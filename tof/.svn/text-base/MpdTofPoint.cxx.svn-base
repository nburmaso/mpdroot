//------------------------------------------------------------------------------------------------------------------------

#include <iostream>

#include "MpdTofPoint.h"

//------------------------------------------------------------------------------------------------------------------------
MpdTofPoint::MpdTofPoint() : FairMCPoint() { }
//------------------------------------------------------------------------------------------------------------------------
MpdTofPoint::MpdTofPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t tof, Double_t length, Double_t eLoss)
 : FairMCPoint(trackID, detID, pos, mom, tof, length, eLoss)
{ }
//------------------------------------------------------------------------------------------------------------------------
MpdTofPoint::~MpdTofPoint() { }
//------------------------------------------------------------------------------------------------------------------------
void MpdTofPoint::Print(const Option_t* opt) const 
{
	cout 	<<"\n-I- MpdTofPoint: TOF point for track "<< fTrackID <<" in detector "<< fDetectorID 
	 	<<"\n    Position ("<< fX <<", "<< fY <<", "<< fZ << ") cm" 
	 	<<"\n    Momentum ("<< fPx <<", "<< fPy <<", "<< fPz <<") GeV"
	 	<<"\n    Time "<< fTime <<" ns,  Length "<< fLength <<" cm,  Energy loss "<< fELoss*1.e06 <<" keV.";
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdTofPoint)
