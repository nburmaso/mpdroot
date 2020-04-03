//-----------------------------------------------------------
// File and Version Information:
// $Id$
//
// Description:
//      Implementation of class MbbPoint
//      see MbbPoint.h for details
//
// Environment:
//      Software developed for the PANDA Detector at FAIR.
//
// Author List:
//      Pedro Gonzalez    TUM            (original author)
//      Luis Valenzuela-Cazares
//
//-----------------------------------------------------------

// Panda Headers ----------------------

// This Class' Header ------------------
#include "MbbPoint.h"

// C/C++ Headers ----------------------
#include <iostream>

// Collaborating Class Headers --------


// Class Member definitions -----------

MbbPoint::MbbPoint()
  : FairMCPoint()
{}

MbbPoint::MbbPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom,
             Double_t tof, Double_t length, Double_t eLoss,Int_t statusCode)
  : FairMCPoint(trackID, detID, pos, mom, tof, length, eLoss)
{

  pT = mom.Pt();
  eta = mom.Eta();
  fStatusCode = statusCode;




}

void
MbbPoint::Print(const Option_t*) const {
  std::cout<<"MbbPoint\n"
	   <<" Pos("<<fX<<","<<fY<<","<<fZ<<")\n"
	   <<" dE="<< fELoss << fTrackID
	   <<std::endl;
}



ClassImp(MbbPoint)
