// -------------------------------------------------------------------------
// -----                CbmStsHitProducerIdeal source file             -----
// -----                  Created 10/01/06  by V. Friese               -----
// -------------------------------------------------------------------------


#include "TClonesArray.h"

#include "FairRootManager.h"
#include "CbmSttHitProducerIdeal.h"
#include "CbmSttHit.h"
#include "CbmSttHitInfo.h"
#include "CbmSttPoint.h"
#include "TMath.h"
#include <iostream>


// TODO: read this from a configuration file
#define foldResolution 0 // <===== NO SMEARING
#define radialResolutionPolynomialConstant1 0.0150
#define radialResolutionPolynomialConstant2 0.
#define radialResolutionPolynomialConstant3 0.
//#define longitudinalResolutionPolynomialConstant1 3.
#define longitudinalResolutionPolynomialConstant1 0.0001
#define longitudinalResolutionPolynomialConstant2 0.
#define longitudinalResolutionPolynomialConstant3 0.

// TODO: read this from geant initialization
#define innerStrawDiameter 1. 

// -----   Default constructor   -------------------------------------------
CbmSttHitProducerIdeal::CbmSttHitProducerIdeal() :
  FairTask("Ideal STT Hit Producer")
{ 
}
// -------------------------------------------------------------------------



// -----   Destructor   ----------------------------------------------------
CbmSttHitProducerIdeal::~CbmSttHitProducerIdeal() 
{ 
}
// -------------------------------------------------------------------------



// -----   Public method Init   --------------------------------------------
InitStatus CbmSttHitProducerIdeal::Init() 
{
  // Get RootManager
  FairRootManager* ioman = FairRootManager::Instance();

  if ( ! ioman ) 
    {
      cout << "-E- CbmSttHitProducerIdeal::Init: "
	   << "RootManager not instantiated!" << endl;
      return kFATAL;
    }

  // Get input array
  fPointArray = (TClonesArray*) ioman->GetObject("STTPoint");

  if ( ! fPointArray ) 
    {
      cout << "-W- CbmSttHitProducerIdeal::Init: "
	   << "No STTPoint array!" << endl;
      return kERROR;
  }

  // Create and register output array
  fHitArray = new TClonesArray("CbmSttHit");
  ioman->Register("STTHit", "STT", fHitArray, kTRUE);

  // Create and register output array
  fHitInfoArray = new TClonesArray("CbmSttHitInfo");
  ioman->Register("STTHitInfo", "STT", fHitInfoArray, kTRUE);

  cout << "-I- CbmSttHitProducerIdeal: Intialisation successfull" << endl;
  return kSUCCESS;
}
// -------------------------------------------------------------------------



// -----   Public method Exec   --------------------------------------------
void CbmSttHitProducerIdeal::Exec(Option_t* opt) 
{
  // Reset output array
  if ( ! fHitArray ) 
    Fatal("Exec", "No HitArray");
  
  fHitArray->Clear();
  fHitInfoArray->Clear();
  
  // Declare some variables
  CbmSttPoint
    *point = NULL;

  Int_t 
    detID = 0,       // Detector ID
    trackID = 0;     // Track index

  TVector3 
    pos, dpos;       // Position and error vectors

  // Loop over SttPoints
  Int_t 
    nPoints = fPointArray->GetEntriesFast();

  for (Int_t iPoint = 0; iPoint < nPoints; iPoint++) 
    {
      point = (CbmSttPoint*) fPointArray->At(iPoint);

      if ( ! point) 
	continue;

      // Detector ID
      detID = point->GetDetectorID();

      // MCTrack ID
      trackID = point->GetTrackID();

      // Determine hit position and isochrone (x,y of wire, measured z  position)
      TVector3
	posInLocal(point->GetXInLocal(), point->GetYInLocal(), point->GetZInLocal()),
	posOutLocal(point->GetXOutLocal(), point->GetYOutLocal(), point->GetZOutLocal()),
	position(point->GetX(), point->GetY(), point->GetZ());

      Double_t
	closestDistance,
	closestDistanceError;
      
      GetClostestApproachToWire(closestDistance, closestDistanceError, 
				posInLocal, posOutLocal);

      Double_t
	zpos = position.Z() + ((posOutLocal.Z() + posInLocal.Z()) / 2.),
	zposError;
      
      FoldZPosWithResolution(zpos, zposError, 
			     posInLocal, posOutLocal);
 
      TVector3
	  wireDirection(point->GetXWireDirection(), point->GetYWireDirection(), point->GetZWireDirection());

      // Create new hit
      pos.SetXYZ(position.X(), position.Y(), zpos);
      dpos.SetXYZ(innerStrawDiameter / 2., innerStrawDiameter / 2., GetLongitudinalResolution(position.Z()));
      new ((*fHitArray)[iPoint]) CbmSttHit(detID, pos, dpos, iPoint, 0,
					   closestDistance, closestDistanceError, wireDirection);
      new ((*fHitInfoArray)[iPoint]) CbmSttHitInfo(0, 0, trackID, iPoint,
						   0, kFALSE);

    }   // Loop over MCPoints

  // Event summary
  cout << "-I- CbmSttHitProducerIdeal: " << nPoints << " SttPoints, "
       << nPoints << " Hits created." << endl;

}
// -------------------------------------------------------------------------

// -----   Private method GetRadialResolution-------------------------------
Double_t CbmSttHitProducerIdeal::GetRadialResolution(Double_t radius)
{
  return radialResolutionPolynomialConstant1 + 
    radius * radialResolutionPolynomialConstant2 + 
    radius * radius * radialResolutionPolynomialConstant3;
}
// -------------------------------------------------------------------------



// -----   Private method GetLongitudinalResolution ------------------------
Double_t CbmSttHitProducerIdeal::GetLongitudinalResolution(Double_t zpos)
{
  return longitudinalResolutionPolynomialConstant1 + 
    zpos * longitudinalResolutionPolynomialConstant2 + 
    zpos * zpos * longitudinalResolutionPolynomialConstant3;
}
// -------------------------------------------------------------------------

// -----   Private method GetClostestApproachToWire ------------------------
void CbmSttHitProducerIdeal::GetClostestApproachToWire(Double_t &closestDistance, 
						       Double_t &closestDistanceError,
						       TVector3 localInPos, 
						       TVector3 localOutPos)
{
  Double_t
    a = (localOutPos.X() - localInPos.X()),
    b = (localOutPos.Y() - localInPos.Y()),
    c = sqrt(a * a + b * b) / 2.;
    
  // fold with Gaussian for resolution

  if (c > innerStrawDiameter / 2.) 
    {
      c = innerStrawDiameter / 2.;
    }
                
  closestDistance = sqrt((innerStrawDiameter / 2.) * (innerStrawDiameter / 2.) - c * c); 
  closestDistanceError = 0.;

  if (foldResolution)
    {
      closestDistanceError = GetRadialResolution(closestDistance);
      closestDistance += gRandom->Gaus(0., closestDistanceError);
    }
}
// -------------------------------------------------------------------------

void CbmSttHitProducerIdeal::FoldZPosWithResolution(Double_t &zpos, Double_t &zposError, 
						    TVector3 localInPos, TVector3 localOutPos)
{
  Double_t
    zPosInStrawFrame = (localOutPos.Z() - localInPos.Z()) / 2.;
 
  zposError = gRandom->Gaus(0., GetLongitudinalResolution(zPosInStrawFrame));

  zpos += zposError;
  //  cout << "zpos in prod: " << zpos << endl;
}
 


ClassImp(CbmSttHitProducerIdeal)
