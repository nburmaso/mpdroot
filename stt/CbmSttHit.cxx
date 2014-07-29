/** CbmStsMapsHit 
 *@author Michael Deveaux <m.deveaux@gsi.de>
 **Acknowledgements to M. Al-Turany, D. Bertini, G. Gaycken
 **Version beta 0.1 (02.02.2005)
 **/


#include "CbmSttHit.h"

#include <iostream>
#include "TMath.h"

/** Default constructor **/
CbmSttHit::CbmSttHit() 
{
    Clear();
}


/** Standard constructor **/
CbmSttHit::CbmSttHit(Int_t detID, TVector3& pos, TVector3& dpos, 
		     Int_t index, Int_t flag, Double_t isochrone,
		     Double_t isochroneError, TVector3 wireDir) 
  : FairHit(detID, pos, dpos, index)
{
  fIsochrone = isochrone;
  fIsochroneError = isochroneError;
  fRadial = sqrt(pos.X() * pos.X() + pos.Y() * pos.Y());
  fWireDirection = wireDir;
  fAssigned = kFALSE;
  // stt1
  fXint = fX;
  fYint = fY;
  fZint = fZ;
}

CbmSttHit::CbmSttHit(Int_t detID, TVector3& pos, TVector3& dpos, Int_t index) 
  : FairHit(detID, pos, dpos, index)
{
    Clear();
}

//CbmSttHit::CbmSttHit(Int_t trackID,Int_t eventID,Double_t p, Double_t rr, Double_t rt,TString nam,TVector3 center,TVector3 tubemax,TVector3 tubemin) {
CbmSttHit::CbmSttHit(Int_t detID, TVector3& pos, TVector3& dpos, Int_t index, Int_t trackID, Double_t p, Double_t rr, Double_t rt, Double_t isochroneError, TVector3 wireDir) 
  : FairHit(detID, pos, dpos, index){
  fPulse   = p;
  fRsim    = rr;
  fRtrue   = rt;
  //  fFlag    = 1;       // ??
  //  fnam     = nam;
  fTrackID = trackID;
  //  fEventID = eventID; // ??
  //  tube_c   = center;  // ??
  //  tube_max = tubemax; // ??
  //  tube_min = tubemin; // ??
  // stt2
  fIsochrone = rr;  // isochrone = simulated radius <----
  fIsochroneError = isochroneError;
  fRadial = sqrt(pos.X() * pos.X() + pos.Y() * pos.Y());
  fWireDirection = wireDir;
  fAssigned = kFALSE;
  fXint = fX;
  fYint = fY;
  fZint = fZ;
  
}

/** Public method Clear **/
void CbmSttHit::Clear() 
{
  fIsochrone = 0.;
  fIsochroneError = 0.;
  fRadial = 0.;
  fWireDirection.SetXYZ(0., 0., 0.);
  fAssigned = kFALSE;
}  

 
/** Destructor **/
CbmSttHit::~CbmSttHit() 
{
} 



ClassImp(CbmSttHit)
