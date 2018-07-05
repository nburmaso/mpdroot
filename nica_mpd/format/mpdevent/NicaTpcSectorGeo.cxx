/// \class NicaTpcSectorGeo
///
/// MPD TPC sector geometry description
/// \author Alexander Zinchenko (LHEP, JINR, Dubna)

#include "TpcGas.h"
//#include "TpcGeoPar.h"
//#include "FairGeoNode.h"
//#include "FairRunAna.h"
//#include "FairRuntimeDb.h"

#include <TGeoManager.h>
#include <TGeoPgon.h>
#include <TGeoTube.h>
#include <TMath.h>
#include <TSystem.h>
#include <Riostream.h>
#include "NicaTpcSectorGeo.h"

using std::cout;
using std::endl;

NicaTpcSectorGeo* NicaTpcSectorGeo::fgTpcSec = 0x0;

//__________________________________________________________________________
NicaTpcSectorGeo* NicaTpcSectorGeo::Instance(TString geo)
{
  /// Get pointer to the TPC sector singleton object
  if (!fgTpcSec){
    fgTpcSec = new NicaTpcSectorGeo(geo);

    // Automatic deletion
    std::atexit(DestroyInstance);
  }
  return fgTpcSec;
}

//__________________________________________________________________________
void NicaTpcSectorGeo::Init(TString geo)
{
  /// Initialization of sector parameters
  fDphi = TMath::TwoPi() / fgkNsect;
  fPhi0 = -fDphi / 2.;

  cout << "\n !!!!! ***** ***** ***** !!!!!" << endl;
  //if (sensVol->getShape() == "TUBE") {
  if (geo =="simple") {
    // Old sensitive volume
	  /*
    fNrows[0] = fNrows[1] = 50; 

    Double_t rMin = ((TGeoTube*)shape)->GetRmin();
    Double_t rMax = ((TGeoTube*)shape)->GetRmax();
    cout << " ***** TPC sensitive volume: " << sv->GetName() << " " << shape->ClassName() << " " << rMin << " " << rMax << " " << ((TGeoTube*)shape)->GetDZ() << endl;

    fYsec[0] = 36.; // for consistency with earlier results
    fYsec[1] = fYsec[2] = 96.; // for consistency with earlier results
    fPadH[0] = fPadH[1] = (fYsec[1] - fYsec[0]) / fNrows[0];
*/
  } else if (geo == "full") {
    // New sensitive volume
    //fYsec[0] = params->At(5);
    //fYsec[1] = params->At(6);
    fYsec[0] = 40.3;
    //fYsec[2] = ((TGeoPgon*)shape)->Rmax(0);
    fPadH[0] = 1.2; // approx. pad height in inner ROC region
    fPadH[1] = 1.8; //1.2; // approx. pad height in outer ROC region
    fNrows[0] = 27; //33;
    fNrows[1] = 26; //33;
    Double_t dy = fPadH[0] * fNrows[0] + fPadH[1] * fNrows[1];
    fYsec[2] = fYsec[0] + dy;
    Double_t scale = (fYsec[2] - fYsec[0]) / dy;
    fPadH[0] *= scale;
    fPadH[1] *= scale;
    fYsec[1] = fYsec[0] + fPadH[0] * fNrows[0];
    fPhi0 = 15.0/TMath::RadToDeg();
    // Extract sensitive volume Z-coordinates
    fZmin = 0.4;
    //fZmax = fZmin + 2 * ((TGeoTube*)shape)->GetDZ();
    fZmax = 170.; 
  }

  //fPadH = (fYsec[1] - fYsec[0]) / fNrows;
  cout << " ***** TPC sector params - inner radius: " << fYsec[0] << ", outer radius: " << fYsec[2] 
       << ", boundary: " << fYsec[1] << endl;
  cout << " Number of sectors: " << fgkNsect << ", phi0: " << fPhi0*TMath::RadToDeg() << ", numbers of padrows: " 
       << fNrows[0] << " " << fNrows[1] << ", pad heights: " << fPadH[0] << " " << fPadH[1] << endl; 
  cout << " !!!!! ***** ***** ***** !!!!!" << endl;

  fPadW[0] = fPadW[1] = 0.5; // pad widths
  // Numbers of pads in rows
  //Double_t tan = TMath::Tan(fDphi/2), dead = 1.35; // dead area on one side 
  Double_t tan = TMath::Tan(fDphi/2), dead = 0.15; // dead area on one side 
  fNPadsInRows = new Int_t [NofRows()];
  for (Int_t j = 0; j < fNrows[0]; ++j) 
    fNPadsInRows[j] = Int_t ((tan * (fYsec[0] + j * fPadH[0]) - dead) / fPadW[0]); 
  for (Int_t j = 0; j < fNrows[1]; ++j) 
    fNPadsInRows[j+fNrows[0]] = Int_t ((tan * (fYsec[1] + j * fPadH[1]) - dead) / fPadW[1]); 
}

//__________________________________________________________________________
Int_t NicaTpcSectorGeo::Global2Local(const Double_t *xyzGlob, Double_t *xyzLoc, Int_t iSec0)
{
  /// Transform global coordinates to local (sector)

  Double_t safety = 0.01;
  xyzLoc[2] = xyzGlob[2];
  Int_t iSec = iSec0;
  if (iSec0 < 0) {
    // Find sector No.
    Double_t phGlob = TMath::ATan2 (xyzGlob[1], xyzGlob[0]);
    if (phGlob < 0) phGlob += TMath::TwoPi();
    iSec = Int_t ((phGlob - fPhi0) / fDphi);
    if (iSec == fgkNsect) iSec = 0;
  } else iSec = (iSec0 >> kSectorS) & kSectorM;
  Double_t phSec = iSec * fDphi;
  Double_t cosPh = TMath::Cos(phSec);
  Double_t sinPh = TMath::Sin(phSec);
  Double_t x = xyzGlob[0] * cosPh + xyzGlob[1] * sinPh;
  Double_t y = -xyzGlob[0] * sinPh + xyzGlob[1] * cosPh;
  if (x < fYsec[0] + safety || x > fYsec[2] - safety) return -1; // outside sector in Y
  xyzLoc[0] = y;
  xyzLoc[1] = x - fYsec[0];
  //if (x < fYsec[0] + safety || x > fYsec[1] - safety) return -1; // outside sector in Y
  Int_t row = fNrows[0];
  if (x <= fYsec[1]) row = Int_t (xyzLoc[1] / fPadH[0]);
  else row += Int_t ((x - fYsec[1]) / fPadH[1]);
  return (iSec |= (row << kPadrowS));
}

//__________________________________________________________________________
Int_t NicaTpcSectorGeo::Global2Local(const TVector3 &xyzGlob, TVector3 &xyzLoc, Int_t iSec0)
{
  /// Transform global coordinates to local (sector)

  Double_t xyz1[3], xyz2[3];

  xyzGlob.GetXYZ(xyz1);
  Int_t iSec = Global2Local(xyz1, xyz2, iSec0);
  xyzLoc.SetXYZ(xyz2[0], xyz2[1], xyz2[2]);
  return iSec;
}

//__________________________________________________________________________
void NicaTpcSectorGeo::Local2Global(Int_t iSec, const Double_t *xyzLoc, Double_t *xyzGlob)
{
  /// Transform local coordinates of sector iSec to global

  xyzGlob[2] = xyzLoc[2];
  Double_t phSec = iSec * fDphi;
  Double_t cosPh = TMath::Cos(phSec);
  Double_t sinPh = TMath::Sin(phSec);
  Double_t x = xyzLoc[1] + fYsec[0];
  Double_t y = xyzLoc[0];
  xyzGlob[0] = x * cosPh - y * sinPh;
  xyzGlob[1] = x * sinPh + y * cosPh;
}

//__________________________________________________________________________
void NicaTpcSectorGeo::Local2Global(Int_t iSec, const TVector3 &xyzLoc, TVector3 &xyzGlob)
{
  /// Transform local coordinates of sector iSec to global

  Double_t xyz1[3], xyz2[3];
 
  xyzLoc.GetXYZ(xyz1);
  Local2Global(iSec, xyz1, xyz2);
  xyzGlob.SetXYZ(xyz2[0], xyz2[1], xyz2[2]);
}

//__________________________________________________________________________
TVector2 NicaTpcSectorGeo::LocalPadPosition(Int_t padID)
{
  /// Return local pad position for padID

  Int_t row = PadRow(padID);
  Double_t x = 0.0, y = 0.0;
  if (row < fNrows[0]) y = fPadH[0] * (row + 0.5);
  else y = fYsec[1] - fYsec[0] + fPadH[1] * (row - fNrows[0] + 0.5);
  return TVector2(x,y);
}

//__________________________________________________________________________

void NicaTpcSectorGeo::PadID(Float_t xloc, Float_t yloc, UInt_t &row, UInt_t &pad, Float_t &yNext)
{
  /// Compute row, pad and distance to the nearest row (+ or -)

  row = fNrows[0];
  Double_t lowHeight = fYsec[1] - fYsec[0];
  if (yloc <= lowHeight) {
    row = Int_t (yloc / fPadH[0]);
    yNext = (row + 1) * fPadH[0] - yloc + 0.000001;
    if (yNext > fPadH[0] / 2) yNext -= (fPadH[0] + 0.000002);
    pad = Int_t (xloc / fPadW[0] + fNPadsInRows[row]);
  } else {
    Int_t dRow = Int_t ((yloc - lowHeight) / fPadH[1]);
    row += dRow;
    yNext = (dRow + 1) * fPadH[1] + lowHeight - yloc + 0.000001;
    if (yNext > fPadH[1] / 2) yNext -= (fPadH[1] + 0.000002);
    pad = Int_t (xloc / fPadW[1] + fNPadsInRows[row]);
  }
}

//__________________________________________________________________________
