//--------------------------------------------------------------------------
//-----                    MpdStrawECTHit                           -----
//-----                Created 8/12/09  by A. Zinchenko                -----
//--------------------------------------------------------------------------

/**  MpdStrawECTHit.cxx
 *@author A.Zinchenko <Alexander.Zinchenko@jinr.ru>
 **
 ** MPD End-Cap Tracker (ECT) hit
 **/

#include <iostream>
using namespace std;

#include "MpdStrawECTHit.h"


// -----   Default constructor   -------------------------------------------

MpdStrawECTHit::MpdStrawECTHit()
  : FairHit(),
    fFlag(0),
    fNofDim(1),
    fPhi(0.)
{
  fMeas[1] = fError[1] = 0.;
}


// -----   Standard constructor   ------------------------------------------

MpdStrawECTHit::MpdStrawECTHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index, Int_t flag)
  : FairHit(detID, pos, dpos, index),
    fFlag(flag),
    fNofDim(1),
    fPhi(0.)
{
  fMeas[1] = fError[1] = 0.;
}

// -----   Constructor without flag  ------------------------------------------

MpdStrawECTHit::MpdStrawECTHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index)
  : FairHit(detID, pos, dpos, index),
    fFlag(0),
    fNofDim(1),
    fPhi(0.)
{
  fMeas[1] = fError[1] = 0.;
}

// -----   Destructor   ----------------------------------------------------
MpdStrawECTHit::~MpdStrawECTHit() { }


// -----  Print  -----------------------------------------------------------
void MpdStrawECTHit::Print(const Option_t* opt) const
{
  cout << "-I- MpdStrawECTHit" << endl;
  cout << "    DetectorID: " << fDetectorID << endl;
  cout << "    Position: (" << fX
       << ", " << fY
       << ", " << fZ << ") cm"
       << endl;
  cout << "    Position error: (" << fDx
       << ", " << fDy
       << ", " << fDz << ") cm"
       << endl;
  cout << "    Flag: " << fFlag
       << endl;
}
// -------------------------------------------------------------------------

//__________________________________________________________________________
void MpdStrawECTHit::SetIndex(Int_t indx)
{
  /// Add point index

  Int_t size = fIndex.GetSize();
  fIndex.Set (size + 1);
  fIndex[size] = indx;
}
// -------------------------------------------------------------------------

//________________________________________________________________
Int_t MpdStrawECTHit::Compare(const TObject* hit) const
{
  /// "Compare" function to sort in ascending order in abs(Z)

  MpdStrawECTHit *kHit = (MpdStrawECTHit*) hit;
  if (GetLayer() < kHit->GetLayer()) return -1;
  else if (GetLayer() > kHit->GetLayer()) return 1;
  else {
    if (TMath::Abs(fZ) < TMath::Abs(kHit->GetZ())) return -1;
    else if (TMath::Abs(fZ) > TMath::Abs(kHit->GetZ())) return 1;
    else return 0;
  }
}
// -------------------------------------------------------------------------

ClassImp(MpdStrawECTHit)
