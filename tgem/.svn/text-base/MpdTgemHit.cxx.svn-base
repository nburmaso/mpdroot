//--------------------------------------------------------------------------
//-----                    MpdTgemHit                           -----
//-----                Created 8/12/09  by A. Zinchenko                -----
//--------------------------------------------------------------------------

/**  MpdTgemHit.cxx
 *@author A.Zinchenko <Alexander.Zinchenko@jinr.ru>
 **
 ** MPD End-Cap Tracker (ECT) hit
 **/
 
 // modified by A. Kuznetsov for Tgem
 

#include <iostream>
using namespace std;

#include "MpdTgemHit.h"


// -----   Default constructor   -------------------------------------------

MpdTgemHit::MpdTgemHit()
  : FairHit(),
    fFlag(0),
    fNofDim(1),
    fPhi(0.)
{
  fMeas[1] = fError[1] = 0.;
}


// -----   Standard constructor   ------------------------------------------

MpdTgemHit::MpdTgemHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index, Int_t flag)
  : FairHit(detID, pos, dpos, index),
    fFlag(flag),
    fNofDim(1),
    fPhi(0.)
{
  fMeas[1] = fError[1] = 0.;
}

// -----   Constructor without flag  ------------------------------------------

MpdTgemHit::MpdTgemHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index)
  : FairHit(detID, pos, dpos, index),
    fFlag(0),
    fNofDim(1),
    fPhi(0.)
{
  fMeas[1] = fError[1] = 0.;
}

// -----   Destructor   ----------------------------------------------------
MpdTgemHit::~MpdTgemHit() { }


// -----  Print  -----------------------------------------------------------
void MpdTgemHit::Print(const Option_t* opt) const
{
  cout << "-I- MpdTgemHit" << endl;
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
void MpdTgemHit::SetIndex(Int_t indx)
{
  /// Add point index

  Int_t size = fIndex.GetSize();
  fIndex.Set (size + 1);
  fIndex[size] = indx;
}
// -------------------------------------------------------------------------

//________________________________________________________________
Int_t MpdTgemHit::Compare(const TObject* hit) const
{
  /// "Compare" function to sort in ascending order in abs(Z)

  MpdTgemHit *kHit = (MpdTgemHit*) hit;
  if (GetLayer() < kHit->GetLayer()) return -1;
  else if (GetLayer() > kHit->GetLayer()) return 1;
  else {
    if (TMath::Abs(fZ) < TMath::Abs(kHit->GetZ())) return -1;
    else if (TMath::Abs(fZ) > TMath::Abs(kHit->GetZ())) return 1;
    else return 0;
  }
}
// -------------------------------------------------------------------------

ClassImp(MpdTgemHit)
