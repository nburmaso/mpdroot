//--------------------------------------------------------------------------
//----                     MpdFsaHit                                    ----
//--------------------------------------------------------------------------

#include <iostream>
using namespace std;

#include "MpdFsaHit.h"


// -----   Default constructor   -------------------------------------------

MpdFsaHit::MpdFsaHit()
{
  fFlag = 1;
}


// -----   Standard constructor   ------------------------------------------

MpdFsaHit::MpdFsaHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index, Int_t flag)
: FairHit(detID, pos, dpos, index)
{
    fFlag = flag;
}

// -----   Constructor without flag  ------------------------------------------

MpdFsaHit::MpdFsaHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index)
: FairHit(detID, pos, dpos, index)
{
}

// -----   Destructor   ----------------------------------------------------
MpdFsaHit::~MpdFsaHit() { }


// -----  Print  -----------------------------------------------------------
void MpdFsaHit::Print(const Option_t* opt) const
{
  cout << "-I- MpdFsaHit" << endl;
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


ClassImp(MpdFsaHit)
