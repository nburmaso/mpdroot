//--------------------------------------------------------------------------
//----                     MpdFfdHit                                    ----
//--------------------------------------------------------------------------

#include <iostream>
using namespace std;

#include "MpdFfdHit.h"


// -----   Default constructor   -------------------------------------------

MpdFfdHit::MpdFfdHit()
{
  fFlag = 1;
}


// -----   Standard constructor   ------------------------------------------

MpdFfdHit::MpdFfdHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index, Int_t flag)
: FairHit(detID, pos, dpos, index)
{
    fFlag = flag;
}

// -----   Constructor without flag  ------------------------------------------

MpdFfdHit::MpdFfdHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index)
: FairHit(detID, pos, dpos, index)
{
}

// -----   Destructor   ----------------------------------------------------
MpdFfdHit::~MpdFfdHit() { }


// -----  Print  -----------------------------------------------------------
void MpdFfdHit::Print(const Option_t* opt) const
{
  cout << "-I- MpdFfdHit" << endl;
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


ClassImp(MpdFfdHit)
