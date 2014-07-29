//--------------------------------------------------------------------------
//----                     MpdCpcHit                                    ----
//--------------------------------------------------------------------------

#include <iostream>
using namespace std;

#include "MpdCpcHit.h"


// -----   Default constructor   -------------------------------------------

MpdCpcHit::MpdCpcHit()
{
  fFlag = 1;
}


// -----   Standard constructor   ------------------------------------------

MpdCpcHit::MpdCpcHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index, Int_t flag)
: FairHit(detID, pos, dpos, index)
{
    fFlag = flag;
}

// -----   Constructor without flag  ------------------------------------------

MpdCpcHit::MpdCpcHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index)
: FairHit(detID, pos, dpos, index)
{
}

// -----   Destructor   ----------------------------------------------------
MpdCpcHit::~MpdCpcHit() { }


// -----  Print  -----------------------------------------------------------
void MpdCpcHit::Print(const Option_t* opt) const
{
  cout << "-I- MpdCpcHit" << endl;
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


ClassImp(MpdCpcHit)
