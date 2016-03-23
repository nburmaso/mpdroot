//--------------------------------------------------------------------------
//----                     MpdEmcHit                                    ----
//--------------------------------------------------------------------------

#include <iostream>
using namespace std;

#include "MpdEmcHit.h"


// -----   Default constructor   -------------------------------------------

MpdEmcHit::MpdEmcHit()
{
  fFlag = 1;
}


// -----   Standard constructor   ------------------------------------------

MpdEmcHit::MpdEmcHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index, Int_t flag)
: FairHit(detID, pos, dpos, index)
{
    fFlag = flag;
}

// -----   Constructor without flag  ------------------------------------------

MpdEmcHit::MpdEmcHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index)
: FairHit(detID, pos, dpos, index)
{
}


MpdEmcHit::MpdEmcHit(UInt_t sec, UInt_t row, UInt_t mod, UInt_t E) {
    fSecId = sec;
    fModId = mod;
    fRowId = row;
    fE = E;
}

// -----   Destructor   ----------------------------------------------------
MpdEmcHit::~MpdEmcHit() { }


// -----  Print  -----------------------------------------------------------
void MpdEmcHit::Print(const Option_t* opt) const
{
  cout << "-I- MpdEmcHit" << endl;
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


ClassImp(MpdEmcHit)
