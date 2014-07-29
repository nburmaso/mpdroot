//--------------------------------------------------------------------------
//----                     MpdSftHit                                    ----
//----                Created Everard Cordier 14/09/05                  ----
//----                Modified by D. Gonzalez-Diaz 06/09/06             ----
//--------------------------------------------------------------------------

#include <iostream>
using namespace std;

#include "MpdSftHit.h"


// -----   Default constructor   -------------------------------------------

MpdSftHit::MpdSftHit()
{
  fTime = 0.;
  fFlag = 1;
}


// -----   Standard constructor   ------------------------------------------

MpdSftHit::MpdSftHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index, Double_t time, Int_t flag)
: FairHit(detID, pos, dpos, index)
{
    fTime = time;
    fFlag = flag;
}

// -----   Constructor without flag  ------------------------------------------

MpdSftHit::MpdSftHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index, Double_t time)
: FairHit(detID, pos, dpos, index)
{
    fTime = time;
}

// -----   Destructor   ----------------------------------------------------
MpdSftHit::~MpdSftHit() { }


// -----  Print  -----------------------------------------------------------
void MpdSftHit::Print(const Option_t* opt) const
{
  cout << "-I- MpdSftHit" << endl;
  cout << "    DetectorID: " << fDetectorID << endl;
  cout << "    Position: (" << fX
       << ", " << fY
       << ", " << fZ << ") cm"
       << endl;
  cout << "    Position error: (" << fDx
       << ", " << fDy
       << ", " << fDz << ") cm"
       << endl;
  cout << "    Time: " << fTime << " ns" 
       << endl;
  cout << "    Flag: " << fFlag
       << endl;
}
// -------------------------------------------------------------------------


ClassImp(MpdSftHit)
