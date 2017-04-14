//--------------------------------------------------------------------------
//----                     MpdEmcHit                                    ----
//--------------------------------------------------------------------------

#include <iostream>
#include "MpdEmcHit.h"

using namespace std;


// -----   Default constructor   -------------------------------------------

MpdEmcHit::MpdEmcHit() :
fSecId(-1),
fRowId(-1),
fModId(-1),
fSupModId(-1),
fE(-1.0),
fTrackID(-1),
fFlag(0),
fPDG(0),
fNumTracks(0),
fZCenter(0.0),
fPhiCenter(0.0) {
}


// -----   Standard constructor   ------------------------------------------

MpdEmcHit::MpdEmcHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index, Int_t flag)
: FairHit(detID, pos, dpos, index) {
    fFlag = flag;
}

// -----   Constructor without flag  ------------------------------------------

MpdEmcHit::MpdEmcHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index)
: FairHit(detID, pos, dpos, index) {
}

MpdEmcHit::MpdEmcHit(UInt_t sec, UInt_t row, UInt_t supMod, UInt_t mod, Float_t e) :
fSecId(sec),
fRowId(row),
fModId(mod),
fSupModId(supMod),
fE(e),
fTrackID(-1),
fFlag(kFALSE),
fPDG(0),
fNumTracks(0) {
}

// -----   Destructor   ----------------------------------------------------

MpdEmcHit::~MpdEmcHit() {
}

// -----  Print  -----------------------------------------------------------

void MpdEmcHit::Print(const Option_t* opt) const {
    cout << "MpdEmcHit: " << endl;
    cout << "\tSec: " << fSecId << "   Row: " << fRowId << "   Super module: " << fSupModId << "   Module:" << fModId << endl;
    cout << "\tDeposited energy: " << fE << "   ZCenter: " << fZCenter << "   PhiCenter: " << fPhiCenter << endl;
    cout << "\tNumber of tracks in module: " << fNumTracks << endl;
    if (fNumTracks == 1) cout << "PDG code: " << fPDG << "   Track ID: " << fTrackID << endl;

}
// -------------------------------------------------------------------------


ClassImp(MpdEmcHit)
