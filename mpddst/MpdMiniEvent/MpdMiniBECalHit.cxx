//
// MpdMiniBECalHit stores ECal tower information
//

// C++ headers
#include <limits>

// ROOT headers
#include "TString.h"

// MiniDst headers
#include "MpdMiniMessMgr.h"
#include "MpdMiniBECalHit.h"

ClassImp(MpdMiniBECalHit)

//_________________
MpdMiniBECalHit::MpdMiniBECalHit(): TObject(),
  mBEcalMatchFlag(kFALSE), fEnergy(0), fTime(0),
  fFlag(0), fNumOfTracks(0), fX(-1.), fY(-1.), fZ(-1.),
  fdPhi(-1.), fdZ(-1.) {
  /* empty */
  fCellIds.resize(0);
}

//_________________
MpdMiniBECalHit::MpdMiniBECalHit(const MpdMiniBECalHit &hit) : TObject(),  
							       fCellIds( hit.fCellIds),
  mBEcalMatchFlag(hit.mBEcalMatchFlag),
  fEnergy( hit.fEnergy ),
  fTime( hit.fTime ), 
  fFlag( hit.fFlag ),
  fNumOfTracks( hit.fNumOfTracks ),
  fX(hit.fX), 
  fY(hit.fY), 
  fZ(hit.fZ),
  fdPhi(hit.fdPhi), 
  fdZ(hit.fdZ) {
  // Copy constructor
}

//_________________
MpdMiniBECalHit::~MpdMiniBECalHit() {
  /* empty */
}

//__________________
void MpdMiniBECalHit::Print(const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << " cellIds.size(): " << Form( "%lu", fCellIds.size())
           << " energy/time: " << Form( "%3.2f/%6.1f", energy(),time() ) << endm;
}

//_________________
void MpdMiniBECalHit::setFlag(Int_t flag) {
  if ( flag > std::numeric_limits<char>::max() ) {
    fFlag = std::numeric_limits<char>::max();
  }
  else if ( flag < std::numeric_limits<char>::min() ) {
    fFlag = std::numeric_limits<char>::min();
  }
  else {
    fFlag = (Char_t)flag;
  }
}

//_________________
void MpdMiniBECalHit::setNumberOfTracks(Int_t ntrk) {
  if ( ntrk > std::numeric_limits<UChar_t>::max() ) {
    fNumOfTracks = std::numeric_limits<UChar_t>::max();
  }
  else if ( ntrk > std::numeric_limits<UChar_t>::min() ) {
    // Next line is not a mistake. It will also indicate a bad tower.
    fNumOfTracks = (UChar_t)ntrk;
  }
  else {
    fNumOfTracks = 0;
  }
}
