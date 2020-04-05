//
// MpdMiniBECalHit stores ECal tower information
//

// C++ headers
#include <limits>

// ROOT headers
#include "TString.h"

// PicoDst headers
#include "MpdMiniMessMgr.h"
#include "MpdMiniBECalHit.h"

ClassImp(MpdMiniBECalHit)

//_________________
MpdMiniBECalHit::MpdMiniBECalHit(): TObject(),
  fSectorId(0), fRowId(0), fModuleId(0), fEnergy(0), fTime(0),
  fTrackId(0), fFlag(0), fNumOfTracks(0),
  fRhoCenter(0), fZCenter(0), fPhiCenter(0), fThetaCenter(0) {
  /* empty */
}

//_________________
MpdMiniBECalHit::MpdMiniBECalHit(const MpdMiniBECalHit &hit) : TObject(),
  fSectorId( hit.fSectorId ),
  fRowId( hit.fRowId ),
  fModuleId( hit.fModuleId ),
  fEnergy( hit.fEnergy ),
  fTime( hit.fTime ),
  fTrackId( hit.fTrackId ),
  fFlag( hit.fFlag ),
  fNumOfTracks( hit.fNumOfTracks ),
  fRhoCenter( hit.fRhoCenter ),
  fZCenter( hit.fZCenter ),
  fPhiCenter( hit.fPhiCenter ),
  fThetaCenter( hit.fThetaCenter ) {
  // Copy constructor
}

//_________________
MpdMiniBECalHit::~MpdMiniBECalHit() {
  /* empty */
}

//__________________
void MpdMiniBECalHit::Print(const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << " s/r/m " << Form( "%d/%d/%d", sector(), row(), module() )
           << " energy/time: " << Form( "%3.2f/%6.1f", energy(),time() )
           << " trkId/flag/ntrk: " << Form( "%d/%d/%d", trackId(), flag(), numberOfTracks() )
           << " rho/z/phi/theta: " << Form( "%4.1f/%4.1f/%4.1f/%4.1f",
                                            rhoCenter(), zCenter(), phiCenter(), thetaCenter() )
           << endm;
}

//_________________
void MpdMiniBECalHit::setTrackId(Int_t id) {
  fTrackId = (id > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() 
          : (Short_t) id;
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
  if ( ntrk > std::numeric_limits<unsigned char>::max() ) {
    fNumOfTracks = std::numeric_limits<unsigned char>::max();
  }
  else if ( ntrk > std::numeric_limits<unsigned char>::min() ) {
    // Next line is not a mistake. It will also indicate a bad tower.
    fNumOfTracks = std::numeric_limits<unsigned char>::max();
  }
  else {
    fNumOfTracks = (UChar_t)ntrk;
  }
}
