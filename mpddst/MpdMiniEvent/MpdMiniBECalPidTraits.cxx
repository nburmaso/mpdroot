//
// MpdMiniBECalPidTraits holds information about ECal-matched tracks
//

// C++ headers
#include <limits>

// ROOT headers
#include "TMath.h"

// PicoDst headers
#include "MpdMiniMessMgr.h"
#include "MpdMiniBECalPidTraits.h"

ClassImp(MpdMiniBECalPidTraits)

//_________________
MpdMiniBECalPidTraits::MpdMiniBECalPidTraits() : 
mTrackIndex(-1) {
  /* emtpy */
}

//_________________
MpdMiniBECalPidTraits::MpdMiniBECalPidTraits(Int_t index) : TObject() {
  mTrackIndex = (index > std::numeric_limits<short>::max()) ? -1 : (Short_t)index;
}

//_________________
MpdMiniBECalPidTraits::MpdMiniBECalPidTraits(const MpdMiniBECalPidTraits &traits) : TObject() {
  mTrackIndex = traits.mTrackIndex;
}

//_________________
MpdMiniBECalPidTraits::~MpdMiniBECalPidTraits() {
  /* empty */
}

//_________________
void MpdMiniBECalPidTraits::Print(const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << "Matched track index = " << mTrackIndex << endm;
}
