//
// MpdMiniBTofPidTraits keeps information about tracks that matched BTOF
//

// C++ headers
#include <limits>
#include <cmath>

// ROOT headers
#include "TMath.h"

// PicoDst headers
#include "MpdMiniMessMgr.h"
#include "MpdMiniBTofPidTraits.h"

ClassImp(MpdMiniBTofPidTraits)

//_________________
MpdMiniBTofPidTraits::MpdMiniBTofPidTraits() : TObject(),
  mTrackIndex(-1), mHitIndex(-1),
  fMass2(0.), fLength(0.), mBTofBeta(0) {
  /* empty */
}

//_________________
MpdMiniBTofPidTraits::MpdMiniBTofPidTraits(const MpdMiniBTofPidTraits &traits) : TObject() {
  mTrackIndex = traits.mTrackIndex;  
  mHitIndex = traits.mHitIndex;
  mBTofBeta = traits.mBTofBeta;
  fMass2 = traits.fMass2;
  fLength = traits.fLength; 
}

//_________________
MpdMiniBTofPidTraits::~MpdMiniBTofPidTraits() {
  /* empty */
}

//_________________
void MpdMiniBTofPidTraits::setBeta(Float_t beta) {
  if( beta <= 0) {
    mBTofBeta = 0;
  }
  else {
    mBTofBeta = ( (beta * 20000.) > std::numeric_limits<unsigned short>::max() ?
		  std::numeric_limits<unsigned short>::max() :
		  (UShort_t)( TMath::Nint( beta * 20000. ) ) );
  }
}

//_________________
void MpdMiniBTofPidTraits::Print(const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << " Matched track index = " << mTrackIndex << " Matched hit index = " << mHitIndex << endm;
  LOG_INFO << " beta = " << btofBeta() << " M^2 = " << fMass2 << " Length = " << fLength << endm;
}

