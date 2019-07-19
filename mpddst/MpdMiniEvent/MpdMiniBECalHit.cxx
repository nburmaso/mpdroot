//
// MpdMiniBECalHit stores ECal tower information
//

// C++ headers
#include <limits>

// ROOT headers
#include "TMath.h"

// PicoDst headers
#include "MpdMiniMessMgr.h"
#include "MpdMiniBECalHit.h"

ClassImp(MpdMiniBECalHit)

//_________________
MpdMiniBECalHit::MpdMiniBECalHit(): TObject(), mAdc(0), mE(-9000) {
  /* empty */
}

//_________________
MpdMiniBECalHit::MpdMiniBECalHit(Int_t adc, Float_t e) : TObject() {

  if (adc < 0) return;
  mAdc  = (adc > std::numeric_limits<unsigned short>::max()) ? 
    std::numeric_limits<unsigned short>::max() : (UShort_t)adc;

  mE = ( fabs(e * 1000.) > std::numeric_limits<short>::max() ?
	 ( (e > 0.) ? std::numeric_limits<short>::max() :
	   std::numeric_limits<short>::min() ) :
	 (Short_t)( TMath::Nint(e * 1000.) ) );
}

//_________________
MpdMiniBECalHit::MpdMiniBECalHit(const MpdMiniBECalHit &hit) : TObject() {
  mAdc = hit.mAdc;
  mE = hit.mE;
}

//_________________
MpdMiniBECalHit::~MpdMiniBECalHit() {
  /* empty */
}

//_________________
void MpdMiniBECalHit::Print(const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << " Adc = " << adc() << " Energy = " << energy() << endm;
}

//_________________
Bool_t MpdMiniBECalHit::isBad() const {
  if( energy()<=-2. && mAdc==0) {
    return kTRUE;
  }
  else {
    return kFALSE;
  }
}

//_________________
void MpdMiniBECalHit::setEnergy(Float_t energy) {
  mE = ( fabs(energy * 1000) > std::numeric_limits<short>::max() ?
	 ( (energy > 0) ? std::numeric_limits<short>::max() :
	   std::numeric_limits<short>::min() ) :
	 (Short_t)( TMath::Nint(energy * 1000) ) );
}

//_________________
void MpdMiniBECalHit::setAdc(Int_t adc) {
  if( adc<0 ) {
    mAdc = 0;
  }
  else {
    mAdc = (adc > std::numeric_limits<unsigned short>::max()) ?
      std::numeric_limits<unsigned short>::max() : (UShort_t)adc;
  }
}
