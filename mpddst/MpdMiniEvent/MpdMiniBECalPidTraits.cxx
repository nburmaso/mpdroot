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
MpdMiniBECalPidTraits::MpdMiniBECalPidTraits() : TObject(),
  mTrackIndex(-1),
  mBecalId(-9999), mBecalAdc0(-9999), mBecalE0(-9999), mBecalE(-9999),
  mBecalZDist(-9999), mBecalPhiDist(-9999),
  mBtowId(-9999), mBtowId23(std::numeric_limits<unsigned char>::max()),
  mBtowE(-9999), mBtowE2(-9999), mBtowE3(-9999),
  mBtowEtaDist(-9999), mBtowPhiDist(-9999) {
  /* emtpy */
}

//_________________
MpdMiniBECalPidTraits::MpdMiniBECalPidTraits(Int_t index, Int_t id, Int_t adc0, const Float_t* e,
					     const Float_t* dist, const Int_t* ntow) : TObject() {

  mTrackIndex = (index > std::numeric_limits<short>::max()) ? -1 : (Short_t)index;

  auto getConstrainedShort = [](float x) {
    return ( ( TMath::Abs(x) >= std::numeric_limits<short>::max() ) ?
	     std::numeric_limits<short>::max() : (short)(TMath::Nint(x)) );
  };

  mBecalId       = (id > std::numeric_limits<short>::max()) ? -1 : (Short_t)id;
  mBecalAdc0     = ( (adc0 > std::numeric_limits<unsigned short>::max()) ?
		    std::numeric_limits<unsigned short>::max() : (UShort_t)adc0 );
  mBecalE0       = getConstrainedShort(e[0] * 1000.);
  mBecalE        = getConstrainedShort(e[1] * 1000.);
  mBecalZDist    = getConstrainedShort(dist[0] * 100.);
  mBecalPhiDist  = getConstrainedShort(dist[1] * 10000.);
  mBtowId       = (ntow[0] <= 0 || ntow[0] > 4800) ? -1 : (Short_t)ntow[0];

  // Logic: If at least one closest to the mactched tower was
  // found than we check the second one. The 1st and the 2nd
  // digits of mBtowId23 represent Ids of the 1st and the 2nd highest
  // towers that are the closest to the track-matched one, respectively.
  if (ntow[1] < 0 || ntow[1] >= 9) {
    if (!(ntow[2] < 0 || ntow[2] >= 9))  { // If 2 towers were found
      mBtowId23 = (Char_t)(ntow[1] * 10 + ntow[2]);
    }
    else {                                 // If only 1 tower was found
      mBtowId23 = (Char_t)(ntow[1] * 10 + 9); 
    }
  }
  else { // If none of the towers with energy>0.2 GeV were found near the matched tower
    mBtowId23 = 99;
  }

  mBtowE        = getConstrainedShort(e[2] * 1000.);
  mBtowE2       = getConstrainedShort(e[3] * 1000.);
  mBtowE3       = getConstrainedShort(e[4] * 1000.);
  mBtowEtaDist  = getConstrainedShort(dist[2] * 10000.);
  mBtowPhiDist  = getConstrainedShort(dist[3] * 10000.);
}

//_________________
MpdMiniBECalPidTraits::MpdMiniBECalPidTraits(const MpdMiniBECalPidTraits &traits) : TObject() {
  mTrackIndex = traits.mTrackIndex;
  mBecalId = traits.mBecalId;
  mBecalAdc0 = traits.mBecalAdc0;
  mBecalE0 = traits.mBecalE0;
  mBecalE = traits.mBecalE;
  mBecalZDist = traits.mBecalZDist;
  mBecalPhiDist = traits.mBecalPhiDist;

  mBtowId = traits.mBtowId;
  mBtowId23 = traits.mBtowId23;
  mBtowE = traits.mBtowE;
  mBtowE2 = traits.mBtowE2;
  mBtowE3 = traits.mBtowE3;
  mBtowEtaDist = traits.mBtowEtaDist;
  mBtowPhiDist = traits.mBtowPhiDist;
}

//_________________
MpdMiniBECalPidTraits::~MpdMiniBECalPidTraits() {
  /* empty */
}

//_________________
void MpdMiniBECalPidTraits::Print(const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << "Matched track index = " << mTrackIndex << endm;
  LOG_INFO << " BEMC Id = " << bemcId() << " BTOW Adc0 = " << bemcAdc0()
	   << " bemc E0 = " << bemcE0() << " e = " << bemcE() << endm;
  LOG_INFO << " BEMC distz = " << bemcZDist() << " distphi = " << bemcPhiDist() << endm;
  LOG_INFO << " BTOW Id = " << btowId() << " tower Id 2/3 = " << btowId2() << " " << btowId3() << endm;
  LOG_INFO << " BTOW energy = " << btowE() << " " << btowE2() << " " << btowE3() << endm;
  LOG_INFO << " BTOW position to center = " << btowEtaDist() << " " << btowPhiDist() << endm;
}

//_________________
void MpdMiniBECalPidTraits::setEnergy(Float_t energy[5]) {
  auto getConstrainedShort = [](float x) {
    return fabs(x) >= std::numeric_limits<short>::max() ?
    std::numeric_limits<short>::max() : (short)(TMath::Nint(x));
  };
  mBecalE0 = getConstrainedShort(energy[0] * 1000.);
  mBecalE = getConstrainedShort(energy[1] * 1000.);
  mBtowE = getConstrainedShort(energy[2] * 1000.);
  mBtowE2 = getConstrainedShort(energy[3] * 1000.);
  mBtowE3 = getConstrainedShort(energy[4] * 1000.);  
}

//_________________
void MpdMiniBECalPidTraits::setDistances(Float_t dist[4]) {
  auto getConstrainedShort = [](float x) {
    return fabs(x) >= std::numeric_limits<short>::max() ?
    std::numeric_limits<short>::max() : (short)(TMath::Nint(x));
  };
  mBecalZDist = getConstrainedShort(dist[0] * 100.);
  mBecalPhiDist = getConstrainedShort(dist[1] * 10000.);
  mBtowEtaDist = getConstrainedShort(dist[2] * 10000.);
  mBtowPhiDist = getConstrainedShort(dist[3] * 10000.);
}


//_________________
void MpdMiniBECalPidTraits::setNTOW(Int_t ntow[3]) {
  mBtowId = (ntow[0] <= 0 || ntow[0] > 4800) ? -1 : (Short_t)ntow[0];
  // Logic: If at least one closest to the mactched tower was
  // found than we check the second one. The 1st and the 2nd
  // digits of mBtowId23 represent Ids of the 1st and the 2nd highest
  // towers that are the closest to the track-matched one, respectively.
  if (ntow[1] < 0 || ntow[1] >= 9) {
    if (!(ntow[2] < 0 || ntow[2] >= 9))  { // If 2 towers were found
      mBtowId23 = (Char_t)(ntow[1] * 10 + ntow[2]);
    }
    else {                                 // If only 1 tower was found
      mBtowId23 = (Char_t)(ntow[1] * 10 + 9); 
    }
  }
  else { // If none of the towers with energy>0.2 GeV were found near the matched tower
    mBtowId23 = 99;
  }  
}
