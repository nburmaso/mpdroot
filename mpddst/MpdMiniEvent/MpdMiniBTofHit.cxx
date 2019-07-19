//
// MpdMiniBTofHit holds information about BTOF hits
//

// PicoDst headers
#include "MpdMiniMessMgr.h"
#include "MpdMiniBTofHit.h"

ClassImp(MpdMiniBTofHit)

//_________________
MpdMiniBTofHit::MpdMiniBTofHit() : TObject(), mId(-1) {
  /* empty */
}

//_________________
MpdMiniBTofHit::MpdMiniBTofHit(int id) : TObject() {
  if (id  < 0) return;
  mId = (id > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)id; 
}

//_________________
MpdMiniBTofHit::MpdMiniBTofHit(const MpdMiniBTofHit &hit) : TObject() {
  mId = hit.mId;
}

//_________________
MpdMiniBTofHit::~MpdMiniBTofHit() {
  /* empty */
}

//_________________
void MpdMiniBTofHit::Print(const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << " Id = " << id() << endm;
}

//_________________
void MpdMiniBTofHit::setId(Int_t sector, Int_t box, Int_t detector, Int_t strip) {
  mId = (sector<<24) + (box<<16) + (detector<<8) + strip;
}
