/// \class MpdTpcHit
/// 
/// Hit in MPD TPC
/// \author Alexander Zinchenko (LHEP, JINR, Dubna) - extension of TpcHit

//#include "TpcCommon.h"
#include "MpdTpcHit.h"

//---------------------------------------------------------------------------
MpdTpcHit::MpdTpcHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index)
  : FairHit(detID, pos, dpos, index),
    fiPad(-1), fiBin(-1), fLayer(-1), fQ(0), fStep(0), fLength(0), fLocalX(0), fLocalY(0), fLocalZ(0)
{

}

//---------------------------------------------------------------------------
Int_t MpdTpcHit::Compare(const TObject* hit) const
{
  /// "Compare" function to sort in descending order in radius

  MpdTpcHit *tpcHit = (MpdTpcHit*) hit;
  if (fLayer < tpcHit->GetLayer()) return 1;
  else if (fLayer > tpcHit->GetLayer()) return -1;
  else {
    if (GetR() < tpcHit->GetR()) return 1;
    else if (GetR() > tpcHit->GetR()) return -1;
    else return 0;
  }
}
//---------------------------------------------------------------------------

ClassImp(MpdTpcHit)
