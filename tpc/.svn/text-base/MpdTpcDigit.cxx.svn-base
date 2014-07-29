#include "MpdTpcDigit.h"

ClassImp(MpdTpcDigit)

MpdTpcDigit::MpdTpcDigit() : fRow(-1), fPad(-1), fOrigin(-1), fSector(-1), fAdc(-1.0), fTimeBin(-1) {}

//......................................................................

MpdTpcDigit::MpdTpcDigit(Int_t ori, Int_t pad, Int_t row, Int_t bin, Int_t sec, Float_t adc) : 
fRow(row), fPad(pad), fOrigin(ori), fSector(sec), fAdc(adc), fTimeBin(bin) {}

//......................................................................

MpdTpcDigit::MpdTpcDigit(MpdTpcDigit* digit) : 
fRow(digit->GetRow()), fPad(digit->GetPad()), fOrigin(digit->GetOrigin()), fSector(digit->GetSector()), fAdc(digit->GetAdc()), fTimeBin(digit->GetTimeBin()) {}

//......................................................................

MpdTpcDigit::~MpdTpcDigit() {}

//......................................................................
