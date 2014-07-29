//--------------------------------------------------------------------
//
// Description:
//      Class for storing TPC Digits
//
//
// Author List:
//      Sergey Merts          
//
//--------------------------------------------------------------------

#ifndef MPDTPCDIGIT_HH
#define MPDTPCDIGIT_HH

#include <TNamed.h>

class MpdTpcDigit : public TNamed {
    
public:
    MpdTpcDigit();
    MpdTpcDigit(MpdTpcDigit* digit);
    MpdTpcDigit(Int_t ori, Int_t pad, Int_t row, Int_t bin, Int_t sec, Float_t adc);
    virtual ~MpdTpcDigit();
    
    const Int_t   GetOrigin ()  const {return fOrigin;}
    const Int_t   GetPad ()     const {return fPad;}
    const Int_t   GetRow ()     const {return fRow;}
    const Int_t   GetTimeBin () const {return fTimeBin;}
    const Float_t GetAdc ()     const {return fAdc;}
    const Int_t   GetSector ()  const {return fSector;}
    
    void SetOrigin (Int_t ori)  {fOrigin = ori;}
    void SetPad (Int_t pad)     {fPad = pad;}
    void SetRow (Int_t row)     {fRow = row;}
    void SetTimeBin (Int_t bin) {fTimeBin = bin;}
    void SetAdc (Float_t adc)   {fAdc = adc;}
    void SetSector (Int_t sec)  {fSector = sec;}
    
private:    
    Int_t   fPad;
    Int_t   fRow;
    Int_t   fTimeBin;
    Int_t   fSector;
    Float_t fAdc;
    Int_t   fOrigin;
    
    ClassDef(MpdTpcDigit, 1)
};

#endif

//--------------------------------------------------------------
// $Log$
//--------------------------------------------------------------
