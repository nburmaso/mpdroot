#ifndef MPDEMCHIT_H
#define MPDEMCHIT_H

#include "FairHit.h"

class MpdEmcHit : public FairHit {
public:

    /** Default constructor **/
    MpdEmcHit();

    /** Constructor with hit parameters (1)**/
    MpdEmcHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex, Int_t flag);

    /** Constructor with hit parameters (2) [not the flag]**/
    MpdEmcHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex);

    MpdEmcHit(UInt_t mod, UInt_t row, UInt_t dig, UInt_t E);

    /** Destructor **/
    virtual ~MpdEmcHit();

    void Print(const Option_t* opt = 0) const;

    /** Accessors **/
    Int_t GetFlag() const {
        return fFlag;
    };

    Int_t GetSec() const {
        return fSecId;
    };

    Int_t GetMod() const {
        return fModId;
    };

    Int_t GetRow() const {
        return fRowId;
    };

    Float_t GetE() const {
        return fE;
    };

    /** Modifiers **/
    void SetFlag(Int_t flag) {
        fFlag = flag;
    };

    void SetEnergy(Float_t e) {
        fE = e;
    };

    void IncreaseEnergy(Float_t e) {
        fE += e;
    };

protected:

    Int_t fTrackID;
    Int_t fFlag; // Flag for general purposes [TDC, event tagging...]
    UInt_t fSecId;
    UInt_t fRowId;
    UInt_t fModId;
    Float_t fE; //energy

    ClassDef(MpdEmcHit, 1)

};


#endif
