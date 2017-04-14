#ifndef MPDEMCHIT_H
#define MPDEMCHIT_H 1

#include "FairHit.h"

class MpdEmcHit : public FairHit {
public:

    /** Default constructor **/
    MpdEmcHit();

    /** Constructor with hit parameters (1)**/
    MpdEmcHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex, Int_t flag);

    /** Constructor with hit parameters (2) [not the flag]**/
    MpdEmcHit(Int_t detectorID, TVector3 pos, TVector3 dpos, Int_t refIndex);

    MpdEmcHit(UInt_t sec, UInt_t row, UInt_t supMod, UInt_t mod, Float_t e);

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

    Int_t GetSupMod() const {
        return fSupModId;
    };

    Int_t GetRow() const {
        return fRowId;
    };

    Float_t GetE() const {
        return fE;
    };

    Float_t GetZcenter() const {
        return fZCenter;
    }

    Float_t GetPhiCenter() const {
        return fPhiCenter;
    }

    Int_t GetTrackId() const {
        return fTrackID;
    }

    Int_t GetPdg() const {
        return fPDG;
    }

    Int_t GetNumTracks() const {
        return fNumTracks;
    }

    Float_t GetZ() const {
        return fZ;
    }

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

    void SetTrackId(Int_t id) {
        fTrackID = id;
    };

    void SetPdg(Int_t pdg) {
        fPDG = pdg;
    };

    void SetNumTracks(Int_t n) {
        fNumTracks = n;
    };

    void SetZCenter(Float_t z) {
        fZCenter = z;
    };

    void SetPhiCenter(Float_t phi) {
        fPhiCenter = phi;
    };

    void SetSecId(UInt_t id) {
        fSecId = id;
    };

    void SetRowId(UInt_t row) {
        fRowId = row;
    };

    void SetSupModId(UInt_t id) {
        fSupModId = id;
    };

    void SetModId(UInt_t id) {
        fModId = id;
    };

protected:

    // base digi-part
    UInt_t fSecId;
    UInt_t fRowId;
    UInt_t fModId;
    UInt_t fSupModId; // super module = square of modules (3x3)
    Float_t fE; //energy

    //extra reco-part
    Int_t fTrackID; // -1 if more than one track in module
    Int_t fFlag; // Flag for general purposes [TDC, event tagging...]
    Int_t fPDG; // code of particle if only one track presented in module 
    UInt_t fNumTracks; // number of tracks, which have contribution in module
    Float_t fZCenter; // z-coordinate of the center of module
    Float_t fPhiCenter; // phi-angle of the center of module

    ClassDef(MpdEmcHit, 1)

};


#endif
