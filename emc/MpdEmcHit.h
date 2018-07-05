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

    MpdEmcHit(UInt_t sec, UInt_t row, UInt_t supMod, UInt_t mod, Float_t e, Float_t time);

    virtual ~MpdEmcHit();

    void Print(const Option_t* opt = 0) const;

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

    Float_t GetTime() const {
        return fTime;
    };

    Int_t GetSupMod() const {
        return fSupModId;
    }; 

    Float_t GetRhoCenter() const {
        return fRhoCenter;
    }

    Float_t GetZCenter() const {
        return fZCenter;
    }

    Float_t GetPhiCenter() const {
        return fPhiCenter;
    }

    Float_t GetThetaCenter() const {
        return fThetaCenter;
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

    void SetFlag(Int_t flag) {
        fFlag = flag;
    };

    void SetEnergy(Float_t e) {
        fE = e;
    };

    void SetTime(Float_t time) {
	fTime = time;     
    };

    void IncreaseEnergy(Float_t e) {
        fE += e;
    };

    void IncreaseEnergyTime(Float_t timeEnergy) {
        fTime += timeEnergy;
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

    void SetRhoCenter(Float_t rho) {
        fRhoCenter = rho;
    };

    void SetZCenter(Float_t z) {
        fZCenter = z;
    };

    void SetPhiCenter(Float_t phi) {
        fPhiCenter = phi;
    };

    void SetThetaCenter(Float_t theta) {
        fThetaCenter = theta;
    };

    void SetSecId(UInt_t id) {
        fSecId = id;
    };

    void SetRowId(UInt_t row) {
        fRowId = row;
    };


    void SetModId(UInt_t id) {
        fModId = id;
    };

protected:

    UInt_t fSecId;
    UInt_t fRowId;
    UInt_t fModId;
    UInt_t fSupModId; // super module = square of modules (3x3) (only)

    Float_t fE; //energy
    Float_t fTime; // hit mean time

    Int_t fTrackID; // -1 if more than one track in module
    Int_t fFlag; // Flag for general purposes [TDC, event tagging...]
    Int_t fPDG; // code of particle if only one track presented in module 
    UInt_t fNumTracks; // number of tracks, which have contribution in module
    Float_t fRhoCenter;// rho-coordinate of the center of module
    Float_t fZCenter; // z-coordinate of the center of module
    Float_t fPhiCenter; // phi-angle of the center of module
    Float_t fThetaCenter; // theta-angle of the center of module

    ClassDef(MpdEmcHit, 1)

};


#endif
