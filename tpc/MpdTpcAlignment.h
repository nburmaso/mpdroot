#ifndef MPDTPCALIGNMENT_H
#define MPDTPCALIGNMENT_H

#include <FairTask.h>
#include <TClonesArray.h>
#include <TRotation.h>

#include "MpdTpcAlignmentParams.h"
#include "MpdTpcHit.h"
#include "MpdTpcSectorGeo.h"

//#define _TEST_

class MpdTpcAlignment : public FairTask, virtual public MpdTpcAlignmentParams
{
public:

class MpdRotation : public TRotation
{
public:
    MpdRotation() : TRotation() {};
 
    MpdRotation(const MpdRotation & m) : TRotation( (TRotation&)m ) {};
    MpdRotation(const TRotation & m) : TRotation(m) {};
    MpdRotation(const TQuaternion & Q) : TRotation(Q) {};
 
    virtual ~MpdRotation() {;};
public:
    inline void SetXX(Double_t val) { fxx = val; }
    inline void SetXY(Double_t val) { fxy = val; }
    inline void SetXZ(Double_t val) { fxz = val; }
    inline void SetYX(Double_t val) { fyx = val; }
    inline void SetYY(Double_t val) { fyy = val; }
    inline void SetYZ(Double_t val) { fyz = val; }
    inline void SetZX(Double_t val) { fzx = val; }
    inline void SetZY(Double_t val) { fzy = val; }
    inline void SetZZ(Double_t val) { fzz = val; }

protected:
    MpdRotation(Double_t mxx, Double_t mxy, Double_t mxz, Double_t myx, Double_t myy, Double_t myz, Double_t mzx, Double_t mzy, Double_t mzz) : TRotation(mxx, mxy, mxz, myx, myy, myz, mzx, mzy, mzz) {};
};

    MpdTpcAlignment();
    ~MpdTpcAlignment();
    
    virtual InitStatus Init() override;
    
    virtual void Exec(Option_t* opt);
    
    void SetPersistence(Bool_t val = kTRUE);
    
private:
    TClonesArray* fHitsMisalign;
    TClonesArray* fHitsAlign;
#ifdef _TEST_
    TClonesArray* fHits;
#endif
    
    Bool_t fPersistence;
    
    MpdTpcSectorGeo * fSecGeo;
    
    ClassDef(MpdTpcAlignment,0)
};

#endif
