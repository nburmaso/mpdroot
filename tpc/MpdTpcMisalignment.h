#ifndef MPDTPCMISALIGNMENT_H
#define MPDTPCMISALIGNMENT_H

#include <FairTask.h>
#include <TClonesArray.h>

#include "MpdTpcAlignmentParams.h"
#include "MpdTpcHit.h"
#include "MpdTpcSectorGeo.h"

//#define _TEST_
//#define _WRITETXTDATA_

class MpdTpcMisalignment : public FairTask, virtual public MpdTpcAlignmentParams
{
public:
    MpdTpcMisalignment();
    ~MpdTpcMisalignment();
    
    virtual InitStatus Init() override;
    
    virtual void Exec(Option_t* opt);
    
    void SetPersistence(Bool_t val = kTRUE);
    
private:
    TClonesArray* fHits;
#ifdef _TEST_
    TClonesArray* fHitsMisalign;
#endif
    
    Bool_t fPersistence;
    
    MpdTpcSectorGeo * fSecGeo;
    
    ClassDef(MpdTpcMisalignment,0)
};

#endif
