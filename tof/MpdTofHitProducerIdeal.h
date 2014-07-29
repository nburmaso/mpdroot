//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_HIT_PRODUCER_IDEAL_H
#define __MPD_TOF_HIT_PRODUCER_IDEAL_H 1


#include "FairTask.h"

class TClonesArray;
//------------------------------------------------------------------------------------------------------------------------
class MpdTofHitProducerIdeal : public FairTask
{

public:
	MpdTofHitProducerIdeal();
	~MpdTofHitProducerIdeal();

	virtual InitStatus Init();
	virtual void Exec(Option_t* opt);

private:

	// Input array of MpdTofPoints 
	TClonesArray* fPointArray;

	// Output array of MpdTofHits 
	TClonesArray* fHitArray;  


ClassDef(MpdTofHitProducerIdeal,1);
};
//------------------------------------------------------------------------------------------------------------------------
#endif
