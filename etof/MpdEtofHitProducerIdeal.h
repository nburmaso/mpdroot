//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_ETOF_HIT_PRODUCER_IDEAL_H
#define __MPD_ETOF_HIT_PRODUCER_IDEAL_H 1


#include "FairTask.h"

class TClonesArray;
//------------------------------------------------------------------------------------------------------------------------
class MpdEtofHitProducerIdeal : public FairTask
{

public:
	MpdEtofHitProducerIdeal();
	~MpdEtofHitProducerIdeal();

	virtual InitStatus Init();
	virtual void Exec(Option_t* opt);

private:

	// Input array of MpdEtofPoints 
	TClonesArray* fPointArray;

	// Output array of MpdEtofHits 
	TClonesArray* fHitArray;  


ClassDef(MpdEtofHitProducerIdeal,1);
};
//------------------------------------------------------------------------------------------------------------------------
#endif
