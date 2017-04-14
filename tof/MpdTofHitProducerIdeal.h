//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_HIT_PRODUCER_IDEAL_H
#define __MPD_TOF_HIT_PRODUCER_IDEAL_H 1

#include <TVector3.h>
#include <TList.h>

#include "FairTask.h"
//------------------------------------------------------------------------------------------------------------------------
class TClonesArray;
class MpdTofHitProducerQA;
//------------------------------------------------------------------------------------------------------------------------
class MpdTofHitProducerIdeal : public FairTask
{
protected:

        TClonesArray 			*aMcPoints;	//! <--- MC input
        TClonesArray 			*aMcTracks;	//! <--- MC input
        TClonesArray 			*aExpDigits;	//! <--- Exp input
        TClonesArray 			*aTofHits;	//! ---> output

	Bool_t				fDoTest;
	Bool_t				fDoMergeHits;
	Bool_t				fUseMCData;
	Bool_t				fOnlyPrimary;	

       	MpdTofHitProducerQA		*pHitProducerQA; 	//!      
  
  	void 				AddHit(Int_t detUID, const TVector3& posHit, const TVector3& posHitErr, Int_t mcPointIndex, Int_t mcTrackIndex, Double_t time, Int_t flag);
   	void 				AddHit(Int_t detUID, const TVector3& posHit, const TVector3& posHitErr, Int_t expDigitIndex, Double_t time, Int_t flag); 		
 	Int_t 				CompressHits();
	Int_t 				MergeHitsOnStrip(); // save only the fastest hit in the strip 
	       
public:
	MpdTofHitProducerIdeal(const char *name = "TOF Ideal HitProducer", Bool_t useMCdata = true, Int_t verbose = 1, Bool_t DoTest = false, Bool_t DoMergeHits = false, const char *flnm = "QA.MpdTofHitProducerIdeal.root", bool IsEndcap = false);
	virtual ~MpdTofHitProducerIdeal();

	virtual InitStatus	Init();
	virtual void		Exec(Option_t * option);
	virtual void		Finish();	

	void 			SetOnlyPrimary(Bool_t opt) { fOnlyPrimary = opt; }	
	void			Dump(const char* comment = nullptr, ostream& out = std::cout) const;


ClassDef(MpdTofHitProducerIdeal,3);
};
//------------------------------------------------------------------------------------------------------------------------
#endif
