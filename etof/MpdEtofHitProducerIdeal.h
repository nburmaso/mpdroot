//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_ETOF_HIT_PRODUCER_IDEAL_H
#define __MPD_ETOF_HIT_PRODUCER_IDEAL_H 1

//------------------------------------------------------------------------------------------------------------------------
/// \class MpdEtofHitProducerIdeal
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include <TVector3.h>
#include <TList.h>

#include "FairTask.h"
//------------------------------------------------------------------------------------------------------------------------
class TH1D;
class TH2D;
class TClonesArray;
//------------------------------------------------------------------------------------------------------------------------
class MpdEtofHitProducerIdeal : public FairTask
{
protected:

        TClonesArray 			*aTofPoints;
        TClonesArray 			*aMCTracks;
        TClonesArray 			*aTofHits;

	Bool_t				fDoTest, fDoMergeHits;
	TString				fTestFlnm;

	// QA test histos
        TList				fList;
	TH1D   				*h1TestOccup;
  
  	void 				AddHit(Int_t detUID, const TVector3& posHit, const TVector3& posHitErr, Int_t pointIndex, Int_t trackIndex, Double_t time, Int_t flag);	
 	Int_t 				CompressHits();
	Int_t 				MergeHitsOnStrip(); // save only the fastest hit in the strip 
	       
public:
	MpdEtofHitProducerIdeal(const char *name = "ETOF Ideal HitProducer", Int_t verbose = 1, Bool_t DoTest = false, Bool_t DoMergeHits = false);
	virtual ~MpdEtofHitProducerIdeal();

	virtual InitStatus	Init();
	virtual void		Exec(Option_t * option);
	virtual void		Finish();	
	
	void			Dump(const char* comment = nullptr, ostream& out = std::cout) const;
	void 			SetTestFlnm(const char* flnm){ fTestFlnm = flnm; };

ClassDef(MpdEtofHitProducerIdeal,3);
};
//------------------------------------------------------------------------------------------------------------------------
#endif
