//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_ETOF_HIT_PRODUCER_H
#define __MPD_ETOF_HIT_PRODUCER_H 1

//------------------------------------------------------------------------------------------------------------------------
/// \class MpdEtofHitProducer
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include <map>
#include <math.h>

#include "TString.h"
#include "TList.h"
#include "TVector3.h"
#include "TRandom2.h"

#include "MpdEtofHitProducerIdeal.h"

class TH1D;
class TH2D;

class MpdKalmanFilter;
class TpcLheKalmanTrack;
class MpdEtofHit;
class TClonesArray; 
 
//------------------------------------------------------------------------------------------------------------------------
class MpdEtofHitProducer : public MpdEtofHitProducerIdeal
{
	Double_t 			fTimeSigma; 	// Uncertainties of time, gaus sigma [ns],  default: 100 ps
   	Double_t			fErrPhi, fErrR; 	// Uncertainties of coordinates, gaus sigma [cm], dR= 10./sqrt(12.) mm, default: dPhi= 5 mm. 	
	TRandom2 			*pRandom;
	
	// QA test histos
        TList				fList;		
	TH1D				*h1TestDistance;
	TH2D				*h2TestNeighborPair;
	TH2D				*h2TestMergedTimes, *h2TestChainPID, *h2TestStrips;
							
	Bool_t 				HitExist(Double_t val); 
	Bool_t 				DoubleHitExist(Double_t val);	
	
public:
	MpdEtofHitProducer(const char *name = "ETOF HitProducer", Int_t verbose = 1, Bool_t DoTest = false);
	virtual ~MpdEtofHitProducer();

	InitStatus		Init();
	void			Exec(Option_t * option);
	void			Finish();

	void			SetTimeResolution(Double_t sigma){ fTimeSigma = sigma; };	
	void			SetAlongStripzResolution(Double_t err){ fErrPhi = err;};
	void 			SetSeed(UInt_t seed = 0);

ClassDef(MpdEtofHitProducer,2) // MpdEtofHitProducer
};
//------------------------------------------------------------------------------------------------------------------------
#endif 

