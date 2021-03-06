//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_HIT_PRODUCER_H
#define __MPD_TOF_HIT_PRODUCER_H 1

//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTofHitProducer
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------
#include <iostream>

#include <TString.h>

#include "MpdTofHitProducerIdeal.h"
//------------------------------------------------------------------------------------------------------------------------
class TRandom2;
class TEfficiency;
class TH1D;
class TH2D;
//------------------------------------------------------------------------------------------------------------------------
class MpdTofHitProducer : public MpdTofHitProducerIdeal 
{
        Double_t 			fTimeSigma;	// Uncertainties of time, gaus sigma [ns],  default: 100 ps
   	Double_t			fErrX, fErrZ; 	// Uncertainties of coordinates, gaus sigma [cm], dZ= 10./sqrt(12.) mm, default: dX= 5 mm.        
        TRandom2 			*pRandom;            
						
	Bool_t 				HitExist(Double_t val); 
	Bool_t 				DoubleHitExist(Double_t val);
				
public:
	MpdTofHitProducer(const char *name = "TOF HitProducer", Bool_t useMCdata = true, Int_t verbose = 1, Bool_t DoTest = false, const char *flnm = "QA.MpdTofHitProducer.root");
	virtual ~MpdTofHitProducer();

	virtual InitStatus	Init();
	virtual void		Exec(Option_t * option);
	virtual void		Finish();

	void			SetTimeResolution(Double_t sigma){ fTimeSigma = sigma; };
	void			SetAlongStripzResolution(Double_t Xerr){ fErrX = Xerr;};	
	void 			SetSeed(UInt_t seed = 0);	
	
	TString			GetParameters()
	{ 
		char s[32];
		TString buf = "\n Run parameters: fTimeSigma="; sprintf(s, "%.5g", fTimeSigma); buf+=(s);  
		buf+=" ns, fErrX="; sprintf(s, "%.4g", fErrX); buf+=(s); 
		buf+=" cm, fErrZ="; sprintf(s, "%.4g", fErrZ); buf+=(s); 
		buf+=" cm, fDoTest="; buf+=fDoTest; buf+=", fDoMergeHits="; buf+=fDoMergeHits;
		return buf;
	}

ClassDef(MpdTofHitProducer,3) 
};
//------------------------------------------------------------------------------------------------------------------------
#endif 

