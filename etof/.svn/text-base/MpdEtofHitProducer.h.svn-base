//------------------------------------------------------------------------------------------------------------------------
#ifndef MPDETOFHITPRODUCER_H
#define MPDETOFHITPRODUCER_H

#include <map>
#include <math.h>

#include "FairTask.h"

#include "TString.h"
#include "TList.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TVector3.h"
#include "TRandom2.h"

//------------------------------------------------------------------------------------------------------------------------
/// \class MpdEtofHitProducer
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include "MpdTofUtils.h"

class MpdKalmanFilter;
class TpcLheKalmanTrack;
class MpdEtofHit;
class TClonesArray; 
 
//------------------------------------------------------------------------------------------------------------------------
class MpdEtofHitProducer : public FairTask
{
	typedef map<Double_t, MpdTofUtils::k_side> deltaMap;
	typedef deltaMap::iterator 		deltaIter;
	
	MpdTofUtils::PadMap 		mapPads;
	MpdTofUtils::ModMMap		mmapModules;
	MpdTofUtils::RegVec		vecRegions;

	Bool_t				fDoTest;
	TString				fGeoFlNm, fTestFlnm;
	TList				fList;	
	TRandom2 			*pRandom;
	Double_t 			fTimeSigma; // [sigma]=ns,  default 100 ps
		
	TH1D				*hTestR;
	TH2D				*hTestC_D, *hTestXY, *hTest1thRegion,  *hTestCross1, *hTestCross2, *hTestDeadTime, *hTestUnion, *htXYpoints;
	TH2D				*htPointsHits;

	TClonesArray 			*pETofPoints;		// ETOF MC points
	TClonesArray 			*pMCTracks;		// MC tracks
	TClonesArray 			*pHitCollection;	// TOFhits
	TClonesArray 			*pHitTmp;		//! TOF tmp hits	

	MpdEtofHit* 			FindTmpHit(Int_t UID);
	Int_t				MakeClusterHits();
	Int_t				SimPadDeadTime();
	void 				AddRawHit(Int_t hitIndex, Int_t detUID, const TVector3& posHit, const TVector3& posHitErr, 
						Int_t pointIndex, Int_t trackIndex, Double_t time, Int_t flag);

						
	Bool_t 				HitExist(Double_t val); 
	Bool_t 				DoubleHitExist(Double_t val);
	
public:
	MpdEtofHitProducer(const char *name = "ETOF HitProducer", Int_t verbose = 1, Bool_t DoTest = false);
	virtual ~MpdEtofHitProducer();

  	InitStatus 	Init();
  	void 		Exec(Option_t * option);
  	void 		Finish();

	
	void		SetParamFlnm(const char* flnm){ fGeoFlNm = flnm; };
	void 		SetTestFlnm(const char* flnm){ fTestFlnm = flnm;};	
	void		SetTimeSigma(Double_t sigma){ fTimeSigma = sigma; };

ClassDef(MpdEtofHitProducer,1) // MpdEtofHitProducer
};
//------------------------------------------------------------------------------------------------------------------------
#endif 

