//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_HIT_PRODUCER_H
#define __MPD_TOF_HIT_PRODUCER_H 1

//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTofHitProducer
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include "MpdTofUtils.h"
#include "MpdKalmanFilter.h"
#include "MpdTofHit.h"

#include "FairTask.h"

#include "TString.h"
#include "TList.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TVector3.h"
#include "TRandom2.h"
#include "TClonesArray.h"

#include <iostream>
#include <vector>
#include <map>
#include <math.h>

using namespace std;
                         
//------------------------------------------------------------------------------------------------------------------------
class MpdTofHitProducer : public FairTask 
{

        typedef map<Double_t, MpdTofUtils::k_side> deltaMap;
        typedef deltaMap::iterator 		deltaIter;
	
        MpdTofUtils::PadMap 		mapPads;                //! exclude because there is no dictionary for this sctructure
        MpdTofUtils::ModMMap		mmapModules;            //! exclude because there is no dictionary for this sctructure
        MpdTofUtils::RegVec		vecRegions;             //! exclude because there is no dictionary for this sctructure

	Bool_t				fDoTest;
	TString				fGeoFlNm, fTestFlnm;
        TList				fList;
        TRandom2 			*pRandom;               //! doesn't serialize. possible not required for other
        Double_t 			fTimeSigma;             // [sigma]=ns,  default 100 ps
		
        TH1D                *htR;
        TH2D                *htC_D, *htXY, *htXZregionP, *htXZregion, *htXZmodule, *htXZpad,  *htCross1, *htCross2;
        TH2D                *htDeadTime, *htUnion, *htEtaPhi, *htChainPID, *htNeighPID;

        TClonesArray 			*pTofPoints;		// TOF MC points
        TClonesArray 			*pMCTracks;		// MC tracks
        TClonesArray 			*pHitCollection;	// TOFhits
        TClonesArray 			*pHitTmp;		// TOF tmp hits

	MpdTofHit* 			FindTmpHit(Int_t detUID);
	Int_t				MakeClusterHits();
	Int_t				SimPadDeadTime();
	void 				AddRawHit(Int_t hitIndex, Int_t detUID, const TVector3& posHit, const TVector3& posHitErr, 
						Int_t pointIndex, Int_t trackIndex, Double_t time, Int_t flag);
								
	Bool_t 				HitExist(Double_t val); 
	Bool_t 				DoubleHitExist(Double_t val);

	void			Dump(TClonesArray *array, const char* title = NULL, ostream& out = std::cout);

public:
	MpdTofHitProducer(const char *name = "TOF HitProducer", Int_t verbose = 1, Bool_t DoTest = false);
	virtual ~MpdTofHitProducer();

	InitStatus		Init();
	void			Exec(Option_t * option);
	void			Finish();

	void			DumpRaw(const char* title = NULL, ostream& out = std::cout){Dump(pHitTmp, title, out);};
	void			Dump(const char* title = NULL, ostream& out = std::cout){Dump(pHitCollection, title, out);};

	void			SetParamFlnm(const char* flnm){ fGeoFlNm = flnm; };
	void 			SetTestFlnm(const char* flnm){ fTestFlnm = flnm;};	
	void			SetTimeSigma(Double_t sigma){ fTimeSigma = sigma; };
		

ClassDef(MpdTofHitProducer,1) // MpdTofHitProducer
};

//------------------------------------------------------------------------------------------------------------------------
#endif 

