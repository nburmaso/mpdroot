//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_DCH_HIT_PRODUCER_H
#define __MPD_DCH_HIT_PRODUCER_H 1

#include <map>

#include "FairTask.h"
#include "TList.h"
#include "TVector3.h"

class TClonesArray;
class TObjectArray;
class MpdDchHit;
class TRandom;
class TH1D;
class TH2D;
//------------------------------------------------------------------------------------------------------------------------
class MpdDchHitProducer : public FairTask
{
  	TRandom				*pRandom;
	TClonesArray			*pHitCollection; 
	TClonesArray			*pDchPoints;
	TClonesArray			*pMCTracks;
	TList				fList;	
	Bool_t				fDoTest;
	Double_t			fRSigma, fRPhiSigma; // [cm] default 2mm in R, 200um in R-Phi

	TH1D				*htOccup,  *htWireN, *htMCTime;
	TH2D				*htXYlocal, *htRvsR;	
	TH1D				*htGasDrift,*htGasDriftA, *htTime, *htTimeA, *htPerp, *htPerpA;
	
	typedef multimap<Double_t, Int_t> 	occupMap;
	typedef occupMap::iterator 		occupIter;
	occupMap			fMapOccup;

	void		Rotate(Int_t proj, Double_t x,Double_t y, Double_t& xRot, Double_t& yRot, Bool_t back=false);	
	Double_t	GetDriftLenght(Int_t proj, Int_t gasgap, Double_t x, Double_t& wirePos);
	Double_t	GetTShift(Double_t driftLength, Double_t wirePos, Double_t R, Double_t&);
	Bool_t 		HitExist(Double_t delta);	
	Double_t	GetPhi(Int_t proj);
	Int_t		WireID(Int_t uid, Double_t wirePos, Double_t R);
	
  	MpdDchHit* 	AddHit(Int_t index, Int_t detID, const TVector3& posHit, const TVector3& posHitErr, 
					Int_t trackIndex, Int_t pointIndex, Int_t flag);
		
public:

  	MpdDchHitProducer(const char* fileGeo, Int_t verbose = 1, Bool_t DoTest = false);
  	~MpdDchHitProducer();

 	InitStatus	Init();
  	void 		Exec(Option_t* opt);
	void		Finish();

	void		SetErrors(Double_t errR, Double_t errRphi){ fRSigma = errR; fRPhiSigma = errRphi; };
  
ClassDef(MpdDchHitProducer,1); 
};
//------------------------------------------------------------------------------------------------------------------------
#endif
