//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_ETOF_MATCHING_H
#define __MPD_ETOF_MATCHING_H 1

//------------------------------------------------------------------------------------------------------------------------
/// \class MpdEtofMatching
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include "TObject.h"
#include "TVector3.h"

#include "FairTask.h"
#include "MpdTofUtils.h"

#include "LMatchingFilter.h"

class MpdEtofHit;
//------------------------------------------------------------------------------------------------------------------------
class MpdEtofMatchingData : public TObject 
{
  	Double32_t 	fX, fY, fZ;      	// Position of hit [cm] {TofHit copy}	
	Double32_t	fTime;              	// Time since event start [ns]	{TofHit copy}
	Double_t 	fLength;		// [KF]  KF Track length;
	Double_t    	fBeta;          	// [TOF] 
	Double_t 	fMass2;			// [TOF]
	Double_t	fPx, fPy, fPz;		// [KF] momentum
        Double_t	fEstPointP[3];		// [KF] Estimated impact point on pad plate(x,y,z).

        Int_t      	fNmbTrHits;		// [KF] number of track hits 	
	Int_t      	fDetectorUID;     	// Detector unique identifier {TofHit copy}
	Int_t      	fFlag;			// 
	Int_t 		fCharge;		// [KF] charge
	Int_t		fPDGcode;		// [MC]		
	Int_t 		fKFTrackIndex;		//  KF Track index( branchname: "TpcKalmanTrack", classname: "MpdTpcKalmanTrack")		
	Int_t 		fTofHitIndex;		//  TofHit index( branchname: "ETOFHit", classname: "MpdEtofHit")
	
        Float_t		fDeltaR, fDeltaPhi;

public:
	MpdEtofMatchingData(){};
	MpdEtofMatchingData(Int_t kfTrackId, Int_t tofHitId, Int_t nTrHits, const MpdEtofHit* hit, Int_t pid, Int_t flag, Double_t length, const TVector3& pointP, const TVector3& Momentum, Int_t charge, Double_t deltaR=0., Double_t deltaPhi= 0.);	
		
	void 			Print(void)const;		
	Int_t			GetDetectorUID(void)const{ return fDetectorUID;};
	Int_t			GetFlag(void)const{ return fFlag;};	
	void			GetHitPosition(TVector3& point)const{ point.SetXYZ(fX, fY, fZ);};	
	Double32_t		GetTime(void)const{ return fTime;};	
	Double_t		GetTrackLength(void)const{ return fLength;};	
	Int_t			GetNmbTrHits(void)const{ return fNmbTrHits;};	
	Double_t		GetMass2(void)const{ return fMass2;};	
	Int_t			GetPDGcode(void)const{ return fPDGcode;}; 		// =0 if undefined
	Int_t			GetKFTrackIndex(void)const{ return fKFTrackIndex;};	
	Int_t			GetTofHitIndex(void)const{ return fTofHitIndex;};
	void 			GetEstPointOnPlate(TVector3& point)const{point.SetXYZ(fEstPointP[0], fEstPointP[1], fEstPointP[2]);};
	TVector3		GetMomentum(void)const {return TVector3(fPx, fPy, fPz);};
	Int_t			GetCharge(void)const {return fCharge; }
	Double32_t		GetDelta(void)const{ return sqrt(fDeltaR*fDeltaR + fDeltaPhi*fDeltaPhi);};
							
	void			AddFlag(Int_t add){fFlag = fFlag | add;}	
					
ClassDef(MpdEtofMatchingData,1)
};
//------------------------------------------------------------------------------------------------------------------------
class MpdEctKalmanTrack;
class TClonesArray; 
class MpdKalmanFilter;
class TEfficiency;
//------------------------------------------------------------------------------------------------------------------------
class MpdEtofMatching : public FairTask 
{
	TClonesArray 		*aTofPoints;		// MC TOF points
	TClonesArray 		*aTofHits;		// TOF hits
	TClonesArray 		*aMCTracks;		// MC tracks
	TClonesArray 		*aKFTracks;		// KF tracks
	TClonesArray 		*aTofMatching;		// TOF Matching data;
		 
	Bool_t			fDoTest, fIsMCrun;
	TString			fGeoFlNm, fTestFlnm;
	TList			fList;
	MpdKalmanFilter 	*pKF;
	Double_t		fTofEndCapZ;
		
	LMatchingFilter<MpdEtofHit, MpdEtofMatchingData>	*pMF; //!

	// QA test histos
	TH2D			*htCandNmb,*htTrackPerEvent, *htKfMcPlate;		
	TH1D			*htRest;
	TH2D			*htKFTrack, *htTMatch, *htMisMatch, *htMcEst_DeltaP, *htMcEst_dRP, *htMcEst_dPhiP, *htMcEst_dPhidR;
	TH2D			*htTrueDelta, *htMisDelta, *htKFTrackCand, *htKFTrackTrueCand;
	TEfficiency		*pEfficiencyP, *pEfficiencyEta, *pEfficiencyEtaP;		// Efficiency = N true matchings / N tpc kf tracks having TOF hit;
	TEfficiency		*pContaminationP, *pContaminationEta, *pContaminationEtaP; 	// Contamination = N wrong matchings / ( N true matchings + n wrong matchings)
	
	bool			EstTrackOnPlane(const MpdEctKalmanTrack *tr, Double_t Z, TVector3& pos, Double_t& length,  TVector3& Mom, Int_t& charge)const;

	MpdEtofMatching(const MpdEtofMatching&);
	MpdEtofMatching& operator=(const MpdEtofMatching&);

public:
	MpdEtofMatching(const char *name = "ETOF Matching", Int_t verbose = 1, Bool_t DoTest = false);
	~MpdEtofMatching();

	InitStatus	Init();
	void		Exec(Option_t * option);
	void		Finish();
	
	void		SetParamFlnm(const char* flnm){ fGeoFlNm = flnm; };
	void 		SetTestFlnm(const char* flnm){fTestFlnm = flnm;};	
	void		SetETofZposition(Double_t z){ fTofEndCapZ = z; }; // default 242.5 cm.
	
	
ClassDef(MpdEtofMatching,1) // MpdEtofMatching
};
//------------------------------------------------------------------------------------------------------------------------
#endif 


