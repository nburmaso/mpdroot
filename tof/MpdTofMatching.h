//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_MATCHING_H
#define __MPD_TOF_MATCHING_H 1

//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTofMatching
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------
#include <TObject.h>
#include <TVector3.h>
#include <TClonesArray.h>
#include <TGeoMatrix.h>

#include "MpdTpcKalmanTrack.h"

#include "MpdTofHit.h"

#include "LMatchingFilter.h"

#include "FairTask.h"
//------------------------------------------------------------------------------------------------------------------------
class MpdTofMatchingData : public TObject 
{
        Double32_t 	fX, fY, fZ;      	// Position of hit [cm] {TofHit copy}
        Double32_t	fTime;              	// Time since event start [ns]	{TofHit copy}
        Double_t 	fLength;		// [KF]  KF Track length;     
        Double_t    	fBeta;          	// [TOF] 
        Double_t 	fMass2;			// [TOF]
        Double_t	fPx, fPy, fPz;		// [KF] momentum
        Double_t	fEstPointR[3];		// [KF] Estimated impact point on cylinder(x,y,z).
        Double_t	fEstPointP[3];		// [KF] Estimated impact point on pad plate(x,y,z).
        Double_t	fEstPointPPhi, fEstPointPTheta;	// [KF] Estimated impact direction on pad plate.  REMOVE
        Float_t		fStripPerp[3];	      	// perpendicular to strip plate
      
        Int_t      	fNmbTrHits;		// [KF] number of track hits          	
        Int_t      	fDetectorUID;     	// Detector unique identifier {TofHit copy}
        Int_t      	fFlag;			//
        Int_t 		fCharge;		// [KF] charge	
        Int_t		fPDGcode;		// [MC]
        Int_t 		fKFTrackIndex;		//  KF Track index( branchname: "TpcKalmanTrack", classname: "MpdTpcKalmanTrack")
        Int_t 		fTofHitIndex;		//  TofHit index( branchname: "TOFHit", classname: "MpdTofHit")
        
        Float_t		fDelta;
        	
public: 
        MpdTofMatchingData(){}
	MpdTofMatchingData(Int_t kfTrackId, Int_t tofHitId, const MpdTofHit* hit, Int_t pid, Int_t flag, Double_t length, const TVector3& pointR, const TVector3& pointP, 
				Double_t pointPPhi, Double_t pointPTheta,  const TVector3& Momentum, Int_t charge);
				
	MpdTofMatchingData(Int_t kfTrackId, Int_t tofHitId, Int_t nTrHits, const MpdTofHit* hit, Int_t pid, Int_t flag, Double_t length, const TVector3& pointR, const TVector3& pointP, 
				const TVector3& perp,  const TVector3& Momentum, Int_t charge, Double_t delta);	
				
	void 			Print(void)const;
	
	Int_t			GetDetectorUID(void)const{ return fDetectorUID;};
	Int_t			GetFlag(void)const{ return fFlag;};	
	void			GetHitPosition(TVector3& point)const{ point.SetXYZ(fX, fY, fZ);};	
	Double32_t		GetTime(void)const{ return fTime;};	
	Double_t		GetTrackLength(void)const{ return fLength;};	
	Int_t			GetNmbTrHits(void)const{ return fNmbTrHits;};	
    	Double_t        	GetBeta(void) const { return fBeta;};  
	Double_t		GetMass2(void)const{ return fMass2;};	
	Int_t			GetPDGcode(void)const{ return fPDGcode;}; 		// =0 if undefined
	Int_t			GetKFTrackIndex(void)const{ return fKFTrackIndex;};	
	Int_t			GetTofHitIndex(void)const{ return fTofHitIndex;};
	void 			GetEstPointR(TVector3& point)const{point.SetXYZ(fEstPointR[0], fEstPointR[1], fEstPointR[2]);};	
	void 			GetEstPointP(TVector3& point)const{point.SetXYZ(fEstPointP[0], fEstPointP[1], fEstPointP[2]);};
	void 			GetEstPointPDir(Double_t& theta, Double_t& phi) const{ theta = fEstPointPTheta; phi =fEstPointPPhi;};
	TVector3		GetMomentum(void)const {return TVector3(fPx, fPy, fPz);};	
	Int_t			GetCharge(void)const {return fCharge; }
	Double32_t		GetDelta(void)const{ return fDelta;};
							
	void			AddFlag(Int_t add){fFlag = fFlag | add;}	

ClassDef(MpdTofMatchingData, 3)
};

class TofMatchingFilter;
class MpdKalmanFilter;
class MpdTofHit;
class TEfficiency;
//------------------------------------------------------------------------------------------------------------------------
class MpdTofMatching : public FairTask 
{
        TClonesArray 		*aTofPoints;		// MC TOF points
        TClonesArray 		*aTofHits;		// TOF hits
        TClonesArray 		*aMCTracks;		// MC tracks
        TClonesArray 		*aKFTracks;		// KF TPC tracks
        TClonesArray 		*aKFectTracks;		// KF ECT tracks
        TClonesArray 		*aTofMatching;		// TOF Matching data;

        Bool_t			fDoTest, fIsMCrun;
        TString			fGeoFlNm, fTestFlnm;
        MpdKalmanFilter 	*pKF;
        Double_t		fTofBarrelRadius;
        
        LMatchingFilter<MpdTofHit, MpdTofMatchingData>	*pMF; //!
        
	// QA test histos	
        TList			fList;	
        TH2D           		*htCandNmb,*htTrackPerEvent, *htKfMcCyl, *htKFTrack, *htKFTrackCand, *htKFTrackTrueCand, *htTMatch, *htMisMatch;
        TH2D           		*htTrueDelta, *htMisDelta, *htMcEst_DeltaP, *htMcEst_dZP, *htMcEst_dPhiP, *htMcEst_dPhidZ;
	TEfficiency		*pEfficiencyP, *pEfficiencyEta, *pEfficiencyEtaP;		// Efficiency = N true matchings / N tpc kf tracks having TOF hit;
	TEfficiency		*pContaminationP, *pContaminationEta, *pContaminationEtaP; 	// Contamination = N wrong matchings / ( N true matchings + n wrong matchings)
	        		
	MpdTpcKalmanTrack 	RefitTrack(const MpdTpcKalmanTrack*);
        TVector3		EstTrackOnR(const MpdTpcKalmanTrack *tr)const;
	bool			EstTrackOnPlane(const MpdTpcKalmanTrack& tr, const TVector3& point, const TVector3& perp, TVector3& pos, Double_t& length, TVector3& Mom, Int_t& charge) const;


	MpdTofMatching(const MpdTofMatching&);
	MpdTofMatching& operator=(const MpdTofMatching& rhs);
	
public:
	MpdTofMatching(const char *name = "TOF Matching", Int_t verbose = 1, Bool_t DoTest = false);
	virtual ~MpdTofMatching();

	InitStatus	Init();
	void		Exec(Option_t * option);
	void		Finish();
	
	void		SetParamFlnm(const char* flnm){ fGeoFlNm = flnm; };
	void 		SetTestFlnm(const char* flnm){fTestFlnm = flnm;};	
	void		SetTofBarrelRadius(Double_t R){ fTofBarrelRadius = R; }; 
	
	
ClassDef(MpdTofMatching,2) // MpdTofMatching
};

//------------------------------------------------------------------------------------------------------------------------

#endif 
