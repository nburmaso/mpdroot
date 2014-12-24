//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_MATCHING_H
#define __MPD_TOF_MATCHING_H 1

//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTofMatching
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include "MpdTofUtils.h"
#include "MatchingFilter.h"
#include "MpdTpcKalmanTrack.h"
#include "MpdKalmanFilter.h"
#include "MpdTofHit.h"

#include "LSpatialGrid2D.h"

#include "FairTask.h"

#include <TObject.h>
#include <TVector3.h>
#include <TClonesArray.h>
#include <TGeoMatrix.h>
//------------------------------------------------------------------------------------------------------------------------
class MpdTofMatchingData : public TObject 
{
        Double32_t 	fX, fY, fZ;      		// Position of hit [cm] {TofHit copy}
        Double32_t	fTime;              	// Time since event start [ns]	{TofHit copy}
        Double_t 	fLength;			// [KF]  KF Track length;
        Double_t    fBeta;          // [TOF] 
        Double_t 	fMass2;			// [TOF]
        Double_t		fPx, fPy, fPz;		// [KF] momentum
        Double_t		fEstPointR[3];		// [KF] Estimated impact point on cylinder(x,y,z).
        Double_t		fTofImpactPoint[3];	// [KF] Estimated impact point on pad plate(x,y,z).
        Double_t		fTofImpactPointPhi, fTofImpactPointTheta;	// [KF] Estimated impact direction on pad plate.

        Int_t      		fDetectorUID;     	// Detector unique identifier {TofHit copy}
        Int_t      		fFlag;			//
        Int_t 		fCharge;			// [KF] charge	
        Int_t			fPDGcode;		// [MC]
        Int_t 		fKFTrackIndex;		//  KF Track index( branchname: "TpcKalmanTrack", classname: "MpdTpcKalmanTrack")
        Int_t 		fTofHitIndex;		//  TofHit index( branchname: "TOFHit", classname: "MpdTofHit")
	
public:
        MpdTofMatchingData(){}
	MpdTofMatchingData(Int_t kfTrackId, Int_t tofHitId, const MpdTofHit* hit, Int_t pid, Int_t flag,
				Double_t length, const TVector3& est_point_R, const TVector3& est_point_Plane, 
				Double_t est_point_PlanePhi, Double_t est_point_PlaneTheta,  const TVector3& P, Int_t charge);
	
	void 		Print(void)const;
	
	Int_t			GetDetectorUID(void)const{ return fDetectorUID;};
	Int_t			GetFlag(void)const{ return fFlag;};	
	void		GetHitPosition(TVector3& point)const{ point.SetXYZ(fX, fY, fZ);};	
	Double32_t	GetTime(void)const{ return fTime;};	
	Double_t		GetTrackLength(void)const{ return fLength;};	
    Double_t        GetBeta(void) const { return fBeta;};  
	Double_t		GetMass2(void)const{ return fMass2;};	
	Int_t			GetPDGcode(void)const{ return fPDGcode;}; 		// =0 if undefined
	Int_t			GetKFTrackIndex(void)const{ return fKFTrackIndex;};	
	Int_t			GetTofHitIndex(void)const{ return fTofHitIndex;};
	void 		GetEstPointR(TVector3& point)const{point.SetXYZ(fEstPointR[0], fEstPointR[1], fEstPointR[2]);};	
	void 		GetTofImpactPoint(TVector3& point)const{point.SetXYZ(fTofImpactPoint[0], fTofImpactPoint[1], fTofImpactPoint[2]);};
	void 		GetTofImpactPointDir(Double_t& theta, Double_t& phi) const{ theta = fTofImpactPointTheta; phi = fTofImpactPointPhi;};
	TVector3		GetMomentum(void)const {return TVector3(fPx, fPy, fPz);};	
	Int_t			GetCharge(void)const {return fCharge; }
						
	void		AddFlag(Int_t add){fFlag = fFlag | add;}	

ClassDef(MpdTofMatchingData,1)
};

class TofMatchingFilter;
class MpdTofHit;

//------------------------------------------------------------------------------------------------------------------------
class MpdTofMatching : public FairTask 
{
        friend class TofMatchingFilter;

        Bool_t			fDoTest, fMCDataExist;
        TString			fGeoFlNm, fTestFlnm;
        TList			fList;
        MpdKalmanFilter 	*pKF;
        Double_t			fTofBarrelRadius;
		
        TH2D           		*htCandNmb,*htTrackPerEvent, *htKfMc;
        TH2D            		*hPads_Theta_Phi, *hTest_C_D, *htKFTrack, *htTMatch, *htMisMatch;

        TClonesArray 		*pTofPoints;		// MC TOF points
        TClonesArray 		*pTofHits;		// TOF hits
        TClonesArray 		*pMCTracks;		// MC tracks
        TClonesArray 		*pKFTracks;		// KF TPC tracks
        TClonesArray 		*pKFectTracks;		// KF ECT tracks
        TClonesArray 		*pMatchingCollection;	// TOF Matching data;

	typedef	LSpatialGrid2D<int>				TGridPad;
	typedef	LSpatialGrid2D<MpdTofUtils::modPar>	TGridModule;

	TGridPad 			*padGrid; 	//!	pads spatial grid,  transient member data
	TGridModule 		*moduleGrid;	//!	modules spatial grid,  transient member data
	TGeoHMatrix 		fGeoReper1_1;
        TofMatchingFilter	*pMatchingFilter;       //!  transient member data
		
	MpdTpcKalmanTrack 	RefitTrack(const MpdTpcKalmanTrack*);
        TVector3			EstTrackOnR(const MpdTpcKalmanTrack *tr)const;
	bool			EstTrackOnPlane(const MpdTpcKalmanTrack& tr, const TVector3& point, const TVector3& perp,
							TVector3& pos, Double_t& length, TVector3& Mom, Int_t& charge) const;
	void 			AddEntry(Int_t hitID, Int_t kfTrackId, Int_t tofHitId, const MpdTofHit* hit, Int_t pid, Int_t flag,
							Double_t length, const TVector3& est_point_R, const TVector3& est_point_Plane, 
							Double_t est_point_PlanePhi, Double_t est_point_PlaneTheta, const TVector3& Mom, Int_t charge);

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
	
	
ClassDef(MpdTofMatching,1) // MpdTofMatching
};

//------------------------------------------------------------------------------------------------------------------------
class TofMatchingCandidate : public MatchingCandidate
{
public:
        Double_t delta;
        Double_t  trackLength;
        MpdTofHit *TofHit;
        TVector3 estPointR, estPointPl;

        TofMatchingCandidate(Int_t trackIndex, Int_t hitindex, Int_t mcpid, Int_t flag, Int_t charge, TVector3 mom,
                                                MpdTofHit *tofHit, Double_t Delta, TVector3 EstPointR, TVector3 EstPointPl, Double_t  TrackLength)
        : MatchingCandidate(trackIndex, hitindex, mcpid, flag, charge, mom), 
		TofHit(tofHit), delta(Delta), trackLength(TrackLength), estPointR(EstPointR), estPointPl(EstPointPl){}
};
//------------------------------------------------------------------------------------------------------------------------
class TofMatchingFilter : public MatchingFilter
{
public:
        Int_t 	Processing(Int_t nKFTracks, TH2D*);
        void	FillMatchings(MpdTofMatching*);
        void	AddCandidate(Int_t mcTrackIndex, Int_t KfIndex, Int_t hitIndex, MpdTofHit *TofHit, Int_t mcPID, Int_t flag, Double_t trackLength,
                                TVector3 estPointR, TVector3 estPointPl, TVector3 Mom, Int_t charge, Double_t delta);

private:
        candIter 	FindClosestHit(Int_t KfIndex);
        candIter 	FindClosestTrack(linkIter itLink, Int_t size, Double_t& minDelta);
};
//------------------------------------------------------------------------------------------------------------------------
#endif 
