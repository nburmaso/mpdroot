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

class MpdEtofHit;
class LTHStack;
class MpdKalmanFilter;
class MpdEctKalmanTrack;
class TClonesArray; 
//------------------------------------------------------------------------------------------------------------------------
class MpdEtofMatchingData : public TObject 
{
  	Double32_t 	fX, fY, fZ;      		// Position of hit [cm] {TofHit copy}	
	Double32_t	fTime;              	// Time since event start [ns]	{TofHit copy}
	Double_t 	fLength;			// [KF]  KF Track length;
	Double_t 	fMass2;			// [KF]	
	Double_t		fPx, fPy, fPz;		// [KF] momentum
	Double_t		fTofImpactPoint[3];	// [KF] Estimated impact point on pad plate(x,y,z).
	Int_t      		fDetectorUID;     	// Detector unique identifier {TofHit copy}
	Int_t      		fFlag;			// 
	Int_t 		fCharge;			// [KF] charge
	Int_t			fPDGcode;		// [MC]		
	Int_t 		fKFTrackIndex;		//  KF Track index( branchname: "TpcKalmanTrack", classname: "MpdTpcKalmanTrack")		
	Int_t 		fTofHitIndex;		//  TofHit index( branchname: "ETOFHit", classname: "MpdEtofHit")

public:
	MpdEtofMatchingData(){};
	MpdEtofMatchingData(Int_t kfTrackId, Int_t tofHitId, const MpdEtofHit* hit, Int_t pid, Int_t flag,
				Double_t length, const TVector3& est_point_Plane, const TVector3& P, Int_t charge);
	
	void 		Print(void)const;
		
	Int_t			GetDetectorUID(void)const{ return fDetectorUID;};
	Int_t			GetFlag(void)const{ return fFlag;};	
	void		GetHitPosition(TVector3& point)const{ point.SetXYZ(fX, fY, fZ);};	
	Double32_t	GetTime(void)const{ return fTime;};	
	Double_t		GetTrackLength(void)const{ return fLength;};	
	Double_t		GetMass2(void)const{ return fMass2;};	
	Int_t			GetPDGcode(void)const{ return fPDGcode;}; 		// =0 if undefined
	Int_t			GetKFTrackIndex(void)const{ return fKFTrackIndex;};	
	Int_t			GetTofHitIndex(void)const{ return fTofHitIndex;};
	void 		GetTofImpactPoint(TVector3& point)const{point.SetXYZ(fTofImpactPoint[0], fTofImpactPoint[1], fTofImpactPoint[2]);};
	TVector3		GetMomentum(void)const {return TVector3(fPx, fPy, fPz);};
	Int_t			GetCharge(void)const {return fCharge; }
						
	void		AddFlag(Int_t add){fFlag = fFlag | add;}	
	
					
ClassDef(MpdEtofMatchingData,1)
};
//------------------------------------------------------------------------------------------------------------------------
class EtofMatchingFilter;
class MpdEtofMatching : public FairTask 
{
	friend class EtofMatchingFilter;
	 
	Bool_t			fDoTest, fMCDataExist, fPdfExist;
	TString			fGeoFlNm, fTestFlnm;
	TList			fList;
	MpdKalmanFilter 	*pKF;
	Double_t		fTofEndCapZ;

	TH2D			*htCandNmb,*htTrackPerEvent;		
	TH1D			*hKF_est, *hKF_MC, *htRest, *htRPest;
	TH2D			*hPads_Theta_Phi, *hTest_C_D, *htKFTrack, *htTMatch, *htMisMatch, *htMC_est, *htHit_est;	
	
	EtofMatchingFilter	*pMatchingFilter;
		
	MpdTofUtils::PadMap 	mapPads;
	MpdTofUtils::ModMMap	mmapModules;
	MpdTofUtils::RegVec	vecRegions;

	bool			EstTrackOnPlane(const MpdEctKalmanTrack *tr, Double_t Z, TVector3& pos, Double_t& length,  TVector3& Mom, Int_t& charge)const;
	void 			AddEntry(Int_t hitIndex, Int_t kfTrackId, Int_t tofHitId, const MpdEtofHit* hit, Int_t pid, Int_t flag,
							Double_t length,  const TVector3& est_point_Plane, const TVector3& Mom, Int_t charge);

	TClonesArray 		*pTofPoints;		// MC TOF points
	TClonesArray 		*pTofHits;		// TOF hits
	TClonesArray 		*pMCTracks;		// MC tracks
	TClonesArray 		*pKFTracks;		// KF tracks
	TClonesArray 		*pMatchingCollection;	// TOF Matching data;

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
#include "MatchingFilter.h"
class EtofMatchingCandidate : public MatchingCandidate
{
public:
	MpdEtofHit *TofHit;
	Double_t deltaR, deltaPhi; 
	TVector3 estPoint;
	Double_t  trackLength; 

	EtofMatchingCandidate(Int_t trackIndex, Int_t hitindex, Int_t mcpid, Int_t flag, Int_t charge, Double_t dedx, TVector3 mom, 
						MpdEtofHit *tofHit, Double_t DeltaR,  Double_t DeltaPhi, TVector3 EstPoint, Double_t  TrackLength)
	: MatchingCandidate(trackIndex, hitindex, mcpid, flag, charge, mom), TofHit(tofHit), deltaR(DeltaR), deltaPhi(DeltaPhi), trackLength(TrackLength)
	{
		estPoint = EstPoint;
	};
};
//------------------------------------------------------------------------------------------------------------------------
class EtofMatchingFilter : public MatchingFilter
{
public:	
	Int_t 	Processing(Int_t nKFTracks, TH2D*);
	void	FillMatchings(MpdEtofMatching*);
	void	AddCandidate(Int_t mcTrackIndex, Int_t KfIndex, Int_t hitIndex, MpdEtofHit *TofHit, Int_t mcPID, Int_t flag, Double_t trackLength, 
					Double_t dEdX, TVector3 estPoint, TVector3 Mom, Int_t charge, Double_t deltaR, Double_t deltaPhi);
	
private:
	candIter 	FindClosestHit(Int_t KfIndex);
	candIter 	FindClosestTrack(linkIter itLink, Int_t size, Double_t& minDelta);
};
//------------------------------------------------------------------------------------------------------------------------
#endif 


