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
#include "MpdTofMatchingData.h"

#include "FairTask.h"
//------------------------------------------------------------------------------------------------------------------------
class LMatchingFilter;
class MpdKalmanFilter;
class MpdTofMatchingQA;
class MpdTofPoint;
class TRandom2;
class LStrip;

struct ltPt
{
  bool operator()(const MpdKalmanTrack* p1, const MpdKalmanTrack* p2) const //  sorted by Pt descending
  {
    return (p1->Pt() > p2->Pt());
  }
};

typedef std::multimap<const MpdKalmanTrack*, Int_t, ltPt>		TsPt;	// <MpdKalmanTrack*  sorted by Pt descending, KfIndex>
typedef std::multimap<Int_t, MpdTofPoint*> 				TmmP2T; // pair< MCtrackID, MpdTofPoint*>
typedef std::map<Int_t, double>			 			TmStripWeight; // pair<stripUID, matching weight>
typedef std::multimap<Int_t, std::pair<MpdTofHit*, Int_t> > 		TmmD2H; // pair< detUID, pair<MpdTofHit*, hitIndex> >
typedef std::map<Int_t, std::pair<MpdTofHit*, Int_t> > 			TmH2S; // pair< stripUID, pair<MpdTofHit*, hitIndex> >

struct mcInfo
{
        Int_t		trackIndex, pid, Npoints, stripUID;        
        TVector3 	Position;			 	
        bool		TofTouch, HaveCand, HaveTrueCand; //IsSameIDs
       
        mcInfo() : trackIndex(-1), pid(-1), Npoints(-1), stripUID(-1) {}
        
};
//------------------------------------------------------------------------------------------------------------------------
class MpdTofMatching : public FairTask 
{
public:
	enum kMode {kBruteForce, kIntervalTree, kSmearedDummyTracks};
	static const Double_t	isNan; 
	
private:	
        TClonesArray 		*aMcPoints;		//! <--- MC input
        TClonesArray 		*aMcTracks;		//! <--- MC input
        TClonesArray 		*aTofHits;		//! <--- input TOF hits        
        TClonesArray 		*aKFtpcTracks;		//! <--- input KF TPC tracks
        TClonesArray 		*aKFectTracks;		//! <--- input KF Ect Tracks
        TClonesArray 		*aTofMatchings;		//! ---> output
        
	kMode			fMode;
        Bool_t			fDoTest, fUseMCData, fDoMCTest;
        TRandom2		*pRandom;
        Double_t		fTofBarrelRadius;	// [cm]
        Double_t		fThreshZ, fThreshPhi; 	// [cm], acceptance window
        Double_t		fSmearedTheta_plate; 	// [rads], default = 0.01,  msc Theta_{plate} rms for kSmearedDummyTracks method
        Int_t			fNSmeared;		// default = 20, N probe tracks for kSmearedDummyTracks method
 
        MpdKalmanFilter 	*pKF;  			//!
       	MpdTofMatchingQA	*pMatchingQA; 		//!                 
        LMatchingFilter		*pMF; 			//!
	        		
	MpdTpcKalmanTrack 	RefitTrack(const MpdTpcKalmanTrack*);
        TVector3		EstTrackOnR(const MpdTpcKalmanTrack *tr)const;
	bool			EstTrackOnPlane(const MpdTpcKalmanTrack& tr, const TVector3& point, const TVector3& perp, TVector3& pos, Double_t& length, TVector3& Mom, Int_t& charge) const;
	bool			EstSmearedTrackOnPlane(const MpdTpcKalmanTrack& tr, const TVector3& point, const TVector3& perp, TVector3& pos, Double_t& length, TVector3& Mom, Int_t& charge) const;
	

	Int_t 		AddProbeTrackWeights(TmStripWeight&, const TVector3&, const TmH2S&);
	Int_t		ExecByBruteForce(const TsPt& tids, double&);
	Int_t		ExecByIntervalTree(const TsPt& tids, const TmmD2H& dets, const TmmP2T&, double& chi2);
	Int_t		ExecBySmearedDummyTracks(const TsPt& tids, const TmmD2H&, const TmH2S&, const TmmP2T&, double& chi2);

	MpdTofMatching(const MpdTofMatching&);
	MpdTofMatching& operator=(const MpdTofMatching& rhs);
	
public:
	MpdTofMatching(const char *name = "TOF Matching", Int_t verbose = 1, Bool_t DoTest = false, const char *flnm = "QA.MpdTofMatching.root");
	virtual ~MpdTofMatching();

	virtual InitStatus	Init();
	virtual void		Exec(Option_t * option);
	virtual void		Finish();
	
	static void	MappingMcPoints2McTracks(TClonesArray *aMcPoints, TmmP2T& mmMCpoints); // mapping pair<MCtrackID, MpdTofPoint*> and sorting by time
	static void	GetMcInfo(mcInfo& mcData, const TmmP2T& p2ts, const MpdKalmanTrack* pKfTrack, TClonesArray* aMcTracks);	
	static void	GetDelta(const TVector3& mcPos, const TVector3& estPos, double& dev,  double& devZ, double& devR, double& devPhi);
	
	void		SetMatchingMethod(kMode method){ fMode = method; };	
	void		SetTofBarrelRadius(Double_t R){ fTofBarrelRadius = R; }; 
	void		SetRoIWindow(Double_t dZ, Double_t dPhi){ fThreshZ = dZ; fThreshPhi = dPhi; }; 
	void		SetMSCsmearing(Double_t val){ fSmearedTheta_plate = val; }; 	
	
ClassDef(MpdTofMatching,4) // MpdTofMatching
};
//------------------------------------------------------------------------------------------------------------------------

#endif 
