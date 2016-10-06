//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_MATCHING_DATA_H
#define __MPD_TOF_MATCHING_DATA_H 1

//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTofMatchingData
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------
#include <TObject.h>
#include <TVector3.h>
//------------------------------------------------------------------------------------------------------------------------
class MpdTofHit;
class MpdTofMatchingData : public TObject 
{
        Int_t 		fKFTrackIndex;		//  KF Track index( branchname: "TpcKalmanTrack", classname: "MpdTpcKalmanTrack")
        Int_t 		fTofHitIndex;		//  TofHit index( branchname: "TOFHit", classname: "MpdTofHit")

        Double_t 	fLength;		// Track length [cm], 	{KFTrack refit}
        Int_t      	fNmbTrHits;		// number of track hits	{KFTrack refit}          
        Double_t    	fBeta;          	// calc. 
        Double_t 	fMass2;			// calc., [GeV^2]
        
        // copy data
        Double_t	fMom[3];		// Momentum [GeV/c]	{KFTrack copy}
        Double_t 	fXYZ[3];	      	// Position of hit [cm] {TofHit copy}
        Double_t	fTime;              	// Time since event start [ns]	{TofHit copy}
   	Int_t 		fPDGcode;		// for MC data only {TofPoint copy}
              	
 	// CAUTION: transparent data, MUST be used only at run-time, for debug purpose     
	Float_t		fDelta1, fDelta2; 	//! delta(hitPosition, extrapolated kalman track interception position), [cm]  {KFTrack refit}
        Float_t		fEstPointR[3];		//! Estimated impact point on cylinder(x,y,z), [cm] {KFTrack refit}
        Float_t		fEstPointP[3];		//! Estimated impact point on pad plate(x,y,z), [cm] {KFTrack refit}	
        Float_t		fStripPerp[3];	      	//! perpendicular to strip plate           
      	Int_t      	fFlag;			//! copy of MpdTofHit::fFlag instance.
        Int_t      	fDetectorUID;     	//! copy of FairHit::fDetectorID instance.    
        Int_t 		fCharge;		//! return by MpdTpcKalmanTrack::Charge(), {KFTrack refit}	
        	
public: 
        MpdTofMatchingData();
        MpdTofMatchingData(Int_t kfTrackId, Int_t tofHitId);        
  	MpdTofMatchingData(Int_t kfTrackId, Int_t tofHitId, Int_t nTrHits, const MpdTofHit* hit, Int_t pid, Int_t flag, Double_t length, const TVector3& pointP, const TVector3& Momentum, Int_t charge, Double_t delta1, Double_t delta2);	
  	MpdTofMatchingData(Int_t kfTrackId, Int_t tofHitId, Int_t nTrHits, const MpdTofHit* hit, Int_t pid, Int_t flag, Double_t length, const TVector3& pointR, const TVector3& pointP, 
				const TVector3& perp,  const TVector3& Momentum, Int_t charge, Double_t delta1,  Double_t delta2);							
				
	bool operator==(const MpdTofMatchingData& rhs){ return ( this->fKFTrackIndex == rhs.fKFTrackIndex &&  this->fTofHitIndex == rhs.fTofHitIndex ); }
				
	virtual void 	Print(void)const;
	
	Int_t			GetKFTrackIndex(void)const{ return fKFTrackIndex;};	
	Int_t			GetTofHitIndex(void)const{ return fTofHitIndex;};

	Double_t		GetTrackLength(void)const{ return fLength;};	
	Int_t			GetNmbTrHits(void)const{ return fNmbTrHits;};	
    	Double_t        	GetBeta(void) const { return fBeta;};  
	Double_t		GetMass2(void)const{ return fMass2;};	
	TVector3		GetMomentum(void)const {return TVector3(fMom);};
	
	// CAUTION: getters for transparent(NOT serialized) data
	Float_t			GetDelta(void)const{ return sqrt(fDelta1*fDelta1 + fDelta2*fDelta2);};
	Float_t			GetDelta1(void)const{ return fDelta1;};
	Float_t			GetDelta2(void)const{ return fDelta2;};	

ClassDef(MpdTofMatchingData, 4)
};
//------------------------------------------------------------------------------------------------------------------------

#endif 

