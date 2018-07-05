//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTofMatching
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------
#include <iostream>

#include <TMath.h>

#include "MpdTofHit.h"
#include "MpdTofMatching.h"

#include "MpdTofMatchingData.h"

using namespace std;

ClassImp(MpdTofMatchingData)
//------------------------------------------------------------------------------------------------------------------------
MpdTofMatchingData::MpdTofMatchingData(Int_t kfTrackId, Int_t tofHitId, Int_t nTrHits, const MpdTofHit* hit, Int_t pid, Int_t flag, Double_t length, 
					const TVector3& pointR, const TVector3& pointP, const TVector3& perp, const TVector3& Momentum, Int_t charge, Double_t delta1, Double_t delta2, Double_t weight)
 : fTime(hit->GetTime()), fLength(length), fDelta1(delta1),  fDelta2(delta2), fDetectorUID(hit->GetDetectorID()), fFlag(flag), fCharge(charge), fPDGcode(pid), fKFTrackIndex(kfTrackId), fTofHitIndex(tofHitId), fNmbTrHits(nTrHits), fWeight(weight)
{
	Momentum.GetXYZ(fMom);
	hit->GetXYZ(fXYZ);
	
	pointR.GetXYZ(fEstPointR);
	pointP.GetXYZ(fEstPointP);
	perp.GetXYZ(fStripPerp);
	
	const static Double_t c_vel = TMath::C();
	fBeta  = fLength / (fTime  * 1.e-7) / c_vel;// [cm/nc] -> m/c
	Double_t beta2  = fBeta*fBeta;	
	Double_t gamma2 = 1. / (1. - beta2);			
	fMass2 = Momentum.Mag2() / ( gamma2 * beta2 );
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofMatchingData::MpdTofMatchingData(Int_t kfTrackId, Int_t tofHitId, Int_t nTrHits, const MpdTofHit* hit, Int_t pid, Int_t flag, Double_t length, 
						const TVector3& pointP, const TVector3& Momentum, Int_t charge, Double_t delta1, Double_t delta2, Double_t weight)
 : fTime(hit->GetTime()), fLength(length), fDelta1(delta1), fDelta2(delta2), fDetectorUID(hit->GetDetectorID()), fFlag(flag), fCharge(charge), fPDGcode(pid), fKFTrackIndex(kfTrackId), fTofHitIndex(tofHitId), fNmbTrHits(nTrHits), fWeight(weight)
{
	Momentum.GetXYZ(fMom);
	hit->GetXYZ(fXYZ);

	pointP.GetXYZ(fEstPointP);
	
	const static Double_t c_vel = TMath::C();
	fBeta  = fLength / (fTime  * 1.e-7) / c_vel;// [cm/nc] -> m/c
	Double_t beta2  = fBeta*fBeta;	
	Double_t gamma2 = 1. / (1. - beta2);
	fMass2 = Momentum.Mag2() / ( gamma2 * beta2 );
}						
//------------------------------------------------------------------------------------------------------------------------
MpdTofMatchingData::MpdTofMatchingData()
: fKFTrackIndex(-1), fTofHitIndex(-1), fNmbTrHits(0), fFlag(0), fCharge(0), fPDGcode(0), fWeight(0.)
{
	fTime = fBeta = fMass2 = fLength = fMom[0] = fMom[1] = fMom[2] = fXYZ[0] = fXYZ[1] = fXYZ[2] = MpdTofMatching::isNan;
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofMatchingData::MpdTofMatchingData(Int_t kfTrackId, Int_t tofHitId, Double_t weight)
 : fKFTrackIndex(kfTrackId), fTofHitIndex(tofHitId), fNmbTrHits(0),  fFlag(0), fCharge(0), fPDGcode(0), fWeight(weight)
{
	fTime = fBeta = fMass2 = fLength = fMom[0] = fMom[1] = fMom[2] = fXYZ[0] = fXYZ[1] = fXYZ[2] = MpdTofMatching::isNan;
}
//------------------------------------------------------------------------------------------------------------------------
void 	MpdTofMatchingData::Print(void) const
{
	cout<<"\n-I- pair<"<<fKFTrackIndex<<", "<<fTofHitIndex<<">, fDelta1="<<fDelta1<<", fDelta2="<<fDelta2<<", PDGcode="<<fPDGcode<<", UID="<<fDetectorUID<<", flag="<<fFlag
		<<", Momentum("<<fMom[0]<<","<<fMom[1]<<","<<fMom[2]<<"), Pos("<<fXYZ[0]<<","<<fXYZ[1]<<","<<fXYZ[2]<<"), length="<<fLength;
}
//------------------------------------------------------------------------------------------------------------------------
