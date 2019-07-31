//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTofHitProducer
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------
#include <assert.h>

#include <TMath.h>
#include <TH1D.h>
#include <TH2D.h>
#include <TEfficiency.h>
#include <TRandom2.h>
#include <TClonesArray.h>

#include "FairMCTrack.h"
#include "FairLogger.h"

#include "MpdTofUtils.h"
#include "MpdTofPoint.h"
#include "MpdTofHit.h"
#include "MpdTofGeoUtils.h"
#include "MpdTofHitProducerQA.h"

#include "MpdTofHitProducer.h"

using namespace std;

ClassImp(MpdTofHitProducer)
//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducer::MpdTofHitProducer(const char *name, Bool_t useMCdata, Int_t verbose, Bool_t test, const char *flnm)
  : MpdTofHitProducerIdeal(name, useMCdata, verbose, test, true, flnm, false)
{
	pRandom = new TRandom2;
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducer::~MpdTofHitProducer()
{
	delete pRandom;
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdTofHitProducer::AddParameters(TString& buf)const
{
	MpdTofHitProducerIdeal::AddParameters(buf);
	buf += TString::Format(", fTimeSigma=%.5g ns", fTimeSigma);
	buf += TString::Format(", fErrX=%.4g cm", fErrX);
	buf += TString::Format(", fErrZ=%.4g cm", fErrZ);	
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus 	MpdTofHitProducer::Init()
{
	InitStatus status = MpdTofHitProducerIdeal::Init();

	MpdTofGeoUtils::Instance()->ParseTGeoManager(pQA, true);//, "geometryTree.txt"); // forced
							
	MpdTofGeoUtils::Instance()->FindNeighborStrips(0.8, pQA, // 0.8 [cm] <--- thresh. distance between neighbor strips			
							true); // forced

	LOG(DEBUG)<<"[MpdTofHitProducer::Init] Initialization finished succesfully.";

return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t 	MpdTofHitProducer::HitExist(Double_t value, Int_t gap) // value - distance to the strip edge [cm]
{
	constexpr Double_t slope = (0.98 - 0.95)/0.2;
	Double_t efficiency = (value > 0.2) ? 0.98 : ( 0.95 + slope*value);
	
  //-------------------------------------
  // 99% ---------
  //              \
  //               \
  //                \
  // 95%             \ 
  //  <-----------|--|
  //            0.2  0. value
  //-------------------------------------
	if(gap == 1 || gap == 3) efficiency /= 2.; // reduce efficiency on outer strip gap

	if(pRandom->Rndm() < efficiency) return true;
return false;	
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t 	MpdTofHitProducer::CrossHitExist(Double_t value, Int_t gap) // value - distance to the strip edge  [cm]
{
	constexpr Double_t slope = (0.3 - 0.0)/0.5;
	Double_t efficiency = (value > 0.5) ? 0. : (0.3 - slope*value);
	
  //-------------------------------------
  // 30%               /
  //                  /
  //                 / 
  //                /
  // 0%            /  
  //  <-----------|----| 
  //            0.5    0. value
  //-------------------------------------
	
	if(efficiency == 0.) return false;

	if(gap == 1 || gap == 3) efficiency /= 2.; // reduce efficiency on outer strip gap	

	if(pRandom->Rndm() < efficiency) return true;
return false;	
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdTofHitProducer::Exec(Option_t *option)
{
	static const TVector3 XYZ_err(fErrX, 0., fErrZ); // error for perpendicular Y axis strips

	aTofHits->Clear();

	TVector3 	mcPosition, smearedPosition, errRotated; 	
    	int		nSingleHits = 0, nCrossHits = 0;
	
	if(fUseMCData)
	{	
		for(Int_t pointIndex = 0, nTofPoint = aMcPoints->GetEntriesFast(); pointIndex < nTofPoint; pointIndex++ )  // cycle by TOF points
		{
			auto pPoint = (MpdTofPoint*) aMcPoints->UncheckedAt(pointIndex);
		
			if(fVerbose > 2) pPoint->Print(""); 		
  
			Int_t tid = pPoint->GetTrackID();	
			Int_t suid = pPoint->GetDetectorID();
			Int_t gap = pPoint->GetGap();

			Double_t time = pRandom->Gaus(pPoint->GetTime(), fTimeSigma); // default 100 ps		
			pPoint->Position(mcPosition);

			const LStrip *pStrip = MpdTofGeoUtils::Instance()->FindStrip(suid);
			double perpPhi = pStrip->perp.Phi(); // [rad]
		
			TVector3 posRotated(mcPosition);
			posRotated.RotateZ(-perpPhi); // rotate to perpendicular Y axis LRS
		
			smearedPosition.SetXYZ(pRandom->Gaus(posRotated.X(), fErrX), posRotated.Y(),  pStrip->center.Z()); // smearing along x axis
			smearedPosition.RotateZ(perpPhi); // rotate to real strip orientation

			errRotated = XYZ_err;
			errRotated.RotateZ(perpPhi); // rotate to real strip orientation

			LStrip::Side_t side;
			Double_t distance = pStrip->MinDistanceToEdge(&mcPosition, side); // [cm]

			bool stripFired;
			if(stripFired = HitExist(distance, gap))  // simulate hit efficiency
			{
			 	AddHit(MpdTofPoint::ClearGap(suid), smearedPosition, errRotated, pointIndex, tid, time, MpdTofUtils::IsSingle); 	
			 	nSingleHits++;		 	
			} 
		
			if(pQA) pQA->FillHitEfficiency(stripFired, distance, gap, mcPosition, smearedPosition);
	
	     		if(stripFired = CrossHitExist(distance, gap)) // simulate cross hit
        		{
        			Int_t crossSuid = (side == LStrip::kRight) ? pStrip->neighboring[LStrip::kRight] : pStrip->neighboring[LStrip::kLeft];
  
  				if(LStrip::kInvalid  == crossSuid) continue; // edge strip on detector
  			
  				pStrip = MpdTofGeoUtils::Instance()->FindStrip(crossSuid);
        			smearedPosition.SetXYZ(pRandom->Gaus(posRotated.X(), fErrX), posRotated.Y(),  pStrip->center.Z());
        			smearedPosition.RotateZ(perpPhi);
        			
        			AddHit(MpdTofPoint::ClearGap(crossSuid), smearedPosition, errRotated, pointIndex, tid, time, MpdTofUtils::IsDouble); 
        			nCrossHits++;			
        		}

        		if(pQA) pQA->FillCrossHitEfficiency(stripFired, distance, gap, mcPosition, smearedPosition);

		}	// cycle by the TOF points
	
	}
	else	//  exp. data used    
	{
		// FIXME: now not realized
		//AddHit(Int_t detUID, const TVector3 &posHit, const TVector3 &posHitErr, Int_t expDigitIndex, Double_t time, Int_t flag)
		assert(false);
	}
	
	MergeHitsOnStrip(); // save only the fastest hit in the strip

	int Nhits = CompressHits(); // remove blank slotes

	if(fUseMCData && pQA)
	{
		pQA->FillNPointsHits(aMcPoints->GetEntriesFast(), Nhits);
		pQA->FillHitDistance(aTofHits);
	}

	LOG(DEBUG1)<<"[MpdTofHitProducer::Exec] single hits= "<<nSingleHits<<", cross hits= "<<nCrossHits<<", final hits= "<<Nhits;
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdTofHitProducer::Finish()
{
	if(pQA) pQA->Finish(); 
}
//--------------------------------------------------------------------------------------------------------------------------------------
void 		MpdTofHitProducer::SetSeed(UInt_t seed)
{
	pRandom->SetSeed(seed);
}
//------------------------------------------------------------------------------------------------------------------------


