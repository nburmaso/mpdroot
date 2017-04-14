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
  : MpdTofHitProducerIdeal(name, useMCdata, verbose, test, true, flnm, false), fTimeSigma(0.100), fErrZ(1./sqrt(12.)), fErrX(0.5),  pRandom(new TRandom2)
{

}
//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducer::~MpdTofHitProducer()
{
	delete pRandom;
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus MpdTofHitProducer::Init()
{
	TString buf = "[MpdTofHitProducer::Init] Begin initialization."; buf += GetParameters();
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, buf.Data());

    //	if(fOnlyPrimary) cout<<" Only primary particles are processed!!! \n"; // FIXME NOT used now ADDD

	if(fUseMCData)
	{
    		aMcPoints = (TClonesArray*) FairRootManager::Instance()->GetObject("TOFPoint");
    		aMcTracks = (TClonesArray*) FairRootManager::Instance()->GetObject("MCTrack");
assert(aMcPoints);
assert(aMcTracks);
	}
	else
	{
    		aExpDigits = (TClonesArray*) FairRootManager::Instance()->GetObject("??????");// FIXME: NOW unknown name
assert(aExpDigits);	
	}
	
        // Create and register output array
        aTofHits = new TClonesArray("MpdTofHit");
        FairRootManager::Instance()->Register("TOFHit", "Tof", aTofHits, kTRUE);

	MpdTofGeoUtils::Instance()->ParseTGeoManager(	fUseMCData, 
							pHitProducerQA ? pHitProducerQA->GetStripLocationHisto() : nullptr, 
							true); // forced
							
	MpdTofGeoUtils::Instance()->FindNeighborStrips(	0.8,	// 0.8 [cm] <--- thresh. distance between neighbor strips,  (see h1TestDistance histo)
							pHitProducerQA ? pHitProducerQA->GetDistanceHisto() : nullptr, 
							pHitProducerQA ? pHitProducerQA->GetNeighborPairHisto(): nullptr, 
							true); // forced

        FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdTofHitProducer::Init] Initialization finished succesfully.");

return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t 	MpdTofHitProducer::HitExist(Double_t val) // val - distance to the pad edge [cm]
{
  const static Double_t slope = (0.98 - 0.95)/0.2;
  Double_t efficiency = (val > 0.2) ? 0.98 : ( 0.95 + slope*val);
	
  //-------------------------------------
  // 99% ---------
  //              \
    //               \
    //                \
    // 95%             \ 
    //  <-----------|--|
    //            0.2  0.
    //-------------------------------------
	
    if(pRandom->Rndm() < efficiency) return true;
    return false;	
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t 	MpdTofHitProducer::DoubleHitExist(Double_t val) // val - distance to the pad edge  [cm]
{
  const static Double_t slope = (0.3 - 0.0)/0.5;
  Double_t efficiency = (val > 0.5) ? 0. : (0.3 - slope*val);
	
  //-------------------------------------
  // 30%               /
  //                  /
  //                 / 
  //                /
  // 0%            /  
  //  <-----------|----|
  //            0.5    0.
  //-------------------------------------
	
  if(efficiency == 0.) return false;
	
  if(pRandom->Rndm() < efficiency) return HitExist(val);
  return false;	
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdTofHitProducer::Exec(Option_t *option)
{
	static const TVector3 XYZ_err(fErrX, 0., fErrZ); // FIXME:  MUST BE ROTATED!!!!

	aTofHits->Clear();
	
	Int_t 		UID, CrossUID, trackID;	
	TVector3 	pos, XYZ_smeared; 	
    	int		nSingleHits = 0, nDoubleHits = 0;
	
	if(fUseMCData)
	{	
		for(Int_t pointIndex = 0, nTofPoint = aMcPoints->GetEntriesFast(); pointIndex < nTofPoint; pointIndex++ )  // cycle by TOF points
		{
			MpdTofPoint *pPoint = (MpdTofPoint*) aMcPoints->UncheckedAt(pointIndex);
		
			if(fVerbose > 2) pPoint->Print(""); 		
  
			trackID = pPoint->GetTrackID();	
			UID	= pPoint->GetDetectorID();
			Double_t time = pRandom->Gaus(pPoint->GetTime(), fTimeSigma); // 100 ps		
			pPoint->Position(pos);
	
			const LStrip *pStrip = MpdTofGeoUtils::Instance()->FindStrip(UID);
			double perpPhi = pStrip->perp.Phi(); // [rad]
		
			TVector3 posRotated(pos);
			posRotated.RotateZ(-perpPhi);
		
			XYZ_smeared.SetXYZ(pRandom->Gaus(posRotated.X(), fErrX), posRotated.Y(),  pStrip->center.Z());
			XYZ_smeared.RotateZ(perpPhi);

			LStrip::Side_t side;
			Double_t distance = pStrip->MinDistanceToEdge(&pos, side); // [cm]

			bool passed;
			if(passed = HitExist(distance)) // check efficiency 
			{
			 	AddHit(UID, XYZ_smeared, XYZ_err, pointIndex, trackID, time, MpdTofUtils::IsSingle); 	
			 	nSingleHits++;
			 	
			 	if(pHitProducerQA) pHitProducerQA->FillSingleHitPosition(pos, XYZ_smeared);			 	
			} 
		
			if(pHitProducerQA) pHitProducerQA->GetSingleHitEfficiency()->Fill(passed, distance);
        		
        		if(passed = DoubleHitExist(distance)) // check cross hit
        		{
        			Int_t CrossUID = (side == LStrip::kRight) ? pStrip->neighboring[LStrip::kRight] : pStrip->neighboring[LStrip::kLeft];
  			
  				if(LStrip::kInvalid  == CrossUID) continue; // last strip on module
  			
  				pStrip = MpdTofGeoUtils::Instance()->FindStrip(CrossUID);
        			XYZ_smeared.SetXYZ(pRandom->Gaus(posRotated.X(), fErrX), posRotated.Y(),  pStrip->center.Z());
        			XYZ_smeared.RotateZ(perpPhi);
        			
        			AddHit(CrossUID, XYZ_smeared, XYZ_err, pointIndex, trackID, time, MpdTofUtils::IsDouble); 
        			nDoubleHits++;
        			
        			if(pHitProducerQA) pHitProducerQA->FillDoubleHitPosition(pos, XYZ_smeared);        			
        		}
        	
        		if(pHitProducerQA) pHitProducerQA->GetDoubleHitEfficiency()->Fill(passed, distance);

		}	// cycle by the TOF points
	
	}
	else	//  exp. data used    
	{
		// FIXME: now not realized
		//AddHit(Int_t detUID, const TVector3 &posHit, const TVector3 &posHitErr, Int_t expDigitIndex, Double_t time, Int_t flag)
		assert(false);
	}
	
	MergeHitsOnStrip(); // save only the fastest hit in the strip

	int nFinally = CompressHits(); // remove blank slotes

        cout<<" -I- [MpdTofHitProducer::Exec] single hits= "<<nSingleHits<<", double hits= "<<nDoubleHits<<", final hits= "<<nFinally<<endl;
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdTofHitProducer::Finish()
{
	if(pHitProducerQA) pHitProducerQA->Finish(); 
}
//--------------------------------------------------------------------------------------------------------------------------------------
void 		MpdTofHitProducer::SetSeed(UInt_t seed)
{
	pRandom->SetSeed(seed);
}
//------------------------------------------------------------------------------------------------------------------------


