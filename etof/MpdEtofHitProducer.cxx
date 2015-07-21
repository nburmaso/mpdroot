
//------------------------------------------------------------------------------------------------------------------------
/// \class MpdEtofHitProducer
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include <iostream>
#include <fstream>
#include <valarray>
#include <assert.h>

#include "TClonesArray.h"
#include "TRandom.h"
#include "TVector3.h"
#include "TStopwatch.h"
#include "TMath.h"

#include "FairRunAna.h"
#include "FairBaseParSet.h"
#include "FairRuntimeDb.h"
#include "FairMCApplication.h"
#include "FairDetector.h"
#include "FairRootManager.h"
#include "FairMCTrack.h"
#include "FairGeoVolume.h"

#include "MpdEtofGeoPar.h"
#include "MpdEtofPoint.h"
#include "MpdEtofHit.h"
#include "MpdEtof.h"

#include "MpdEtofHitProducer.h"

using namespace std;

ClassImp(MpdEtofHitProducer)
//------------------------------------------------------------------------------------------------------------------------
MpdEtofHitProducer::MpdEtofHitProducer(const char *name, Int_t verbose, Bool_t test)
  : MpdEtofHitProducerIdeal(name, verbose, test, true), fTimeSigma(0.100), pRandom(new TRandom2), h2TestStrips(nullptr), h1TestDistance(nullptr), h2TestNeighborPair(nullptr), fErrR(1./sqrt(12.)), fErrPhi(0.5)
{
	if(fDoTest)
    	{
    	    	h2TestMergedTimes = new TH2D("eTestMergedTimes", "Merged hits on strip times test;faster hit time, ns;slower hit time, ns", 1000, 5., 105., 1000, 5., 105.);	fList.Add(h2TestMergedTimes);
  		h2TestChainPID = new TH2D("eTestChainPID", "Merged hits on strip pids test;pid;pid", 2000, -2250.5, 2250.5, 2000, -2250.5, 2250.5);				fList.Add(h2TestChainPID);
  		h2TestStrips = new TH2D("eTestStrips", ";R, cm;#phi, rads", 1000, 20., 170., 1000, -3.15, 3.15);								fList.Add(h2TestStrips);  
  		  
  		h1TestDistance = new TH1D("eTestDistance", "Distance between strips;M, cm;Side", 1000, 0., 1000.); 								fList.Add(h1TestDistance); 
  		h2TestNeighborPair = new TH2D("eTestNeighborPair", "Neighbor strip pairs test;stripID1;stripID2", 150, -0.5, 149.5, 150, -0.5, 149.5);				fList.Add(h2TestNeighborPair);
    	}
}
//------------------------------------------------------------------------------------------------------------------------
MpdEtofHitProducer::~MpdEtofHitProducer()
{
  	delete pRandom;	
  	fList.Delete();
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus		MpdEtofHitProducer::Init()
{
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdEtofHitProducer::Init] Begin initialization.");

	FairRootManager *ioman = FairRootManager::Instance(); assert(ioman);
  	aTofPoints  = (TClonesArray *) ioman->GetObject("ETOFPoint");
  	aMCTracks   = (TClonesArray *) ioman->GetObject("MCTrack"); 

  	if(!aTofPoints || !aMCTracks){ FairLogger::GetLogger()->Error(MESSAGE_ORIGIN, " Branch not found!"); return kERROR; }
	
  	// Create and register output array
  	aTofHits = new TClonesArray("MpdEtofHit");
  	ioman->Register("ETOFHit", "ETof", aTofHits, kTRUE);

 	MpdEtof::ParseTGeoManager(h2TestStrips, true);
	MpdEtof::FindNeighborStrips(h1TestDistance, h2TestNeighborPair, fDoTest);
	
        FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdEtofHitProducer::Init] Initialization finished succesfully.");

return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t 	MpdEtofHitProducer::HitExist(Double_t val) // val - rasstojanie do kraja pad
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
Bool_t 	MpdEtofHitProducer::DoubleHitExist(Double_t val) // val - rasstojanie do kraja pad
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
void 		MpdEtofHitProducer::Exec(Option_t *option)
{
	static const TVector3 XYZ_err(0.1, 0.1, 0.1); // FIXME: dummy now,  MUST BE ROTATED!!!!
	
	aTofHits->Clear();
	
	Int_t 		nTofPoint = aTofPoints->GetEntriesFast();  
	Int_t 		UID, CrossUID, trackID;	
	TVector3 	pos, XYZ_smeared; 	
    	int		nSingleHits = 0, nDoubleHits = 0;
	
	for(Int_t pointIndex = 0; pointIndex < nTofPoint; pointIndex++ )  // cycle by TOF points
	{
		MpdEtofPoint *pPoint = (MpdEtofPoint*) aTofPoints->UncheckedAt(pointIndex);
		
		if(fVerbose > 2) pPoint->Print(""); 		
  
		trackID = pPoint->GetTrackID();	
		UID	= pPoint->GetDetectorID();
		Double_t time = pRandom->Gaus(pPoint->GetTime(), fTimeSigma); // 100 ps		
		pPoint->Position(pos);
		
		// Calc Rerr, PhiErr onto MRF
		const LStrip *pStrip = MpdEtof::FindStrip(UID);
		double centerPhi = pStrip->center.Phi(); // [rad]
		
		TVector3 posRotated(pos);
		posRotated.RotateZ(-centerPhi);
		
		XYZ_smeared.SetXYZ(posRotated.X(), pRandom->Gaus(posRotated.Y(), fErrPhi), posRotated.Z());
		XYZ_smeared.RotateZ(centerPhi);
			
		LStrip::Side_t side;
		Double_t distance = pStrip->MinDistanceToEdge(&pos, side); // [cm]

		bool passed;
		if(passed = HitExist(distance)) // check efficiency 
		{
		 	AddHit(UID, XYZ_smeared, XYZ_err, pointIndex, trackID, time, MpdTofUtils::IsSingle); 	
		 	nSingleHits++;

		 	if(fDoTest)
		 	{
		 	//	h2TestXYSmeared->Fill((pos - XYZ_smeared).Mag(), pos.Z() - XYZ_smeared.Z());
		 	//	h2TestXYSmeared2->Fill(XYZ_smeared.X(), XYZ_smeared.Y());
		 	//	h2TestEtaPhi->Fill(pos.Eta(), pos.Phi()*TMath::RadToDeg());
		 	//	h2TestRZ->Fill(pos.Z(), pos.Perp());
		 	}
		}		
		
		//if(fDoTest) effTestEfficiencySingleHit->Fill(passed, distance);
        	
        	if(passed = DoubleHitExist(distance)) // check cross hit
        	{
        		Int_t CrossUID = (side == LStrip::kRight) ? pStrip->neighboring[LStrip::kRight] : pStrip->neighboring[LStrip::kLeft];
  			
  			if(LStrip::kInvalid  == CrossUID) continue; // last strip on module
  			
  			// Calc Rerr, PhiErr onto MRF
   			pStrip = MpdEtof::FindStrip(CrossUID); 			
        	//	XYZ_smeared.SetXYZ(pRandom->Gaus(posRotated.X(), fErrX), posRotated.Y(),  pStrip->center.Z());
        	//	XYZ_smeared.RotateZ(perpPhi);
        			
        		AddHit(CrossUID, XYZ_smeared, XYZ_err, pointIndex, trackID, time, MpdTofUtils::IsDouble); 
        		nDoubleHits++;
  		
        		if(fDoTest)
        		{
        			//h2TestXYSmearedDouble->Fill((pos - XYZ_smeared).Mag(), pos.Z() - XYZ_smeared.Z());
        			//h2TestXYSmearedDouble2->Fill(XYZ_smeared.X(), XYZ_smeared.Y());
        		}
        	}
        	
        	//if(fDoTest) effTestEfficiencyDoubleHit->Fill(passed, distance);
		
	}	// <---Loop over the TOF points
	
	MergeHitsOnStrip(); // save only the fastest hit in the strip

	int nFinally = CompressHits(); // remove blank slotes

        cout<<" -I- [MpdEtofHitProducer::Exec] MCpoints= "<<nTofPoint<<", single hits= "<<nSingleHits<<", double hits= "<<nDoubleHits<<", final hits= "<<nFinally<<endl;	
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdEtofHitProducer::Finish()
{
	if(fDoTest)
	{
		TFile *ptr = gFile;
		TFile file(fTestFlnm.Data(), "RECREATE");
		fList.Write(); 
		file.Close();
		gFile = ptr;
	}
}
//------------------------------------------------------------------------------------------------------------------------



