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

#include "FairMCTrack.h"

#include "MpdTofUtils.h"
#include "MpdTofPoint.h"
#include "MpdTofHit.h"

#include "MpdTofHitProducer.h"

using namespace std;

ClassImp(MpdTofHitProducer)
//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducer::MpdTofHitProducer(const char *name, Int_t verbose, Bool_t test)
  : MpdTofHitProducerIdeal(name, verbose, test, true), fTimeSigma(0.100), fErrZ(1./sqrt(12.)), fErrX(0.5),  pRandom(new TRandom2), h2TestStrips(nullptr) , h1TestDistance(nullptr), h2TestNeighborPair(nullptr)
{
        if(fDoTest)
    	{
    	    	effTestEfficiencySingleHit = new TEfficiency("effSingleHit", "Efficiency single hit;R, cm;Side", 1000, -0.1, 1.); 						fList.Add(effTestEfficiencySingleHit);
		effTestEfficiencyDoubleHit = new TEfficiency("effDoubleHit", "Efficiency double hit;R, cm;Side", 1000, -0.1, 1.); 						fList.Add(effTestEfficiencyDoubleHit);
		
    		h1TestDistance = new TH1D("TestDistance", "Distance between strips;M, cm;Side", 1000, 0., 100.); 								fList.Add(h1TestDistance); 
     		h2TestStrips = new TH2D("TestStrips", ";Z, cm;#phi, rads", 2000, -300., 300., 500, -3.5, 3.5);									fList.Add(h2TestStrips);    		
    		h2TestMergedTimes = new TH2D("TestMergedTimes", "Merged hits on strip times test;faster hit time, ns;slower hit time, ns", 1000, 5., 105., 1000, 5., 105.);	fList.Add(h2TestMergedTimes);
		h2TestChainPID = new TH2D("TestChainPID", "Merged hits on strip pids test;pid;pid", 2000, -2250.5, 2250.5, 2000, -2250.5, 2250.5);				fList.Add(h2TestChainPID);
		h2TestNeighborPair = new TH2D("TestNeighborPair", "Neighbor strip pairs test;stripID1;stripID2", 30, -0.5, 29.5, 30, -0.5, 29.5);				fList.Add(h2TestNeighborPair);    
		h2TestXYSmeared = new TH2D("TestXYSmeared", "Smeared XY (single hit) test;#Delta, cm;#DeltaZ, cm", 1000, 0., 5., 1000, -1., 1.);				fList.Add(h2TestXYSmeared);
		h2TestXYSmeared2 = new TH2D("TestXYSmeared2", "Smeared XY (single hit) test;X, cm;Y, cm", 1000, -150., 150., 1000, -150., 150.);				fList.Add(h2TestXYSmeared2);		
		h2TestXYSmearedDouble = new TH2D("TestXYSmearedDouble", "Smeared XY (double hit) test;#Delta, cm;#DeltaZ, cm", 1000, 0., 5., 1000, -3., 3.);			fList.Add(h2TestXYSmearedDouble);
		h2TestXYSmearedDouble2 = new TH2D("TestXYSmearedDouble2", "Smeared XY (double hit) test;X, cm;Y, cm", 1000, -150., 150., 1000, -150., 150.);			fList.Add(h2TestXYSmearedDouble2);		
		h2TestEtaPhi = new TH2D("TestEtaPhi", ";#eta;#phi, degree", 1000, -1.6, 1.6, 1000, -181., 181.);								fList.Add(h2TestEtaPhi);
		h2TestRZ = new TH2D("TestRZ", ";Z, cm;R, cm", 1000, -300., 300., 1000, 100., 200.);										fList.Add(h2TestRZ);				
    	}
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducer::~MpdTofHitProducer()
{
	delete pRandom;
	fList.Delete();
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus MpdTofHitProducer::Init()
{
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdTofHitProducer::Init] Begin initialization.");

	FairRootManager *ioman = FairRootManager::Instance(); assert(ioman);
  	aTofPoints  = (TClonesArray *) ioman->GetObject("TOFPoint");
  	aMCTracks   = (TClonesArray *) ioman->GetObject("MCTrack"); 

  	if(!aTofPoints || !aMCTracks){ FairLogger::GetLogger()->Error(MESSAGE_ORIGIN, " Branch not found!"); return kERROR; }
	
        // Create and register output array
        aTofHits = new TClonesArray("MpdTofHit");
        ioman->Register("TOFHit", "Tof", aTofHits, kTRUE);

	MpdTof::ParseTGeoManager(h2TestStrips, true);
	MpdTof::FindNeighborStrips(h1TestDistance, h2TestNeighborPair, fDoTest);

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
	
	Int_t 		nTofPoint = aTofPoints->GetEntriesFast();  
	Int_t 		UID, CrossUID, trackID;	
	TVector3 	pos, XYZ_smeared; 	
    	int		nSingleHits = 0, nDoubleHits = 0;
	
	for(Int_t pointIndex = 0; pointIndex < nTofPoint; pointIndex++ )  // cycle by TOF points
	{
		MpdTofPoint *pPoint = (MpdTofPoint*) aTofPoints->UncheckedAt(pointIndex);
		
		if(fVerbose > 2) pPoint->Print(""); 		
  
		trackID = pPoint->GetTrackID();	
		UID	= pPoint->GetDetectorID();
		Double_t time = pRandom->Gaus(pPoint->GetTime(), fTimeSigma); // 100 ps		
		pPoint->Position(pos);
	
		const LStrip *pStrip = MpdTof::FindStrip(UID);
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

		 	if(fDoTest)
		 	{
		 		h2TestXYSmeared->Fill((pos - XYZ_smeared).Mag(), pos.Z() - XYZ_smeared.Z());
		 		h2TestXYSmeared2->Fill(XYZ_smeared.X(), XYZ_smeared.Y());
		 		h2TestEtaPhi->Fill(pos.Eta(), pos.Phi()*TMath::RadToDeg());
		 		h2TestRZ->Fill(pos.Z(), pos.Perp());
		 	}
		} 
		
		if(fDoTest) effTestEfficiencySingleHit->Fill(passed, distance);
        	
        	if(passed = DoubleHitExist(distance)) // check cross hit
        	{
        		Int_t CrossUID = (side == LStrip::kRight) ? pStrip->neighboring[LStrip::kRight] : pStrip->neighboring[LStrip::kLeft];
  			
  			if(LStrip::kInvalid  == CrossUID) continue; // last strip on module
  			
  			pStrip = MpdTof::FindStrip(CrossUID);
        		XYZ_smeared.SetXYZ(pRandom->Gaus(posRotated.X(), fErrX), posRotated.Y(),  pStrip->center.Z());
        		XYZ_smeared.RotateZ(perpPhi);
        			
        		AddHit(CrossUID, XYZ_smeared, XYZ_err, pointIndex, trackID, time, MpdTofUtils::IsDouble); 
        		nDoubleHits++;
  		
        		if(fDoTest)
        		{
        			h2TestXYSmearedDouble->Fill((pos - XYZ_smeared).Mag(), pos.Z() - XYZ_smeared.Z());
        			h2TestXYSmearedDouble2->Fill(XYZ_smeared.X(), XYZ_smeared.Y());
        		}
        	}
        	
        	if(fDoTest) effTestEfficiencyDoubleHit->Fill(passed, distance);

	}	// cycle by the TOF points
	
	MergeHitsOnStrip(); // save only the fastest hit in the strip

	int nFinally = CompressHits(); // remove blank slotes

        cout<<" -I- [MpdTofHitProducer::Exec] MCpoints= "<<nTofPoint<<", single hits= "<<nSingleHits<<", double hits= "<<nDoubleHits<<", final hits= "<<nFinally<<endl;
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdTofHitProducer::Finish()
{
  	if(fDoTest)
    	{
      		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " [MpdTofHitProducer::Finish] Update  %s file. ", fTestFlnm.Data());
		TFile *ptr = gFile;
		TFile file(fTestFlnm.Data(), "RECREATE");
		fList.Write(); 
		file.Close();
		gFile = ptr;
	}
}
//--------------------------------------------------------------------------------------------------------------------------------------
void 		MpdTofHitProducer::SetSeed(UInt_t seed)
{
	pRandom->SetSeed(seed);
}
//------------------------------------------------------------------------------------------------------------------------


