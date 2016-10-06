//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTofMatching
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------
#include <iostream>
#include <fstream>
#include <limits>

#include <TMath.h>
#include <TFile.h>
#include <TRandom2.h>
#include <TRotation.h>
#include <TStorage.h>
#include <TEfficiency.h>

#include "FairRootManager.h"
#include "FairMCTrack.h"
#include "FairRunAna.h"
#include "FairLogger.h" 

#include "MpdEctKalmanTrack.h"
#include "MpdTofPoint.h" 
#include "MpdTof.h"
#include "MpdKalmanFilter.h"
#include "MpdTofMatchingQA.h"
#include "MpdTofGeoUtils.h"
#include "LMatchingFilter.h"

#include "MpdTofMatching.h"

#ifdef _OPENMP
#include "omp.h"
#include <sys/time.h> 
#endif

using namespace std;

//------------------------------------------------------------------------------------------------------------------------
struct less_by_pointer 
{
    inline bool operator() (const intervalType& struct1, const intervalType& struct2)
    {
        return (struct1.value < struct2.value);
    }
};
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdTofMatching)
const Double_t	MpdTofMatching::isNan = numeric_limits<double>::quiet_NaN();
//------------------------------------------------------------------------------------------------------------------------
MpdTofMatching::MpdTofMatching(const char *name, Int_t verbose, Bool_t test, const char *flnm)
  : FairTask(name, verbose), fDoTest(test), fUseMCData(false), fMode(kIntervalTree), pKF(nullptr), pRandom(new TRandom2),
  fThreshZ(1.5), fThreshPhi(15.), fSmearedTheta_plate(0.001), fNSmeared(20), fTofBarrelRadius(152.),  
  aMcPoints(nullptr), aMcTracks(nullptr), aTofHits(nullptr), aKFtpcTracks(nullptr), aKFectTracks(nullptr), aTofMatchings(nullptr)
{
	pMatchingQA = fDoTest ? new MpdTofMatchingQA(flnm, false) : nullptr;

	pMF = new LMatchingFilter(pMatchingQA, fVerbose);
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofMatching::~MpdTofMatching()
{
    	delete pMF;
    	delete pRandom;    
    	delete pMatchingQA;	
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus	  MpdTofMatching::Init()
{
  	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdTofMatching::Init] Begin initialization(matching method = %d).", fMode);

  	aMcPoints = 	(TClonesArray*) FairRootManager::Instance()->GetObject("TOFPoint");
   	aMcTracks   = 	(TClonesArray*) FairRootManager::Instance()->GetObject("MCTrack");  	
	aTofHits  = 	(TClonesArray*) FairRootManager::Instance()->GetObject("TOFHit");
	aKFtpcTracks   = (TClonesArray*) FairRootManager::Instance()->GetObject("TpcKalmanTrack"); 
	aKFectTracks   = (TClonesArray*) FairRootManager::Instance()->GetObject("EctTrack"); 
	
	if(aMcPoints && aMcTracks) fUseMCData = true;
	
  	if(!aTofHits  || !aKFtpcTracks){ FairLogger::GetLogger()->Error(MESSAGE_ORIGIN, "Branch not found!"); return kERROR; }

  	pKF = MpdKalmanFilter::Instance("KF","KF");
	
	// Create and register output array
  	aTofMatchings = new TClonesArray("MpdTofMatchingData");
  	FairRootManager::Instance()->Register("TOFMatching", "Tof", aTofMatchings, kTRUE);
  	
	pMF->SetContainer(aTofMatchings);

	MpdTofGeoUtils::Instance()->ParseTGeoManager(fUseMCData, nullptr, false); 
	MpdTofGeoUtils::Instance()->FindNeighborStrips(0.8, nullptr, nullptr, false);// 0.8 [cm] <--- thresh. distance between neighbor strips,  (see h1TestDistance histo)

	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdTofMatching::Init] Initialization finished succesfully.");

return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdTofMatching::Finish()
{ 
	if(pMatchingQA) pMatchingQA->Finish(); 
}
//------------------------------------------------------------------------------------------------------------------------
Int_t		MpdTofMatching::ExecByBruteForce(const TsPt& tids, double& chi2)
{
	Int_t 		charge, uid, mcPID = -1; 
	Double_t 	trackLength;
     	TVector3	hitPosition, estPointOnCyl, estPointOnPlate, Momentum;    	
     	const LStrip *pStrip;
     	
     	Int_t nTofHits = aTofHits->GetEntriesFast(); 
	for(TsPt::const_iterator it = tids.begin(), itEnd = tids.end(); it != itEnd; it++)// cycle by TPC KF tracks
	{   	
		const MpdTpcKalmanTrack *pKfTrack = (MpdTpcKalmanTrack*) it->first;
		Int_t KfIndex = it->second;
	
		for(Int_t hitIndex = 0; hitIndex < nTofHits; hitIndex++) // cycle by tof hits
		{   
			MpdTofHit *pTofHit = (MpdTofHit*) aTofHits->At(hitIndex);
			Int_t volumeUID = pTofHit->GetDetectorID();

			pStrip = MpdTofGeoUtils::Instance()->FindStrip(volumeUID);

			MpdTpcKalmanTrack ReFittedTrack(RefitTrack(pKfTrack));
			
			if(EstTrackOnPlane(ReFittedTrack, pStrip->center, pStrip->perp, estPointOnPlate, trackLength, Momentum, charge)) // Estimate point on plane; true, if point exist
			{
				pTofHit->Position(hitPosition);
							 
				double devHit, devHitZ, devHitR, devHitPhi;
				GetDelta(hitPosition, estPointOnPlate, devHit, devHitZ, devHitR, devHitPhi); 		
			
				if(abs(devHitZ) < fThreshZ && abs(devHitPhi) < fThreshPhi)	
				{	
					pMF->AddCandidate(MpdTofMatchingData(KfIndex, hitIndex, pKfTrack->GetNofTrHits(), pTofHit, mcPID, pTofHit->GetFlag(), trackLength, estPointOnCyl, estPointOnPlate, pStrip->center, Momentum, charge, devHitR, devHitPhi));
				}
			}						

		} // cycle by tof hits		

	} // cycle by TPC KF tracks
	
	Int_t nKFTracks = aKFtpcTracks->GetEntriesFast();
	Int_t MatchingOK = pMF->Processing(nKFTracks, chi2);	// accept candidates
	Int_t nEntries = pMF->UpdateContainer(); 		// remove unmatched candidates

assert(nEntries	== MatchingOK);
return MatchingOK;	
}
//------------------------------------------------------------------------------------------------------------------------
Int_t		MpdTofMatching::ExecByIntervalTree(const TsPt& tids, const TmmD2H& dets, const TmmP2T& p2ts, double& chi2)
{
	const intervalTreeType*	mDetectorsZ = MpdTofGeoUtils::Instance()->GetDetZ();
	const intervalTreeType*	mDetectorsPhi = MpdTofGeoUtils::Instance()->GetDetPhi();
			
     	TVector3 	hitPosition, estPointOnCyl, estPointOnPlate, Momentum;		
	vector<intervalType> 	segmentZ, segmentPhi, intersect;
	
	// The MC run variables. VALID ONLY if fUseMCData = true	
        Int_t		mcTrackIndex = -1,  mcPID = -1;        
        TVector3 	mcPosition;			 
        Int_t		mcNpoints = -1; 			
        bool		mcTofTouch, mcIsSameIDs, mcHaveCand, mcHaveTrueCand;
        Int_t 		mcPadUID;
     
//cout<<"\n ------------------------------------------------------------------------------------------------------------->> EVENT";    
//mDetectorsZ->dump("\n\n ----->>>	mDetectorsZ INTERVALS");
//mDetectorsPhi->dump("\n\n ----->>>          mDetectorsPhi INTERVALS");

	TmmP2T::const_iterator 	mmMCpointCiter;
	TmmD2H::const_iterator	mmHitCiter;
	Int_t  selectedTracks  = 0;

	for(TsPt::const_iterator it = tids.begin(), itEnd = tids.end(); it != itEnd; it++)// cycle by TPC KF tracks
	{   	
		const MpdTpcKalmanTrack *pKfTrack = (MpdTpcKalmanTrack*) it->first;
		Int_t KfIndex = it->second;
	
		if(fUseMCData) mcTofTouch = MpdTofMatching::GetMcInfo(mcTrackIndex, mcPadUID, mcPosition, mcPID, mcNpoints, p2ts, pKfTrack, aMcTracks);
	
                estPointOnCyl = EstTrackOnR(pKfTrack); 		// Estimate point on cylinder		
			
		if(fDoMCTest)
		{
			if(mcNpoints == 1) // only SINGLE tof point per MCtrack
				pMatchingQA->FillDevPoint2EstCyl(mcPosition, estPointOnCyl, pKfTrack->Momentum());
				
		 	if(mcTofTouch) // KF track have TOFpoint 
			{
				selectedTracks++; 	
				pMatchingQA->FillSelectedKfTracks(pKfTrack->Momentum3().Eta(), pKfTrack->Momentum());
			}
		}
             	
		// ---------------------------------------------------------------------------------------->>> Looking for overlaping of estPointR & detectors
		double estZ = estPointOnCyl.Z(), estPhi = estPointOnCyl.Phi();	

		const double Zerror = 10.; // [cm] // FIXME:  should be dependent on the parameters of the KFtrack
		const double PhiError = 0.1; // [rads] // FIXME:  should be dependent on the parameters of the KFtrack

		segmentZ.clear();
		segmentPhi.clear();
		intersect.clear();
		
		mDetectorsZ->findOverlapping(estZ-Zerror,  estZ+Zerror, segmentZ); 	
		mDetectorsPhi->findOverlapping(estPhi-PhiError, estPhi+PhiError, segmentPhi);

//cout<<"\n <<<--Z-->>>>> ("<<estZ-Zerror<<", "<<estZ+Zerror<<")";
//MpdTof::intervalTreeType::dumpIntervals(&segmentZ, "\n\n ----->>>	mDetectorsZ findOverlapping");

//cout<<"\n <<<--Phi-->>>>> ("<<estPhi-PhiError<<", "<<estPhi+PhiError<<")";
//MpdTof::intervalTreeType::dumpIntervals(&segmentPhi, "\n\n ----->>>	mDetectorsPhi findOverlapping");
		
		if(!segmentZ.empty() && !segmentPhi.empty()) // have overlaped segments both Z and Phi 
		{
			// calc. intersection
			sort(segmentZ.begin(), segmentZ.end(), less_by_pointer());
    			sort(segmentPhi.begin(), segmentPhi.end(), less_by_pointer());  
		 	set_intersection(segmentZ.begin(), segmentZ.end(), segmentPhi.begin(), segmentPhi.end(), std::back_inserter(intersect), less_by_pointer());  //FIXME: MAYBE  std::inserter ???? 

			MpdTpcKalmanTrack ReFittedTrack(RefitTrack(pKfTrack));

			for(vector<intervalType>::const_iterator cit = intersect.begin(), citEnd = intersect.end(); cit != citEnd; cit++) // cycle by the overlaped detectors
			{
				Int_t detUID = (*cit).value->volumeUID;
				Int_t charge;
				Double_t trackLength;
				
				if(EstTrackOnPlane(ReFittedTrack, (*cit).value->center, (*cit).value->perp, estPointOnPlate, trackLength, Momentum, charge)) // Estimate point on detector plane; true, if point exist
				{		
					if(fDoMCTest && mcNpoints == 1) // only one tof point per MCtrack
						pMatchingQA->FillDevPoint2EstP(mcPosition, estPointOnPlate, pKfTrack->Momentum());
				
					mmHitCiter = dets.find(detUID);
					if(mmHitCiter != dets.end()) // the estimated detector have hits
					{
						int counter = dets.count(detUID);
						for(int hit = 0; hit < counter; hit++, mmHitCiter++) // cycle by hits into the estimated detector
						{
	 						MpdTofHit *TofHit = mmHitCiter->second.first;
	 						Int_t hitIndex = mmHitCiter->second.second;
							TofHit->Position(hitPosition);
							 
							double devHit, devHitZ, devHitR, devHitPhi;
							GetDelta(hitPosition, estPointOnPlate, devHit, devHitZ, devHitR, devHitPhi); 
							
							bool mcIsSameVolumeUID = (TofHit->GetDetectorID() == mcPadUID);						

							if(fDoMCTest) pMatchingQA->FillHitDev2EstP(mcIsSameVolumeUID, devHitZ, devHitPhi);
					
							devHitZ = abs(devHitZ);
							devHitPhi = abs(devHitPhi);
							
							if(devHitZ < fThreshZ && devHitPhi < fThreshPhi) // inside matching window
							{	
								if(fDoMCTest)
								{
									if(mcIsSameVolumeUID) mcHaveTrueCand = true;
									mcHaveCand = true;
								}
								
								pMF->AddCandidate(MpdTofMatchingData(KfIndex, hitIndex, pKfTrack->GetNofTrHits(), TofHit, mcPID, TofHit->GetFlag(), trackLength, estPointOnCyl, estPointOnPlate, (*cit).value->center, Momentum, charge, devHitZ, devHitPhi));
							}
							
						} // cycle by hits into the estimated detector
					}
		
				} // Estimate point on plane; true, if point exist
				
			} // cycle by the overlaped detectors 
		
		} // have overlaped segments both Z and Phi 
		
		if(fDoMCTest && mcTofTouch) pMatchingQA->FillCandidates(mcHaveTrueCand, mcHaveCand, pKfTrack->Momentum3().Eta(), pKfTrack->Momentum());
			
	} // cycle by KF tracks

	Int_t nKFTracks = aKFtpcTracks->GetEntriesFast();
	if(fDoTest) pMatchingQA->FillTrackPerEvent(nKFTracks, selectedTracks);	

	Int_t MatchingOK = pMF->Processing(nKFTracks, chi2);	// accept candidates
	Int_t nEntries = pMF->UpdateContainer(); 		// remove unmatched candidates

assert(nEntries	== MatchingOK);
return MatchingOK;
}
//------------------------------------------------------------------------------------------------------------------------	
Int_t		MpdTofMatching::ExecBySmearingDummyTracks(const TsPt& tids, const TmmD2H& dets, double& chi2)
{
	Int_t 		detUID;
	TVector3 	estPointOnPlate, Momentum, hitPosition;
	Double_t 	trackLength, devRough;
	Int_t 		charge;
	
	TmmD2H::const_iterator	mmHitCiter;		
	const LStrip *pStrip;   	
     	TmmW2C mmWeightedHits; // pair< cand. weight, pair<MpdTofHit*, LStrip*> >
	
	for(TsPt::const_iterator it = tids.begin(), itEnd = tids.end(); it != itEnd; it++)// cycle by TPC KF tracks
	{   	
		const MpdTpcKalmanTrack *pKfTrack = (MpdTpcKalmanTrack*) it->first;
		Int_t KfIndex = it->second;
		
		for(TmmD2H::const_iterator iter = dets.begin(), iterEnd = dets.end(); iter != iterEnd;  iter = dets.upper_bound(iter->first)) // cycle by unique detectorUID
		{
	 		detUID  = iter->first;
	 		
	 		// rough test
 			pStrip = MpdTofGeoUtils::Instance()->FindStrip(detUID+1);// detectorUID ---> volumeUID, stripID = 1

			MpdTpcKalmanTrack ReFittedTrack(RefitTrack(pKfTrack));	 		
	 		
	 		if(EstTrackOnPlane(ReFittedTrack, pStrip->center, pStrip->perp, estPointOnPlate, trackLength, Momentum, charge)) // Estimate point on plane; true, if point exist
			{
	 			devRough = (estPointOnPlate - pStrip->center).Mag(); 			
	 			if(devRough > 80.) continue; // [cm]  			
 	
 				mmWeightedHits.clear();
 				
 				for(int sample = 0; sample < fNSmeared; sample++)
 				{	
 					MpdTpcKalmanTrack ReFittedTrack(RefitTrack(pKfTrack));	 
 					
 					if(EstSmearedTrackOnPlane(ReFittedTrack, pStrip->center, pStrip->perp, estPointOnPlate, trackLength, Momentum, charge))
 						AddSample(mmWeightedHits, estPointOnPlate, iter, dets.count(detUID));				
	 			}
	 			
	 			
	 			
/*	 		
				mmHitCiter = dets.find(detUID); //  pair< detUID, pair<MpdTofHit*, hitIndex> >		
assert(mmHitCiter != dets.end()); // the  detector have hits
					
			
			
				int counter = dets.count(detUID);
				for(int hit = 0; hit < counter; hit++, mmHitCiter++) // cycle by hits into the estimated detector
				{
	 				MpdTofHit *TofHit = mmHitCiter->second.first;
	 				Int_t hitIndex = mmHitCiter->second.second;
					TofHit->Position(hitPosition);
			
			
				}
				
*/				
							 
	 		}
		}
		
	} // cycle by TPC KF tracks
}
//------------------------------------------------------------------------------------------------------------------------
Int_t 	MpdTofMatching::AddSample(const TmmW2C& mWeights, const TVector3& position, TmmD2H::const_iterator iter, Int_t nHits)
{   	
	for(int hit = 0; hit < nHits; hit++, iter++) // cycle by hits on selected detector  it ---> pair< detUID, pair<MpdTofHit*, hitIndex> >
	{
		Int_t detUID  = iter->first;
		
		const LStrip *pStrip = MpdTofGeoUtils::Instance()->FindStrip(detUID+1);



	
	}
	

}
//------------------------------------------------------------------------------------------------------------------------
bool	MpdTofMatching::GetMcInfo(Int_t& mcTrackIndex, Int_t& mcPadUID, TVector3& mcPosition, Int_t& mcPID, Int_t& mcNpoints, const TmmP2T& p2ts, const MpdKalmanTrack* pKfTrack, TClonesArray* aMcTracks)
{
	mcTrackIndex = pKfTrack->GetTrackID();
	FairMCTrack *mcTrack = (FairMCTrack*) aMcTracks->UncheckedAt(mcTrackIndex);
			
	mcPID = mcTrack->GetPdgCode();               
	mcNpoints = p2ts.count(mcTrackIndex);
	bool mcTofTouch = (mcNpoints >= 1);
		              	
	TmmP2T::const_iterator cit = p2ts.find(mcTrackIndex); 
	
	if(cit != p2ts.end())
	{	
		cit->second->Position(mcPosition); // update mcPosition
		mcPadUID = cit->second->GetDetectorID();
	}
	else	assert(mcTofTouch == false);
	
return 	mcTofTouch;
}
//------------------------------------------------------------------------------------------------------------------------
void	MpdTofMatching::MappingMcPoints2McTracks(TClonesArray *aMcPoints, TmmP2T& mmMCpoints)
{
	for(Int_t index = 0, nTofPoints = aMcPoints->GetEntriesFast(); index < nTofPoints; index++)  // cycle by MpdTofPoint
	{
		MpdTofPoint *mcPoint = (MpdTofPoint*) aMcPoints->UncheckedAt(index);

		Int_t trackID = mcPoint->GetTrackID();
		Double_t time = mcPoint->GetTime();

		TmmP2T::iterator iter = mmMCpoints.find(trackID);
		if(iter != mmMCpoints.end()) // same trackID already inserted, insert to position (sorting by time)
		{
			int count = mmMCpoints.count(trackID);
			for(int i = 0; i < count; i++, iter++) // cycle by hits with same trackID
			{
 				if(time < iter->second->GetTime())
				{
					mmMCpoints.insert(iter, make_pair(trackID, mcPoint));
					break;	
				}

				if(i == count-1) mmMCpoints.insert(++iter, make_pair(trackID, mcPoint)); // insert to last		
			}
		}
		else 	mmMCpoints.insert(make_pair(trackID, mcPoint));

	} // cycle by MpdTofPoint	
}		
//------------------------------------------------------------------------------------------------------------------------
void 		MpdTofMatching::Exec(Option_t *option)
{
	fDoMCTest = fUseMCData && fDoTest;

	// Reset event
        aTofMatchings->Clear();
	pMF->Reset();
	
	if(fDoMCTest) pMF->mcFindTrueMachings<MpdTpcKalmanTrack>(aMcPoints, aTofHits, aMcTracks, aKFtpcTracks, false);// true);		
	
        Int_t nTofPoints = -1, nMCTracks = -1;
	if(fUseMCData){ nTofPoints = aMcPoints->GetEntriesFast(); nMCTracks = aMcTracks->GetEntriesFast();}
	Int_t nTofHits = aTofHits->GetEntriesFast();  	
	Int_t nKFTracks = aKFtpcTracks->GetEntriesFast();
	
	Int_t nKFectTracks = (aKFectTracks) ?  aKFectTracks->GetEntriesFast() : 0;
        if(fVerbose) cout<<" -I- [MpdTofMatching::Exec] points= "<<nTofPoints<<", hits= "<<nTofHits<<", mc tracks= "<<nMCTracks<<", kf tracks= "<<nKFTracks<<endl;
        
	// ---------------------------------------------------------------------------------------->>> Select (! propagated to Etof) & sort by Pt  the KfTpcTracks
	std::set<Int_t> sTIDs;
	
	if(aKFectTracks != nullptr)
	for(Int_t index = 0; index < nKFectTracks; index++) // cycle by ECT KF tracks
	{
		MpdEctKalmanTrack *KfeTrack = (MpdEctKalmanTrack*) aKFectTracks->UncheckedAt(index);
		if(KfeTrack->IsFromTpc()) sTIDs.insert(KfeTrack->GetTpcIndex());
	}	
	
	TsPt		sTpcTracks;
        for(Int_t KfIndex = 0; KfIndex < nKFTracks; KfIndex++) 	// cycle by TPC KF tracks
	{   			
		if(aKFectTracks != nullptr) 
		{
			if(sTIDs.find(KfIndex) != sTIDs.end()) continue; // postpone for matching with ETof					
		}
		
		MpdTpcKalmanTrack *pKfTrack = (MpdTpcKalmanTrack*) aKFtpcTracks->UncheckedAt(KfIndex);	
		
		sTpcTracks.insert(make_pair(pKfTrack, KfIndex));
	}
	// ---------------------------------------------------------------------------------------->>> Sorting & Mapping points to MC tracks
	TmmP2T 			mmMCpoints;  // pair< MCtrackID, MpdTofPoint*>
	TmmP2T::iterator 	mmMCpointIter;	
		
	if(fUseMCData) MpdTofMatching::MappingMcPoints2McTracks(aMcPoints, mmMCpoints);
	
	// ---------------------------------------------------------------------------------------->>> Mapping hits to detectorUIDs	
	TmmD2H 			mmHits; // pair< detUID, pair<MpdTofHit*, hitIndex> >

        for(Int_t hitIndex = 0; hitIndex < nTofHits; hitIndex++) // cycle by tof hits
	{   
		MpdTofHit *pTofHit = (MpdTofHit*) aTofHits->At(hitIndex);
		Int_t volumeUID = pTofHit->GetDetectorID();	
	
		mmHits.insert(make_pair( (volumeUID & 0xFFFFFF00), make_pair(pTofHit, hitIndex))); // convert strip volumeUID to detectorUID (reset stripID to 0)
	}	
	// ----------------------------------------------------------------------------------------
		
	Double_t 	chi2 = isNan;
	Int_t 		MatchingOK = 0;	
	
	switch(fMode)
	{
		case kBruteForce:
			MatchingOK = ExecByBruteForce(sTpcTracks, chi2);
		break;	
		case kIntervalTree:
			MatchingOK = ExecByIntervalTree(sTpcTracks, mmHits, mmMCpoints, chi2);
		break;	
		case kSmearingDummyTracks:
			MatchingOK = ExecBySmearingDummyTracks(sTpcTracks, mmHits, chi2);
		break;
	};
	
	if(fDoMCTest) pMatchingQA->FillMatchingEfficiency(aTofMatchings, aTofHits, aKFtpcTracks, aMcTracks);	
				
	if(fVerbose)
	{
		cout<<"\n -I- [MpdTofMatching::Exec] MatchingOK = "<<MatchingOK;
		if(MatchingOK != 0) cout<<" ("<<chi2/MatchingOK<<")";	
	}	
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdTofMatching::GetDelta(const TVector3& mcPos, const TVector3& estPos, double& dev,  double& devZ, double& devR, double& devPhi)
{
	dev = (mcPos - estPos).Mag();
	devZ = mcPos.Z() - estPos.Z();
	devR = mcPos.Perp() - estPos.Perp();					
	devPhi = sqrt(dev*dev - devZ*devZ - devR*devR);
	devPhi = (mcPos.Phi() > estPos.Phi()) ? devPhi : -1.* devPhi;
}
//------------------------------------------------------------------------------------------------------------------------
TVector3	MpdTofMatching::EstTrackOnR(const MpdTpcKalmanTrack *tr)const
{
	MpdKalmanHit hEnd; 
	hEnd.SetType(MpdKalmanHit::kFixedR);
	MpdTpcKalmanTrack tr1(*tr);
	TObjArray *hits = tr1.GetHits();
	
	if (hits->GetEntriesFast() == 0) 
	{
	  Int_t nh = tr1.GetNofTrHits();
	  for (Int_t j = 0; j < nh; ++j) hits->Add(tr1.GetTrHits()->UncheckedAt(j));
	  tr1.SetNofHits(nh);
	}
	
	tr1.SetParam(*tr1.GetParamAtHit());
        tr1.SetParamNew(*tr1.GetParamAtHit());
        tr1.SetWeight(*tr1.GetWeightAtHit());
        tr1.SetLength(tr1.GetLengAtHit());
        MpdKalmanHit *h = (MpdKalmanHit*) tr1.GetTrHits()->First();
        tr1.SetPos(tr1.GetPosAtHit());
        if (h->GetType() == MpdKalmanHit::kFixedZ) tr1.SetType(MpdKalmanTrack::kEndcap);
        tr1.SetPosNew(tr1.GetPos());

	hEnd.SetPos(fTofBarrelRadius); // barrel TOF radius, [cm]
	pKF->PropagateToHit(&tr1,&hEnd,kTRUE);

        TVector3 pos(tr1.GetPosNew(), 0.,0.);
	pos.SetPhi(tr1.GetParamNew(0)/tr1.GetPosNew());
	pos.SetZ(tr1.GetParamNew(1));

return pos;
}
//------------------------------------------------------------------------------------------------------------------------
MpdTpcKalmanTrack 		MpdTofMatching::RefitTrack(const MpdTpcKalmanTrack *tr)
{
	MpdTpcKalmanTrack tr1(*tr);
	tr1.SetDirection(MpdKalmanTrack::kOutward);
	TObjArray *hits = tr1.GetHits();
	
	if (hits->GetEntriesFast() == 0) 
	{
	  Int_t nh = tr1.GetNofTrHits();
	  for (Int_t j = 0; j < nh; ++j) hits->Add(tr1.GetTrHits()->UncheckedAt(j));
	  tr1.SetNofHits(nh);
	}
	
	tr1.SetParam(*tr1.GetParamAtHit());
        tr1.SetParamNew(*tr1.GetParamAtHit());
        tr1.SetWeight(*tr1.GetWeightAtHit());
        tr1.SetLength(tr1.GetLengAtHit());
        tr1.SetPos(tr1.GetPosAtHit());
        tr1.SetPosNew(tr1.GetPos());

return tr1;
}
//------------------------------------------------------------------------------------------------------------------------
bool		MpdTofMatching::EstTrackOnPlane(const MpdTpcKalmanTrack& tr, const TVector3& point, const TVector3& perp, 
				TVector3& pos, Double_t& length,  TVector3& Mom, Int_t& charge) const
{
	MpdTpcKalmanTrack tr1(tr);
	
	Double_t plane[6] = {point.X(), point.Y(), point.Z(),perp.X(), perp.Y(), perp.Z()};
	if (!pKF->PropagateParamP(&tr1,plane,kTRUE)) return false;

	Double_t R = tr1.GetPosNew(); 
	if(TMath::IsNaN(R)) return false; 

	length = tr1.GetLength();
	Mom = tr1.Momentum3();
	charge = tr1.Charge();
	
	pos.SetXYZ(R, 0., 0.);
	pos.SetPhi(tr1.GetParamNew(0)/R);
	pos.SetZ(tr1.GetParamNew(1));

return true;
}
//------------------------------------------------------------------------------------------------------------------------
bool		MpdTofMatching::EstSmearedTrackOnPlane(const MpdTpcKalmanTrack& tr, const TVector3& point, const TVector3& perp, 
				TVector3& pos, Double_t& length,  TVector3& Mom, Int_t& charge) const
{
	static TVector3 dir, origDir;
	static TRotation rot;

	MpdTpcKalmanTrack tr1(tr);
	
	TMatrixD& param = *tr1.GetParamNew();
	double origPhi = param(2,0);
	double origTheta = param(3,0);	
	
	origDir.SetMagThetaPhi(1., origTheta, origPhi);
	rot.SetZAxis(  origDir);
	
	// smearing Z axis direction vector
	dir.SetMagThetaPhi(1., pRandom->Gaus(0., fSmearedTheta_plate), pRandom->Uniform(TMath::TwoPi()));
	
	// back rotation to original MRS
	dir = rot * dir;
	
    	param(2,0) = dir.Phi();
    	param(3,0) = dir.Theta();
    	tr1.SetParamNew(param);
	
	Double_t plane[6] = {point.X(), point.Y(), point.Z(),perp.X(), perp.Y(), perp.Z()};
	if (!pKF->PropagateParamP(&tr1,plane,kTRUE)) return false;

	Double_t R = tr1.GetPosNew(); 
	if(TMath::IsNaN(R)) return false; 

	length = tr1.GetLength();
	Mom = tr1.Momentum3();
	charge = tr1.Charge();
	
	pos.SetXYZ(R, 0., 0.);
	pos.SetPhi(tr1.GetParamNew(0)/R);
	pos.SetZ(tr1.GetParamNew(1));

return true;
}
//------------------------------------------------------------------------------------------------------------------------

