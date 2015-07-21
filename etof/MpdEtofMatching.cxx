//------------------------------------------------------------------------------------------------------------------------
/// \class MpdEtofMatching
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------
#include <iostream>
#include <fstream>
#include <map>

#include <TMath.h>
#include <TFile.h>
#include <TRandom3.h>
#include <TStorage.h>
#include <TEfficiency.h>

#include "FairRootManager.h"
#include "FairMCTrack.h"
#include "FairRunAna.h"
#include "FairLogger.h"

#include "MpdEctKalmanTrack.h"
#include "MpdKalmanFilter.h"
#include "MpdEtofPoint.h"
#include "MpdEtofHit.h"
#include "MpdEtof.h"

#include "MpdEtofMatching.h"

using namespace std;

struct less_by_pointer 
{
    inline bool operator() (const MpdTof::intervalType& struct1, const MpdTof::intervalType& struct2)
    {
        return (struct1.value < struct2.value);
    }
};

ClassImp(MpdEtofMatchingData)
//------------------------------------------------------------------------------------------------------------------------
MpdEtofMatchingData::MpdEtofMatchingData(Int_t kfTrackId, Int_t tofHitId, Int_t nTrHits, const MpdEtofHit* hit, Int_t pid, Int_t flag, Double_t length, const TVector3& pointP, const TVector3& Momentum, Int_t charge, Double_t deltaR, Double_t deltaPhi)
: fKFTrackIndex(kfTrackId), fTofHitIndex(tofHitId), fPDGcode(pid),  fFlag(flag), fLength(length), fCharge(charge), fDeltaR(deltaR), fDeltaPhi(deltaPhi)
{
	fPx = Momentum.Px(); fPy = Momentum.Py(); fPz = Momentum.Pz(); 
	fX = hit->GetX(); fY = hit->GetY(); fZ = hit->GetZ();
	fTime = hit->GetTime();
	fDetectorUID = hit->GetDetectorID();

	fEstPointP[0] = pointP.X(); fEstPointP[1] = pointP.Y(); fEstPointP[2] = pointP.Z();
	
	fBeta  = (fLength / 100.)  / (fTime  * 1.e-9) / TMath::C();// [cm/nc] -> m/c
	
	Double_t beta2  = fBeta*fBeta;	
	Double_t gamma2 = 1. / (1. - beta2);			
	Double_t Mom = GetMomentum().Mag();
	
	fMass2 = (Mom * Mom) / ( gamma2 * beta2 );
} 
//------------------------------------------------------------------------------------------------------------------------
void 	MpdEtofMatchingData::Print(void) const
{
	cout<<"\n-I- KFTrackIndex= "<<fKFTrackIndex<<", EtofHitIndex="<<fTofHitIndex<<", PDGcode="<<fPDGcode<<", UID="<<fDetectorUID<<", flag="<<fFlag
		<<", Momentum("<<fPx<<","<<fPy<<","<<fPz<<"), Pos("<<fX<<","<<fY<<","<<fZ<<"), length="<<fLength;
}
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------	
ClassImp(MpdEtofMatching)
//------------------------------------------------------------------------------------------------------------------------
MpdEtofMatching::MpdEtofMatching(const char *name, Int_t verbose, Bool_t test)
  : FairTask(name, verbose), aTofPoints(nullptr), aTofHits(nullptr), aMCTracks(nullptr), aKFTracks(nullptr), aTofMatching(nullptr),
  fDoTest(test), fIsMCrun(false), fTestFlnm("test.MpdEtofMatching.root"), htCandNmb(NULL), fTofEndCapZ(295.2) // 250.2
{
	pMF = new LMatchingFilter<MpdEtofHit, MpdEtofMatchingData>(fVerbose);
	
	if(fDoTest)
    	{
    		const double Pmax = 5.; // [GeV/c]
       		const double EtaMax = 5.;  		
    		
       	    	pEfficiencyP = new TEfficiency("EfficiencyP", ";P, GeV/c;Efficiency", 100, 0., Pmax); 							fList.Add(pEfficiencyP);
    	    	pEfficiencyEta = new TEfficiency("EfficiencyEta", ";#eta;Efficiency", 100, -EtaMax, EtaMax);						fList.Add(pEfficiencyEta);   	    	
      	    	pEfficiencyEtaP = new TEfficiency("EfficiencyEtaP", ";#eta;P, GeV/c",  100, -EtaMax, EtaMax, 100, 0., Pmax); 				fList.Add(pEfficiencyEtaP);  	    		    	
     	    	pContaminationP = new TEfficiency("ContaminationP", ";P, GeV/c;Contamination", 100, 0., Pmax);	 					fList.Add(pContaminationP);
    	    	pContaminationEta = new TEfficiency("ContaminationEta", ";#eta;Contamination", 100, -EtaMax, EtaMax);					fList.Add(pContaminationEta);   	    	
      	    	pContaminationEtaP = new TEfficiency("ContaminationEtaP", ";#eta;P, GeV/c",  100, -EtaMax, EtaMax, 100, 0., Pmax); 			fList.Add(pContaminationEtaP);   		
    		  		
		htMcEst_DeltaP = new TH2D("eMcEst_DeltaMom", "est. point <-> Mc point;#Delta, cm;P, GeV/c", 1000, 0., 10., 1000, 0., Pmax); 		fList.Add(htMcEst_DeltaP);
		htMcEst_dRP = new TH2D("eMcEst_dRMom", "est. point <-> Mc point;#Delta_{Z}, cm;P, GeV/c", 1000, 0., 10., 1000, 0., Pmax);		fList.Add(htMcEst_dRP);		
		htMcEst_dPhiP = new TH2D("eMcEst_dPhiMom", "est. point <-> Mc point;#Delta_{#phi}, cm;P, GeV/c", 1000, 0., 10., 1000, 0., Pmax);	fList.Add(htMcEst_dPhiP);
		htMcEst_dPhidR = new TH2D("eMcEst_dPhidR", "est. point <-> Mc point;#Delta_{#phi}, cm;#Delta_{Z}, cm", 1000, 0., 50., 1000, 0., 50.);	fList.Add(htMcEst_dPhidR);
	
		htKfMcPlate = new TH2D("htKfMcPlate", "est KF point on cylinder <-> TofPoint;#Delta, cm;P, GeV/c", 1000, 0.,10., 1000, 0., Pmax); 	fList.Add(htKfMcPlate);
 		
		htTMatch = new TH2D("eTestTMatch", ";P, GeV/c;#eta", 1000, 0., Pmax, 1000, -EtaMax, EtaMax);						fList.Add(htTMatch);
		htMisMatch = (TH2D*) htTMatch->Clone("eTestMisMatch"); 											fList.Add(htMisMatch);
		htKFTrack = (TH2D*) htTMatch->Clone("eTestKFTrack"); 											fList.Add(htKFTrack);
		htKFTrackCand = (TH2D*) htTMatch->Clone("eTestKFTrackCand");										fList.Add(htKFTrackCand);
		htKFTrackTrueCand = (TH2D*) htTMatch->Clone("eTestKFTrackTrueCand");									fList.Add(htKFTrackTrueCand);				
							
		htTrackPerEvent = new TH2D("eTestTrackPerEvent", "KF tracks vs KF tracks&point;N_{tracks};N_{tracks&point} ", 1000, -0.5, 2999.5, 1000, -0.5,2999.5); 	fList.Add(htTrackPerEvent);		
		htCandNmb = new TH2D("eTestCandNmb",  "Number of candidate hits;N candidates;iteration", 1000, -0.5, 1999.5, 1000, -0.5, 999.5);			fList.Add(htCandNmb);
		
		htTrueDelta = new TH2D("ehtTrueDelta",  ";#Delta_{R}, cm;#Delta_{#phi}, cm", 1000, 0., 100., 1000, 0., 100.);						fList.Add(htTrueDelta);
		htMisDelta = (TH2D*) htTrueDelta->Clone("ehtMisDelta"); 												fList.Add(htMisDelta);	
		
 		htRest = new TH1D("eRest", ";Estimated R on etof plate, cm;Events", 1000, 0., 180.);							fList.Add(htRest);		
	}
}
//------------------------------------------------------------------------------------------------------------------------
MpdEtofMatching::~MpdEtofMatching()
{
    	delete pMF;
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus	  MpdEtofMatching::Init()
{
  	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdEtofMatching::Init] Begin initialization.");

	FairRootManager *ioman = FairRootManager::Instance(); assert(ioman);
  	aTofPoints = (TClonesArray*) ioman->GetObject("ETOFPoint");
	aTofHits  = (TClonesArray*) ioman->GetObject("ETOFHit");
  	aMCTracks   = (TClonesArray*) ioman->GetObject("MCTrack"); 
	aKFTracks   = (TClonesArray*) ioman->GetObject("EctTrack");
	 
	if(aTofPoints && aMCTracks) fIsMCrun = true;
	
  	if(!aTofHits  || !aKFTracks){ FairLogger::GetLogger()->Error(MESSAGE_ORIGIN, "Branch not found!"); return kERROR; }

  	pKF = MpdKalmanFilter::Instance("KF","KF");
	
	// Create and register output array
  	aTofMatching = new TClonesArray("MpdEtofMatchingData");
  	ioman->Register("ETOFMatching", "ETof", aTofMatching, kTRUE);

	pMF->SetContainer(aTofMatching);

	MpdEtof::ParseTGeoManager();

	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdEtofMatching::Init] Initialization finished succesfully.");

return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
void 		MpdEtofMatching::Exec(Option_t *option)
{
	const double threshR = 1.5, threshPhi = 15.; // [cm] smallWindows
//	const double threshR = 3., threshPhi = 70.; //  [cm] 

	const double 		EndcapRad = 140.; // [cm]
		
	// Reset event
        aTofMatching->Clear();
	pMF->Reset();

        Int_t nTofPoints = -1, nMCTracks = -1, selectedTracks  = 0;
	if(fIsMCrun){ nTofPoints = aTofPoints->GetEntriesFast(); nMCTracks = aMCTracks ->GetEntriesFast();}
	Int_t nTofHits = aTofHits->GetEntriesFast();  	
	Int_t nKFTracks = aKFTracks->GetEntriesFast();

        if(fVerbose) cout<<"\n -I- [MpdEtofMatching::Exec] points= "<<nTofPoints<<", hits= "<<nTofHits<<", mc tracks= "<<nMCTracks<<", kf tracks= "<<nKFTracks<<endl;
	
	// ---------------------------------------------------------------------------------------->>> Sorting & Mapping points to MC tracks
	typedef multimap<Int_t, MpdEtofPoint*> mmP2TYPE; // pair< MCtrackID, MpdTofPoint*>
	mmP2TYPE 			mmMCpoints;
	mmP2TYPE::iterator 		mmMCpointIter;	
	mmP2TYPE::const_iterator 	mmMCpointCiter;
		
	if(fIsMCrun)
	{
		for(Int_t index = 0; index < nTofPoints; index++)  // cycle by MpdTofPoint
		{
			MpdEtofPoint *mcTofPoint = (MpdEtofPoint*) aTofPoints->UncheckedAt(index);

			Int_t trackID = mcTofPoint->GetTrackID();
			Double_t time = mcTofPoint->GetTime();

			mmMCpointIter = mmMCpoints.find(trackID);
			if(mmMCpointIter != mmMCpoints.end()) // same trackID already inserted, insert to position (sorting by time)
			{
				int count = mmMCpoints.count(trackID);
				for(int i = 0; i < count; i++, mmMCpointIter++) // cycle by hits with same trackID
				{
 					if(time < mmMCpointIter->second->GetTime())
					{
						mmMCpoints.insert(mmMCpointIter, make_pair(trackID, mcTofPoint));
						break;	
					}

					if(i == count-1) mmMCpoints.insert(++mmMCpointIter, make_pair(trackID, mcTofPoint)); // insert to last		
				}
			}
			else 	mmMCpoints.insert(make_pair(trackID, mcTofPoint));

		} // cycle by MpdTofPoint
	}
	// ---------------------------------------------------------------------------------------->>> Mapping points to detectors
	typedef map<Int_t, MpdEtofPoint*> mapPoints;	// pair< mcTrackIndex, fastest MpdEtofPoint* >
	mapPoints mPoints;
	
	if(fIsMCrun)	
	for(int index = 0; index < nTofPoints; index++ ) 
	{
		MpdEtofPoint *mcTofPoint = (MpdEtofPoint*) aTofPoints->UncheckedAt(index);	
		int track = mcTofPoint->GetTrackID();
		mapPoints::iterator iter = mPoints.find(track);
		if(iter != mPoints.end())	// exist
		{
			if(iter->second->GetTime() > mcTofPoint->GetTime()) mPoints[track] = mcTofPoint;
		}
		else 	mPoints.insert(mapPoints::value_type(track, mcTofPoint));
	}	
	// ---------------------------------------------------------------------------------------->>> Mapping hits to detectors	
	typedef multimap<Int_t, pair<MpdEtofHit*, Int_t> > 	mmD2HTYPE; // pair< detUID, pair<MpdTofHit*, hitIndex> >
	mmD2HTYPE::const_iterator	mmHitCiter;
	mmD2HTYPE 			mmHits;
	
	set<int>			sUIDs;
        for(Int_t hitIndex = 0; hitIndex < nTofHits; hitIndex++) // cycle by tof hits
	{   
		MpdEtofHit *pTofHit = (MpdEtofHit*) aTofHits->At(hitIndex);
		Int_t volumeUID = pTofHit->GetDetectorID();

//TString comment =" hitIndex="; comment += hitIndex;pTofHit->Print(comment.Data());
		
		sUIDs.insert(volumeUID);	
		mmHits.insert(make_pair( (volumeUID & 0xFFFFFF00), make_pair(pTofHit, hitIndex))); // convert strip volumeUID to detectorUID (reset stripID to 0)
	}
	// ----------------------------------------------------------------------------------------
	const MpdTof::intervalTreeType*	mDetectorsR = MpdEtof::GetDetR();
	const MpdTof::intervalTreeType*	mDetectorsPhi = MpdEtof::GetDetPhi();			
	
	bool	DoMCTest = fIsMCrun && fDoTest;
	
        TVector3 	Mom, estPoint, estPointR, estPointL;
        bool 		CheckLeft, CheckRight, IsInside;
        Double_t 	Pz, trackLength;
        
 	Double_t 		thR, thPhi; 
        vector<MpdTof::intervalType> 	segmentR, segmentPhi, intersect;

	// The MC run variables (prefix mc). It's valid values only if fIsMCrun = true
	Int_t 		mcTrackIndex, mcPID, mcNpoints, charge;
        TVector3 	mcPosition, hitPosition;	
        bool		mcTofTouch, mcIsSameIDs, mcHasCand, mcHasTrueCand;
	Int_t 		mcRegion, mcModule, mcStrip;
       	set<int>	mcTofTouchKfTracks;  // index of kf tracks having TOF hit
       	
//cout<<"\n ------------------------------------------------------------------------------------------------------------->> EVENT";  
//mDetectorsR->dump("\n\n ----->>>	mDetectorsR INTERVALS");
//mDetectorsPhi->dump("\n\n ----->>>          mDetectorsPhi INTERVALS");
//	nKFTracks =1; 
	for(Int_t KfIndex = 0; KfIndex < nKFTracks; KfIndex++) 		// cycle by TPC KF tracks
	{
		MpdEctKalmanTrack *pKfTrack = (MpdEctKalmanTrack*) aKFTracks->UncheckedAt(KfIndex);
	
		if(fIsMCrun)
		{
			mcTrackIndex = pKfTrack->GetTrackID();
			FairMCTrack *mcTrack = (FairMCTrack*) aMCTracks->UncheckedAt(mcTrackIndex);
			
                        mcPID = mcTrack->GetPdgCode();               
                  	mcNpoints = mmMCpoints.count(mcTrackIndex);
                  	mcTofTouch = mcTrack->GetNPoints(kETOF); 
                  	
                  	if(mcTofTouch) mcTofTouchKfTracks.insert(KfIndex);
        	
                  	mmMCpointCiter = mmMCpoints.find(mcTrackIndex); 
			if(mmMCpointCiter != mmMCpoints.end())
			{	
				mmMCpointCiter->second->Position(mcPosition); // update mcPosition

				Int_t  uid = mmMCpointCiter->second->GetDetectorID();
				mcRegion = MpdEtofPoint::GetRegion(uid);
				mcModule = MpdEtofPoint::GetModule(uid);
				mcStrip = MpdEtofPoint::GetStrip(uid);
//cout<<"\n MC POINT region= "<<mcRegion<<" module="<<mcModule<<" strip="<<mcStrip;
			}
			else
			{
				assert(mcTofTouch == false);           
			}	
		}
		
///if(! mcTofTouch) continue; // FIXME:  FOR TEST 
//cout<<"\n pass mcTofTouch KfIndex="<<KfIndex<<" mcTrackIndex="<<mcTrackIndex<<"  touch="<<mcTofTouch<<" nMCpoints="<<mmMCpoints.count(mcTrackIndex);

		Pz = pKfTrack->Momentum3().Pz();
		
		if(Pz > 0.) 	// Right side
		{		
			if(EstTrackOnPlane(pKfTrack, fTofEndCapZ, estPointR, trackLength, Mom, charge)) // have RightEndcap point  +Z
			{
				if(estPointR.Perp() < EndcapRad) CheckRight = true; // Inside R
				estPoint = estPointR; 	
				if(DoMCTest) htRest->Fill(estPointR.Perp());
			}	
		}
		else 		// Left side
		{		
			if(EstTrackOnPlane(pKfTrack, -fTofEndCapZ, estPointL, trackLength, Mom, charge)) // have LeftEndcap point -Z
			{
				if(estPointL.Perp() < EndcapRad) CheckLeft = true; // Inside R
				estPoint = estPointL; 
				if(DoMCTest) htRest->Fill(estPointL.Perp());				
			}	
		}
	
		if(!(CheckLeft || CheckRight))	 continue;	// KF track out ETof regions
	
		if(DoMCTest)
		{
			if(mcNpoints == 1) // only one tof point per MCtrack
				htKfMcPlate->Fill((mcPosition - estPoint).Mag(), pKfTrack->Momentum()); // mcTofPoint <-> est. KFtrack point	
				
		 	if(mcTofTouch) // KF track have TOFpoint 
			{
				selectedTracks++; 	
				htKFTrack->Fill(pKfTrack->Momentum(), pKfTrack->Momentum3().Eta());
			}
		}		
		
		// ---------------------------------------------------------------------------------------->>> Looking for overlaping of estPointR & detectors
		double estR = estPoint.Perp(), estPhi = estPoint.Phi();	
		
		double Rerror = 10.; // [cm] // FIXME:  should be dependent on the parameters of the KFtrack
		double PhiError = 0.1; // [rads] // FIXME:  should be dependent on the parameters of the KFtrack
					
		segmentR.clear();
		segmentPhi.clear();
		intersect.clear();	
		
		mDetectorsR->findOverlapping(estR-Rerror,  estR+Rerror, segmentR); 	
		mDetectorsPhi->findOverlapping(estPhi-PhiError, estPhi+PhiError, segmentPhi);	
	
//cout<<"\n <<<--R-->>>>> ("<<estR-Rerror<<", "<<estR+Rerror<<")";
//MpdTof::intervalTreeType::dumpIntervals(&segmentR, "\n\n ----->>>	mDetectorsR findOverlapping");

//cout<<"\n <<<--Phi-->>>>> ("<<estPhi-PhiError<<", "<<estPhi+PhiError<<")";
//MpdTof::intervalTreeType::dumpIntervals(&segmentPhi, "\n\n ----->>>	mDetectorsPhi findOverlapping");	
		
		if(!segmentR.empty() && !segmentPhi.empty()) // have overlaped segments both R and Phi 
		{
			// calc. intersection
			sort(segmentR.begin(), segmentR.end(), less_by_pointer());
    			sort(segmentPhi.begin(), segmentPhi.end(), less_by_pointer());  
		 	set_intersection(segmentR.begin(), segmentR.end(), segmentPhi.begin(), segmentPhi.end(), std::back_inserter(intersect), less_by_pointer());  //FIXME: MAYBE  std::inserter ???? 

			for(vector<MpdTof::intervalType>::const_iterator cit = intersect.begin(), citEnd = intersect.end(); cit != citEnd; cit++) // cycle by the overlaped detectors
			{
				Int_t detUID = (*cit).value->volumeUID;

				if(DoMCTest)
				{
					if(mcNpoints == 1) // only one tof point per MCtrack
					{
						double Mom = pKfTrack->Momentum();
						double dR = abs(mcPosition.Perp() - estPoint.Perp());
						double delta = (mcPosition - estPoint).Mag();
						double dPhi = sqrt(delta*delta - dR*dR);
						
						htMcEst_DeltaP->Fill(delta, Mom); // mcTofPoint <-> est. KFtrack point
						htMcEst_dRP->Fill(dR, Mom);
						htMcEst_dPhiP->Fill(dPhi, Mom);
						htMcEst_dPhidR->Fill(dPhi, dR);
					}			
				}

				mmHitCiter = mmHits.find(detUID);
				if(mmHitCiter != mmHits.end()) // the estimated detector have hits
				{
					int counter = mmHits.count(detUID);					
					
					for(int hit = 0; hit < counter; hit++, mmHitCiter++) // cycle by hits into the estimated detector
					{
	 					MpdEtofHit *TofHit = mmHitCiter->second.first;
	 					Int_t hitIndex = mmHitCiter->second.second;
						TofHit->Position(hitPosition); 
						
						double delta = (hitPosition - estPoint).Mag();							
						double deltaR = abs(hitPosition.Perp() - estPoint.Perp());
						double deltaPhi = sqrt(delta*delta - deltaR*deltaR);	
												
//cout<<"\n eTOFWWWhit delta="<<delta<<" point delta="<<(mcPosition - estPoint).Mag();
					
						if(DoMCTest)
						{
							Int_t uid = TofHit->GetDetectorID();
							Int_t region = MpdEtofPoint::GetRegion(uid);
							Int_t module = MpdEtofPoint::GetModule(uid);
							Int_t strip = MpdEtofPoint::GetStrip(uid);
	
							mcIsSameIDs = (mcRegion == region && mcModule == module && mcStrip == strip) ? true : false;
//cout<<"\n MC etofHIT region= "<<region<<" module="<<module<<" strip="<<strip<<"  delta="<<delta<<" mcIsSameIDs="<<mcIsSameIDs;
	
							if(mcIsSameIDs)	htTrueDelta->Fill(deltaR, deltaPhi); 
							else		htMisDelta->Fill(deltaR, deltaPhi); 					
						}
		
						if(deltaR < threshR && deltaPhi < threshPhi)	
						{	
							if(DoMCTest)
							{
								if(mcIsSameIDs) mcHasTrueCand = true;
								mcHasCand = true;
							}
							
							pMF->AddCandidate(MpdEtofMatchingData(KfIndex, hitIndex, pKfTrack->GetNofTrHits(), TofHit, mcPID, TofHit->GetFlag(), trackLength, estPoint, pKfTrack->Momentum3(), charge, delta));				
//cout<<"\n AddCandidate	KfIndex="<<KfIndex<<" hitIndex="<<hitIndex<<" delta="<<delta<<" NmbTrHits="<<pKfTrack->GetNofTrHits()<<"  mcHasCand="<<mcHasCand;
						}	
								
					} // cycle by hits into the estimated detector
				}
						
			} // cycle by the overlaped detectors
			
		} // have overlaped segments both R and Phi
		

		if(DoMCTest && mcTofTouch)
		{ 	
			if(mcHasCand)		htKFTrackCand->Fill(pKfTrack->Momentum(), pKfTrack->Momentum3().Eta());
			if(mcHasTrueCand) 	htKFTrackTrueCand->Fill(pKfTrack->Momentum(), pKfTrack->Momentum3().Eta());
		}			
	
	} // cycle by KF tracks

	if(fDoTest) htTrackPerEvent->Fill(nKFTracks, selectedTracks);

	double chi2;
	Int_t MatchingOK = pMF->Processing(nKFTracks, htCandNmb, chi2);	// accept candidates
	Int_t nEntries = pMF->UpdateContainer(); 			// remove unmatched candidates

assert(nEntries	== MatchingOK);

	if(DoMCTest) // Fill the matching efficiency histos
	{
		map<int, MpdEtofHit*> mMatchings;
		map<int, MpdEtofHit*>::iterator Iter;
		TVector3 momentum;
		
		for(int entry = 0, size = aTofMatching->GetEntriesFast(); entry < size; entry++)  // cycle by the accepted matching candidates
		{
			MpdEtofMatchingData *pData = (MpdEtofMatchingData*) aTofMatching->At(entry);
			MpdEtofHit *hit = (MpdEtofHit*) aTofHits->At(pData->GetTofHitIndex());
			mMatchings.insert(make_pair(pData->GetKFTrackIndex(), hit));	
		}
		
		for(Int_t KfIndex = 0; KfIndex < nKFTracks; KfIndex++) 	// cycle by Ect KF tracks
		{   		
			MpdEctKalmanTrack *pKfTrack = (MpdEctKalmanTrack*) aKFTracks->UncheckedAt(KfIndex);
			mcTrackIndex = pKfTrack->GetTrackID();               
                    	FairMCTrack *pMCtrack = (FairMCTrack*) aMCTracks->UncheckedAt(mcTrackIndex);
                                     
                  	bool mcTofTouch = pMCtrack->GetNPoints(kETOF);
	
//			momentum = pKfTrack->Momentum3(); 	// momentum from KF track	
			pMCtrack->GetMomentum(momentum); 	// momentum from MC track

			double Eta = momentum.Eta(), P = momentum.Mag();
			
			Iter = mMatchings.find(KfIndex);
			bool IsMatchingExist = (Iter != mMatchings.end());
			bool IsTrueMatching = false;
			
			if(mcTofTouch)
			{	
				if(IsMatchingExist)	IsTrueMatching = Iter->second->CheckTrackID(mcTrackIndex);							
				
				pEfficiencyP->Fill(IsTrueMatching, P);		
				pEfficiencyEta->Fill(IsTrueMatching, Eta);	
				pEfficiencyEtaP->Fill(IsTrueMatching, Eta, P);	
			}
						
			if(IsMatchingExist)
			{		
				pContaminationP->Fill( !IsTrueMatching, P);		
				pContaminationEta->Fill( !IsTrueMatching, Eta);	
				pContaminationEtaP->Fill( !IsTrueMatching, Eta, P);				
			}			
		}	
		
	} // Fill the matching efficiency histos

	if(fVerbose) cout<<" -I- [MpdEtofMatching::Exec] MatchingOK = "<<MatchingOK;	
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdEtofMatching::Finish()
{
	if(fDoTest)
    	{
      		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " [MpdEtofMatching::Finish] Update  %s file. ", fTestFlnm.Data());	
		TFile *ptr = gFile;
		TFile file(fTestFlnm.Data(), "RECREATE");
      		fList.Write(); 
      		file.Close();
		gFile = ptr;
    	}
}
//------------------------------------------------------------------------------------------------------------------------
bool		MpdEtofMatching::EstTrackOnPlane(const MpdEctKalmanTrack *tr, Double_t Zetof, TVector3& pos, Double_t& length,  TVector3& Mom, Int_t& charge) const
{
	MpdEctKalmanTrack tr1(*tr);
	TObjArray *hits = tr1.GetHits();

	if (hits->GetEntriesFast() == 0) 
	{
		Int_t nh = tr1.GetNofTrHits();
		for (Int_t j = 0; j < nh; ++j) hits->Add(tr1.GetTrHits()->UncheckedAt(j));
		tr1.SetNofHits(nh);
	}
	
	MpdKalmanHit *h = (MpdKalmanHit*) tr1.GetTrHits()->First();
        if (h->GetType() == MpdKalmanHit::kFixedZ) tr1.SetType(MpdKalmanTrack::kEndcap);
        tr1.SetPos(tr1.GetPosAtHit());
        tr1.SetPosNew(tr1.GetPos());
        tr1.SetParamNew(*tr1.GetParamAtHit());
        tr1.SetParam(*tr1.GetParamAtHit());
        tr1.SetWeight(*tr1.GetWeightAtHit());
        tr1.SetLength(tr1.GetLengAtHit());
        tr1.SetDirection(MpdKalmanTrack::kOutward); 

	MpdKalmanHit hEnd; 
	hEnd.SetType(MpdKalmanHit::kFixedZ);
	hEnd.SetPos(Zetof); // eTOF Z position, [cm]	

	if (!pKF->PropagateParamZ(&tr1, &hEnd, kTRUE)) return false;

	Double_t Z = tr1.GetPosNew(); 
	if(TMath::IsNaN(Z)) return false;  

	Double_t Pt_inv = tr1.GetParamNew(4); // 1/Pt
	if(Pt_inv == 0.) return false;

	length = tr1.GetLength();
	Mom = tr1.Momentum3();
	charge = tr1.Charge();		
	pos.SetXYZ(tr1.GetParamNew(0), tr1.GetParamNew(1), Z);

return true;
}
//------------------------------------------------------------------------------------------------------------------------


