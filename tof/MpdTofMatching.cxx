//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTofMatching
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------
#include <iostream>
#include <fstream>

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
#include "MpdTofPoint.h" 
#include "MpdTof.h"
#include "MpdTofMatching.h"
#include "MpdKalmanFilter.h"

#ifdef _OPENMP
#include "omp.h"
#include <sys/time.h> 
#endif

using namespace std;


struct less_by_pointer 
{
    inline bool operator() (const MpdTof::intervalType& struct1, const MpdTof::intervalType& struct2)
    {
        return (struct1.value < struct2.value);
    }
};


ClassImp(MpdTofMatchingData)
//------------------------------------------------------------------------------------------------------------------------
MpdTofMatchingData::MpdTofMatchingData(Int_t kfTrackId, Int_t tofHitId, const MpdTofHit* hit, Int_t pid, Int_t flag, Double_t length, const TVector3& pointR, const TVector3& pointP, 
				Double_t pointPPhi, Double_t pointPTheta,  const TVector3& P, Int_t charge)
 : fX(hit->GetX()), fY(hit->GetY()), fZ(hit->GetZ()), fTime(hit->GetTime()), fPx(P.Px()), fPy(P.Py()), fPz(P.Pz()), fLength(length), 
	fEstPointPPhi(pointPPhi), fEstPointPTheta(pointPTheta), fDetectorUID(hit->GetDetectorID()), fFlag(flag), fCharge(charge), fPDGcode(pid), fKFTrackIndex(kfTrackId), fTofHitIndex(tofHitId)
{
	fEstPointR[0] = pointR.X(); fEstPointR[1] = pointR.Y(); fEstPointR[2] = pointR.Z();
	fEstPointP[0] = pointP.X(); fEstPointP[1] = pointP.Y(); fEstPointP[2] = pointP.Z();
	
	fBeta  = (fLength / 100.)  / (fTime  * 1.e-9) / TMath::C();// [cm/nc] -> m/c
	Double_t beta2  = fBeta*fBeta;	
	Double_t gamma2 = 1. / (1. - beta2);			
	Double_t Mom2 = fPx*fPx + fPy*fPy +fPz*fPz;
	fMass2 = Mom2 / ( gamma2 * beta2 );		
} 
//------------------------------------------------------------------------------------------------------------------------
MpdTofMatchingData::MpdTofMatchingData(Int_t kfTrackId, Int_t tofHitId, Int_t nTrHits, const MpdTofHit* hit, Int_t pid, Int_t flag, Double_t length, const TVector3& pointR, const TVector3& pointP, const TVector3& perp, 
					const TVector3& P, Int_t charge, Double_t delta)
 : fX(hit->GetX()), fY(hit->GetY()), fZ(hit->GetZ()), fTime(hit->GetTime()), fPx(P.Px()), fPy(P.Py()), fPz(P.Pz()), fLength(length), fDelta(delta), 
	fDetectorUID(hit->GetDetectorID()), fFlag(flag), fCharge(charge), fPDGcode(pid), fKFTrackIndex(kfTrackId), fTofHitIndex(tofHitId), fNmbTrHits(nTrHits)
{
	fEstPointR[0] = pointR.X(); fEstPointR[1] = pointR.Y(); fEstPointR[2] = pointR.Z();
	fEstPointP[0] = pointP.X(); fEstPointP[1] = pointP.Y(); fEstPointP[2] = pointP.Z();
	fStripPerp[0] = perp.X(); fStripPerp[1] = perp.Y(); fStripPerp[2] = perp.Z();	
	
	fBeta  = (fLength / 100.)  / (fTime  * 1.e-9) / TMath::C();// [cm/nc] -> m/c
	Double_t beta2  = fBeta*fBeta;	
	Double_t gamma2 = 1. / (1. - beta2);			
	Double_t Mom2 = fPx*fPx + fPy*fPy +fPz*fPz;
	fMass2 = Mom2 / ( gamma2 * beta2 );
}
//------------------------------------------------------------------------------------------------------------------------
void 	MpdTofMatchingData::Print(void) const
{
	cout<<"\n-I- KFTrackIndex= "<<fKFTrackIndex<<", TofHitIndex="<<fTofHitIndex<<", PDGcode="<<fPDGcode<<", UID="<<fDetectorUID<<", flag="<<fFlag
		<<", Momentum("<<fPx<<","<<fPy<<","<<fPz<<"), Pos("<<fX<<","<<fY<<","<<fZ<<"), length="<<fLength;
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdTofMatching)
//------------------------------------------------------------------------------------------------------------------------
MpdTofMatching::MpdTofMatching(const char *name, Int_t verbose, Bool_t test)
  : FairTask(name, verbose),  aTofPoints(nullptr), aTofHits(nullptr), aMCTracks(nullptr), aKFTracks(nullptr), aKFectTracks(nullptr), aTofMatching(nullptr),
  fDoTest(test), fIsMCrun(false), fTestFlnm("test.MpdTofMatching.root"),  htCandNmb(NULL), pKF(nullptr), fTofBarrelRadius(146.2)// [cm]
{
	pMF = new LMatchingFilter<MpdTofHit, MpdTofMatchingData>(fVerbose);
	  
	if(fDoTest)
    	{
    		const double Pmax = 5.; // [GeV/c]
      		const double EtaMax = 5.;
      				
    	    	pEfficiencyP = new TEfficiency("EfficiencyP", ";P, GeV/c;Efficiency", 100, 0., Pmax); 							fList.Add(pEfficiencyP);
    	    	pEfficiencyEta = new TEfficiency("EfficiencyEta", ";#eta;Efficiency", 100, -EtaMax, EtaMax);						fList.Add(pEfficiencyEta);   	    	
      	    	pEfficiencyEtaP = new TEfficiency("EfficiencyEtaP", ";#eta;P, GeV/c",  100, -EtaMax, EtaMax, 100, 0., Pmax); 				fList.Add(pEfficiencyEtaP);  	    		    	
     	    	pContaminationP = new TEfficiency("ContaminationP", ";P, GeV/c;Contamination", 100, 0., Pmax); 						fList.Add(pContaminationP);
    	    	pContaminationEta = new TEfficiency("ContaminationEta", ";#eta;Contamination", 100, -EtaMax, EtaMax);					fList.Add(pContaminationEta);   	    	
      	    	pContaminationEtaP = new TEfficiency("ContaminationEtaP", ";#eta;P, GeV/c",  100, -EtaMax, EtaMax, 100, 0., Pmax); 			fList.Add(pContaminationEtaP);   	    	
    	
		htMcEst_DeltaP = new TH2D("McEst_DeltaMom", "est. point <-> Mc point;#Delta, cm;P, GeV/c", 1000, 0., 10., 1000, 0., Pmax); 		fList.Add(htMcEst_DeltaP);
		htMcEst_dZP = new TH2D("McEst_dZMom", "est. point <-> Mc point;#Delta_{Z}, cm;P, GeV/c", 1000, 0., 10., 1000, 0., Pmax);		fList.Add(htMcEst_dZP);		
		htMcEst_dPhiP = new TH2D("McEst_dPhiMom", "est. point <-> Mc point;#Delta_{#phi}, cm;P, GeV/c", 1000, 0., 10., 1000, 0., Pmax);		fList.Add(htMcEst_dPhiP);
		htMcEst_dPhidZ = new TH2D("McEst_dPhidZ", "est. point <-> Mc point;#Delta_{#phi}, cm;#Delta_{Z}, cm", 1000, 0., 50., 1000, 0., 50.);	fList.Add(htMcEst_dPhidZ);		
				
		htKfMcCyl = new TH2D("htKfMcCyl", "est KF point on cylinder <-> TofPoint;#Delta, cm;P, GeV/c", 1000, 0.,10., 1000, 0., Pmax); 		fList.Add(htKfMcCyl);
		
		htTMatch = new TH2D("TestTMatch", ";P, GeV/c;#eta", 1000, 0., Pmax, 1000, -EtaMax, EtaMax);						fList.Add(htTMatch);
		htMisMatch = (TH2D*) htTMatch->Clone("TestMisMatch"); 											fList.Add(htMisMatch);
		htKFTrack = (TH2D*) htTMatch->Clone("TestKFTrack"); 											fList.Add(htKFTrack);
		htKFTrackCand = (TH2D*) htTMatch->Clone("TestKFTrackCand");										fList.Add(htKFTrackCand);
		htKFTrackTrueCand = (TH2D*) htTMatch->Clone("TestKFTrackTrueCand");									fList.Add(htKFTrackTrueCand);			

		htTrackPerEvent = new TH2D("htTrackPerEvent", "KF tracks vs KF tracks&point;N_{tracks};N_{tracks&point} ", 1000, -0.5, 2999.5, 1000, -0.5,2999.5); fList.Add(htTrackPerEvent);		
		htCandNmb = new TH2D("htCandNmb",  "Number of candidate hits;N candidates;iteration", 1000, -0.5, 1999.5, 1000, -0.5, 999.5);		fList.Add(htCandNmb);		
		
		htTrueDelta = new TH2D("htTrueDelta",  ";#Delta_{Z}, cm;#Delta_{#phi}, cm", 1000, 0., 100., 1000, 0., 100.);				fList.Add(htTrueDelta);
		htMisDelta = (TH2D*) htTrueDelta->Clone("htMisDelta"); 											fList.Add(htMisDelta);			
	}
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofMatching::~MpdTofMatching()
{
    	delete pMF;
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus	  MpdTofMatching::Init()
{
  	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdTofMatching::Init] Begin initialization.");

	FairRootManager *ioman = FairRootManager::Instance(); assert(ioman);
  	aTofPoints = (TClonesArray*) ioman->GetObject("TOFPoint");
	aTofHits  = (TClonesArray*) ioman->GetObject("TOFHit");
  	aMCTracks   = (TClonesArray*) ioman->GetObject("MCTrack"); 
	aKFTracks   = (TClonesArray*) ioman->GetObject("TpcKalmanTrack"); 
	aKFectTracks   = (TClonesArray*) ioman->GetObject("EctTrack"); 
	
	if(aTofPoints && aMCTracks) fIsMCrun = true;
	
  	if(!aTofHits  || !aKFTracks){ FairLogger::GetLogger()->Error(MESSAGE_ORIGIN, "Branch not found!"); return kERROR; }

  	pKF = MpdKalmanFilter::Instance("KF","KF");
	
	// Create and register output array
  	aTofMatching = new TClonesArray("MpdTofMatchingData");
  	ioman->Register("TOFMatching", "Tof", aTofMatching, kTRUE);
  	
	pMF->SetContainer(aTofMatching);

	MpdTof::ParseTGeoManager();

	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdTofMatching::Init] Initialization finished succesfully.");

return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
void 		MpdTofMatching::Exec(Option_t *option)
{
	const double threshZ = 1.5, threshPhi = 15.; // smallWindows
//	const double threshZ = 3., threshPhi = 70.; // [cm]	

	// Reset event
        aTofMatching->Clear();
	pMF->Reset();
	
        Int_t nTofPoints = -1, nMCTracks = -1, selectedTracks  = 0;
	if(fIsMCrun){ nTofPoints = aTofPoints->GetEntriesFast(); nMCTracks = aMCTracks ->GetEntriesFast();}
	Int_t nTofHits = aTofHits->GetEntriesFast();  	
	Int_t nKFTracks = aKFTracks->GetEntriesFast();
	
	Int_t nKFectTracks = (aKFectTracks) ?  aKFectTracks->GetEntriesFast() : 0;
        if(fVerbose) cout<<" -I- [MpdTofMatching::Exec] points= "<<nTofPoints<<", hits= "<<nTofHits<<", mc tracks= "<<nMCTracks<<", kf tracks= "<<nKFTracks<<endl;
        
	// ---------------------------------------------------------------------------------------->>> Check clone tracks into ECT
	typedef set<Int_t>  		sTIDsTYPE;
	sTIDsTYPE 			sTIDs; 
	sTIDsTYPE::const_iterator 	sTIDCiter;	
	
	if(aKFectTracks)
	for(Int_t index = 0; index < nKFectTracks; index++) // cycle by ECT KF tracks
	{
		MpdEctKalmanTrack *KfeTrack = (MpdEctKalmanTrack*) aKFectTracks->UncheckedAt(index);
		if(KfeTrack->IsFromTpc()) sTIDs.insert(KfeTrack->GetTpcIndex());
	}

	// ---------------------------------------------------------------------------------------->>> Sorting & Mapping points to MC tracks
	typedef multimap<Int_t, MpdTofPoint*> mmP2TYPE; // pair< MCtrackID, MpdTofPoint*>
	mmP2TYPE 			mmMCpoints;
	mmP2TYPE::iterator 		mmMCpointIter;	
	mmP2TYPE::const_iterator 	mmMCpointCiter;
		
	if(fIsMCrun)
	{
		for(Int_t index = 0; index < nTofPoints; index++)  // cycle by MpdTofPoint
		{
			MpdTofPoint *mcTofPoint = (MpdTofPoint*) aTofPoints->UncheckedAt(index);

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
	// ---------------------------------------------------------------------------------------->>> Mapping hits to detectors	
	typedef multimap<Int_t, pair<MpdTofHit*, Int_t> > 	mmD2HTYPE; // pair< detUID, pair<MpdTofHit*, hitIndex> >
	mmD2HTYPE::const_iterator	mmHitCiter;
	mmD2HTYPE 			mmHits;
	
	set<int>			sUIDs;
        for(Int_t hitIndex = 0; hitIndex < nTofHits; hitIndex++) // cycle by tof hits
	{   
		MpdTofHit *pTofHit = (MpdTofHit*) aTofHits->At(hitIndex);
		Int_t volumeUID = pTofHit->GetDetectorID();

//TString comment =" hitIndex="; comment += hitIndex;
//pTofHit->Print(comment.Data());
		
		sUIDs.insert(volumeUID);	
		mmHits.insert(make_pair( (volumeUID & 0xFFFFFF00), make_pair(pTofHit, hitIndex))); // convert strip volumeUID to detectorUID (reset stripID to 0)
	}	
	// ----------------------------------------------------------------------------------------
	const MpdTof::intervalTreeType*	mDetectorsZ = MpdTof::GetDetZ();
	const MpdTof::intervalTreeType*	mDetectorsPhi = MpdTof::GetDetPhi();	
	
	bool		DoMCTest = fIsMCrun && fDoTest;
	
     	TVector3 	hitPosition, estPointR, estPointPl, Momentum;	
	
	vector<MpdTof::intervalType> 	segmentZ, segmentPhi, intersect;
	
	// The MC run variables (prefix mc). It's valid values only if fIsMCrun = true	
        Int_t		mcTrackIndex = -1,  mcPID = -1;        
        TVector3 	mcPosition;			 
        Int_t		mcNpoints = -1; 			
        bool		mcTofTouch, mcIsSameIDs, mcHasCand, mcHasTrueCand;
        Int_t 		mcSector, mcBox, mcDetector, mcStrip;
     	set<int>	mcTofTouchKfTracks; // index of kf tracks having TOF hit
     
//cout<<"\n ------------------------------------------------------------------------------------------------------------->> EVENT";    
//mDetectorsZ->dump("\n\n ----->>>	mDetectorsZ INTERVALS");
//mDetectorsPhi->dump("\n\n ----->>>          mDetectorsPhi INTERVALS");
//	nKFTracks =1;   
        for(Int_t KfIndex = 0; KfIndex < nKFTracks; KfIndex++) 	// cycle by TPC KF tracks
	{   	
		// check clone track into ECT	
		if(aKFectTracks)
		{	
			sTIDCiter = sTIDs.find(KfIndex);			
			if(sTIDCiter != sTIDs.end()) continue; // matching with ETof			
		}
		
		MpdTpcKalmanTrack *pKfTrack = (MpdTpcKalmanTrack*) aKFTracks->UncheckedAt(KfIndex);	
		
///cout<<"\n ------------>> KF track  KfIndex="<<KfIndex<<" mcTrackIndex="<<KfTrack->GetTrackID()<<"   P="<<KfTrack->Momentum();	

                estPointR = EstTrackOnR(pKfTrack); 		// Estimate point on cylinder

		if(fIsMCrun)
		{
			mcTrackIndex = pKfTrack->GetTrackID();
			FairMCTrack *mcTrack = (FairMCTrack*) aMCTracks->UncheckedAt(mcTrackIndex);
			
                        mcPID = mcTrack->GetPdgCode();               
                  	mcNpoints = mmMCpoints.count(mcTrackIndex);
                  	mcTofTouch = mcTrack->GetNPoints(kTOF);
                  	
///if(-1 !=  mcTrack->GetMotherId()) continue; // pass ONLY primary tracks  FIXME:  FOR TEST 
                  	
			if(mcTofTouch) mcTofTouchKfTracks.insert(KfIndex);
			              	
                  	mmMCpointCiter = mmMCpoints.find(mcTrackIndex); 
			if(mmMCpointCiter != mmMCpoints.end())
			{	
				mmMCpointCiter->second->Position(mcPosition); // update mcPosition

				Int_t uid = mmMCpointCiter->second->GetDetectorID();
				mcSector = MpdTofPoint::GetSector(uid);
				mcBox = MpdTofPoint::GetBox(uid);
				mcDetector = MpdTofPoint::GetDetector(uid);
				mcStrip = MpdTofPoint::GetStrip(uid);							
//cout<<"\n MC POINT sector= "<<mcSector<<" box="<<mcBox<<" detector="<<mcDetector<<" strip="<<mcStrip;
			}
			else
			{
				assert(mcTofTouch == false);           
			}	
		}
		
//if(! mcTofTouch) continue; // FIXME:  FOR TEST 
///cout<<"\n pass mcTofTouch KfIndex="<<KfIndex<<" mcTrackIndex="<<mcTrackIndex<<"  touch="<<mcTofTouch<<" nMCpoints="<<mmMCpoints.count(mcTrackIndex);
		
		if(DoMCTest)
		{
			if(mcNpoints == 1) // only one tof point per MCtrack
				htKfMcCyl->Fill((mcPosition - estPointR).Mag(), pKfTrack->Momentum()); // mcTofPoint <-> est. KFtrack point	
				
		 	if(mcTofTouch) // KF track have TOFpoint 
			{
				selectedTracks++; 	
				htKFTrack->Fill(pKfTrack->Momentum(), pKfTrack->Momentum3().Eta());
			}
		}

		MpdTpcKalmanTrack ReFittedTrack(RefitTrack(pKfTrack));
		
		// ---------------------------------------------------------------------------------------->>> Looking for overlaping of estPointR & detectors
		double estZ = estPointR.Z(), estPhi = estPointR.Phi();	

		double Zerror = 10.; // [cm] // FIXME:  should be dependent on the parameters of the KFtrack
		double PhiError = 0.1; // [rads] // FIXME:  should be dependent on the parameters of the KFtrack

		segmentZ.clear();
		segmentPhi.clear();
		intersect.clear();
		
		mDetectorsZ->findOverlapping(estZ-Zerror,  estZ+Zerror, segmentZ); 	
		mDetectorsPhi->findOverlapping(estPhi-PhiError, estPhi+PhiError, segmentPhi);

//cout<<"\n <<<--Z-->>>>> ("<<estZ-Zerror<<", "<<estZ+Zerror<<")";
//MpdTof::intervalTreeType::dumpIntervals(&segmentZ, "\n\n ----->>>	mDetectorsZ findOverlapping");

//cout<<"\n <<<--Phi-->>>>> ("<<estPhi-PhiError<<", "<<estPhi+PhiError<<")";
//MpdTof::intervalTreeType::dumpIntervals(&segmentPhi, "\n\n ----->>>	mDetectorsPhi findOverlapping");

		// -------------------------------------   bruteforce ------------------------------------------------------------->>>>>>>>>>>>>>>>>>>>>>>>>>
/*		if(fMCDataExist)
		if(mcTofTouch)
		{
			multimap<double, pair<const LStrip*, TVector3> > mEstPoint; // <delta, <LStrip*, estPointPl> >
			for(MpdTof::MStripCIT it = MpdTof::GetStripMap()->begin(), itEnd = MpdTof::GetStripMap()->end(); it != itEnd; it++) // cycle by all strips
			{			
				Int_t 	UIDforce = it->second.volumeUID;
				if(sUIDs.find(UIDforce) == sUIDs.end()) continue; // pass only strips with hits
									
				Int_t charge;
				Double_t trackLength;	


Int_t sector = MpdTofPoint::GetSector(UIDforce);
Int_t box = MpdTofPoint::GetBox(UIDforce);
Int_t detector = MpdTofPoint::GetDetector(UIDforce);
Int_t strip = MpdTofPoint::GetStrip(UIDforce);	
				
///cout<<"\n EstTrackOnPlane UID="<<it->second.volumeUID<<", center:("<<it->second.center.X()<<","<<it->second.center.Y()<<","<<it->second.center.Z()<<") perp: ("<<it->second.perp.X()<<","<<it->second.perp.Y()<<","<<it->second.perp.Z()<<")";
///cout<<"\n  UID="<<UIDforce<<", sector= "<<sector<<" box="<<box<<" detector="<<detector<<" strip="<<strip;	
					
				if(EstTrackOnPlane(ReFittedTrack, it->second.center, it->second.perp, estPointPl, trackLength, Momentum, charge)) // Estimate point on plane; true, if point exist
				{
					double delta = (mcPosition - estPointPl).Mag();
					mEstPoint.insert(make_pair(delta, make_pair(&(it->second), estPointPl)));
					
///cout<<"\n EstTrackOnPlane OKKK ";					
				}
			} // cycle by all strips
			
			// -------------------------------------   bruteforce
			
cout<<"\n SIZEEEE "<<	MpdTof::GetStripMap()->	size()<<"  "<<mEstPoint.size();
	
			int nMaxPrints = 100, nPrint = 0;
			for(map<double, pair<const LStrip*, TVector3> >::iterator it = mEstPoint.begin(), itEnd = mEstPoint.end(); it != itEnd; it++, nPrint++)
			{
Int_t 	UIDforce = it->second.first->volumeUID;
Int_t sector = MpdTofPoint::GetSector(UIDforce);
Int_t box = MpdTofPoint::GetBox(UIDforce);
Int_t detector = MpdTofPoint::GetDetector(UIDforce);
Int_t strip = MpdTofPoint::GetStrip(UIDforce);	
		
				bool HavePoint = false;	
				for(Int_t index = 0; index < nTofPoints; index++)  // cycle by MpdTofPoint
				{
					MpdTofPoint *mcTofPoint = (MpdTofPoint*) aTofPoints->UncheckedAt(index);
					if(UIDforce == mcTofPoint->GetDetectorID())
					{
						HavePoint = true;
						break;
					}
				}

cout<<"\n bruteforce point delta="<<it->first<<" UID="<<UIDforce<<", sector= "<<sector<<" box="<<box<<" detector="<<detector<<" strip="<<strip<<"   HavePoint= "<<HavePoint;	
		
				if(nPrint > nMaxPrints)break;
			}

			cout<<	endl;
		}
		// -------------------------------------   bruteforce -------------------------------------------------------------<<<<<<<<<<<<<<<<<<<<<<<<<
*/
		
		if(!segmentZ.empty() && !segmentPhi.empty()) // have overlaped segments both Z and Phi 
		{
			// calc. intersection
			sort(segmentZ.begin(), segmentZ.end(), less_by_pointer());
    			sort(segmentPhi.begin(), segmentPhi.end(), less_by_pointer());  
		 	set_intersection(segmentZ.begin(), segmentZ.end(), segmentPhi.begin(), segmentPhi.end(), std::back_inserter(intersect), less_by_pointer());  //FIXME: MAYBE  std::inserter ???? 

			for(vector<MpdTof::intervalType>::const_iterator cit = intersect.begin(), citEnd = intersect.end(); cit != citEnd; cit++) // cycle by the overlaped detectors
			{
				Int_t detUID = (*cit).value->volumeUID;
				Int_t charge;
				Double_t trackLength;
				
				if(EstTrackOnPlane(ReFittedTrack, (*cit).value->center, (*cit).value->perp, estPointPl, trackLength, Momentum, charge)) // Estimate point on detector plane; true, if point exist
				{		
					if(DoMCTest)
					{
						if(mcNpoints == 1) // only one tof point per MCtrack
						{
							double Mom = pKfTrack->Momentum();
							double dZ = abs(mcPosition.Z() - estPointPl.Z());
							double delta = (mcPosition - estPointPl).Mag();
							double dPhi = sqrt(delta*delta - dZ*dZ);
						
							htMcEst_DeltaP->Fill(delta, Mom); // mcTofPoint <-> est. KFtrack point	
							htMcEst_dZP->Fill(dZ, Mom); 
							htMcEst_dPhiP->Fill(dPhi, Mom); 
							htMcEst_dPhidZ->Fill(dPhi, dZ); 							
						}			
					}
				
					mmHitCiter = mmHits.find(detUID);
					if(mmHitCiter != mmHits.end()) // the estimated detector have hits
					{
						int counter = mmHits.count(detUID);
						for(int hit = 0; hit < counter; hit++, mmHitCiter++) // cycle by hits into the estimated detector
						{
	 						MpdTofHit *TofHit = mmHitCiter->second.first;
	 						Int_t hitIndex = mmHitCiter->second.second;
							TofHit->Position(hitPosition);
							 
							double delta = (hitPosition - estPointPl).Mag();						
							double deltaZ = abs(hitPosition.Z() - estPointPl.Z());
							double deltaPhi = sqrt(delta*delta - deltaZ*deltaZ);
							
//cout<<"\n tTOFWWWhit delta="<<delta<<" point delta="<<(mcPosition - estPointPl).Mag();

							if(DoMCTest)
							{
								Int_t uid = TofHit->GetDetectorID();
								Int_t sector = MpdTofPoint::GetSector(uid);
								Int_t box = MpdTofPoint::GetBox(uid);
								Int_t detector = MpdTofPoint::GetDetector(uid);
								Int_t strip = MpdTofPoint::GetStrip(uid);
														
								mcIsSameIDs = (mcSector == sector && mcBox == box && mcDetector == detector && mcStrip == strip) ? true : false;
//cout<<"\n MC tofHIT sector= "<<sector<<" box="<<box<<" detector="<<detector<<" strip="<<strip<<"  delta="<<delta<<" mcIsSameIDs="<<mcIsSameIDs;
	
								if(mcIsSameIDs)	htTrueDelta->Fill(deltaZ, deltaPhi); 
								else		htMisDelta->Fill(deltaZ, deltaPhi); 
							}
							
							if(deltaZ < threshZ && deltaPhi < threshPhi)	
							{	
								if(DoMCTest)
								{
									if(mcIsSameIDs) mcHasTrueCand = true;
									mcHasCand = true;
								}
								
								pMF->AddCandidate(MpdTofMatchingData(KfIndex, hitIndex, pKfTrack->GetNofTrHits(), TofHit, mcPID, TofHit->GetFlag(), trackLength, estPointR, estPointPl, (*cit).value->center, Momentum, charge, delta));
//cout<<"\n AddCandidate	KfIndex="<<KfIndex<<" hitIndex="<<hitIndex<<" delta="<<delta<<" NmbTrHits="<<NmbTrHits<<"  hasTrueCand="<<hasTrueCand;
							}
							
						} // cycle by hits into the estimated detector
					}
		
				} // Estimate point on plane; true, if point exist
				
			} // cycle by the overlaped detectors 
		
		} // have overlaped segments both Z and Phi 

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
		map<int, MpdTofHit*> mMatchings;
		map<int, MpdTofHit*>::iterator Iter;
		TVector3 momentum;
		
		for(int entry = 0, size = aTofMatching->GetEntriesFast(); entry < size; entry++)  // cycle by the accepted matching candidates
		{
			MpdTofMatchingData *pData = (MpdTofMatchingData*) aTofMatching->At(entry);
			MpdTofHit *hit = (MpdTofHit*) aTofHits->At(pData->GetTofHitIndex());
			mMatchings.insert(make_pair(pData->GetKFTrackIndex(), hit));	
		}
		
		for(Int_t KfIndex = 0; KfIndex < nKFTracks; KfIndex++) 	// cycle by TPC KF tracks
		{   	
			if(aKFectTracks)
			{	
				sTIDCiter = sTIDs.find(KfIndex);			
				if(sTIDCiter != sTIDs.end()) continue; // matching with ETof			
			}
		
			MpdTpcKalmanTrack *pKfTrack = (MpdTpcKalmanTrack*) aKFTracks->UncheckedAt(KfIndex);
			mcTrackIndex = pKfTrack->GetTrackID();               
                    	FairMCTrack *pMCtrack = (FairMCTrack*) aMCTracks->UncheckedAt(mcTrackIndex);
                                     
                  	bool mcTofTouch = pMCtrack->GetNPoints(kTOF);
	
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

	if(fVerbose) cout<<" -I- [MpdTofMatching::Exec] MatchingOK = "<<MatchingOK<<" ("<<chi2/MatchingOK<<")";	
}
//------------------------------------------------------------------------------------------------------------------------
void	MpdTofMatching::Finish()
{
	if(fDoTest)
    	{				
      		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " [MpdTofMatching::Finish] Update  %s file. ", fTestFlnm.Data());
		TFile *ptr = gFile;
		TFile file(fTestFlnm.Data(), "RECREATE");
      		fList.Write(); 
      		file.Close();
		gFile = ptr;
    	}
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

