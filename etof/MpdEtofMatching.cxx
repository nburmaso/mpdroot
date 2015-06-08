//------------------------------------------------------------------------------------------------------------------------
/// \class MpdEtofMatching
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------
#include <iostream>
#include <fstream>
#include <map>

#include "TMath.h"
#include "TFile.h"
#include "TRandom3.h"
#include "TStorage.h"

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
//------------------------------------------------------------------------------------------------------------------------
MpdEtofMatchingData::MpdEtofMatchingData(Int_t kfTrackId, Int_t tofHitId, const MpdEtofHit* hit, Int_t pid, Int_t flag,
				Double_t length, const TVector3& est_point_Plane, const TVector3& P, Int_t charge)
: fKFTrackIndex(kfTrackId), fTofHitIndex(tofHitId), fPDGcode(pid),  fFlag(flag), fLength(length)
{
	fPx = P.Px(); fPy = P.Py(); fPz = P.Pz(); fCharge = charge;
	fX = hit->GetX(); fY = hit->GetY(); fZ = hit->GetZ();
	fTime = hit->GetTime();
	fDetectorUID = hit->GetDetectorID();

	fTofImpactPoint[0] = est_point_Plane.X(); fTofImpactPoint[1] = est_point_Plane.Y(); fTofImpactPoint[2] = est_point_Plane.Z();
	
	Double_t beta  = (fLength / 100.)  / (fTime  * 1.e-9) / TMath::C();// [cm/nc] -> m/c
	Double_t beta2  = beta*beta;	
	Double_t gamma2 = 1. / (1. - beta2);			
	Double_t Mom = GetMomentum().Mag();
	fMass2 = (Mom * Mom) / ( gamma2 * beta2 );
} 
//------------------------------------------------------------------------------------------------------------------------
void 	MpdEtofMatchingData::Print(void) const
{
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " PDGcode=%i, UID=%i", fPDGcode, fDetectorUID);
}
//------------------------------------------------------------------------------------------------------------------------	
ClassImp(MpdEtofMatchingData)
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
MpdEtofMatching::MpdEtofMatching(const char *name, Int_t verbose, Bool_t test)
  : FairTask(name, verbose), fDoTest(test), fMCDataExist(false), fTestFlnm("test.MpdEtofMatching.root"), htCandNmb(NULL), fTofEndCapZ(295.2) // 250.2
{
	pMatchingFilter = new EtofMatchingFilter;
	
	if(fDoTest)
    	{
		htMC_est = MpdTofUtils::Make2D("htMC_est", 1000, -5., 5., 1000, -10., 10., &fList, "MC point <-> est. KF point;#DeltaR, cm;#DeltaPhi");
		htHit_est= MpdTofUtils::Make2D("htHit_est", 1000, 0., 5., 1000, 0., 10., &fList, "hit point <-> est. KF point;#DeltaR, cm;#DeltaPhi");
		hKF_est = MpdTofUtils::Make1D("hKF_est", 10000, 0., 200., &fList, "est KF point on pad <-> center est. pad;#Delta, cm;Events");
		hKF_MC = MpdTofUtils::Make1D("hKF_MC", 10000, 0., 200., &fList, "est KF point on pad <-> MC point;#Delta, cm;Events");			
		
		hPads_Theta_Phi = MpdTofUtils::Make2D("pads_Theta_Phi", 1000, 0., 180., 1000, -190., 190., &fList, "Test, pad centers, Theta vs. Phi;#theta, degree;#phi, degree"); 	
		hTest_C_D = MpdTofUtils::Make2D("test_C_D", 1000, 0., 200., 10, -0.5, 9.5, &fList, ";#Delta, cm;k_side");	
 		htRest = MpdTofUtils::Make1D("htRest", 1000, 0., 180., &fList, ";Estimated R on etof plate, cm;Events");	
 		htRPest = MpdTofUtils::Make1D("htRPest", 1000, 0., 180., &fList, ";Estimated R on etof plate(etof point exist), cm;Events");
 		
 		htTMatch = MpdTofUtils::Make2D("htTMatch", 1000, 0., 5., 1000, -5., 5., &fList, ";P, GeV/c;#eta");
		htMisMatch = MpdTofUtils::Make2D("htMisMatch", htTMatch, &fList);
		htKFTrack = MpdTofUtils::Make2D("htKFTrack", htTMatch, &fList);	

		htTrackPerEvent = MpdTofUtils::Make2D("htTrackPerEvent", 1001, -0.5, 1000.5, 1001, -0.5, 1000.5, &fList, "KF tracks vs KF tracks&point;N_{tracks};N_{tracks&point}");		
		htCandNmb = MpdTofUtils::Make2D("htCandNmb", 100, -0.5, 99.5, 1000, -0.5, 999.5, &fList, "Number of candidate hits;N candidates;iteration");	
	}
}
//------------------------------------------------------------------------------------------------------------------------
MpdEtofMatching::~MpdEtofMatching()
{
    	delete pMatchingFilter;
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus	  MpdEtofMatching::Init()
{
  	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " <MpdEtofMatching::Init> Begin etof matching initialization.");

	FairRootManager *ioman = FairRootManager::Instance(); assert(ioman);
  	pTofPoints = (TClonesArray*) ioman->GetObject("ETOFPoint");
	pTofHits  = (TClonesArray*) ioman->GetObject("ETOFHit");
  	pMCTracks   = (TClonesArray*) ioman->GetObject("MCTrack"); 
	pKFTracks   = (TClonesArray*) ioman->GetObject("EctTrack"); 
	if(pTofPoints && pMCTracks) fMCDataExist = true;
	
  	if(!pTofHits  || !pKFTracks){ FairLogger::GetLogger()->Error(MESSAGE_ORIGIN, "Branch not found!"); return kERROR; }

	TString parFlnm = MpdTofUtils::GetEtofParFlnm(fGeoFlNm); 
  	if(!MpdTofUtils::ReadParamFromXML(parFlnm.Data(), vecRegions, mmapModules, mapPads))  // file invalid || don't exist
	{
		if(!MpdEtof::ParseTGeoManager(vecRegions, mmapModules, mapPads)) return kFATAL;	
		MpdTofUtils::WriteParamToXML(parFlnm.Data(), "ETOF_parameters", "The MPD ETOF geo parameters.", vecRegions, mmapModules, mapPads);
	}

	if(fDoTest)	
  		for(MpdTofUtils::PadIter iter = mapPads.begin(); iter != mapPads.end(); iter++) 	// cycle by pads    	
			hPads_Theta_Phi->Fill((&iter->second)->center.Theta() * TMath::RadToDeg(), (&iter->second)->center.Phi() * TMath::RadToDeg());
	
  	pKF = MpdKalmanFilter::Instance("KF","KF");
	
	// Create and register output array
  	pMatchingCollection = new TClonesArray("MpdEtofMatchingData");
  	ioman->Register("ETOFMatching", "ETof", pMatchingCollection, kTRUE);

return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
void 		MpdEtofMatching::Exec(Option_t *option)
{
	pMatchingCollection->Delete();
	pMatchingFilter->Reset();
	
	Int_t nTofPoints = -1, nMCTracks = -1; 
	if(fMCDataExist){ nTofPoints = pTofPoints->GetEntries(); nMCTracks = pMCTracks ->GetEntries();}
	Int_t nTofHits = pTofHits->GetEntries();  	
	Int_t nKFTracks = pKFTracks ->GetEntries();

	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " <MpdEtofMatching::Exec> %i EtofPoints, %i EtofHits, %i MCTracks, %i KFTracks for this event.", nTofPoints, nTofHits, nMCTracks, nKFTracks);

	Int_t 				candTracks = 0, MatchingOK = 0, charge, mcPointIndex, mcTrackIndex, flag, mcPID, mcUID, UID;   
	MpdEctKalmanTrack 	*KfTrack;
	MpdEtofHit 			*TofHit;
	MpdTofUtils::PadIter 	it;	
	TVector3 				Mom, mcPosition, HitPosition, estPoint, estPointR, estPointL, dir, dev;	
	bool 				CheckLeft, CheckRight, IsInside; Double_t trackLength, deltaR, deltaPhi, dPhi, dR, Z, Pz, dEdX, momentum, eta; 
	Double_t 		thR, thPhi; 
	const double 		EndcapRad =  141.;
	const double 		threshR_2 = 12.15, threshPhi_2 = 3.24;	// 3 sigma ^2,  [cm*cm] 	  3.5^2=12.15;  1.8^2=3.24	

	// ----------->>> collect eTof points data
	typedef map<Int_t, MpdEtofPoint*> mapPoints;	// pair< mcTrackIndex, fastest MpdEtofPoint* >
	mapPoints mPoints;
	
	if(fMCDataExist)	
	for(int index = 0; index < nTofPoints; index++ ) 
	{
		MpdEtofPoint *mcTofPoint = (MpdEtofPoint*) pTofPoints->At(index);	
		int track = mcTofPoint->GetTrackID();
		mapPoints::iterator iter = mPoints.find(track);
		if(iter != mPoints.end())	// exist
		{
			if(iter->second->GetTime() > mcTofPoint->GetTime()) mPoints[track] = mcTofPoint;	// change MpdEtofPoint	
		}
		else 	mPoints.insert(mapPoints::value_type(track, mcTofPoint));
	}	
	// <<<----------- collect eTof points data

	// ----------->>> cycle by KF tracks
	for(Int_t KfIndex = 0; KfIndex < nKFTracks; KfIndex++) 	
	{
     		bool DoMCTest = fMCDataExist && fDoTest;
		mcPID = 0;		
		flag = 0;		
		
		KfTrack = (MpdEctKalmanTrack*) pKFTracks->UncheckedAt(KfIndex);
		mcTrackIndex = KfTrack->GetTrackID();		
		dEdX = KfTrack->GetPartID(); // FIXME: check new function name 
		
		CheckLeft = CheckRight = false;
		estPoint.SetXYZ(0.,0.,0.);
		
		Pz = KfTrack->Momentum3().Pz();
		
		if(Pz > 0.) 	// Right side
		{		
			if(EstTrackOnPlane(KfTrack, fTofEndCapZ, estPointR, trackLength, Mom, charge)) // have RightEndcap point  +Z
			{
				if(estPointR.Perp() < EndcapRad) CheckRight = true; // Inside R
				estPoint = estPointR; 	
				if(DoMCTest) htRest->Fill(estPointR.Perp());
			}	
		}
		else 		// Left side
		{		
			if(EstTrackOnPlane(KfTrack, -fTofEndCapZ, estPointL, trackLength, Mom, charge)) // have LeftEndcap point -Z
			{
				if(estPointL.Perp() < EndcapRad) CheckLeft = true; // Inside R
				estPoint = estPointL; 
				if(DoMCTest) htRest->Fill(estPointL.Perp());				
			}	
		}
	
		if(!(CheckLeft || CheckRight))	 continue;	// KF track out ETof region
		
		// ----------->>> Looking eTof point  	 		
		if(fMCDataExist)
		{			
			mcPID   = ((FairMCTrack*) pMCTracks->UncheckedAt(mcTrackIndex))->GetPdgCode();
			
			mapPoints::iterator iter = mPoints.find(mcTrackIndex);
			if(iter != mPoints.end()) // exist
			{
				mcUID = iter->second->GetDetectorID();
				iter->second->Position(mcPosition);
			}
			else 
			{ 
				DoMCTest = false;  flag += MpdTofUtils::McAbsent;
			} 
		}
		// <<<----------- Looking eTof point 
		
		if(DoMCTest) // etof point exist
		{
			htKFTrack->Fill(Mom.Mag(), Mom.Eta());			
			htRPest->Fill(estPoint.Perp());
			
			it = mapPads.find(mcUID); assert(it != mapPads.end());
			dir = it->second.point[0] - it->second.point[1];	// axis along  pad
			dev = mcPosition - estPoint;			
									
			dPhi = (dir * dev ) / dir.Mag(); 	
			dR = dev.Perp(dir);
			dR = mcPosition.Perp() > estPoint.Perp() ? dR : -dR;
	
			htMC_est->Fill(dR, dPhi);
			candTracks++;
		}
		
		// ----------->>> cycle by Hits
		for(Int_t hitIndex = 0; hitIndex < nTofHits; hitIndex++) 
		{
			TofHit = (MpdEtofHit*) pTofHits->At(hitIndex);	
			TofHit->Position(HitPosition); 	
			UID = TofHit->GetDetectorID();	
			
			it = mapPads.find(UID); assert(it != mapPads.end());
			dir = it->second.point[0] - it->second.point[1];	// axis along  pad
			
			dev = estPoint - HitPosition;									
			deltaPhi = TMath::Abs(dir * dev ) / dir.Mag(); 		// projection to strip long side (Phi) 
			deltaR = dev.Perp(dir);					// projection to strip short side (R)	

			if(fDoTest) htHit_est->Fill(deltaR, deltaPhi);
				
			Z = HitPosition.Z();	
							
			if( (CheckRight && Z > 0.) || (CheckLeft &&  Z < 0.) )
			{					
				thR = sqrt(TofHit->GetDy()*TofHit->GetDy() * 4.+ threshR_2);
				thPhi = sqrt(TofHit->GetDx()*TofHit->GetDx() * 4. + threshPhi_2);

				if(deltaPhi < thPhi && deltaR < thR) // [cm] add matching candidate 
				{ 		
					flag += TofHit->GetFlag();				
								
					pMatchingFilter->AddCandidate(mcTrackIndex, KfIndex, hitIndex, TofHit, mcPID, flag, trackLength, dEdX, estPoint, Mom, charge, deltaR, deltaPhi);
							
					if(DoMCTest)
					{
						hKF_est->Fill((estPoint - HitPosition).Mag());
						hKF_MC->Fill((mcPosition - estPoint).Mag());
					}
																										
				} // add matching candidate 							
			}
		} // <<<----------- cycle by Hits 
			
	} // <<<----------- cycle by KF tracks

	if(fDoTest) htTrackPerEvent->Fill(nKFTracks, candTracks);
//pMatchingFilter->Dump(); //debug

	MatchingOK = pMatchingFilter->Processing(nKFTracks, htCandNmb);
	pMatchingFilter->FillMatchings(this);

	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " <MpdEtofMatching::Exec> MatchingOK = %i ", MatchingOK);	
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdEtofMatching::Finish()
{
	if(fDoTest)
    	{
      		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " <MpdEtofMatching::Finish> Update  %s file. ", fTestFlnm.Data());	
		TFile *ptr = gFile;
		TFile file(fTestFlnm.Data(), "RECREATE");
      		fList.Write(); 
      		file.Close();
		gFile = ptr;
    	}
}
//------------------------------------------------------------------------------------------------------------------------
void 	MpdEtofMatching::AddEntry(Int_t hitIndex, Int_t kfTrackId, Int_t tofHitId, const MpdEtofHit* hit, Int_t pid, Int_t flag,
				Double_t length,  const TVector3& est_point_Plane, const TVector3& Mom, Int_t charge)
{
	MpdEtofMatchingData *data = new  ((*pMatchingCollection)[hitIndex]) MpdEtofMatchingData(kfTrackId, tofHitId, hit, pid,  flag,
						length, est_point_Plane, Mom, charge);
				
	if(fVerbose > 1) data->Print();
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
ClassImp(MpdEtofMatching)
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
void	EtofMatchingFilter::AddCandidate(Int_t mcTrackIndex, Int_t KfIndex, Int_t hitIndex, MpdEtofHit *TofHit, Int_t mcPID, Int_t flag, 
			Double_t trackLength, Double_t dEdX, TVector3 estPoint, TVector3 Momentum, Int_t charge, Double_t deltaR, Double_t deltaPhi)
{										
	EtofMatchingCandidate *ptr;
	candIter iterC = InsertCand(KfIndex,  ptr = new EtofMatchingCandidate(mcTrackIndex, hitIndex, mcPID, flag, charge, dEdX, Momentum, TofHit, deltaR,  deltaPhi, estPoint, trackLength));
	ptr->iterL = InsertLink(hitIndex, HitData(KfIndex, iterC, -1., deltaR, deltaPhi));		

//cout<<"\n   EtofMatchingFilter::AddCandidate "<<ptr<<"  mcTrackIndex= "<<iterC->second->mcTrackIndex<<"  hitIndex="<< iterC->second->hitIndex
//<<" KfIndex= "<<ptr->iterL->second.KfIndex<<flush;		
}
//------------------------------------------------------------------------------------------------------------------------
EtofMatchingFilter::candIter EtofMatchingFilter::FindClosestTrack(linkIter itLink, Int_t size, Double_t& minDelta)	
{
assert(size != 0); // debug
	candIter		retvalue = mmapCand.end(); // debug
	
	Int_t KfIndex;
	Double_t deltaR, deltaPhi, delta; minDelta = 1.e+10; // big value

	for(int i=0; i< size; i++) // cycle by concurrent
	{		
		deltaR = itLink->second.deltaR;
		deltaPhi = itLink->second.deltaPhi;		
		delta = sqrt(deltaR*deltaR + deltaPhi*deltaPhi);		
		if(delta < minDelta){ retvalue = itLink->second.iterC; minDelta = delta; } // best (closest) candidate		
		++itLink;
	}	
assert(retvalue != mmapCand.end()); // debug		
return retvalue;
}
//------------------------------------------------------------------------------------------------------------------------
EtofMatchingFilter::candIter EtofMatchingFilter::FindClosestHit(Int_t KfIndex)
{
	EtofMatchingCandidate 	*pMatchCand;
	candIter  retCandIter = mmapCand.end();	// debug
	
	candIter itC1 = mmapCand.find(KfIndex);
	candIter itC2 = mmapCand.upper_bound(KfIndex);

	Double_t deltaR, deltaPhi, delta, minDelta = 1.e+10; // big value	
	for(candIter itC = itC1; itC != itC2; ++itC) // cycle by candidate hits
	{
		pMatchCand = (EtofMatchingCandidate*) itC->second;	
		deltaR = pMatchCand->deltaR;
		deltaPhi = pMatchCand->deltaPhi;		
		delta = sqrt(deltaR*deltaR + deltaPhi*deltaPhi);

		if(delta < minDelta) // best (closest) candidate
		{ 
			retCandIter = itC; 
			minDelta = delta; 	
		} 
	}
assert(retCandIter != mmapCand.end()); // debug
return retCandIter;
}		
//------------------------------------------------------------------------------------------------------------------------
Int_t 	EtofMatchingFilter::Processing(Int_t nKFTracks, TH2D* h2)
{
//cout<<"\n  WWWW	EtofMatchingFilter::Processing --------------------------------------------------------------"<<flush;

	RecreateCounterMaps(nKFTracks);
	Int_t MatchingOK = ProcessSimpleTracks(); 	// process only one hit simple tracks
	RecreateCounterMaps(nKFTracks); 		// update CounterMaps after ProcessSimpleTracks

	EtofMatchingCandidate 	*pMatchCand;
	Int_t counter, iterNmb = 0;
	counterIter iter;

newIteration:
	iterNmb++;

	if(iterNmb > 1000){ FairLogger::GetLogger()->Warning(MESSAGE_ORIGIN, " <EtofMatchingFilter::Processing> Too many tries."); goto end; }	
	
	if(h2) // fDoTest == true
	{
		for(int nCand = 0; nCand < 100; nCand++)
			if(mmapCounters.find(nCand) != mmapCounters.end()) h2->Fill(nCand, iterNmb);	
	}
	
	// ----------->> Processing tracks with one candidate 
	iter = mmapCounters.find(1);
	if(iter != mmapCounters.end())				// exist
	{
		Int_t KfIndex = iter->second;		
		candIter itCand = mmapCand.find(KfIndex);
assert(itCand != mmapCand.end()); // debug

		Int_t hitIndex = itCand->second->hitIndex;
		int conNmb  = mmapLinks.count(hitIndex);		// number of concurrent track
		linkIter itLink = mmapLinks.find(hitIndex);		// iter to first track
assert(itLink != mmapLinks.end()); // debug
//cout<<"\n  WWWW 1 "<<iterNmb<<" track="<<KfIndex<<" hit="<<hitIndex<<flush;
//Commit();			
		if(conNmb == 1)		AcceptCandidate(itCand); 	// no concurrent track	
		else if(conNmb>1)
		{
			Double_t delta;
			itCand = FindClosestTrack(itLink, conNmb, delta);
			AcceptCandidate(itCand);
		}	
//Status();	
		goto newIteration;	// try new iteration after map entries erased
	
	} // <<<----------- Processing tracks with one candidate 	
	
	// ----------->> Processing tracks with two candidates 	
	iter = mmapCounters.find(2);
	if(iter != mmapCounters.end())				// exist	
	{
		Int_t KfIndex = iter->second;
		candIter itCand1 = mmapCand.find(KfIndex);
assert(itCand1 != mmapCand.end()); // debug
		Int_t hitIndex = itCand1->second->hitIndex;				// first candidate
		int conNmb  = mmapLinks.count(hitIndex);				// number of concurrent track
		linkIter itLink = mmapLinks.find(hitIndex);				// iter to first track
assert(itLink != mmapLinks.end()); // debug		
//cout<<"\n  WWWW 2 "<<iterNmb<<" track="<<KfIndex<<" hit="<<hitIndex<<flush;
							
		Double_t delta1 = -1., delta2 = -1.;
		candIter itClosest1, itClosest2;
		if(conNmb>0) itClosest1 = FindClosestTrack(itLink, conNmb, delta1);
			
		candIter itCand2 = itCand1; itCand2++;
		hitIndex = itCand2->second->hitIndex;					// second candidate		
		conNmb  = mmapLinks.count(hitIndex);				// number of concurrent track
		itLink = mmapLinks.find(hitIndex);						// iter to first track
//cout<<" hit2="<<hitIndex;
			
		if(conNmb>0) itClosest2 = FindClosestTrack(itLink, conNmb, delta2);
//Commit();		
		if(delta1 > 0. &&  delta2 > 0.)	// both exist
		{
			if(delta1 <  delta2)	AcceptCandidate(itClosest1); 
			else 			AcceptCandidate(itClosest2); 		 
		}
		else if(delta1 > 0.) AcceptCandidate(itClosest1); // only 1 exist
		else if(delta2 > 0.) AcceptCandidate(itClosest2); // only 2 exist
//Status();						
		goto newIteration;	// try new iteration after map entries erased			
	} // <<<----------- Processing tracks with two candidates
	
	// ----------->> Processing tracks with 3 candidates 
	iter = mmapCounters.find(3);
	if(iter != mmapCounters.end())					
	{
//cout<<"\n  WWWW 3 "<<iterNmb<<" "<<iter->second;
		Int_t KfIndex = iter->second;
		AcceptCandidate(FindClosestHit(KfIndex)); 
		goto newIteration;		
	} 
	// <<<----------- Processing tracks with 3 candidates 
	
	iter = mmapCounters.find(4);
	if(iter != mmapCounters.end())					
	{
//cout<<"\n  WWWW 4 "<<iterNmb<<" "<<iter->second;
		Int_t KfIndex = iter->second;	
		AcceptCandidate(FindClosestHit(KfIndex)); 
		goto newIteration;		
	} 

	iter = mmapCounters.upper_bound(4);
	if(iter != mmapCounters.end())					
	{
//cout<<"\n  WWWW > 4 "<<iterNmb<<" "<<iter->second;
		Int_t KfIndex = iter->second;
		AcceptCandidate(FindClosestHit(KfIndex)); 
		goto newIteration;		
	}

end:	

	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " <EtofMatchingFilter::Processing> Finished with  %i iterations.", iterNmb);			
return 	mmapAccepted.size();
}	
//------------------------------------------------------------------------------------------------------------------------
void	EtofMatchingFilter::FillMatchings(MpdEtofMatching* ptr)
{
	Int_t entryID = 0;
	EtofMatchingCandidate 	*pMatchCand;
	for(candIter iter = mmapAccepted.begin(); iter != mmapAccepted.end(); ++iter)
	{
	//	pMatchCand = dynamic_cast<EtofMatchingCandidate*> ( iter->second ); if(pMatchCand == NULL) continue;
		pMatchCand = (EtofMatchingCandidate*)  iter->second;

		// add Entry to collection		
		ptr->AddEntry(entryID++, iter->first, pMatchCand->hitIndex, pMatchCand->TofHit, pMatchCand->mcPID, pMatchCand->flag, 
					pMatchCand->trackLength, pMatchCand->estPoint, pMatchCand->Momentum, pMatchCand->charge);
		
		if(ptr->fDoTest)			
		if(pMatchCand->TofHit->CheckTrackID(pMatchCand->mcTrackIndex))	 // TRUE Matching		
			ptr->htTMatch->Fill(pMatchCand->Momentum.Mag(), pMatchCand->Momentum.Eta());	
		else 
			ptr->htMisMatch->Fill(pMatchCand->Momentum.Mag(), pMatchCand->Momentum.Eta());						
	}
}
//------------------------------------------------------------------------------------------------------------------------	


