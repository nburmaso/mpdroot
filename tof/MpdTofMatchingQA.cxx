//------------------------------------------------------------------------------------------------------------------------
#include <map>
#include <iostream>

#include "FairLogger.h" 
#include "FairMCTrack.h"
#include "MpdTpcKalmanTrack.h"
#include "MpdTofMatching.h"

#include "MpdTofMatchingQA.h"
using namespace std;
//------------------------------------------------------------------------------------------------------------------------
MpdTofMatchingQA::MpdTofMatchingQA(const char *flnm, bool isEndcap)
: fFlnm(flnm), fIsEndcap(isEndcap)
{
	const double Pmax = 5.; // [GeV/c]
	const double EtaMax = 5.;
      	
      	fList.SetOwner(); //  all objects will be deleted whenever the collection itself is delete.	
      			
	Add(pEff1P = new TEfficiency(mangling("Eff1P"), ";P, GeV/c;Efficiency", 100, 0., Pmax));
	Add(pEff1Eta = new TEfficiency(mangling("Eff1Eta"), ";#eta;Efficiency", 100, -EtaMax, EtaMax));    	
	Add(pEff1EtaP = new TEfficiency(mangling("Eff1EtaP"), ";#eta;P, GeV/c",  100, -EtaMax, EtaMax, 100, 0., Pmax));    		    	
	Add(pCont1P = new TEfficiency(mangling("Cont1P"), ";P, GeV/c;Contamination", 100, 0., Pmax));
	Add(pCont1Eta = new TEfficiency(mangling("Cont1Eta"), ";#eta;Contamination", 100, -EtaMax, EtaMax));   	    	
	Add(pCont1EtaP = new TEfficiency(mangling("Cont1EtaP"), ";#eta;P, GeV/c",  100, -EtaMax, EtaMax, 100, 0., Pmax));  	    	
   
  	Add(pEff2P = new TEfficiency(mangling("Eff2P"), ";P, GeV/c;Efficiency", 100, 0., Pmax));
	Add(pEff2Eta = new TEfficiency(mangling("Eff2Eta"), ";#eta;Efficiency", 100, -EtaMax, EtaMax));    	
	Add(pEff2EtaP = new TEfficiency(mangling("Eff2EtaP"), ";#eta;P, GeV/c",  100, -EtaMax, EtaMax, 100, 0., Pmax));    		    	
	Add(pCont2P = new TEfficiency(mangling("Cont2P"), ";P, GeV/c;Contamination", 100, 0., Pmax));
	Add(pCont2Eta = new TEfficiency(mangling("Cont2Eta"), ";#eta;Contamination", 100, -EtaMax, EtaMax));   	    	
	Add(pCont2EtaP = new TEfficiency(mangling("Cont2EtaP"), ";#eta;P, GeV/c",  100, -EtaMax, EtaMax, 100, 0., Pmax));  
    	
	Add(hDeltaPoint_Dev = new TH2D(mangling("DeltaPoint_Dev"), "est. point <-> Mc point;#Delta, cm;P, GeV/c", 1000, 0., 10., 1000, 0., Pmax));
	Add(hDeltaPoint_dR = new TH2D(mangling("DeltaPoint_dR"), "est. point <-> Mc point;#Delta_{R}, cm;P, GeV/c", 1000, -10., 10., 1000, 0., Pmax));	
	Add(hDeltaPoint_dZ = new TH2D(mangling("DeltaPoint_dZ"), "est. point <-> Mc point;#Delta_{Z}, cm;P, GeV/c", 1000, -10., 10., 1000, 0., Pmax));	
	Add(hDeltaPoint_dPhi = new TH2D(mangling("DeltaPoint_dPhi"), "est. point <-> Mc point;#Delta_{#phi}, cm;P, GeV/c", 1000, -10., 10., 1000, 0., Pmax));	
				
	Add(htKfMcCyl = new TH2D(mangling("KfMcCyl"), "est KF point on cylinder <-> TofPoint(only one point);#Delta, cm;P, GeV/c", 1000, 0.,10., 1000, 0., Pmax));	
		
	Add(htKFTrack = new TH2D(mangling("KFTrack"), ";#eta;P, GeV/c", 1000, -EtaMax, EtaMax, 1000, 0., Pmax));		
	Add(htMisMatch = (TH2D*) htKFTrack->Clone(mangling("MisMatchings"))); 
	Add(htTrueMatch = (TH2D*) htKFTrack->Clone(mangling("TrueMatchings"))); 
	Add(htKFTrackCand = (TH2D*) htKFTrack->Clone(mangling("KFTrackCand")));
	Add(htKFTrackTrueCand = (TH2D*) htKFTrack->Clone(mangling("KFTrackTrueCand")));		

	Add(htTrackPerEvent = new TH2D(mangling("TrackPerEvent"), "KF tracks vs KF tracks&point;N_{tracks};N_{tracks&point} ", 1000, -0.5, 2999.5, 1000, -0.5,2999.5));		
	Add(htCandNmb = new TH2D(mangling("CandNmb"),  "Number of candidate hits;N candidates;iteration", 1000, -0.5, 1999.5, 1000, -0.5, 999.5));		
		
	Add(hDeltaTrueHit = new TH2D(mangling("DeltaTrueHit"),  ";#Delta_{R}, cm;#Delta_{#phi}, cm", 1000, 0., fIsEndcap ? 100. : 20., 1000, 0., fIsEndcap ? 20. : 100.));	
	Add(hDeltaMisHit = (TH2D*) hDeltaTrueHit->Clone(mangling("DeltaMisHit")));
	Add(hDeltaPoint = (TH2D*) hDeltaTrueHit->Clone(mangling("DeltaPoint"))); 
	
	Add(heRest = new TH1D(mangling("eRest"), ";Estimated R on etof plate, cm;Events", 1000, 0., 180.));	
}
//------------------------------------------------------------------------------------------------------------------------
void	MpdTofMatchingQA::FillDevPoint2EstCyl(const TVector3& mcPosition, const TVector3& estPointOnCyl, Double_t Momentum)
{
	htKfMcCyl->Fill((mcPosition - estPointOnCyl).Mag(), Momentum); 
}	
//------------------------------------------------------------------------------------------------------------------------
void	MpdTofMatchingQA::FillDevPoint2EstP(const TVector3& mcPosition, const TVector3& estPointOnPlate, Double_t Momentum)
{	
	double devPoint, devPointZ,  devPointR, devPointPhi;
	MpdTofMatching::GetDelta(mcPosition, estPointOnPlate, devPoint, devPointZ, devPointR, devPointPhi);
						
	hDeltaPoint_Dev->Fill(devPoint, Momentum); 
	hDeltaPoint_dR->Fill(devPointR, Momentum); 
	hDeltaPoint_dZ->Fill(devPointZ, Momentum); 
	hDeltaPoint_dPhi->Fill(devPointPhi, Momentum);				
	hDeltaPoint->Fill(abs(devPointZ), abs(devPointPhi));	
}							
//------------------------------------------------------------------------------------------------------------------------
void	MpdTofMatchingQA::FillHitDev2EstP(bool IsTrueMatching, Double_t deltaZ, Double_t deltaPhi)
{
	if(IsTrueMatching)		hDeltaTrueHit->Fill(deltaZ, deltaPhi); 
	else				hDeltaMisHit->Fill(deltaZ, deltaPhi); 
}
//------------------------------------------------------------------------------------------------------------------------
void	MpdTofMatchingQA::FillCandidates(bool mcHaveTrueCand, bool mcHaveCand, Double_t eta, Double_t Momentum)
{
	if(mcHaveCand)		htKFTrackCand->Fill(eta, Momentum);
	if(mcHaveTrueCand) 	htKFTrackTrueCand->Fill(eta, Momentum);
}
//------------------------------------------------------------------------------------------------------------------------			
void	MpdTofMatchingQA::Finish()
{
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " [MpdTofMatchingQA::Finish] Update  %s file. ", fFlnm.Data());
	TFile *ptr = gFile;
	TFile file(fFlnm.Data(), "RECREATE");
	fList.Write(); 
	file.Close();
	gFile = ptr;
}		
//------------------------------------------------------------------------------------------------------------------------		
void	MpdTofMatchingQA::FillMatchingEfficiency(const TClonesArray* aTofMatching, const TClonesArray* aTofHits, const TClonesArray* aKFTracks, const TClonesArray* aMCTracks)
{
	// Sorting & mapping matchings
	map<int, const MpdTofHit*> mMatchings; // pair  <kfTrackIndex, MpdTofHit*>
	multimap<const MpdTofHit*, int> mmHitCounter;	// pair< MpdTofHit*, matchingIndex>
	
	for(int entry = 0, size = aTofMatching->GetEntriesFast(); entry < size; entry++)  // cycle by the  matching N 
	{
		auto pData = (const MpdTofMatchingData*) aTofMatching->At(entry);
		Int_t tofHitIndex = pData->GetTofHitIndex();
		
		if(tofHitIndex >= 0) // matching with the fired strip, i.e. with the hit
		{
			auto hit = (MpdTofHit*) aTofHits->At(tofHitIndex);
			mMatchings.insert(make_pair(pData->GetKFTrackIndex(), hit));	
			mmHitCounter.insert(make_pair(hit, entry));	
		}	
	}
	
	//--------------------------------------------------------------------------------------------------------------------
	// algorithm efficiency definition:
	//
	// efficiency = N true matchings / N tpc kf tracks having TOF hit 
	// contamination = N wrong matchings / ( N true matchings + N wrong matchings)
	//
	// ALICE TDR TOF 2000 definitions:
	//
	// N = N mis + N match
	// N miss = N0 + N2 ( matched to to blank strip or dead regions + matched to fired strip was more than one track)
	// N match = N t + N w ( true matching + wrong matching)
	//
	// efficiency = N t / N
	// contamination = N w / N match		
	//--------------------------------------------------------------------------------------------------------------------	
	
	TVector3 momentum;
	for(Int_t KfIndex = 0, nKFTracks = aKFTracks->GetEntriesFast(); KfIndex < nKFTracks; KfIndex++) // cycle by TPC KF tracks
	{   	
		auto pKfTrack = (const MpdTpcKalmanTrack*) aKFTracks->UncheckedAt(KfIndex);
		Int_t mcTrackIndex = pKfTrack->GetTrackID();               
               	auto pMCtrack = (FairMCTrack*) aMCTracks->UncheckedAt(mcTrackIndex);
                                     
               	bool mcTofTouch = pMCtrack->GetNPoints(fIsEndcap ? kETOF : kTOF);
	
		pMCtrack->GetMomentum(momentum);
		double Eta = momentum.Eta(), P = momentum.Mag();
			
		auto Iter = mMatchings.find(KfIndex);
		bool IsMatchingExist = (Iter != mMatchings.end());
		bool IsTrueMatching = false;
		bool IsNmatch = false;
			
		if(IsMatchingExist)
		{
			const MpdTofHit *hit = Iter->second;
			
			IsTrueMatching = hit->CheckTrackID(mcTrackIndex);												
			IsNmatch = (1 == mmHitCounter.count(hit));	// hit linked ONLY ONE matching			
		
			if(IsTrueMatching) 	htTrueMatch->Fill(Eta, P);	
			else			htMisMatch->Fill(Eta, P);				
		}
			
		if(mcTofTouch)
		{		
			pEff1P->Fill(IsTrueMatching, P);		
			pEff1Eta->Fill(IsTrueMatching, Eta);	
			pEff1EtaP->Fill(IsTrueMatching, Eta, P);			
		}
		
		if(-1.35 < Eta && Eta < 1.35)
		{
			pEff2P->Fill(IsTrueMatching, P);		
			pEff2Eta->Fill(IsTrueMatching, Eta);	
			pEff2EtaP->Fill(IsTrueMatching, Eta, P);
		}		
						
		if(IsMatchingExist)
		{		
			pCont1P->Fill( !IsTrueMatching, P);		
			pCont1Eta->Fill( !IsTrueMatching, Eta);	
			pCont1EtaP->Fill( !IsTrueMatching, Eta, P);				
		}	
			
		if(IsNmatch)
		{
			pCont2P->Fill( !IsTrueMatching, P);		
			pCont2Eta->Fill( !IsTrueMatching, Eta);	
			pCont2EtaP->Fill( !IsTrueMatching, Eta, P);
		}
						
	} // cycle by TPC KF tracks
}			
//------------------------------------------------------------------------------------------------------------------------
