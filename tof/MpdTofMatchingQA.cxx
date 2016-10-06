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
      			
	Add(pEfficiencyP = new TEfficiency(mangling("EfficiencyP"), ";P, GeV/c;Efficiency", 100, 0., Pmax));
	Add(pEfficiencyEta = new TEfficiency(mangling("EfficiencyEta"), ";#eta;Efficiency", 100, -EtaMax, EtaMax));    	
	Add(pEfficiencyEtaP = new TEfficiency(mangling("EfficiencyEtaP"), ";#eta;P, GeV/c",  100, -EtaMax, EtaMax, 100, 0., Pmax));    		    	
	Add(pContaminationP = new TEfficiency(mangling("ContaminationP"), ";P, GeV/c;Contamination", 100, 0., Pmax));
	Add(pContaminationEta = new TEfficiency(mangling("ContaminationEta"), ";#eta;Contamination", 100, -EtaMax, EtaMax));   	    	
	Add(pContaminationEtaP = new TEfficiency(mangling("ContaminationEtaP"), ";#eta;P, GeV/c",  100, -EtaMax, EtaMax, 100, 0., Pmax));  	    	
    	
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
	// Sorting & mapping matchings by pair  <kfTrackIndex, MpdTofHit*>
	map<int, MpdTofHit*> mMatchings;
	map<int, MpdTofHit*>::iterator Iter;

		
	for(int entry = 0, size = aTofMatching->GetEntriesFast(); entry < size; entry++)  // cycle by the  matching
	{
		MpdTofMatchingData *pData = (MpdTofMatchingData*) aTofMatching->At(entry);
		MpdTofHit *hit = (MpdTofHit*) aTofHits->At(pData->GetTofHitIndex());
		mMatchings.insert(make_pair(pData->GetKFTrackIndex(), hit));	
	}
		
	TVector3 momentum;	
	Int_t mcTrackIndex, nKFTracks	= aKFTracks->GetEntriesFast();
	for(Int_t KfIndex = 0; KfIndex < nKFTracks; KfIndex++) 	// cycle by TPC KF tracks
	{   	
		MpdTpcKalmanTrack *pKfTrack = (MpdTpcKalmanTrack*) aKFTracks->UncheckedAt(KfIndex);
		mcTrackIndex = pKfTrack->GetTrackID();               
               	FairMCTrack *pMCtrack = (FairMCTrack*) aMCTracks->UncheckedAt(mcTrackIndex);
                                     
               	bool mcTofTouch = pMCtrack->GetNPoints(fIsEndcap ? kETOF : kTOF);
	
		pMCtrack->GetMomentum(momentum);
		double Eta = momentum.Eta(), P = momentum.Mag();
			
		Iter = mMatchings.find(KfIndex);
		bool IsMatchingExist = (Iter != mMatchings.end());
		bool IsTrueMatching = false;
			
		if(mcTofTouch)
		{	
			if(IsMatchingExist)
			{
				IsTrueMatching = Iter->second->CheckTrackID(mcTrackIndex);							
								
				if(IsTrueMatching) 	htTrueMatch->Fill(Eta, P);	
				else			htMisMatch->Fill(Eta, P);				
			}
				
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
}			
//------------------------------------------------------------------------------------------------------------------------
