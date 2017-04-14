//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_MATCHING_QA_H
#define __MPD_TOF_MATCHING_QA_H 1

#include <TList.h>
#include <TString.h>
#include <TVector3.h>
#include <TEfficiency.h>
#include <TH1D.h>
#include <TH2D.h>
//------------------------------------------------------------------------------------------------------------------------

class MpdTofMatchingQA 
{
	// QA test histos	 
        TList			fList;	
        TH1D			*heRest;
        TH2D           		*htCandNmb,*htTrackPerEvent, *htKfMcCyl, *htKFTrack, *htKFTrackCand, *htKFTrackTrueCand, *htTrueMatch, *htMisMatch; 
        TH2D           		*hDeltaTrueHit, *hDeltaMisHit, *hDeltaPoint, *hDeltaPoint_Dev, *hDeltaPoint_dR, *hDeltaPoint_dZ, *hDeltaPoint_dPhi;
	TEfficiency		*pEfficiencyP, *pEfficiencyEta, *pEfficiencyEtaP;		// Efficiency = N true matchings / N tpc kf tracks having TOF hit;
	TEfficiency		*pContaminationP, *pContaminationEta, *pContaminationEtaP; 	// Contamination = N wrong matchings / ( N true matchings + n wrong matchings)
        TString			fFlnm;
        bool			fIsEndcap;        
  
  	const char* 	mangling(const char* name){ static TString nm; nm = fIsEndcap ? "eTOF_" : "TOF_"; nm += name; return nm.Data();}
	void		Add(TH1 *hist){ hist->SetDirectory(nullptr); fList.Add(hist);}
	void		Add(TEfficiency *hist){ hist->SetDirectory(nullptr); fList.Add(hist);}
	
public :	
	MpdTofMatchingQA(const char *flnm, bool isEndcap);		
	void	Finish();
		
	void	FillDevPoint2EstCyl(const TVector3& mcPosition, const TVector3& estPointOnCyl, Double_t Momentum);
	void	FillDevPoint2EstP(const TVector3& mcPosition, const TVector3& estPointOnPlate, Double_t Momentum);
	void	FillHitDev2EstP(bool IsTrueMatching, Double_t delta1, Double_t delta2);	

	void	FillCandidates(bool mcHaveTrueCand, bool mcHaveCand, Double_t eta, Double_t Momentum);
	void	FillMatchingEfficiency(const TClonesArray* aTofMatching, const TClonesArray* aTofHits, const TClonesArray* aKFTracks, const TClonesArray* aMCTracks);	
	
	void	FillSelectedKfTracks(Double_t eta, Double_t Momentum){ htKFTrack->Fill(eta, Momentum); }
	void	FillTrackPerEvent(Double_t nKFTracks, Double_t selectedTracks){ htTrackPerEvent->Fill(nKFTracks, selectedTracks); }
	void	FillCandidateNumber(Double_t nCands, Double_t nIter){ htCandNmb->Fill(nCands, nIter); }
	
	void	FilleRest(Double_t R){ heRest->Fill(R); }	
};
//------------------------------------------------------------------------------------------------------------------------
#endif
