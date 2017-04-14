//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_HITPRODUCER_QA_H
#define __MPD_TOF_HITPRODUCER_QA_H 1

#include <TList.h>
#include <TString.h>
#include <TVector3.h>
#include <TEfficiency.h>
#include <TMath.h>
#include <TH2D.h>
#include <TH1D.h>
//------------------------------------------------------------------------------------------------------------------------
class MpdTofHitProducerQA 
{
	// QA test histos	
        TList			fList;	
	TH1D   			*hOccup, *htR, *hDistance;        	
        TH2D   			*hMergedTimes, *hNeighborPair, *hXYSmeared, *hXYSmeared2, *hXYSmearedDouble, *hXYSmearedDouble2, *hEtaPhi, *hStrips, *hRZ;
	TEfficiency		*effSingleHit, *effDoubleHit;		
        TString			fFlnm;
        bool			fIsEndcap; 
 
   	const char* 	mangling(const char* name){ static TString nm; nm = fIsEndcap ? "eTOF_" : "TOF_"; nm += name; return nm.Data();}
	void		Add(TH1 *hist){ hist->SetDirectory(nullptr); fList.Add(hist);}
	void		Add(TEfficiency *hist){ hist->SetDirectory(nullptr); fList.Add(hist);}
                	
public :	
	MpdTofHitProducerQA(const char *flnm, bool isEndcap);	
	void	Finish();

	TH1D*		GetDistanceHisto(){ return hDistance;}	
	TH1D*		GetOccupancyHisto(){ return hOccup;}
	TH2D*		GetStripLocationHisto(){ return hStrips;}			
	TH2D*		GetNeighborPairHisto(){ return hNeighborPair;}
	TH2D*		GetMergedTimesHisto(){ return hMergedTimes;}		
	TEfficiency*	GetSingleHitEfficiency(){ return effSingleHit;}
	TEfficiency*	GetDoubleHitEfficiency(){ return effDoubleHit;}
	
	void	FillSingleHitPosition(TVector3 pos, TVector3 XYZ_smeared)
	{
		hXYSmeared->Fill((pos - XYZ_smeared).Mag(), pos.Z() - XYZ_smeared.Z());
		hXYSmeared2->Fill(XYZ_smeared.X(), XYZ_smeared.Y());
		hEtaPhi->Fill(pos.Eta(), pos.Phi()*TMath::RadToDeg());
		hRZ->Fill(pos.Z(), pos.Perp());
	}
	
	void	FillDoubleHitPosition(TVector3 pos, TVector3 XYZ_smeared)
	{	
		hXYSmearedDouble->Fill((pos - XYZ_smeared).Mag(), pos.Z() - XYZ_smeared.Z());
		hXYSmearedDouble2->Fill(XYZ_smeared.X(), XYZ_smeared.Y());
        }
	
};
//------------------------------------------------------------------------------------------------------------------------
#endif
