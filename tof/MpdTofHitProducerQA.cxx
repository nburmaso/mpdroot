//------------------------------------------------------------------------------------------------------------------------
#include <iostream>

#include <TFile.h>
#include <TMath.h>
#include <TClonesArray.h>

#include "FairLogger.h" 
#include "MpdTofHit.h"
#include "MpdTof.h"

#include "MpdTofHitProducerQA.h"
using namespace std;
//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducerQA::MpdTofHitProducerQA(const char *flnm, bool isEndcap)
: fFlnm(flnm), fIsEndcap(isEndcap)
{
      	fList.SetOwner(); //  all objects will be deleted whenever the collection itself is delete.	
      	
	Add(hOccup = new TH1D(mangling("Occupancy"), "occupancy per strips;occupancy;Events", 100, -0.5, 99.5));
	Add(hMergedTimes = new TH2D(mangling("MergedTimes"), "Merged hits on strip times test;faster hit time, ns;slower hit time, ns", 1000, 0., 150., 1000, 0., 150.));
	Add(hHitPointPerEvent = new TH2D(mangling("HitPointPerEvent"), ";N points per event;N hits per event", 1000, 0.5, 3000.5, 1000, 0.5, 1500.5));
	Add(hHitDistance = new TH2D(mangling("HitDistance"), "distance between hits from different mc tracks;distance, cm;time between hits, ns", 1000, 0., 100., 1000, 0., 250.));

    	Add(effHitGap2 = new TEfficiency(mangling("efficiencyHitsMiddle"), "Efficiency of middle gap hits;R, cm;efficiency", 1000, -0.1, 1.)); 
    	Add(effHitGap13 = (TEfficiency*) effHitGap2->Clone(mangling("efficiencyHitsOuter"))); 
	effHitGap13->SetTitle("Efficiency of outer gaps hits;R, cm;efficiency");
    	Add(effCrossHit = (TEfficiency*) effHitGap2->Clone(mangling("efficiencyCrossHits"))); 
	effCrossHit->SetTitle("Efficiency of cross hits;R, cm;efficiency");
	
	// geometrical tests
	Add(hDistance = new TH1D(mangling("geoDistance"), "Distance between strips(minus-left side, plus-right side);Distance, cm;Side", 1000, -100., 100.));  
	Add(h2Strips = new TH2D(mangling("geoStripsMap"), ";Z, cm;#phi, rads", 2000, -300., 300., 500, -3.5, 3.5));
	Add(h2Detectors = new TH2D(mangling("geoDetectorMap"), ";Z, cm;#phi, rads", 2000, -300., 300., 500, -3.5, 3.5));	    		
	Add(hNeighborPair = new TH2D(mangling("geoNeighborPair"), "Neighbor strip pairs test;stripID1;stripID2", 75, -0.5, 74.5, 75, -0.5, 74.5));   
 	Add(hEtaPhi = new TH2D(mangling("geoEtaPhi"), ";#eta;#phi, degree", 1000, -6., 6., 1000, -181., 181.));	
	Add(hRZ = new TH2D(mangling("geoRZ"), ";Z, cm;R, cm", 1000, -300., 300., 1000, 15., 160.));

	// smearing tests
	Add(hXYSmeared = new TH2D(mangling("XYSmeared_ZPhi"), "single hits;#Delta_{Z}, cm;#Delta_{#phi}, cm", 1000, -1., 1., 1000, -1., 1.));
	Add(hXYSmeared2 = new TH2D(mangling("XYSmeared_DevR"), "single hits;#Delta, cm;#DeltaRperp, cm", 1000, 0., 2., 1000, -2., 2.));		
	Add(hXYSmearedCross = new TH2D(mangling("XYSmearedCross_ZPhi"), "cross hits;#Delta_{Z}, cm;#Delta_{#phi}, cm", 1000, -2., 2., 1000, -1., 1.));
	Add(hXYSmearedCross2 = new TH2D(mangling("XYSmearedCross_DevR"), "cross hits;#Delta, cm;#DeltaRperp, cm", 1000, 0., 3., 1000, -3., 3.));
}
//------------------------------------------------------------------------------------------------------------------------			
void	MpdTofHitProducerQA::Finish()
{
	LOG(DEBUG2)<<"[MpdTofHitProducerQA::Finish] Update  "<<fFlnm.Data()<<" file. ";
	auto ptr = gFile;
	TFile file(fFlnm.Data(), "RECREATE");
	fList.Write(); 
	file.Close();
	gFile = ptr;
}
//------------------------------------------------------------------------------------------------------------------------
void	MpdTofHitProducerQA::FillHitDistance(const TClonesArray *aHits)
{
	TVector3 pos1, pos2;
        for(Int_t i = 0, iMax1 = aHits->GetEntriesFast(); i < iMax1; i++) // cycle by tof hits
	{   
		auto pHit1 = (MpdTofHit*) aHits->At(i);

        	for(Int_t j = i, iMax2 = aHits->GetEntriesFast(); j < iMax2; j++) 
		{   
			auto pHit2 = (MpdTofHit*) aHits->At(j);
			
			if(i == j) continue;

			if(! pHit1->IsSameTid(*pHit2))
			{
				pHit1->Position(pos1);
				pHit2->Position(pos2);
				double dT = pHit1->GetTime() - pHit2->GetTime();

				hHitDistance->Fill((pos1-pos2).Mag(), fabs(dT));
			}
		}
	}
}
//------------------------------------------------------------------------------------------------------------------------			
void	MpdTofHitProducerQA::FillDetectorsMap(const TVector3& A, const TVector3& B, const TVector3& C, const TVector3& D)  // detector edges
{
	h2Detectors->Fill(A.Z(), A.Phi());
	h2Detectors->Fill(B.Z(), B.Phi());	
	h2Detectors->Fill(C.Z(), C.Phi());
	h2Detectors->Fill(D.Z(), D.Phi());
}
//------------------------------------------------------------------------------------------------------------------------			
void	MpdTofHitProducerQA::FillStripsMap(const TVector3& A, const TVector3& B, const TVector3& C, const TVector3& D) // strip edges
{
	h2Strips->Fill(A.Z(), A.Phi());
	h2Strips->Fill(B.Z(), B.Phi());	
	h2Strips->Fill(C.Z(), C.Phi());
	h2Strips->Fill(D.Z(), D.Phi());
}
//------------------------------------------------------------------------------------------------------------------------
void	MpdTofHitProducerQA::FillHitEfficiency(bool fired, Double_t distance, size_t gap, const TVector3& position, const TVector3& smearedPosition)
{
	if(2 == gap) 	effHitGap2->Fill(fired, distance);
	else 		effHitGap13->Fill(fired, distance);

	if(fired)
	{
		double delta, deltaZ, deltaR, deltaPhi;
		MpdTof::GetDelta(position, smearedPosition, delta, deltaZ, deltaR, deltaPhi); 

		hXYSmeared->Fill(deltaZ, deltaPhi);
		hXYSmeared2->Fill(delta, deltaR);

		hEtaPhi->Fill(position.Eta(), position.Phi()*TMath::RadToDeg());
		hRZ->Fill(position.Z(), position.Perp());
	}
}
//------------------------------------------------------------------------------------------------------------------------
void	MpdTofHitProducerQA::FillCrossHitEfficiency(bool fired, Double_t distance, size_t gap, const TVector3& position, const TVector3& smearedPosition)
{
	effCrossHit->Fill(fired, distance);

	if(fired)
	{
		double delta, deltaZ, deltaR, deltaPhi;
		MpdTof::GetDelta(position, smearedPosition, delta, deltaZ, deltaR, deltaPhi); 

		hXYSmearedCross->Fill(deltaZ, deltaPhi);
		hXYSmearedCross2->Fill(delta, deltaR);
	}
}
//------------------------------------------------------------------------------------------------------------------------


