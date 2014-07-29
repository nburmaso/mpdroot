//------------------------------------------------------------------------------------------------------------------------
/// \class MpdEtofHitProducer
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------
#include <iostream>
#include <fstream>
#include <valarray>
#include <assert.h>

#include "TClonesArray.h"
#include "TRandom.h"
#include "TVector3.h"
#include "TStopwatch.h"
#include "TMath.h"

#include "FairRunAna.h"
#include "FairBaseParSet.h"
#include "FairRuntimeDb.h"
#include "FairMCApplication.h"
#include "FairDetector.h"
#include "FairRootManager.h"
#include "FairMCTrack.h"
#include "FairGeoVolume.h"

#include "MpdEtofGeoPar.h"
#include "MpdEtofPoint.h"
#include "MpdEtofHit.h"
#include "MpdEtof.h"

#include "MpdEtofHitProducer.h"

using namespace std;

//------------------------------------------------------------------------------------------------------------------------
MpdEtofHitProducer::MpdEtofHitProducer(const char *name, Int_t verbose, Bool_t test)
  : FairTask(name, verbose), fDoTest(test), fTimeSigma(0.100), hTestC_D(NULL),  fTestFlnm("test.MpdEtofHitProducer.root")
{
	if(fDoTest)
    	{
    		hTestR = MpdTofUtils::Make1D("DeltaR;#Delta, cm;Events", 1000, 0., 40., &fList);   
		hTestXY = MpdTofUtils::Make2D("DeltaXY;#Delta X, cm;#Delta Y, cm", 000, -20., 20., 1000, -20., 20., &fList);	
		hTestC_D = MpdTofUtils::Make2D("testC_D;#Delta, cm;k_side", 1000, 0., 200., 10, -0.5, 9.5, &fList);	 
		htXYpoints = MpdTofUtils::Make2D("htXYpoints;X, cm;Y, cm", 1000, -116., 116., 1000, -116., 116., &fList, "Point position");
		htPointsHits = MpdTofUtils::Make2D("htPointsHits;N_{points};N_{hits} ", 1001, -0.5, 1000.5, 1001, -0.5, 1000.5, &fList, "Points vs Hits");
		hTestCross1 = MpdTofUtils::Make2D("hTestCross1;distance from the edge, cm;distance to the pad center, cm", 1000, 0., 6., 1000, 0., 20., &fList, "Double hits");   
		hTestCross2 = MpdTofUtils::Make2D("hTestCross2;distance from the edge, cm;distance to the pad center, cm", 1000, 0., 6., 1000, 0., 20., &fList, "Triple hits");
		hTest1thRegion = MpdTofUtils::Make2D("hTest1thRegion;X, cm;Z, cm", 1000, -50., 50., 1000, -10., 10., &fList);
		hTestDeadTime = MpdTofUtils::Make2D("hTestDeadTime;faster hit time, ns;slower hit time, ns", 1000, 0., 50., 1000, 0., 50., &fList, "Dead time test");
		hTestUnion = MpdTofUtils::Make2D("hTestUnion;#Delta, cm;multiplicity", 1000, 0., 40., 20, 0.5, 20.5, &fList);
    	}

  	pRandom = new TRandom2; 
}
//------------------------------------------------------------------------------------------------------------------------
MpdEtofHitProducer::~MpdEtofHitProducer()
{
  	delete pRandom;	
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus		MpdEtofHitProducer::Init()
{
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " Begin ETof hit producer initialization.");

	FairRootManager *ioman = FairRootManager::Instance(); assert(ioman);
  	pETofPoints  = (TClonesArray *) ioman->GetObject("ETOFPoint");
  	pMCTracks   = (TClonesArray *) ioman->GetObject("MCTrack"); 

  	if(!pETofPoints || !pMCTracks){ FairLogger::GetLogger()->Error(MESSAGE_ORIGIN, " Branch not found!"); return kERROR; }
	
  	TString parFlnm = MpdTofUtils::GetEtofParFlnm(fGeoFlNm);
	if(!MpdTofUtils::ReadParamFromXML(parFlnm.Data(), vecRegions, mmapModules, mapPads)) // file invalid || don't exist
	{
		if(!MpdEtof::ParseTGeoManager(vecRegions, mmapModules, mapPads)) return kFATAL;			
		MpdTofUtils::WriteParamToXML(parFlnm.Data(), "ETOF_parameters", "The MPD ETOF geo parameters.", vecRegions, mmapModules, mapPads);		
	}
	
  	// Create and register output array
  	pHitCollection = new TClonesArray("MpdEtofHit");
  	ioman->Register("ETOFHit", "ETof", pHitCollection, kTRUE);

	pHitTmp = new TClonesArray("MpdEtofHit"); // tmp collection
 	
        FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " Initialization finished succesfully(%i entries).", (int)mapPads.size());

return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t 	MpdEtofHitProducer::HitExist(Double_t val) // val - rasstojanie do kraja pad
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
Bool_t 	MpdEtofHitProducer::DoubleHitExist(Double_t val) // val - rasstojanie do kraja pad
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
void 		MpdEtofHitProducer::Exec(Option_t *option)
{
	pHitCollection->Delete();
 	pHitTmp->Delete();
	
	Int_t nTofPoint = pETofPoints->GetEntries();  
        FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " %i points in ETof for this event.", nTofPoint);
	
	MpdEtofPoint *pPoint;
	const static double sqrt12 = TMath::Sqrt(12.);
	TVector3 position, hitPosErr, hitPos; 	
	MpdTofUtils::padPar *param;
	Int_t UID, CrossUID, trackID;
	Double_t  time, Z; 	
	deltaMap map; deltaIter iter;

	Int_t  hitIndex = 0;          //Index of the  TClonesArray	
	for(Int_t pointIndex = 0; pointIndex < nTofPoint; pointIndex++ )  // <---Loop over the TOF points
	{
		pPoint = (MpdEtofPoint*) pETofPoints->At(pointIndex);
		
		if(fVerbose > 2) pPoint->Print(""); 		
	
                //FairMCTrack  *pTrack	= (FairMCTrack*) pMCTracks->At(pPoint->GetTrackID());
		trackID = pPoint->GetTrackID();	
		UID	= pPoint->GetDetectorID();
		time	= pRandom->Gaus(pPoint->GetTime(), fTimeSigma); // 100 ps		
		pPoint->Position(position); if(fDoTest) htXYpoints->Fill(position.X(), position.Y());
		Z = position.Z();	
			
		if(MpdTofUtils::Find(mapPads, &param, UID))
		{				
			if(fDoTest)
			{
				TVector3 deltaR = position - param->center;
				hTestR->Fill(deltaR.Mag());
				hTestXY->Fill(deltaR.X(), deltaR.Y());
			}	
			
			MpdTofUtils::GetClosestSide(position, param, &map); 							
			iter = map.begin(); // closest, pair<Double_t div, k_side>
			
			if(HitExist(iter->first)) // cm
			{
				double RdPhi = (param->point[0] - param->point[1]).Mag();	// size along strip
				double dR = (param->point[1] - param->point[2]).Mag();		// size across strip
	
				hitPosErr.SetXYZ(RdPhi/sqrt12, dR/sqrt12, 0.);
				hitPos.SetXYZ(param->center.X(), param->center.Y(), Z);
				AddRawHit(hitIndex++, UID, hitPos, hitPosErr, pointIndex, trackID, time, MpdTofUtils::IsSingle);		 
			}
/*	 //////////  v3_geo - "one pad geometry"				
			if(DoubleHitExist(iter->first)) // Double hit, cm  
			{
				hitPosErr.SetXYZ(0., 0., 0.);
				CrossUID = MpdTofUtils::GetNeighboringPadUID(mapPads, UID, iter->second);
				if(CrossUID != MpdTofUtils::Absent)
				{
					hitPos.SetXYZ(MpdTofUtils::GetPadCenter(mapPads, CrossUID).X(), MpdTofUtils::GetPadCenter(mapPads, CrossUID).Y(), Z);
					AddRawHit(hitIndex++, CrossUID, hitPos, hitPosErr, pointIndex, trackID, time, MpdTofUtils::IsDouble);
					if(fDoTest)hTestCross1->Fill(iter->first, (position - MpdTofUtils::GetPadCenter(mapPads, CrossUID)).Mag());
				}						
			}			
			
			iter++; // next by closest
			if(DoubleHitExist(iter->first)) // Triple hit, cm
			{
				hitPosErr.SetXYZ(0., 0., 0.);		
				CrossUID = MpdTofUtils::GetNeighboringPadUID(mapPads,  UID, iter->second);
				if(CrossUID != MpdTofUtils::Absent)
				{
					hitPos.SetXYZ(MpdTofUtils::GetPadCenter(mapPads, CrossUID).X(), MpdTofUtils::GetPadCenter(mapPads, CrossUID).Y(), Z);
					AddRawHit(hitIndex++, CrossUID, hitPos, hitPosErr, pointIndex, trackID, time, MpdTofUtils::IsTriple);
					if(fDoTest)hTestCross2->Fill(iter->first, (position - MpdTofUtils::GetPadCenter(mapPads, CrossUID)).Mag());
				}						
			}
*/							
		}
		else assert(UID != MpdTofUtils::Absent);
		
	}	// <---Loop over the TOF points
	
	
	Int_t reducedByDeadTimeNmb = SimPadDeadTime(); // remove multihits for same pad
	Int_t reducedByClusterNmb = MakeClusterHits(); // cluster finding  
	
	if(fDoTest) htPointsHits->Fill(nTofPoint, hitIndex);

        FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " ( %i hits in ETof created[%i, %i])", hitIndex, reducedByClusterNmb, reducedByDeadTimeNmb);	
}
//------------------------------------------------------------------------------------------------------------------------
Int_t MpdEtofHitProducer::SimPadDeadTime() // leave only the fastest hit in the pad 
{
	typedef map<Int_t, MpdEtofHit*> hitsMap;
	hitsMap 		fastestHits; // <pad UID, MpdTofHit pointer>
	hitsMap::iterator 	it;	
	MpdEtofHit *pHit;
	Int_t UID, nHits = pHitTmp->GetEntriesFast(), reducedNmb = 0;   
	
	for(Int_t hitIndex = 0; hitIndex < nHits; hitIndex++ )
	{	
		pHit = (MpdEtofHit*) pHitTmp->UncheckedAt(hitIndex); 		
		if(!pHit)continue; // blank slot
		
		UID = pHit->GetDetectorID();
		it = fastestHits.find(UID);
		if(it != fastestHits.end()) // exist hit for this UID
		{
			reducedNmb++;
			if(pHit->GetTime() < it->second->GetTime()) // founded more faster hit
			{
				if(fDoTest) hTestDeadTime->Fill(pHit->GetTime(), it->second->GetTime());

				pHit->AddLinks(it->second->GetLinks());			// copy links
				pHitTmp->Remove(it->second); 				// remove old hit   --> make blank slote !!
				pHit->SetFlag(pHit->GetFlag() | MpdTofUtils::HaveTail);	// Set "HaveTail" flag					
				fastestHits[UID]= pHit;					// change pair value to current UID
			}
			else  	pHitTmp->Remove(pHit);					// remove current hit --> make blank slote !!
		}
		else fastestHits.insert(hitsMap::value_type(UID, pHit)); 		// insert new pair
	}

return 	reducedNmb;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdEtofHitProducer::Finish()
{
	if(fDoTest)
	{
		TFile *ptr = gFile;
		TFile file(fTestFlnm.Data(), "RECREATE");
		fList.Write(); 
		file.Close();
		gFile = ptr;
	}
}
//------------------------------------------------------------------------------------------------------------------------
Int_t MpdEtofHitProducer::MakeClusterHits(void)
{
	// cluster finding

/* //////////  v3_geo - "one pad geometry"
        MpdEtofHit *pSideHit; Int_t sideUID, size; TVector3 meanPos, vec;
        Int_t nHits = pHitTmp->GetEntriesFast();
	for(Int_t hitIndex = 0; hitIndex < nHits; hitIndex++ )
	{	
		pHit = (MpdEtofHit*) pHitTmp->UncheckedAt(hitIndex); 
		
		if(!pHit)continue; // blank slot
		
		UID = pHit->GetDetectorID();
		pHit->Position(meanPos); size = 1;
						
		for(Int_t side = MpdTofUtils::Up; side <= MpdTofUtils::Left; side++)			
			if( (sideUID = MpdTofUtils::GetNeighboringPadUID(mapPads, UID,  (MpdTofUtils::k_side) side)) != MpdTofUtils::Absent )	
				if(pSideHit = FindTmpHit(sideUID))
				{
					pSideHit->Position(vec); 
					meanPos += vec;	size++;
					pHit->AddLinks(pSideHit->GetLinks());		// copy links
					pHitTmp->Remove(pSideHit); 			// hit into cluster; removed from collection    --> make blank slote !!
				}
			
		if(size != 1) // !singleton  --> update
		{
			meanPos *= 1./size;
			
			if(fDoTest)
			{
				pHit->Position(vec); 
				hTestUnion->Fill((meanPos - vec).Mag(), size);
			}
			
			pHit->SetPosition(meanPos);
			pHit->SetFlag(pHit->GetFlag() | MpdTofUtils::InCluster);	// Set "InCluster" flag
		}		
	}
*/		
        MpdEtofHit *pHit; Int_t UID;
        // copy&compession collection
	TIterator *iter = pHitTmp->MakeIterator(); Int_t  hitIndex = 0;			
      	while( (pHit = (MpdEtofHit*) iter->Next()) )   						
	{	
	  	MpdEtofHit *data = new  ((*pHitCollection)[hitIndex++]) MpdEtofHit(*pHit);	
		if(fDoTest)
		{
			UID = data->GetDetectorID();
			if(MpdEtofPoint::GetRegion(UID) == 1 && MpdEtofPoint::GetModule(UID) == 1) hTest1thRegion->Fill(data->GetX(), data->GetZ());
		}

		if(fVerbose > 1)	data->Print();	
	}	
	delete iter;
		
    return hitIndex;
}
//------------------------------------------------------------------------------------------------------------------------
MpdEtofHit* MpdEtofHitProducer::FindTmpHit(Int_t detUID)
{
  	MpdEtofHit *pHit; TIterator *iter = pHitTmp->MakeIterator(); 			
      	while( (pHit = (MpdEtofHit*) iter->Next()) )	if(detUID == pHit->GetDetectorID()){ delete iter; return pHit; }

	delete iter;
return NULL;	
}
//------------------------------------------------------------------------------------------------------------------------
void MpdEtofHitProducer::AddRawHit(Int_t index, Int_t detUID, const TVector3 &posHit, const TVector3 &posHitErr, Int_t pointIndex, 
					Int_t trackIndex, Double_t time, Int_t flag)
{
	MpdEtofHit *pHit	= new  ((*pHitTmp)[index]) MpdEtofHit(detUID, posHit, posHitErr, pointIndex, time, flag);
	pHit->AddLink(FairLink(MpdTofUtils::IsTofPointIndex, pointIndex));
	pHit->AddLink(FairLink(MpdTofUtils::IsMCTrackIndex, trackIndex));	
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdEtofHitProducer)


