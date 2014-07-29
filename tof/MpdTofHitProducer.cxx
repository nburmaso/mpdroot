//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTofHitProducer
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include "MpdTofGeoPar.h"
#include "MpdTofPoint.h"
#include "MpdTof.h"
#include "MpdTofHitProducer.h"

#include "FairRunAna.h"
#include "FairBaseParSet.h"
#include "FairRuntimeDb.h"
#include "FairMCApplication.h"
#include "FairDetector.h"
#include "FairRootManager.h"
#include "FairMCTrack.h"
#include "FairGeoVolume.h"

#include "TStopwatch.h"
#include "TMath.h"

#include <fstream>
#include <valarray>
#include <assert.h>

//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducer::MpdTofHitProducer(const char *name, Int_t verbose, Bool_t test)
  : FairTask(name, verbose), fDoTest(test), fTimeSigma(0.100), pTofPoints(NULL), pMCTracks(NULL), 
    pHitCollection(NULL), pHitTmp(NULL), pRandom(new TRandom2), fTestFlnm("test.MpdTofHitProducer.root")
{
        if(fDoTest)
    	{
    		htR = MpdTofUtils::Make1D("DeltaR", 1000, 0., 40., &fList, ";#Delta, cm;Events");   
		htXY = MpdTofUtils::Make2D("DeltaXY", 1000, -5., 5., 1000, -5., 5., &fList, ";#Delta X, cm;#Delta Y, cm");	
		htC_D = MpdTofUtils::Make2D("C_D", 1000, 0., 200., 10, -0.5, 9.5, &fList, ";#Delta, cm;k_side");	 
		htCross1 = MpdTofUtils::Make2D("Cross1", 1000, 0., 2., 1000, 0., 6., &fList, "Double hits;distance from the edge, cm;distance to the pad center, cm");   
		htCross2 = MpdTofUtils::Make2D("Cross2", 1000, 0., 2., 1000, 0., 6., &fList, "Triple hits;distance from the edge, cm;distance to the pad center, cm");  
		htXZregionP = MpdTofUtils::Make2D("htXZregionP", 1000, -31., 31., 1000, -250., 250., &fList, "Point position(region);X, cm;Z, cm");
		htXZregion = MpdTofUtils::Make2D("htXZregion", 1000, -31., 31., 1000, -250., 250., &fList, "Hit position(region);X, cm;Z, cm");
		htXZmodule = MpdTofUtils::Make2D("htXZmodule", 1000, -31., 31., 1000, -4., 4., &fList, "Hit position(module);X, cm;Z, cm");
		htXZpad = MpdTofUtils::Make2D("htXZpad", 1000, -31., 31., 1000, -4., 4., &fList, "Hit position(pad);X, cm;Z, cm");
						
		htDeadTime = MpdTofUtils::Make2D("DeadTime", 1000, 0., 50., 1000, 0., 50., &fList, "Dead time test;faster hit time, ns;slower hit time, ns");
		htUnion = MpdTofUtils::Make2D("Union", 1000, 0., 10., 20, 0.5, 20.5, &fList, ";#Delta, cm;multiplicity");
		htEtaPhi = MpdTofUtils::Make2D("EtaPhi", 1000, -1.6, 1.6, 1000, -181., 181., &fList, ";#eta;#phi, degree");
		
		htChainPID = MpdTofUtils::Make2D("htChainPID", 2000, -2250.5, 2250.5, 2000, -2250.5, 2250.5, &fList, ";pid;pid");  
		htNeighPID = MpdTofUtils::Make2D("htNeighPID", htChainPID, &fList); 
    	}
        else htC_D = htXY = htXZregionP = htXZregion = htXZmodule = htXZpad = htCross1 = htCross2 = htDeadTime = htUnion = htEtaPhi = htChainPID = htNeighPID = NULL, 
		htR = NULL;
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducer::~MpdTofHitProducer()
{
    if (pRandom) delete pRandom;
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus MpdTofHitProducer::Init()
{
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " Begin Tof hit producer initialization.");

	FairRootManager *ioman = FairRootManager::Instance(); assert(ioman);
  	pTofPoints  = (TClonesArray *) ioman->GetObject("TOFPoint");
  	pMCTracks   = (TClonesArray *) ioman->GetObject("MCTrack"); 

  	if(!pTofPoints || !pMCTracks){ FairLogger::GetLogger()->Error(MESSAGE_ORIGIN, " Branch not found!"); return kERROR; }
	
  	TString parFlnm = MpdTofUtils::GetTofParFlnm(fGeoFlNm);
	if(!MpdTofUtils::ReadParamFromXML(parFlnm.Data(), vecRegions, mmapModules, mapPads)) // file invalid || don't exist
	{
		if(!MpdTof::ParseTGeoManager(htC_D, vecRegions, mmapModules, mapPads)) return kFATAL;	
		MpdTofUtils::WriteParamToXML(parFlnm.Data(), "TOF_parameters", "The MPD TOF geo parameters.", vecRegions, mmapModules, mapPads);		
	}
	
        // Create and register output array
        pHitCollection = new TClonesArray("MpdTofHit");
        ioman->Register("TOFHit", "Tof", pHitCollection, kTRUE);

        pHitTmp = new TClonesArray("MpdTofHit"); // tmp collection

        FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " Initialization finished succesfully(%i entries).", (int)mapPads.size());

return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t 	MpdTofHitProducer::HitExist(Double_t val) // val - rasstojanie do kraja pad
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
Bool_t 	MpdTofHitProducer::DoubleHitExist(Double_t val) // val - rasstojanie do kraja pad
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
void 		MpdTofHitProducer::Exec(Option_t *option)
{
	pHitCollection->Delete();
 	pHitTmp->Delete();
	
	Int_t nTofPoint = pTofPoints->GetEntriesFast();  
        FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " %i points in Tof for this event.", nTofPoint);
	
	MpdTofPoint *pPoint; 
	TVector3 point, hitPosErr; 	
	MpdTofUtils::padPar *param;
	Int_t UID, CrossUID, trackID;
	Double_t  time; 	
	deltaMap map; deltaIter iter;

	Int_t  hitIndex = 0;          //Index of the  TClonesArray	
	for(Int_t pointIndex = 0; pointIndex < nTofPoint; pointIndex++ )  // <---Loop over the TOF points
	{
		pPoint = (MpdTofPoint*) pTofPoints->UncheckedAt(pointIndex);
		
		if(fVerbose > 2) pPoint->Print(""); 		
  
		trackID = pPoint->GetTrackID();	
		UID	= pPoint->GetDetectorID();
		time	= pRandom->Gaus(pPoint->GetTime(), fTimeSigma); // 100 ps		
		pPoint->Position(point);
		  
		if(MpdTofUtils::Find(mapPads, &param, UID))
		{				
			if(fDoTest)
			{
				TVector3 deltaR = point - param->center;
				htR->Fill(deltaR.Mag());
				htXY->Fill(deltaR.X(), deltaR.Y());
				htXZregionP->Fill(point.X(), point.Z());
				htEtaPhi->Fill(point.Eta(), point.Phi() * TMath::RadToDeg());						
			}	
			
			MpdTofUtils::GetClosestSide(point, param, &map); 							
			iter = map.begin(); // closest, pair<Double_t div, k_side>
			
			if(HitExist(iter->first)) // cm
			{
				hitPosErr.SetXYZ(0., 0., 0.);
				AddRawHit(hitIndex++, UID, param->center, hitPosErr, pointIndex, trackID, time, MpdTofUtils::IsSingle); 			
			}
					
			if(DoubleHitExist(iter->first)) // Double hit, cm  
			{
				hitPosErr.SetXYZ(0., 0., 0.);
				CrossUID = MpdTofUtils::GetNeighboringPadUID(mapPads, UID, iter->second);
				if(CrossUID != MpdTofUtils::Absent)
				{
					AddRawHit(hitIndex++, CrossUID, MpdTofUtils::GetPadCenter(mapPads, CrossUID), hitPosErr, pointIndex, trackID, time, MpdTofUtils::IsDouble);
					if(fDoTest)htCross1->Fill(iter->first, (point - MpdTofUtils::GetPadCenter(mapPads, CrossUID)).Mag());
				}						
			}			
			
			iter++; // next by closest
			if(DoubleHitExist(iter->first)) // Triple hit, cm
			{
				hitPosErr.SetXYZ(0., 0., 0.);		
				CrossUID = MpdTofUtils::GetNeighboringPadUID(mapPads,  UID, iter->second);
				if(CrossUID != MpdTofUtils::Absent)
				{
					AddRawHit(hitIndex++, CrossUID, MpdTofUtils::GetPadCenter(mapPads, CrossUID), hitPosErr, pointIndex, trackID, time, MpdTofUtils::IsTriple);
					if(fDoTest)htCross2->Fill(iter->first, (point - MpdTofUtils::GetPadCenter(mapPads, CrossUID)).Mag());
				}						
			}				
		}
		else assert(UID != MpdTofUtils::Absent);
		
	}	// <---Loop over the TOF points
	
	Int_t reducedByDeadTimeNmb = SimPadDeadTime(); // remove multihits for same pad
	Int_t reducedByClusterNmb = MakeClusterHits(); // cluster finding

        FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " ( %i hits in Tof created[%i, %i])", hitIndex, reducedByClusterNmb, reducedByDeadTimeNmb);	
}
//------------------------------------------------------------------------------------------------------------------------
Int_t MpdTofHitProducer::SimPadDeadTime() // leave only the fastest hit in the pad 
{
	typedef map<Int_t, MpdTofHit*> hitsMap;
	hitsMap 		fastestHits; // pair<pad UID, MpdTofHit pointer>
	hitsMap::iterator 	it;	
	MpdTofHit *pHit;
	Int_t UID, nHits = pHitTmp->GetEntriesFast(), reducedNmb = 0;   
	
	for(Int_t hitIndex = 0; hitIndex < nHits; hitIndex++ )
	{	
		pHit = (MpdTofHit*) pHitTmp->UncheckedAt(hitIndex); 		
		if(!pHit)continue; // blank slot
		
		UID = pHit->GetDetectorID();
		it = fastestHits.find(UID);
		if(it != fastestHits.end()) // exist hit for this UID
		{
			reducedNmb++;
			if(pHit->GetTime() < it->second->GetTime()) // founded more faster hit
			{
				if(fDoTest)
				{
					htDeadTime->Fill(pHit->GetTime(), it->second->GetTime());
					
					int mcTrackId = ( (MpdTofPoint*) pTofPoints->UncheckedAt(pHit->GetRefIndex()) )->GetTrackID();
					int pid1 = ( (FairMCTrack*) pMCTracks->At(mcTrackId) )->GetPdgCode();
					mcTrackId = ( (MpdTofPoint*) pTofPoints->UncheckedAt(it->second->GetRefIndex()) )->GetTrackID();
					int pid2 = ( (FairMCTrack*) pMCTracks->At(mcTrackId) )->GetPdgCode();
					
					htChainPID->Fill(pid1, pid2);
				}
				
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
void MpdTofHitProducer::Finish()
{
  	if(fDoTest)
    	{
      		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " <MpdTofHitProducer::Finish> Update  %s file. ", fTestFlnm.Data());
		TFile *ptr = gFile;
		TFile file(fTestFlnm.Data(), "RECREATE");
		fList.Write(); 
		file.Close();
		gFile = ptr;
	}
}
//------------------------------------------------------------------------------------------------------------------------
Int_t MpdTofHitProducer::MakeClusterHits(void)
{
	// cluster finding
	MpdTofHit *pHit, *pSideHit; Int_t UID, sideUID, size; TVector3 meanPos, vec;

	Int_t nHits = pHitTmp->GetEntriesFast();    
/*	for(Int_t hitIndex = 0; hitIndex < nHits; hitIndex++ )
	{	
		pHit = (MpdTofHit*) pHitTmp->UncheckedAt(hitIndex); 
		
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
					pHit->AddFlag(pSideHit->GetFlag());		// copy flags
					pHitTmp->Remove(pSideHit); 			// hit into cluster; removed from collection    --> make blank slote !!
					
					if(fDoTest)
					{
						int mcTrackId = ( (MpdTofPoint*) pTofPoints->UncheckedAt(pHit->GetRefIndex()) )->GetTrackID();
						int pid1 = ( (FairMCTrack*) pMCTracks->At(mcTrackId) )->GetPdgCode();
						int mcTrackId2 = ( (MpdTofPoint*) pTofPoints->UncheckedAt(pSideHit->GetRefIndex()) )->GetTrackID();
						int pid2 = ( (FairMCTrack*) pMCTracks->At(mcTrackId2) )->GetPdgCode();
										
						if( (pHit->GetFlag() & BIT(0)) &&  (pSideHit->GetFlag() & BIT(0)) ) htNeighPID->Fill(pid1, pid2);
					}
				}
			
		if(size != 1) // !singleton  --> update
		{
			meanPos *= 1./size;
			
			if(fDoTest)
			{
				pHit->Position(vec); 
				htUnion->Fill((meanPos - vec).Mag(), size);
			}
			
			pHit->SetPosition(meanPos);
			pHit->SetFlag(pHit->GetFlag() | MpdTofUtils::InCluster);	// Set "InCluster" flag
		}		
	}
*/
	// copy&compession collection
	TIterator *iter = pHitTmp->MakeIterator(); Int_t  hitIndex = 0;			
      	while( (pHit = (MpdTofHit*) iter->Next()) )   						
	{	
	  	MpdTofHit *data = new  ((*pHitCollection)[hitIndex++]) MpdTofHit(*pHit);	
		if(fDoTest)
		{
			UID = data->GetDetectorID();
			if(MpdTofPoint::GetRegion(UID) == 1) htXZregion->Fill(data->GetX(), data->GetZ());
			if(MpdTofPoint::GetRegion(UID) == 1 && MpdTofPoint::GetModule(UID) == 1) htXZmodule->Fill(data->GetX(), data->GetZ());
			if(MpdTofPoint::GetVolumeUID(1,1,1) == UID) htXZpad->Fill(data->GetX(), data->GetZ());
		}

		if(fVerbose > 1)	data->Print();	
	}	
	delete iter;
	
return 	hitIndex;
}
//------------------------------------------------------------------------------------------------------------------------
void			MpdTofHitProducer::Dump(TClonesArray *array, const char* title, ostream& out)
{
	out<<"\n [MpdTofHitProducer::Dump]   "; if(title) out<<title;  out<<", size= "<<array->GetEntriesFast();

	TIterator *iter = array->MakeIterator(); MpdTofPoint *point; MpdTofHit *pHit; TVector3 hitPos, pointPos;		
      	while( (pHit = (MpdTofHit*) iter->Next()) )   						
	{
		pHit->Position(hitPos);
		out<<"\n    hit detUID = "<<pHit->GetDetectorID()<<", hit pos("<<hitPos.X()<<","<<hitPos.Y()<<","<<hitPos.Z()<<"), flag ="<<pHit->GetFlag();
		if(pTofPoints)
		{
			point = (MpdTofPoint*) pTofPoints->UncheckedAt(pHit->GetRefIndex());
			point->Position(pointPos);
			out<<"\n point detUID = "<<point->GetDetectorID()<<", point pos("<<pointPos.X()<<","<<pointPos.Y()<<","<<pointPos.Z()<<"), dev="<<(hitPos-pointPos).Mag();
		}
	}

	delete iter;
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofHit* MpdTofHitProducer::FindTmpHit(Int_t detUID)
{
  	MpdTofHit *pHit; TIterator *iter = pHitTmp->MakeIterator(); 			
      	while( (pHit = (MpdTofHit*) iter->Next()) )	if(detUID == pHit->GetDetectorID()){ delete iter; return pHit; }

	delete iter;
return NULL;	
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTofHitProducer::AddRawHit(Int_t index, Int_t detUID, const TVector3 &posHit, const TVector3 &posHitErr, Int_t pointIndex, 
					Int_t trackIndex, Double_t time, Int_t flag)
{
	MpdTofHit *pHit	= new  ((*pHitTmp)[index]) MpdTofHit(detUID, posHit, posHitErr, pointIndex, time, flag);
	pHit->AddLink(FairLink(MpdTofUtils::IsTofPointIndex, pointIndex));
	pHit->AddLink(FairLink(MpdTofUtils::IsMCTrackIndex, trackIndex));
	pHit->AddLink(FairLink(MpdTofUtils::IsVolumeUID, detUID));
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdTofHitProducer)
