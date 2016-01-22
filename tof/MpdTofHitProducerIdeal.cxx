//------------------------------------------------------------------------------------------------------------------------
#include <assert.h>
#include <iostream>

#include "TClonesArray.h"

#include "FairRootManager.h"

#include "MpdTofUtils.h"
#include "MpdTofHit.h"
#include "MpdTofPoint.h"

#include "MpdTofHitProducerIdeal.h"

ClassImp(MpdTofHitProducerIdeal)
//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducerIdeal::MpdTofHitProducerIdeal(const char *name, Int_t verbose, Bool_t test, Bool_t merge) 
 : FairTask(name, verbose), fDoTest(test), fDoMergeHits(merge), fTestFlnm("test.MpdTofHitProducerIdeal.root"), aTofPoints(nullptr), aMCTracks(nullptr), aTofHits(nullptr)
{ 
        if(fDoTest)
    	{
    		h1TestOccup = new TH1D("TestOccup", "occupancy per strips;occupancy;Events", 100, -0.5, 99.5); 									fList.Add(h1TestOccup); 
       		h2TestMergedTimes = new TH2D("TestMergedTimes", "Merged hits on strip times test;faster hit time, ns;slower hit time, ns", 1000, 5., 105., 1000, 5., 105.);	fList.Add(h2TestMergedTimes); 	
	
    	}
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducerIdeal::~MpdTofHitProducerIdeal() 
{ 

}
//------------------------------------------------------------------------------------------------------------------------
InitStatus	MpdTofHitProducerIdeal::Init() 
{
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdTofHitProducerIdeal::Init] Begin initialization.");

	FairRootManager *ioman = FairRootManager::Instance(); assert(ioman);
  	aTofPoints  = (TClonesArray *) ioman->GetObject("TOFPoint");
  	aMCTracks   = (TClonesArray *) ioman->GetObject("MCTrack"); 

  	if(!aTofPoints || !aMCTracks){ FairLogger::GetLogger()->Error(MESSAGE_ORIGIN, " Branch not found!"); return kERROR; }
	
        // Create and register output array
        aTofHits = new TClonesArray("MpdTofHit");
        ioman->Register("TOFHit", "Tof", aTofHits, kTRUE);

        FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdTofHitProducerIdeal::Init] Initialization finished succesfully.");

return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
void 		MpdTofHitProducerIdeal::Exec(Option_t* opt) 
{
	static const TVector3 XYZ_err(0., 0., 0.); // FIXME:

	aTofHits->Clear();

	Int_t 		UID, trackID, nSingleHits = 0, nTofPoint = aTofPoints->GetEntriesFast();	
	TVector3 	pos; 	
	Double_t 	time;
	MpdTofPoint 	*pPoint;
	
	for(Int_t pointIndex = 0; pointIndex < nTofPoint; pointIndex++ )  // cycle by TOF points
	{
		pPoint = (MpdTofPoint*) aTofPoints->UncheckedAt(pointIndex);
		UID	= pPoint->GetDetectorID();
		trackID = pPoint->GetTrackID();
		time = pPoint->GetTime();		
		pPoint->Position(pos);
		
		AddHit(UID, pos, XYZ_err, pointIndex, trackID, time, MpdTofUtils::IsSingle);	
		nSingleHits++;
	}
	
	int nFinally;
	if(fDoMergeHits)
	{
		MergeHitsOnStrip(); 		// save only the fastest hit in the strip
		nFinally = CompressHits(); 	// remove blank slotes
	} 
	else 
		nFinally = aTofHits->GetEntriesFast();
	
        cout<<" -I- [MpdTofHitProducerIdeal::Exec] MCpoints= "<<nTofPoint<<", single hits= "<<nSingleHits<<", final hits= "<<nFinally<<endl;
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdTofHitProducerIdeal::Finish()
{
  	if(fDoTest)
    	{
      		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " [MpdTofHitProducerIdeal::Finish] Update  %s file. ", fTestFlnm.Data());
		TFile *ptr = gFile;
		TFile file(fTestFlnm.Data(), "RECREATE");
		fList.Write(); 
		file.Close();
		gFile = ptr;
	}
}
//------------------------------------------------------------------------------------------------------------------------
Int_t 			MpdTofHitProducerIdeal::MergeHitsOnStrip(void)
{
typedef map<Int_t, MpdTofHit*> hitsMapType;
	hitsMapType 		fHits; // pair<detectorUID, MpdTofHit*> fastest hits map
	hitsMapType::iterator 	it;	
	Int_t mergedNmb = 0;   

typedef multiset<Int_t> msUIDsType; // detectorUID for Hits
	msUIDsType	UIDs;
	
	MpdTofHit *fastHit, *slowHit;
	for(Int_t hitIndex = 0, nHits = aTofHits->GetEntriesFast(); hitIndex < nHits; hitIndex++ ) // cycle by hits
	{	
		MpdTofHit *pHit = (MpdTofHit*) aTofHits->UncheckedAt(hitIndex); 		
assert(nullptr != pHit);
		
		Int_t UID = pHit->GetDetectorID();
		
		if(fDoTest) UIDs.insert(UID);
		
		it = fHits.find(UID);
		if(it != fHits.end()) // hit for this detectorUID already exist
		{
			mergedNmb++; 
			
			if(pHit->GetTime() < it->second->GetTime()) //  faster hit  found
			{
				fastHit = pHit;
				slowHit = it->second;
			}
			else
			{
				fastHit = it->second;
				slowHit = pHit;			
			}
					
			fastHit->AddLinks(slowHit->GetLinks());		// copy links
			aTofHits->Remove(slowHit); 			// remove old hit   --> make blank slote !!
			fastHit->AddFlag(MpdTofUtils::HaveTail);					
			it->second = fastHit;				// change pair value to current UID
				
			if(fDoTest) h2TestMergedTimes->Fill(fastHit->GetTime(), slowHit->GetTime());						 
		}
		else fHits.insert(make_pair(UID, pHit)); 				// insert new detectorUID pair
		
	} // cycle by hits

	// cycle by detector UIDs list
	if(fDoTest) for(msUIDsType::const_iterator it = UIDs.begin(), itEnd = UIDs.end(); it != itEnd;  it = UIDs.upper_bound(*it))	h1TestOccup->Fill(UIDs.count(*it));

return 	mergedNmb;
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdTofHitProducerIdeal::AddHit(Int_t detUID, const TVector3 &posHit, const TVector3 &posHitErr, Int_t pointIndex, Int_t trackIndex, Double_t time, Int_t flag)
{
	MpdTofHit *pHit	= new  ((*aTofHits)[aTofHits->GetEntriesFast()]) MpdTofHit(detUID, posHit, posHitErr, pointIndex, time, flag);
	
	pHit->AddLink(FairLink(MpdTofUtils::IsTofPointIndex, pointIndex));
	pHit->AddLink(FairLink(MpdTofUtils::IsMCTrackIndex, trackIndex));
	pHit->AddLink(FairLink(MpdTofUtils::IsVolumeUID, detUID));	
}
//------------------------------------------------------------------------------------------------------------------------
Int_t 			MpdTofHitProducerIdeal::CompressHits() 
{
	aTofHits->Compress();		
return 	aTofHits->GetEntriesFast();	
}
//------------------------------------------------------------------------------------------------------------------------
void			MpdTofHitProducerIdeal::Dump(const char* title, ostream& out) const
{
	out<<"\n [MpdTofHitProducer::Dump]   "; if(title) out<<title;  out<<", size= "<<aTofHits->GetEntriesFast();
	
	MpdTofPoint *point; MpdTofHit *pHit; TVector3 hitPos, pointPos;
	
	TIterator *iter = aTofHits->MakeIterator(); 		
      	while( (pHit = (MpdTofHit*) iter->Next()) )   						
	{
		pHit->Position(hitPos);
		out<<"\n    hit detUID = "<<pHit->GetDetectorID()<<", hit pos("<<hitPos.X()<<","<<hitPos.Y()<<","<<hitPos.Z()<<"), flag ="<<pHit->GetFlag();
		if(aTofPoints)
		{
			point = (MpdTofPoint*) aTofPoints->UncheckedAt(pHit->GetRefIndex());
			point->Position(pointPos);
			out<<"\n point detUID = "<<point->GetDetectorID()<<", point pos("<<pointPos.X()<<","<<pointPos.Y()<<","<<pointPos.Z()<<"), dev="<<(hitPos-pointPos).Mag();
		}
	}
	
	delete iter;
}
//------------------------------------------------------------------------------------------------------------------------

