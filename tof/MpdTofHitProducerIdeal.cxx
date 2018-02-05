//------------------------------------------------------------------------------------------------------------------------
#include <assert.h>
#include <iostream>

#include <TClonesArray.h>

#include "FairRootManager.h"
#include "FairLogger.h"

#include "MpdTofUtils.h"
#include "MpdTofHit.h"
#include "MpdTofPoint.h"
#include "MpdTofHitProducerQA.h"

#include "MpdTofHitProducerIdeal.h"

ClassImp(MpdTofHitProducerIdeal)
//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducerIdeal::MpdTofHitProducerIdeal(const char *name, Bool_t useMCdata, Int_t verbose, Bool_t test, Bool_t merge, const char *flnm, bool IsEndcap) 
 : FairTask(name, verbose), fDoTest(test), fDoMergeHits(merge),  fUseMCData(useMCdata), fOnlyPrimary(false),
   aMcPoints(nullptr), aMcTracks(nullptr), aExpDigits(nullptr), aTofHits(nullptr)
{ 
        pHitProducerQA = fDoTest ? new MpdTofHitProducerQA(flnm, IsEndcap) : nullptr;

}
//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducerIdeal::~MpdTofHitProducerIdeal() 
{ 
	delete pHitProducerQA;
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdTofHitProducerIdeal::AddParameters(TString& buf)const
{ 
	buf += "\n Run parameters:"; 
	buf += "  fDoTest="; 		buf += fDoTest; 
	buf += ", fDoMergeHits="; 	buf += fDoMergeHits;
	buf += ", fUseMCData="; 	buf += fUseMCData;
	buf += ", fOnlyPrimary="; 	buf += fOnlyPrimary;
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus	MpdTofHitProducerIdeal::Initialize()
{
	TString className(ClassName());
	TString functionName("[");functionName += className; functionName +="::Initialize]";

	TString buf(functionName);  buf += " Begin initialization."; AddParameters(buf);
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, buf.Data());

    	if(fOnlyPrimary)
	{
		buf = functionName;  buf += " Only primary particles are processed!!!.";
		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, buf.Data());	
	}

	if(fUseMCData)
	{
    		aMcPoints = (TClonesArray*) FairRootManager::Instance()->GetObject("TOFPoint");
    		aMcTracks = (TClonesArray*) FairRootManager::Instance()->GetObject("MCTrack");
assert(aMcPoints);
assert(aMcTracks);
	}
	else
	{
    		aExpDigits = (TClonesArray*) FairRootManager::Instance()->GetObject("??????");// FIXME: NOW unknown name
assert(aExpDigits);	
	}
	
        // Create and register output array
        aTofHits = new TClonesArray("MpdTofHit");
        FairRootManager::Instance()->Register("TOFHit", "Tof", aTofHits, kTRUE);

return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus	MpdTofHitProducerIdeal::Init() 
{
	InitStatus status = Initialize();

        FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdTofHitProducerIdeal::Init] Initialization finished succesfully.");

return status;
}
//------------------------------------------------------------------------------------------------------------------------
void 		MpdTofHitProducerIdeal::Exec(Option_t* opt) 
{
	static const TVector3 XYZ_err(0., 0., 0.); // FIXME:

	aTofHits->Clear();

	Int_t 		nSingleHits = 0;	
	TVector3 	pos; 	
	
	if(fUseMCData)
	{
		for(Int_t pointIndex = 0, nTofPoint = aMcPoints->GetEntriesFast(); pointIndex < nTofPoint; pointIndex++ )  // cycle by TOF points
		{
			MpdTofPoint *pPoint = (MpdTofPoint*) aMcPoints->UncheckedAt(pointIndex);		
			pPoint->Position(pos);
		
			AddHit(pPoint->GetDetectorID(), pos, XYZ_err, pointIndex, pPoint->GetTrackID(), pPoint->GetTime(), MpdTofUtils::IsSingle);	
			nSingleHits++;
		}
	}
	else //  exp. data used    
	{
		// FIXME: now not realized
		//AddHit(Int_t detUID, const TVector3 &posHit, const TVector3 &posHitErr, Int_t expDigitIndex, Double_t time, Int_t flag)
		assert(false);		
	}
	
	int nFinally;
	if(fDoMergeHits)
	{
		MergeHitsOnStrip(); 		// save only the fastest hit in the strip
		nFinally = CompressHits(); 	// remove blank slotes
	} 
	else 
		nFinally = aTofHits->GetEntriesFast();
	
        cout<<" -I- [MpdTofHitProducerIdeal::Exec] single hits= "<<nSingleHits<<", final hits= "<<nFinally<<endl;
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdTofHitProducerIdeal::Finish()
{
	if(pHitProducerQA) pHitProducerQA->Finish(); 
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
				
			if(pHitProducerQA) pHitProducerQA->GetMergedTimesHisto()->Fill(fastHit->GetTime(), slowHit->GetTime());						 
		}
		else fHits.insert(make_pair(UID, pHit)); 				// insert new detectorUID pair
		
	} // cycle by hits
	
	if(pHitProducerQA)
	{ 
		for(msUIDsType::const_iterator iter = UIDs.begin(), itEnd = UIDs.end(); iter != itEnd;  iter = UIDs.upper_bound(*iter))	// cycle by detector UIDs list
			 pHitProducerQA->GetOccupancyHisto()->Fill(UIDs.count(*iter));
	}
	
return 	mergedNmb;
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdTofHitProducerIdeal::AddHit(Int_t detUID, const TVector3 &posHit, const TVector3 &posHitErr, Int_t expDigitIndex, Double_t time, Int_t flag)
{
	MpdTofHit *pHit	= new  ((*aTofHits)[aTofHits->GetEntriesFast()]) MpdTofHit(detUID, posHit, posHitErr, expDigitIndex, time, flag);
	
	pHit->AddLink(FairLink(MpdTofUtils::expDigitIndex, expDigitIndex));
	pHit->AddLink(FairLink(MpdTofUtils::volumeUID, detUID));	
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdTofHitProducerIdeal::AddHit(Int_t detUID, const TVector3 &posHit, const TVector3 &posHitErr, Int_t mcPointIndex, Int_t mcTrackIndex, Double_t time, Int_t flag)
{
	MpdTofHit *pHit	= new  ((*aTofHits)[aTofHits->GetEntriesFast()]) MpdTofHit(detUID, posHit, posHitErr, mcPointIndex, time, flag);
	
	pHit->AddLink(FairLink(MpdTofUtils::mcPointIndex, mcPointIndex));
	pHit->AddLink(FairLink(MpdTofUtils::mcTrackIndex, mcTrackIndex));
	pHit->AddLink(FairLink(MpdTofUtils::volumeUID, detUID));	
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
		
		if(aMcPoints)
		{
			point = (MpdTofPoint*) aMcPoints->UncheckedAt(pHit->GetRefIndex());
			point->Position(pointPos);
			out<<"\n point detUID = "<<point->GetDetectorID()<<", point pos("<<pointPos.X()<<","<<pointPos.Y()<<","<<pointPos.Z()<<"), dev="<<(hitPos-pointPos).Mag();
		}
	}
	
	delete iter;
}
//------------------------------------------------------------------------------------------------------------------------



