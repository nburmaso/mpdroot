
//------------------------------------------------------------------------------------------------------------------------
/// \class MpdEtofHitProducerIdeal
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include <assert.h>
#include <iostream>

#include "TClonesArray.h"

#include "FairRootManager.h"

#include "MpdTofUtils.h"
#include "MpdEtofHit.h"
#include "MpdEtofPoint.h"

#include "MpdEtofHitProducerIdeal.h"

ClassImp(MpdEtofHitProducerIdeal)
//------------------------------------------------------------------------------------------------------------------------
MpdEtofHitProducerIdeal::MpdEtofHitProducerIdeal(const char *name, Int_t verbose, Bool_t test, Bool_t merge) 
 : FairTask(name, verbose), fDoTest(test), fDoMergeHits(merge), fTestFlnm("test.MpdEtofHitProducerIdeal.root"), aTofPoints(nullptr), aMCTracks(nullptr), aTofHits(nullptr)
{ 
        if(fDoTest)
    	{
     		h1TestOccup = new TH1D("eTestOccup", "occupancy per strips;occupancy;Events", 100, -0.5, 99.5); 							fList.Add(h1TestOccup);    	
    	}
}
//------------------------------------------------------------------------------------------------------------------------
MpdEtofHitProducerIdeal::~MpdEtofHitProducerIdeal() 
{ 

}
//------------------------------------------------------------------------------------------------------------------------
InitStatus	MpdEtofHitProducerIdeal::Init() 
{
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdEtofHitProducerIdeal::Init] Begin initialization.");

	FairRootManager *ioman = FairRootManager::Instance(); assert(ioman);
  	aTofPoints  = (TClonesArray *) ioman->GetObject("ETOFPoint");
  	aMCTracks   = (TClonesArray *) ioman->GetObject("MCTrack"); 

  	if(!aTofPoints || !aMCTracks){ FairLogger::GetLogger()->Error(MESSAGE_ORIGIN, " Branch not found!"); return kERROR; }
	
        // Create and register output array
        aTofHits = new TClonesArray("MpdEtofHit");
        ioman->Register("ETOFHit", "ETof", aTofHits, kTRUE);

        FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdEtofHitProducerIdeal::Init] Initialization finished succesfully.");

return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
void 		MpdEtofHitProducerIdeal::Exec(Option_t* opt) 
{
	static const TVector3 XYZ_err(0., 0., 0.); // FIXME:

	aTofHits->Clear();

	Int_t 		UID, trackID, nSingleHits = 0, nTofPoint = aTofPoints->GetEntriesFast();	
	TVector3 	pos; 	
	Double_t 	time;
	
	for(Int_t pointIndex = 0; pointIndex < nTofPoint; pointIndex++ )  // cycle by TOF points
	{
		MpdEtofPoint *pPoint = (MpdEtofPoint*) aTofPoints->UncheckedAt(pointIndex);
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
	
        cout<<" -I- [MpdEtofHitProducerIdeal::Exec] MCpoints= "<<nTofPoint<<", single hits= "<<nSingleHits<<", final hits= "<<nFinally<<endl;
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdEtofHitProducerIdeal::Finish()
{
  	if(fDoTest)
    	{
      		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " [MpdEtofHitProducerIdeal::Finish] Update  %s file. ", fTestFlnm.Data());
		TFile *ptr = gFile;
		TFile file(fTestFlnm.Data(), "RECREATE");
		fList.Write(); 
		file.Close();
		gFile = ptr;
	}
}
//------------------------------------------------------------------------------------------------------------------------
Int_t 			MpdEtofHitProducerIdeal::MergeHitsOnStrip(void)
{
typedef map<Int_t, MpdEtofHit*> hitsMapType;
	hitsMapType 		fastestHits; // pair<detectorUID, MpdTofHit*>
	hitsMapType::iterator 	it;	
	Int_t mergedNmb = 0;   

typedef multiset<Int_t> msUIDsType; // detectorUID for Hits
	msUIDsType	UIDs;
	
	for(Int_t hitIndex = 0, nHits = aTofHits->GetEntriesFast(); hitIndex < nHits; hitIndex++ ) // cycle by hits
	{	
		MpdEtofHit *pHit = (MpdEtofHit*) aTofHits->UncheckedAt(hitIndex); 		
assert(nullptr != pHit);
		
		Int_t UID = pHit->GetDetectorID();
		
		if(fDoTest) UIDs.insert(UID);
				
		it = fastestHits.find(UID);
		if(it != fastestHits.end()) // hit for this detectorUID already exist
		{
			mergedNmb++; 
			MpdEtofHit *pOldHit = it->second;
			
			if(pHit->GetTime() < pOldHit->GetTime()) //  hit faster found
			{	
				pHit->AddLinks(pOldHit->GetLinks());			// copy links
				aTofHits->Remove(pOldHit); 				// remove old hit   --> make blank slote !!
				pHit->SetFlag(pHit->GetFlag() | MpdTofUtils::HaveTail);	// Set "HaveTail" flag					
				it->second = pHit;					// change pair value to current UID
			}
			else  	aTofHits->Remove(pHit);					// remove current hit --> make blank slote !!
		}
		else fastestHits.insert(make_pair(UID, pHit)); 				// insert new detectorUID pair
		
	} // cycle by hits
	
	// cycle by detector UIDs list
	if(fDoTest) for(msUIDsType::const_iterator it = UIDs.begin(), itEnd = UIDs.end(); it != itEnd;  it = UIDs.upper_bound(*it))	h1TestOccup->Fill(UIDs.count(*it));
	
return 	mergedNmb;
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdEtofHitProducerIdeal::AddHit(Int_t detUID, const TVector3 &posHit, const TVector3 &posHitErr, Int_t pointIndex, Int_t trackIndex, Double_t time, Int_t flag)
{
	MpdEtofHit *pHit = new  ((*aTofHits)[aTofHits->GetEntriesFast()]) MpdEtofHit(detUID, posHit, posHitErr, pointIndex, time, flag);
	
	pHit->AddLink(FairLink(MpdTofUtils::IsTofPointIndex, pointIndex));
	pHit->AddLink(FairLink(MpdTofUtils::IsMCTrackIndex, trackIndex));
	pHit->AddLink(FairLink(MpdTofUtils::IsVolumeUID, detUID));	
}
//------------------------------------------------------------------------------------------------------------------------
Int_t 			MpdEtofHitProducerIdeal::CompressHits() 
{
	aTofHits->Compress();		
return 	aTofHits->GetEntriesFast();	
}
//------------------------------------------------------------------------------------------------------------------------
void			MpdEtofHitProducerIdeal::Dump(const char* title, ostream& out) const
{
	out<<"\n [MpdEtofHitProducerIdeal::Dump]   "; if(title) out<<title;  out<<", size= "<<aTofHits->GetEntriesFast();
	
	MpdEtofPoint *point; MpdEtofHit *pHit; TVector3 hitPos, pointPos;
	
	TIterator *iter = aTofHits->MakeIterator(); 		
      	while( (pHit = (MpdEtofHit*) iter->Next()) )   						
	{
		pHit->Position(hitPos);
		out<<"\n    hit detUID = "<<pHit->GetDetectorID()<<", hit pos("<<hitPos.X()<<","<<hitPos.Y()<<","<<hitPos.Z()<<"), flag ="<<pHit->GetFlag();
		if(aTofPoints)
		{
			point = (MpdEtofPoint*) aTofPoints->UncheckedAt(pHit->GetRefIndex());
			point->Position(pointPos);
			out<<"\n point detUID = "<<point->GetDetectorID()<<", point pos("<<pointPos.X()<<","<<pointPos.Y()<<","<<pointPos.Z()<<"), dev="<<(hitPos-pointPos).Mag();
		}
	}
	
	delete iter;
}
//------------------------------------------------------------------------------------------------------------------------

