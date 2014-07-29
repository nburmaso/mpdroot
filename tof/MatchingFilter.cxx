#include<assert.h> 

#include "MatchingFilter.h"

using namespace std;
//------------------------------------------------------------------------------------------------------------------------
void	MatchingFilter::RecreateCounterMaps(Int_t nKFTracks) 
{
	mmapCounters.clear();
	mapCounterLinks.clear();

	// Fill mmapCounters & mapCounterLinks maps
	for(Int_t KfIndex = 0; KfIndex < nKFTracks; KfIndex++)
	{
		counterIter iter =  mmapCounters.insert(MMCandCounters::value_type(mmapCand.count(KfIndex), KfIndex)); // pair<number of candidate hits for current KFtrack, KfIndex>
		mapCounterLinks.insert(MBackCounters::value_type(KfIndex, iter));
	}
}
//------------------------------------------------------------------------------------------------------------------------
void	MatchingFilter::RemoveCounterMaps(const candIter& itCand) // remove candidate
{
assert(itCand != mmapCand.end()); // debug

	Int_t hitIndex = itCand->second->hitIndex;
	Int_t KfIndex = itCand->first;	

	counterLinkIter itLink = mapCounterLinks.find(KfIndex);
assert(itLink != mapCounterLinks.end()); // debug

	counterIter iter  = itLink->second;
assert(iter != mmapCounters.end()); // debug

	Int_t counter = iter->first; // number hits for track
assert(counter != 0); // debug

	mmapCounters.erase(iter);
	iter = mmapCounters.insert(MMCandCounters::value_type(--counter, KfIndex));
	mapCounterLinks[KfIndex] = iter;
}
//------------------------------------------------------------------------------------------------------------------------
void	MatchingFilter::RemoveCounterMaps(const candIter& itC1, const candIter& itC2) // remove candidates
{
	for(candIter itC = itC1; itC != itC2; ++itC) RemoveCounterMaps(itC);	
}
//------------------------------------------------------------------------------------------------------------------------
void	MatchingFilter::AcceptCandidate(candIter itCand, bool updateCounters) // invalidate candIter
{
	// Accept candidate
	mmapAccepted.insert(MMcandidates::value_type(itCand->first, itCand->second));

	Int_t KfIndex = itCand->first;
	Int_t hitIndex = itCand->second->hitIndex;

	// erase all candidates hits for this track
	candIter itC1 = mmapCand.find(KfIndex); 
	candIter itC2 = mmapCand.upper_bound(KfIndex);	
assert(itC1 != mmapCand.end()); // debug

	if(updateCounters) RemoveCounterMaps(itC1, itC2);
	eraseCand(itC1, itC2);

	// looking this hit for all tracks	
	linkIter itL1 = mmapLinks.find(hitIndex);
	if(itL1 == mmapLinks.end()) return; 	// mmapLinks entries already removed (only one track linked to this hit)
	linkIter itL2 = mmapLinks.upper_bound(hitIndex);	

	// erase this candidate hit for another tracks	
	for(linkIter itL = itL1; itL != itL2; ++itL) // cycle by tracks
	{
		itC1  = itL->second.iterC;
		if(itC1 != itCand)			// already removed
		{
			if(updateCounters) RemoveCounterMaps(itC1);
			eraseCand(itC1, false); // MUST false, or invalidate linkIter
		}
	}

	// erase back links for eraseCand(candIter, false)
	mmapLinks.erase(itL1, itL2);
}
//------------------------------------------------------------------------------------------------------------------------
Int_t		MatchingFilter::ProcessSimpleTracks(void)
{
	Int_t KfIndex, hitIndex, counter, MatchingOK = 0;
	counterIter it1 = mmapCounters.find(1);
	if(it1 == mmapCounters.end()) return 0;	// no candidates

	counterIter it2 = mmapCounters.upper_bound(1);

	for(counterIter iter = it1; iter != it2; ++iter)	// cycle by one hit tracks
	{
		KfIndex = iter->second;	
		candIter itCand = mmapCand.find(KfIndex);
assert(itCand != mmapCand.end()); // debug 

		hitIndex = itCand->second->hitIndex;
		counter  = mmapLinks.count(hitIndex);		// number of concurrent track for this hit

		if(counter == 1){ AcceptCandidate(itCand, false);  MatchingOK++;} // don't have  concurrent tracks,   AcceptCandidate(..,false) -- valid iter 
	}
return MatchingOK;
}
//------------------------------------------------------------------------------------------------------------------------
void	MatchingFilter::Dump(void) 
{
	cout<<"\n ---------------------- DUMP --------------------------------------   entries: "<<mmapCand.size();
	cout<<"\n 	Matching candidates: ";
	Int_t KfIndex, hitIndex; bool exist; int n = 0;
	for(candIter it = mmapCand.begin(); it != mmapCand.end(); )
	{
		KfIndex = it->first;		
		cout<<"\n "<<++n<<" track = "<<KfIndex<<" --- ";
		int counter = mmapCand.count(KfIndex);
		
		for(int hit=0; hit<counter;hit++)
		{
			hitIndex = it->second->hitIndex;
			cout<<" hit = "<< hitIndex;
			
			linkIter iter = mmapLinks.find(hitIndex);
			if(iter != mmapLinks.end())
			{
				int counter2 = mmapLinks.count(hitIndex);			
				if(counter2 > 1)cout<<" (";			
				for(int track=0; track<counter2;track++)
				{
					if(counter2 > 1)cout<<" "<<iter->second.KfIndex<<" ";
					++iter;
				}
				if(counter2 > 1)cout<<") ";		
			}			
			++it;
		}
	}
	cout<<"\n ---------------------- DUMP --------------------------------------"<<flush;	
}
//------------------------------------------------------------------------------------------------------------------------
void	MatchingFilter::Commit(void) 
{
	mDBCand = mmapCand; 
	mDBAccepted = mmapAccepted;
	mDBLinks = mmapLinks;
}
//------------------------------------------------------------------------------------------------------------------------
void	MatchingFilter::Status(void) 
{	
	cout<<"\n ------------------------------------------------------------";
	//  mmapCand
	cout<<"\n mmapCand added:";
	PrintChanges(mmapCand, mDBCand);

	cout<<"\n mmapCand removed:";
	PrintChanges(mDBCand, mmapCand);

	//  mmapAccepted
	cout<<"\n mmapAccepted added:";
	PrintChanges(mmapAccepted, mDBAccepted);
	
	cout<<"\n mmapAccepted removed:";
	PrintChanges(mDBAccepted, mmapAccepted);

	//  mDBLinks
	cout<<"\n mmapLinks added:";
	PrintChanges(mmapLinks, mDBLinks);
	
	cout<<"\n mmapLinks removed:";
	PrintChanges(mDBLinks, mmapLinks);
	cout<<"\n ------------------------------------------------------------"<<flush;	
}
//------------------------------------------------------------------------------------------------------------------------
void	MatchingFilter::Reset()
{
	// GC: delete MatchingCandidate(instances in heap)
	Delete();

	// clear map entries(pointers)
	mmapCand.clear(); 
	mmapAccepted.clear();
	mmapLinks.clear();
}
//------------------------------------------------------------------------------------------------------------------------	
void	MatchingFilter::PrintChanges(MMcandidates& map1, MMcandidates& map2) const
{
	Int_t KfIndex, hitIndex; bool exist;
	for(candIter it = map1.begin(); it != map1.end(); ++it)
	{
		KfIndex = it->first;
		hitIndex = it->second->hitIndex;
		
		candIter it2 = map2.find(KfIndex);
		if(it2 == map2.end()) cout<<"\n  KfIndex = "<<KfIndex<<", hitIndex = "<<hitIndex;
		else
		{
			exist = false;
			candIter it3 = map2.upper_bound(KfIndex);
			for(candIter iter = it2; iter != it3; ++iter)
			{
				if(hitIndex == iter->second->hitIndex){ exist = true; break; }
			}
			if(!exist) cout<<"\n    KfIndex = "<<KfIndex<<", hitIndex = "<<hitIndex;			
		}	
	}
}	
//------------------------------------------------------------------------------------------------------------------------
void	MatchingFilter::PrintChanges(MMbackLinks& map1, MMbackLinks& map2) const
{
	Int_t KfIndex, hitIndex; bool exist;
	for(linkIter it = map1.begin(); it != map1.end(); ++it)
	{
		KfIndex = it->second.KfIndex;
		hitIndex = it->first;
		
		linkIter it2 = map2.find(hitIndex);
		if(it2 == map2.end()) cout<<"\n  KfIndex = "<<KfIndex<<", hitIndex = "<<hitIndex;
		else
		{
			exist = false;
			linkIter it3 = map2.upper_bound(hitIndex);
			for(linkIter iter = it2; iter != it3; ++iter)
			{
				if(KfIndex == iter->second.KfIndex){ exist = true; break; }
			}
			if(!exist) cout<<"\n    KfIndex = "<<KfIndex<<", hitIndex = "<<hitIndex;			
		}	
	}
}	
//------------------------------------------------------------------------------------------------------------------------

