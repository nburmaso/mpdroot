//----------------------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_LMATCHING_FILTER_H
#define __MPD_LMATCHING_FILTER_H 1

#include<assert.h>
#include<map>
//#include<debug/map>
#include<set>
#include<iostream>

#include<TClonesArray.h>
#include<TH2D.h>

#include "FairLogger.h"

//#define DoTest
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
class LMatchingFilter
{
// matching candidates 
//typedef std::__debug::multimap<Int_t, MdataT*>			MMcandType;
typedef std::multimap<Int_t, MdataT*>			MMcandType;	// pair< KfIndex, MdataT*>
typedef typename MMcandType::iterator			candIter;
typedef typename MMcandType::const_iterator		candCIter;

// back links to matching candidates 
typedef std::multimap<Int_t, candIter>			MMbackLinksType; // pair< hitIndex, candIter>
typedef typename MMbackLinksType::iterator		linkIter;
typedef typename MMbackLinksType::const_iterator	linkCIter;

typedef std::multimap<Int_t, Int_t>			MMCandRankType;	// pair <number of candidate hits for current KFtrack, KfIndex>
typedef typename MMCandRankType::iterator 		rankIter;
typedef typename MMCandRankType::const_iterator		rankCIter;

	TClonesArray 		*aMdata;		//  Matching data container;
	int			fVerbose; 
	bool			isOwner;
	double 			fChi2;
	
	MMcandType		mmCandidate, mmCandidateSnapshot, mmAccepted, mmAcceptedSnapshot;
	MMbackLinksType		mmLinks, mmLinksSnapshot;		
	MMCandRankType 		mmRanks;

	// for debug
	void		Commit(void); 
	void		Status(const char* comment = nullptr, std::ostream& os = std::cout) const; 	
	void		PrintChanges(const MMbackLinksType& src, const MMbackLinksType& checked, std::ostream& os = std::cout) const;
	void		PrintChanges(const MMcandType& src, const MMcandType& checked, std::ostream& os = std::cout) const; 		// cycle by src entries, print missed into the checked container entries.
	bool		CheckAdequacy(const char* comment = nullptr); 									// check mmCandidate & mmLinks entries accordance; return true if pass test	
			
	Int_t		ProcessSingleTracks(void);
	void		ProcessNRankTrack(Int_t rank);
	void		UpdateRanks();
	void		RemoveLink(Int_t hitIndex, Int_t KfIndex);	 								// remove link to pair<KfIndex, hitIndex>
	void		RemoveCandidate(Int_t hitIndex, Int_t KfIndex);	
	candIter	FindCandidate(MMcandType *map, Int_t hitIndex, Int_t KfIndex); 							// find candidate iter to map pair<KfIndex, hitIndex>	
	void		AcceptCandidate(candIter itCand);										// move candidate to accepted, clean links and candidates for corresponding pair<KfIndex, hitIndex>
	candIter	FindClosest1hRankCandidate(const std::pair<linkIter, linkIter>& range, Double_t& minDelta);
	double		FindMinDeltaForTrack(Int_t KfIndex, Int_t disabledHitIndex, candIter& minCand, bool& IsWithoutCompetition);	// Search through all the  KfIndex candidates except disabledHitIndex

	inline bool	IsSameRank(Int_t rank, Int_t KfIndex) 										// return true, if KfIndex cantidate  have the "rank" rank
	{ 
		std::pair<rankCIter, rankCIter> range = mmRanks.equal_range(rank); 	
		for(rankCIter iter = range.first; iter != range.second; ++iter)	
			if(iter->second == KfIndex) return true;	
	return false; 
	}

	inline Int_t 	GetMinRank()const { rankCIter iter = mmRanks.begin(); assert(iter !=  mmRanks.end()); return iter->first; }
	inline Int_t 	GetMinRankAfter(Int_t rank)const{ rankCIter iter = mmRanks.upper_bound(rank); if(iter !=  mmRanks.end()) return iter->first; else return -1; }		
public:
	LMatchingFilter(int verbose = 0);
	~LMatchingFilter();
		
	void		SetContainer(TClonesArray *array);	
	void		AddCandidate(const  MdataT& data);

	void		Reset();
	Int_t		Processing(Int_t nKFTracks, TH2D* h2, double& chi2);
	Int_t		UpdateContainer();
	
	void		Dump(const char* comment = nullptr, std::ostream& os = std::cout) const;
	void		PrintRanks(const char* comment = nullptr, std::ostream& os = std::cout) const;	
};
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
LMatchingFilter<HitT,MdataT>::LMatchingFilter(int verbose)
: fVerbose(verbose), isOwner(true)
{
	aMdata = new TClonesArray(MdataT::Class(), 1000);
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
LMatchingFilter<HitT,MdataT>::~LMatchingFilter()
{
	if(isOwner) delete aMdata;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::AddCandidate(const  MdataT& data)
{
	MdataT *pData =	new ((*aMdata) [aMdata->GetEntriesFast()]) MdataT(data);  // save data to the TClonesArray container

	candIter it = mmCandidate.insert(std::make_pair(pData->GetKFTrackIndex(), pData));	// insert matching candidate into the mmCandidate
	
	mmLinks.insert(std::make_pair(pData->GetTofHitIndex(), it)); 		// insert back link to matching candidate
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
Int_t	LMatchingFilter<HitT, MdataT>::UpdateContainer()
{
	// select accepted candidates
	std::set<MdataT*> sAccepted;
	for(candCIter citer = mmAccepted.begin(), citerEnd = mmAccepted.end(); citer != citerEnd; citer++) sAccepted.insert(citer->second);
		
	for(int entry = 0, Nentries = aMdata->GetEntriesFast(); entry < Nentries; entry++) // cycle by MdataT at TClonesArray container
	{
		MdataT *pData = (MdataT*) aMdata->At(entry);
		
		if(sAccepted.find(pData) == sAccepted.end()) // no exist -> not accepted
			aMdata->RemoveAt(entry);
	}
	
	aMdata->Compress(); 
	
return 	aMdata->GetEntriesFast();
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
typename LMatchingFilter<HitT, MdataT>::candIter	LMatchingFilter<HitT, MdataT>::FindCandidate(MMcandType *map, Int_t hitIndex, Int_t KfIndex)
{
	std::pair<candIter, candIter> rangeCands = map->equal_range(KfIndex); 	
	for(candIter iter = rangeCands.first; iter != rangeCands.second; ++iter)	// cycle by hits for this KfIndex
	{
		if(hitIndex == iter->second->GetTofHitIndex()) return iter;
	}
	
assert(false);	
return map->end();	
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
Int_t	LMatchingFilter<HitT, MdataT>::Processing(Int_t nKFTracks, TH2D* h2, double& chi2)
{
	fChi2 = 0.;
	Int_t candNmb = mmCandidate.size();
	
#ifdef DoTest
std::cout<<"\n -I- LMatchingFilter::Processing --------------------------------------------------------- mmCandidate.size()="<<mmCandidate.size()<<std::flush; int nn;
#endif
	Int_t nAccepted, iterNmb = 0;
			
	if(mmCandidate.empty()) goto finish;
	
	UpdateRanks();
	
#ifdef DoTest	
PrintRanks(" FIRST ITER ");
Commit();
Dump();
#endif	
	ProcessSingleTracks(); // process only single hit (1th rank) candidate tracks
	iterNmb++;

#ifdef DoTest	
Status("AFTER FIRST ProcessSingleTracks");
#endif		
	if(mmCandidate.empty()) goto finish;

newIteration:

	iterNmb++;
	
	UpdateRanks();
	
#ifdef DoTest	
PrintRanks(" new ITER ");
nn = GetMinRankAfter(10); if (nn>0) std::cout<<"\n MInRank	= "<<nn<<" count()= "<<mmRanks.count(nn);
if( mmCandidate.size() <=20) Dump();
#endif

	nAccepted = ProcessSingleTracks(); 

	if(nAccepted == 0)
	{	
		ProcessNRankTrack(GetMinRank());			 	
	}

	if(mmCandidate.empty()) goto finish;

	if(iterNmb > 1000)
	{ 
		FairLogger::GetLogger()->Warning(MESSAGE_ORIGIN, " <TofMatchingFilter::Processing> Too many tries.");
		goto finish; 
	}
	
	goto newIteration;
		
finish:
	chi2 = fChi2;
	
	if(h2 != nullptr) h2->Fill(candNmb, iterNmb);
	
#ifdef DoTest	
std::cout<<"\n -I-  [TofMatchingFilter::Processing] FINAL SIZE: mmLinks="<<mmLinks.size()<<" mmCandidate="<<mmCandidate.size()<<" mmRanks="<<mmRanks.size()<<"\n";	
#endif

	if(fVerbose) std::cout<<" -I- [TofMatchingFilter::Processing] Finished with "<<iterNmb<<" iterations.\n";

return 	mmAccepted.size();
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
int	LMatchingFilter<HitT, MdataT>::ProcessSingleTracks()  //  only single hit (1th rank) candidate tracks processing
{
	std::pair<rankIter, rankIter> range = mmRanks.equal_range(1); 
	if(std::distance(range.first, range.second) == 0) return 0; // no 1th rank candidates
	
#ifdef DoTest	
std::cout<<"\n LMatchingFilter::ProcessSingleTracks_size="<<std::distance(range.first, range.second);
#endif

	Int_t KfIndex, hitIndex, counter, MatchingOK = 0;
	for(rankIter iter = range.first; iter != range.second; ++iter)	// cycle by ONLY 1th rank candidates
	{
		KfIndex = iter->second;	
		candIter itCand = mmCandidate.find(KfIndex);	
#ifdef DoTest			
std::cout<<"\nProcessSingleTracks_KfIndex="<<KfIndex;
#endif	

		if(itCand == mmCandidate.end()) continue; // already removed by competitor track candidate
#ifdef DoTest				
std::cout<<" EXIST";
#endif
		hitIndex = itCand->second->GetTofHitIndex();
		counter  = mmLinks.count(hitIndex);		// number of competitor tracks for this hit
		
		if(counter == 1)// don't have  competitor tracks
		{ 
#ifdef DoTest		
std::cout<<"\t\t\t SINGLE "<<MatchingOK;
#endif		
			AcceptCandidate(itCand);  
			MatchingOK++;			
		} 
		else	// accept best(closest) 1th rank candidate
		{
			Double_t delta;
#ifdef DoTest			
std::cout<<"\n	 counter = "<<	mmLinks.count(hitIndex);			
#endif				
			candIter it = FindClosest1hRankCandidate(mmLinks.equal_range(hitIndex), delta);	
#ifdef DoTest	
std::cout<<"\t\t\t MULTU "<<MatchingOK;	
#endif						
			AcceptCandidate(it);  
			MatchingOK++;		
		}	
	}

#ifdef DoTest
std::cout<<"\n LMatchingFilter::ProcessSingleTracks 1th rank cands="<<std::distance(range.first, range.second)<<" MatchingOK="<<MatchingOK;
#endif
	
return 	MatchingOK;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
double			LMatchingFilter<HitT, MdataT>::FindMinDeltaForTrack(Int_t KfIndex, Int_t disabledHitIndex, candIter& minCand, bool& IsWithoutCompetition)
{
	Double_t  delta, minDelta = 1.e+10; // big value
	std::pair<candIter, candIter> rangeCands = mmCandidate.equal_range(KfIndex); 	
	minCand = mmCandidate.end();
	IsWithoutCompetition = true;
	
assert(	std::distance(rangeCands.first, rangeCands.second) != 0);

	for(candIter iter = rangeCands.first; iter != rangeCands.second; ++iter)	// cycle by candidates for this KfIndex
	{	
		int hitindex = iter->second->GetTofHitIndex();
		int nmbTrHits =  iter->second->GetNmbTrHits();
	
		if(disabledHitIndex != hitindex) // skip <KfIndex, disabledHitIndex> candidate 
		{
			delta =  iter->second->GetDelta();	

			if(delta < minDelta) // new local minimum
			{
				if(1 == mmLinks.count(hitindex))	IsWithoutCompetition = true;
				else					IsWithoutCompetition = false;
				
				minCand = iter;
			 	minDelta = delta;		 
			}		
		}
	}

assert(minCand != mmCandidate.end()); // don't have local minimum ????
return 	minDelta;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::ProcessNRankTrack(Int_t rank)  // process  Nth rank  ONE track
{
	std::pair<rankIter, rankIter> rangeRanks = mmRanks.equal_range(rank); 
	if(std::distance(rangeRanks.first, rangeRanks.second) == 0) return; // no Nth rank candidates

	// select random(first) KfIndex from Nrank list
	Int_t KfIndex = rangeRanks.first->second;	// track index for processing
		
#ifdef DoTest	
std::cout<<"\n  ---------------ProcessNRankTrack---- rank="<<rank<<"  mmRanks.count(rank)="<<std::distance(rangeRanks.first, rangeRanks.second)<<" KfIndex="<<KfIndex<<"  mmCandidate.count(KfIndex)="<<mmCandidate.count(KfIndex);	
#endif	
	bool IsWithoutCompetition = true;
	candIter cand, bestCand;	
	double delta,  minDelta;
	
	minDelta = FindMinDeltaForTrack(KfIndex, -100, bestCand, IsWithoutCompetition);	
	int maxKfTrackWeight = bestCand->second->GetNmbTrHits();
	
	if(IsWithoutCompetition)
	{	
assert(cand != mmCandidate.end());
		AcceptCandidate(bestCand);	
	}
	else
	{
		Int_t hitIndex = bestCand->second->GetTofHitIndex();
		std::pair<linkIter, linkIter> rangeLinks  = mmLinks.equal_range(hitIndex);
		for(linkIter it = rangeLinks.first; it != rangeLinks.second; ++it)	// cycle by tracks for this hitIndex
		{	
			if(it->second == bestCand) continue; // skip himself 
			
			Int_t kfindex = it->second->first;		
			if(IsSameRank(rank, kfindex)) // check only same rank candidates
			{			
				delta = FindMinDeltaForTrack(kfindex, -100, cand, IsWithoutCompetition);
				int kftrackweight = cand->second->GetNmbTrHits();
			
				if(kftrackweight > maxKfTrackWeight) // new track have bigger weight -> set as best candidate
				{
					bestCand = cand;
					minDelta = delta;
				}
				else if(kftrackweight == maxKfTrackWeight)
				{
					if(delta < minDelta)	// new track have same weight but smaller delta -> set as best candidate
					{
						bestCand = cand;
						minDelta = delta;
					}
				}
			}			
		} // cycle by tracks for this hitIndex
		
assert(cand != mmCandidate.end());
		AcceptCandidate(bestCand);					
	}	
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
typename LMatchingFilter<HitT, MdataT>::candIter	LMatchingFilter<HitT, MdataT>::FindClosest1hRankCandidate(const std::pair< linkIter,  linkIter>& range, Double_t& minDelta)
{
	candIter retvalue = mmCandidate.end();
	Double_t  delta; minDelta = 1.e+10; // big value

	for(linkIter it = range.first; it != range.second; it++) // cycle by competitor candidates
	{
		delta =  it->second->second->GetDelta();
		
		if(delta < minDelta) // best (closest) candidate
		{ 
			retvalue = it->second; 
			minDelta = delta; 
		} 		
	}

assert(retvalue != mmCandidate.end()); 		
return retvalue;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::AcceptCandidate(candIter itCand)
{
#ifdef DoTest
Commit();
#endif
	// Accept candidate
	Int_t acceptedKfIndex = itCand->first, acceptedHitIndex = itCand->second->GetTofHitIndex();	
	
	mmAccepted.insert(make_pair(acceptedKfIndex, itCand->second)); // pair< KfIndex, MdataT*> 
	
	double delta = itCand->second->GetDelta();
	fChi2 += delta*delta;
	

	std::pair<candIter, candIter> rangeCand = mmCandidate.equal_range(acceptedKfIndex); 
	for(candIter iter = rangeCand.first, iterEnd = rangeCand.second; iter != iterEnd; iter++) // all candidates for acceptedKfIndex
	{
		int hitindex = iter->second->GetTofHitIndex();
	
		// remove all links to hits for this KfIndex	(track erase) 	
		RemoveLink(hitindex, acceptedKfIndex);		
	}
	
	std::pair<linkIter, linkIter> rangeLinks = mmLinks.equal_range(acceptedHitIndex);	
	for(linkIter iter = rangeLinks.first, iterEnd = rangeLinks.second; iter != iterEnd; iter++) // all candidates for acceptedHitIndex
	{
		int kfindex = iter->second->first;
		
		// remove all candidate  tracks for this hitIndex	(hit erase) 			
		RemoveCandidate(acceptedHitIndex, kfindex);		
	}
			
	// remove all candidates for  this KfIndex
	rangeCand = mmCandidate.equal_range(acceptedKfIndex);
	mmCandidate.erase(rangeCand.first, rangeCand.second);	
	
	// remove all links for this hitIndex hit	(hit erase)
	rangeLinks = mmLinks.equal_range(acceptedHitIndex);
	mmLinks.erase(rangeLinks.first, rangeLinks.second);

#ifdef DoTest		
bool pass =  CheckAdequacy("AFTER ACCEPT");
if(!pass)
{
	Status(" change AFTER ACCEPT"); std::cout<<std::flush;
}
assert(pass);
#else
	assert(CheckAdequacy("AFTER ACCEPT"));
#endif	
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
bool	LMatchingFilter<HitT, MdataT>::CheckAdequacy(const char* comment)
{
	MMbackLinksType		mmLinksTmp;
	for(candIter iter = mmCandidate.begin(), iterEnd = mmCandidate.end(); iter != iterEnd; iter++)	 mmLinksTmp.insert(make_pair(iter->second->GetTofHitIndex(), iter )); 	 	 
	
	bool equalFront = true;// (mmLinksTmp == mmLinks); // iterators point to another mmaps, so operator == return always true
	if(mmLinksTmp.size() == mmLinks.size())
	{
		for(linkCIter iter = mmLinksTmp.begin(), iterEnd = mmLinksTmp.end(); iter != iterEnd; iter++)	
		{
			bool founded = false;
			std::pair<linkCIter, linkCIter> rangeLinks  = mmLinks.equal_range(iter->first);			
			for(linkCIter it = rangeLinks.first; it != rangeLinks.second; ++it)	// cycle by tracks for this hitIndex
			{
				if(it->second->second == iter->second->second) // MdataT*(mmLinksTmp) == MdataT* (mmLinks)
				{
					founded = true; break; 
				}		
			}
		
			if(false == founded){ equalFront = false; break; } 
		}	
	} 
	else equalFront = false;
		
	MMcandType		mmCandidateTmp; 
	for(linkIter iter = mmLinks.begin(), iterEnd = mmLinks.end(); iter != iterEnd; iter++)	 mmCandidateTmp.insert(make_pair(iter->second->first, iter->second->second));

	bool equalBack = true; // (mmCandidateTmp == mmCandidate); // because of a different order filling the entries may be mismatch, so operator == may be return wrong false

	if(mmCandidateTmp.size() == mmCandidate.size())
	{	
		for(candIter iter = mmCandidateTmp.begin(), iterEnd = mmCandidateTmp.end(); iter != iterEnd; iter++)	
		{
			bool founded = false;
			std::pair<candCIter, candCIter> range  = mmCandidate.equal_range(iter->first);
						
			for(candCIter it = range.first; it != range.second; ++it)	// cycle by candidates for same KfIndex
			{
				if(it->second == iter->second) // MdataT*(mmCandidateTmp) == MdataT* (mmCandidate)
				{
					founded = true; break; 
				}		
			}
		
			if(false == founded){ equalFront = false; break; } 
		}
	}
	else equalBack = false;
	
	if( !equalFront || !equalBack) // print report, if it's problems
	{
		std::cout<<"\n CheckAdequacy: ";
		if(comment) std::cout<<comment;
		std::cout<<" mmCandidate.size="<<mmCandidate.size()<<" mmLinks.size="<<mmLinks.size()<<" (mmLinks ---> mmCandidate) = "<<equalBack<<", (mmCandidate ---> mmLinks) = "<<equalFront;
	}

return	(equalFront && equalBack);	
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::RemoveLink(Int_t hitIndex, Int_t KfIndex)
{
	std::pair<linkIter, linkIter> range = mmLinks.equal_range(hitIndex); // links for same  hitIndex 

	for(linkIter it = range.first, itEnd = range.second; it != itEnd; )	// iterating multimap && erasing
	{
		if(it->second->first == KfIndex)	mmLinks.erase(it++);
		else 					++it;
	}
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::RemoveCandidate(Int_t hitIndex, Int_t KfIndex)
{
	std::pair<candIter, candIter> range = mmCandidate.equal_range(KfIndex); // 

	for(candIter it = range.first, itEnd = range.second; it != itEnd; )	// iterating multimap && erasing
	{
		if(it->second->GetTofHitIndex() == hitIndex)	mmCandidate.erase(it++);
		else 						++it;
	}
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::UpdateRanks()
{
	mmRanks.clear();

	for(candCIter it = mmCandidate.begin(), end = mmCandidate.end(); it != end; it = mmCandidate.upper_bound(it->first)) // cycle by unique KfIndex
  	{
      		mmRanks.insert(make_pair(mmCandidate.count(it->first ), it->first)); // pair <count(KfIndex), KfIndex>
  	}
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::Reset()
{
	mmCandidate.clear(); 
	mmAccepted.clear();
	mmLinks.clear();
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::SetContainer(TClonesArray *array)
{
	aMdata = array;
	isOwner = false;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::Commit()
{
	mmCandidateSnapshot = mmCandidate;
	mmAcceptedSnapshot = mmAccepted;
			
	//mmLinksSnapshot = mmLinks; ERROR: DON'T USE THIS:  mmlinks used iterators to mmCandidate, so it may be singular iterator!!!!
	mmLinksSnapshot.clear();
	candIter iter;
	
	for(linkCIter it = mmLinks.begin(); it != mmLinks.end(); ++it)
	{
		Int_t hitIndex = it->first;
		iter = FindCandidate(&mmCandidateSnapshot, hitIndex, it->second->second->GetKFTrackIndex()); //  mmLinksSnapshot used iterators to mmCandidateSnapshot NOW
		
		mmLinksSnapshot.insert(make_pair(hitIndex, iter));
	}	
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::Status(const char *comment, std::ostream& os) const
{
	os<<"\n -------------------------------------------------STATUS-----  "; if(comment != nullptr) os<<comment;

	os<<"\n mmCandidate added:";
	PrintChanges(mmCandidate, mmCandidateSnapshot, os);

	os<<"\n mmCandidate removed:";
	PrintChanges(mmCandidateSnapshot, mmCandidate, os);

	os<<"\n mmAccepted added:";
	PrintChanges(mmAccepted, mmAcceptedSnapshot, os);
	
	os<<"\n mmAccepted removed:";
	PrintChanges(mmAcceptedSnapshot, mmAccepted, os);

	os<<"\n mmLinks added:";
	PrintChanges(mmLinks, mmLinksSnapshot, os);
	
	os<<"\n mmLinks removed:";
		
	PrintChanges(mmLinksSnapshot, mmLinks, os);
	os<<"\n ------------------------------------------------------------"<<std::flush;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::PrintChanges(const MMcandType& src, const MMcandType& checked, std::ostream& os) const
{
	if(src == checked)
	{
		os<<"\t   nothing.";
		return;
	}
	
	Int_t KfIndex, hitIndex; bool exist;	
	for(candCIter it = src.begin(); it != src.end(); ++it)
	{
		KfIndex = it->first;
		std::pair<candCIter, candCIter> range = checked.equal_range(KfIndex); // cands for same  KfIndex 
		
		exist = false;
		hitIndex = it->second->GetTofHitIndex();
		
		for(candCIter iter = range.first; iter != range.second; ++iter) // cycle by same KfIndex entries from checked container
		{
			if(hitIndex == iter->second->GetTofHitIndex()) // same hitIndex found
			{ 
				exist = true; 
				break; 
			}
		}
		
		if(!exist) os<<"\n    KfIndex="<<KfIndex<<", hitIndex="<<hitIndex;			
	}
}	
//------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::PrintChanges(const MMbackLinksType& src, const MMbackLinksType& checked, std::ostream& os) const
{
	if(src == checked)
	{
		os<<"\t   nothing.";
		return;
	}

	Int_t KfIndex, hitIndex; bool exist;	
	for(linkCIter it = src.begin(); it != src.end(); it++)
	{		
		hitIndex = it->first;
		std::pair<linkCIter, linkCIter> range = checked.equal_range(hitIndex); // links for same  hitIndex 
		
		exist = false;
		KfIndex = it->second->second->GetKFTrackIndex();
				
		for(linkCIter iter = range.first; iter != range.second; ++iter) // cycle by same hitIndex entries from checked container
		{
			if(KfIndex == iter->second->second->GetKFTrackIndex()) // same KfIndex found
			{ 
				exist = true; 
				break; 
			}
		}
		
		if(!exist) os<<"\n    KfIndex="<<KfIndex<<", hitIndex="<<hitIndex;		
	}
}
//------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::Dump(const char* comment, std::ostream& os) const
{
	os<<"\n ---------------------- DUMP --------------- "; if(comment) os<<comment; os<<" ------------   entries: "<<mmCandidate.size();	

	Int_t KfIndex, hitIndex; bool exist; int n = 0;
	for(candCIter it = mmCandidate.begin(); it != mmCandidate.end(); )
	{
		KfIndex = it->first;		
		os<<"\n "<<++n<<") KfIndex="<<KfIndex<<" --- ";
		int counter = mmCandidate.count(KfIndex);
		
		for(int hit=0; hit<counter; hit++)
		{
			hitIndex = it->second->GetTofHitIndex();
			os<<" hitIndex="<< hitIndex;
			
			linkCIter iter = mmLinks.find(hitIndex);
			if(iter != mmLinks.end())
			{
				int counter2 = mmLinks.count(hitIndex);			
				if(counter2 > 1)os<<" (";			
				for(int track=0; track<counter2;track++)
				{
					if(counter2 > 1)os<<" "<<iter->second->second->GetKFTrackIndex()<<" ";
					++iter;
				}
				if(counter2 > 1)os<<") ";		
			}			
			++it;
		}
	}
	os<<"\n ---------------------- DUMP --------------------------------------"<<std::flush;	
}
//------------------------------------------------------------------------------------------------------------------------
template<typename HitT, typename MdataT> 
void	LMatchingFilter<HitT, MdataT>::PrintRanks(const char* comment, std::ostream& os) const
{
	os<<std::endl;
	if(comment) 	os<<comment;
	os<<"\n mmRanks.count(n) 1= "<<mmRanks.count(1)<<", 2= "<<mmRanks.count(2)<<", 3= "<<mmRanks.count(3)<<", 4= "<<mmRanks.count(4)<<", 5= "<<mmRanks.count(5)
	<<", 6= "<<mmRanks.count(6)<<", 7= "<<mmRanks.count(7)<<", 8= "<<mmRanks.count(8)<<", 9= "<<mmRanks.count(9)<<", 10= "<<mmRanks.count(10)<<"  cand. total: "<< mmCandidate.size();	
}
//------------------------------------------------------------------------------------------------------------------------
#endif 



