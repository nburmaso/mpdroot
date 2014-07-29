//------------------------------------------------------------------------------------------------------------------------
#ifndef __MATCHING_FILTER_H
#define __MATCHING_FILTER_H 1

#include <map>
#include <vector>
#include <iostream>

#include "TVector3.h"

//------------------------------------------------------------------------------------------------------------------------
class MatchingCandidate;
class HitData
{
public:
	std::multimap<Int_t, MatchingCandidate*>::iterator 		iterC;
//	MatchingFilter::candIter 		iterC;

	Int_t 					KfIndex;
	Double_t 				delta, deltaR, deltaPhi;

	HitData() : KfIndex(-1), delta(-1.), deltaR(-1.), deltaPhi(-1.){};
	HitData(Int_t index, std::multimap<Int_t, MatchingCandidate*>::iterator it, Double_t Delta, Double_t DeltaR, Double_t DeltaPhi)
		 : KfIndex(index), delta(Delta), deltaR(DeltaR), deltaPhi(DeltaPhi){ iterC = it;};
};
//------------------------------------------------------------------------------------------------------------------------
class MatchingCandidate
{
public:
	std::multimap<Int_t, HitData>::iterator 		iterL;
//	MatchingFilter::linkIter			iterL;

	Int_t 			mcTrackIndex, hitIndex, mcPID, flag, charge;
	TVector3 			Momentum;

	MatchingCandidate(Int_t trackIndex, Int_t hitindex, Int_t mcpid, Int_t flg, Int_t chrg, TVector3 mom)
		: mcTrackIndex(trackIndex), hitIndex(hitindex), mcPID(mcpid), flag(flg), charge(chrg)
	{ Momentum = mom;};

	virtual ~MatchingCandidate(){};
};
//------------------------------------------------------------------------------------------------------------------------
class MatchingFilter
{
public:
	// matching candidates 
	typedef std::multimap<Int_t, MatchingCandidate*>	MMcandidates;	// pair< KfIndex, hitindex & data>
	typedef MMcandidates::iterator					candIter;

private:
	// GC
 	typedef std::vector<MatchingCandidate*> GCEntries;
	typedef GCEntries::iterator 	  			GCIter;
	GCEntries	GClist;

	void 	Delete(){for(GCIter it = GClist.begin(); it != GClist.end(); it++)  delete (*it);  GClist.clear(); };
	void 	eraseCand(candIter itC, bool erase = true){  if(erase) mmapLinks.erase(itC->second->iterL);  mmapCand.erase(itC); };
	void 	eraseCand(candIter itC1, candIter itC2){ for(candIter it = itC1; it!=itC2; ++it) mmapLinks.erase(it->second->iterL);  mmapCand.erase(itC1, itC2); };	

protected:

	// back links to matching candidates 
	typedef std::multimap<Int_t, HitData>			MMbackLinks;		// pair< hitindex, KfIndex & data>
	typedef MMbackLinks::iterator					linkIter;
		
	MMcandidates		mmapCand, mmapAccepted, mDBCand, mDBAccepted;
	MMbackLinks		mmapLinks, mDBLinks;

	// mmap of hits counter per track
	typedef std::multimap<Int_t, Int_t>				MMCandCounters;	// pair <number of candidate hits for current KFtrack, KfIndex>
	typedef MMCandCounters::iterator 				counterIter;
	// back links
	typedef std::map<Int_t, counterIter>				MBackCounters;	// pair <KfIndex, iterator to MMCandCounters>
	typedef MBackCounters::iterator 				counterLinkIter;
	
	MMCandCounters 	mmapCounters;
	MBackCounters 	mapCounterLinks;


	candIter 	InsertCand(Int_t KfIndex, MatchingCandidate*cand){return mmapCand.insert(MMcandidates::value_type(KfIndex, cand)); GClist.push_back(cand);};
	linkIter 	InsertLink(Int_t hitIndex, HitData data){ return mmapLinks.insert(MMbackLinks::value_type(hitIndex, data)); };

	void	Commit(void); 
	void	Status(void); 
	void	AcceptCandidate(candIter itCand, bool updateCounters = true);	
	void	PrintChanges(MMbackLinks& map1, MMbackLinks& map2) const;
	void	PrintChanges(MMcandidates& map1, MMcandidates& map2) const;	

	virtual 	candIter 	FindClosestHit(Int_t KfIndex)=0;
	virtual	candIter 	FindClosestTrack(linkIter itLink, Int_t size, Double_t& minDelta)=0;

	Int_t		ProcessSimpleTracks(void);
	void	RecreateCounterMaps(Int_t nKFTracks);
	void	RemoveCounterMaps(const candIter& itCand);
	void	RemoveCounterMaps(const candIter& itC1, const candIter& itC2);

public:
	void	Reset();
	void	Dump(void);
};
//------------------------------------------------------------------------------------------------------------------------
#endif 
