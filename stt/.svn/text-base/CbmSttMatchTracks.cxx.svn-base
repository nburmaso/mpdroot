// -------------------------------------------------------------------------
// -----                  CbmStsMatchTracks source file                -----
// -----                  Created 24/11/05  by V. Friese               -----
// -------------------------------------------------------------------------


#include <iostream>
#include <map>

#include "TClonesArray.h"

#include "FairMCPoint.h"
#include "FairRootManager.h"
#include "CbmSttMatchTracks.h"
#include "CbmSttHit.h"
#include "CbmSttTrack.h"
#include "CbmSttTrackMatch.h"



// -----   Default constructor   -------------------------------------------
CbmSttMatchTracks::CbmSttMatchTracks() 
  : FairTask("STT track match") {
  fTracks     = NULL;
  fMatches    = NULL;
  fVerbose    = 1;
  fCollectionsComplete = kFALSE;
}
// -------------------------------------------------------------------------



// -----   Constructor with verbosity level   ------------------------------
CbmSttMatchTracks::CbmSttMatchTracks(Int_t verbose) 
  : FairTask("STT track match") {
  fTracks     = NULL;
  fMatches    = NULL;
  fVerbose    = verbose;
  fCollectionsComplete = kFALSE;
}
// -------------------------------------------------------------------------



// -----   Constructor with name, title and verbosity  ---------------------
CbmSttMatchTracks::CbmSttMatchTracks(const char* name, const char* title,
				     Int_t verbose) 
  : FairTask(name) {
  fTracks     = NULL;
  fMatches    = NULL;
  fVerbose    = verbose;
  fCollectionsComplete = kFALSE;
}
// -------------------------------------------------------------------------



// -----   Destructor   ----------------------------------------------------
CbmSttMatchTracks::~CbmSttMatchTracks() 
{
  fHitCollectionNames.clear();  
  fPointCollectionNames.clear();
}
// -------------------------------------------------------------------------




// -----   Public method Init   --------------------------------------------
InitStatus CbmSttMatchTracks::Init() {
  
  // Get FairRootManager
  FairRootManager* ioman = FairRootManager::Instance();
  if (! ioman) {
    cout << "-E- CbmSttMatchTracks::Init: "
	 << "RootManager not instantised!" << endl;
    return kFATAL;
  }

  // Get SttTrack Array
  fTracks = (TClonesArray*) ioman->GetObject("STTTrack");
  if ( ! fTracks ) {
    cout << "-E- CbmSttMatchTracks::Init: No SttTrack array!" << endl;
    return kERROR;
  }

  // Create and register SttTrackMatch array
  fMatches = new TClonesArray("CbmSttTrackMatch",100);
  ioman->Register("STTTrackMatch", "STT", fMatches, kTRUE);

  return kSUCCESS;

}
// -------------------------------------------------------------------------



// -----   Public method Exec   --------------------------------------------
void CbmSttMatchTracks::Exec(Option_t* opt) 
{
  AddAllCollections();
  
  if (fHitCollectionList.GetEntries() == 0)
  {
      cout << "-E- CbmSttTrackFinderIdeal::DoFind: "
	   << "No hit arrays present, call AddHitCollection() first (at least once)! " << endl;
  }

  if (fPointCollectionList.GetEntries() == 0)
  {
      cout << "-E- CbmSttTrackFinderIdeal::DoFind: "
	   << "No point arrays present, call AddHitCollection() first (at least once)! " << endl;
  }

  // Clear output array
  fMatches->Delete();

  // Create some pointers and variables
  CbmSttTrack*      track = NULL;
  CbmSttHit*        mHit  = NULL;
  FairMCPoint*       point = NULL;

  Int_t nHits    = 0;
  Int_t nMCTracks = 0;
  Int_t iPoint    = 0;
  Int_t iFlag     = 0;
  Int_t iMCTrack  = 0;
  Int_t nAll      = 0;
  Int_t nTrue     = 0;
  Int_t nWrong    = 0;
  Int_t nFake     = 0;
  Int_t nHitSum     = 0;
  Int_t nTrueSum    = 0;
  Int_t nWrongSum   = 0;
  Int_t nFakeSum    = 0;
  Int_t nMCTrackSum = 0;

  //  map<Int_t, Int_t>::iterator it = 0;
  map<Int_t, Int_t>::iterator it;
  //  it = 0;

  // Loop over SttTracks
  Int_t nTracks = fTracks->GetEntriesFast();

  for (Int_t iTrack=0; iTrack<nTracks; iTrack++) 
    {
      track = (CbmSttTrack*) fTracks->At(iTrack);
      if ( ! track) 
	{
	  cout << "-W- CbmSttMatchTracks::Exec: Empty SttTrack at " 
	       << iTrack << endl;
	  continue;
	}
      
      nHits = track->GetNofHits();

      nAll = nTrue = nWrong = nFake = nMCTracks = 0;

      fMatchMap.clear();
      if (fVerbose > 2) cout << endl << "Track " << iTrack << ", Hits " 
			     << nHits << endl;

      // Loop over Hits of track
      for (Int_t iMHit=0; iMHit<nHits; iMHit++) 
	{
	    // alter here
	  mHit = GetHitFromCollections(track->GetHitIndex(iMHit));

	  if ( ! mHit ) 
	    {
	      cout << "-E- CbmSttMatchTracks::Exec: "
		   << "No Hit " << iMHit << " for track " << iTrack << endl;
	      continue;
	    }

	  iPoint = mHit->GetRefIndex();
	  
	  if ( iPoint < 0 ) 
	    {        
	      nFake++;
	      continue;
	    }
	  
	  // alter here
	  point = GetPointFromCollections(track->GetHitIndex(iMHit));
	  
	  if ( ! point ) 
	    {
	      cout << "-E- CbmSttMatchTracks::Exec: "
		   << "Empty MCPoint " << iPoint << " from Hit " << iMHit
		   << " (track " << iTrack << ")" << endl;
	      continue;
	    }
	  
	  iMCTrack = point->GetTrackID();
	  
	  if ( fVerbose > 2 ) cout << "Track " << iTrack << ", hit "
				   << track->GetHitIndex(iMHit) 
				   << ", STTPoint " << iPoint << ", MCTrack "
				   << iMCTrack << endl;
	  fMatchMap[iMCTrack]++;
	}
    
      // Search for best matching MCTrack
      iMCTrack = -1;

      for (it=fMatchMap.begin(); it!=fMatchMap.end(); ++it)
	{
	  if (fVerbose > 2) cout << it->second 
				 << " common points wth MCtrack " 
				 << it->first << endl;
	  nMCTracks++;
	  nAll += it->second;
	  if ( it->second > nTrue ) 
	    {
	      iMCTrack = it->first;
	      nTrue    = it->second;	
	    }
	}
    
      nWrong = nAll - nTrue;
      if (fVerbose>1) cout << "-I- CbmSttMatchTracks: STTTrack " << iTrack 
			   << ", MCTrack " << iMCTrack << ", true " 
			   << nTrue << ", wrong " << nWrong << ", fake " 
			   << nFake << ", #MCTracks " << nMCTracks << endl;
      
      // Create SttTrackMatch
      new ((*fMatches)[iTrack]) CbmSttTrackMatch(iMCTrack, nTrue, 
						 nWrong, nFake, 
						 nMCTracks);
      
      // Some statistics
      nHitSum     += nHits;
      nTrueSum    += nTrue;
      nWrongSum   += nWrong;
      nFakeSum    += nFake;
      nMCTrackSum += nMCTracks;

    } // Track loop

  // Event statistics

  Double_t qTrue = 0.;
  if ( nHitSum) 
      qTrue  = Double_t(nTrueSum)  / Double_t(nHitSum) * 100.;

  if (fVerbose) 
  {
      Double_t 
	  qWrong = 0.,
	  qFake = 0.,
	  qMC = 0.;
      
      if (nHitSum)
      {
	  qWrong = Double_t(nWrongSum) / Double_t(nHitSum) * 100.;
	  qFake  = Double_t(nFakeSum)  / Double_t(nHitSum) * 100.;
      }
      if (nTracks)
	  qMC = Double_t(nMCTrackSum) / Double_t(nTracks);

    cout << endl;
    cout << "-------------------------------------------------------" 
	 << endl;
    cout << "-I-              STT Track Matching                 -I-"
	 << endl;
    cout << "Reconstructed STTTracks : " << nTracks << endl;;
    cout << "True  hit assignments   : " << qTrue  << " %" << endl;
    cout << "Wrong hit assignments   : " << qWrong << " %" << endl;
    cout << "Fake  hit assignments   : " << qFake  << " %" << endl;
    cout << "MCTracks per STTTrack   : " << qMC << endl;
    cout << "--------------------------------------------------------" 
	 << endl;
  }
  else cout << "-I- CbmSttMatchTracks: rec. " << nTracks << ", quota "
	    << qTrue << " % " << endl;

}
// -------------------------------------------------------------------------

// -------------------------------------------------------------------------
void CbmSttMatchTracks::AddHitCollectionName(char *hitCollectionName, char *pointCollectionName)
{
    string
	newPointName(pointCollectionName),
	newHitName(hitCollectionName);

    fHitCollectionNames.push_back(newHitName);
    fPointCollectionNames.push_back(newPointName);
}

void CbmSttMatchTracks::AddHitCollection(char const *hitCollectionName, char const *pointCollectionName)
{
    // Get and check FairRootManager
    FairRootManager
        *ioman = FairRootManager::Instance();
    
    if (!ioman) 
    {
	cout << "-E- CbmSttFindTracks::AddHitCollection: "
	     << "RootManager not instantised!" << endl;
    }
    
    // Get hit Array
    TClonesArray
	*fHitArray = (TClonesArray*) ioman->GetObject(hitCollectionName);

    if (!fHitArray) 
    {
	cout << "-W- CbmSttFindTracks::AddHitCollection: No " << hitCollectionName << " STT hit array!"
	     << endl;
    }

    // Get point Array
    TClonesArray
	*fPointArray = (TClonesArray*) ioman->GetObject(pointCollectionName);

    if (!fPointArray) 
    {
	cout << "-W- CbmSttFindTracks::AddHitCollection: No " << pointCollectionName << " STT hit array!"
	     << endl;
    }

    fHitCollectionList.Add(fHitArray); 
    fPointCollectionList.Add(fPointArray);
}

void CbmSttMatchTracks::AddAllCollections() 
{
    if (!fCollectionsComplete)
    {
	for (int counter = 0; counter < fHitCollectionNames.size(); counter++)
	{
	    AddHitCollection(fHitCollectionNames[counter].c_str(), fPointCollectionNames[counter].c_str());
	}
	fCollectionsComplete = kTRUE;
    }
}

CbmSttHit* CbmSttMatchTracks::GetHitFromCollections(Int_t hitCounter)
{
    CbmSttHit
	*retval = NULL;
 
    Int_t
	relativeCounter = hitCounter;

    for (Int_t collectionCounter = 0; collectionCounter < fHitCollectionList.GetEntries(); collectionCounter++)
    {
	Int_t
	    size = ((TClonesArray *)fHitCollectionList.At(collectionCounter))->GetEntriesFast();

	if (relativeCounter < size)
	{
	    retval = (CbmSttHit*) ((TClonesArray *)fHitCollectionList.At(collectionCounter))->At(relativeCounter);
	    break;
	}
	else
	{
	    relativeCounter -= size;
	}
    }
    return retval;
}

FairMCPoint* CbmSttMatchTracks::GetPointFromCollections(Int_t hitCounter)
{
    FairMCPoint
	*retval = NULL;
 
    Int_t
	relativeCounter = hitCounter;

    for (Int_t collectionCounter = 0; collectionCounter < fHitCollectionList.GetEntries(); collectionCounter++)
    {
	Int_t
	    size = ((TClonesArray *)fHitCollectionList.At(collectionCounter))->GetEntriesFast();

	if (relativeCounter < size)
	{
	    Int_t
		tmpHit = ((CbmSttHit*) ((TClonesArray *)fHitCollectionList.At(collectionCounter))->At(relativeCounter))->GetRefIndex();
	    
            retval = (FairMCPoint*) ((TClonesArray *)fPointCollectionList.At(collectionCounter))->At(tmpHit);
	    
	    break;
	}
	else
	{
	    relativeCounter -= size;
	}
    }
    return retval;
}

// -----   Public method Finish   ------------------------------------------
void CbmSttMatchTracks::Finish() { }
// -------------------------------------------------------------------------


    
ClassImp(CbmSttMatchTracks)


      

      
