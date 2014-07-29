// -------------------------------------------------------------------------
// -----                   CbmSttFindTracks source file                -----
// -----                  Created 02/02/05  by V. Friese               -----
// -------------------------------------------------------------------------

#include "TClonesArray.h"
#include "FairHit.h"
#include "FairRootManager.h"
#include "CbmSttFindTracks.h"
#include "CbmSttTrack.h"
#include "CbmSttTrackFinder.h"

#include <iostream>

using namespace std;

// -----   Default constructor   -------------------------------------------
CbmSttFindTracks::CbmSttFindTracks() 
  : FairTask("STT Find Tracks")
{
  fFinder      = NULL;
  fTrackArray  = NULL;
  fNofTracks   = 0;
  fVerbose     = 1;
  fCollectionsComplete = kFALSE;
}
// -------------------------------------------------------------------------



// -----   Standard constructor   ------------------------------------------
CbmSttFindTracks::CbmSttFindTracks(CbmSttTrackFinder* finder, 
				   Int_t verbose)
  : FairTask("STT Find Tracks")
{
  fFinder      = finder;
  fTrackArray  = NULL;
  fNofTracks   = 0;
  fVerbose     = verbose;
  fCollectionsComplete = kFALSE;
}
// -------------------------------------------------------------------------



// -----   Constructor with name and title   -------------------------------
CbmSttFindTracks::CbmSttFindTracks(const char* name, const char* title, 
				   CbmSttTrackFinder* finder,
				   Int_t verbose) 
  : FairTask(name)
{
  fFinder      = finder;
  fTrackArray  = NULL;
  fNofTracks   = 0;
  fVerbose     = verbose;
  fCollectionsComplete = kFALSE;
}
// -------------------------------------------------------------------------



// -----   Destructor   ----------------------------------------------------
CbmSttFindTracks::~CbmSttFindTracks() 
{
  fTrackArray->Delete();
  fHitCollectionNames.clear();  
  fPointCollectionNames.clear();
}
// -------------------------------------------------------------------------



// -----   Public method Init (abstract in base class)  --------------------
InitStatus CbmSttFindTracks::Init() 
{
  // Check for Track finder
  if (! fFinder) {
    cout << "-E- CbmSttFindTracks::Init: No track finder selected!" << endl;
    return kERROR;
  }

  FairRootManager
      *ioman = FairRootManager::Instance();
  
  if (!ioman) 
  {
      cout << "-E- CbmSttFindTracks::AddHitCollection: "
	   << "RootManager not instantised!" << endl;
      return kFATAL;
  }

  // Create and register SttTrack array
  fTrackArray = new TClonesArray("CbmSttTrack",100);
  ioman->Register("STTTrack", "STT", fTrackArray, kTRUE);
  
  // Set verbosity of track finder
  fFinder->SetVerbose(fVerbose);
  
  // Call the Init method of the track finder
  fFinder->Init();

  return kSUCCESS;
}

// -------------------------------------------------------------------------
void CbmSttFindTracks::AddHitCollectionName(char *hitCollectionName, char *pointCollectionName)
{
    string
	newPointName(pointCollectionName),
	newHitName(hitCollectionName);

    fHitCollectionNames.push_back(newHitName);
    fPointCollectionNames.push_back(newPointName);
}

void CbmSttFindTracks::AddHitCollection(char const *hitCollectionName, char const *pointCollectionName)
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

    fFinder->AddHitCollection(fHitArray, fPointArray);
}

void CbmSttFindTracks::AddAllCollections() 
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

// -----   Public method Exec   --------------------------------------------
void CbmSttFindTracks::Exec(Option_t* opt) 
{
    AddAllCollections();
    
    fTrackArray->Clear();
    
    fNofTracks = fFinder->DoFind(fTrackArray);
    
    for (Int_t iTrack=0; iTrack < fTrackArray->GetEntriesFast(); iTrack++) 
    {
	CbmSttTrack* track = (CbmSttTrack*) fTrackArray->At(iTrack);
	track->SortHits();
    }
}
// -------------------------------------------------------------------------



// -----   Public method Finish   ------------------------------------------
void CbmSttFindTracks::Finish() 
{
  fFinder->Finish();
}
// -------------------------------------------------------------------------




ClassImp(CbmSttFindTracks)
