// -------------------------------------------------------------------------
// -----                    CbmSttFitTracks source file                -----
// -----                  Created 18/02/05  by V. Friese               -----
// -------------------------------------------------------------------------


#include "TClonesArray.h"
#include "FairRootManager.h"
#include "CbmSttFitTracks.h"
#include "CbmSttTrackFitter.h"
#include "CbmSttTrack.h"

#include <iostream>
using namespace std;

// -----   Default constructor   -------------------------------------------
CbmSttFitTracks::CbmSttFitTracks() 
{
  fFitter        = NULL;
  fTrackArray    = NULL;
  fNofTracks     = 0;
}
// -------------------------------------------------------------------------



// -----   Standard constructor   ------------------------------------------
CbmSttFitTracks::CbmSttFitTracks(const char* name,
				 const char* title,
				 CbmSttTrackFitter* fitter)
  : FairTask(name)
{
  fFitter        = fitter;
  fTrackArray    = NULL;
  fNofTracks     = 0;
  fCollectionsComplete = kFALSE;
}
// -------------------------------------------------------------------------



// -----   Destructor   ----------------------------------------------------
CbmSttFitTracks::~CbmSttFitTracks() 
{ 
  fHitCollectionNames.clear();  
}
// -------------------------------------------------------------------------



// -----   Public method Init (abstract in base class)  --------------------
InitStatus CbmSttFitTracks::Init() 
{
  // Check for Track fitter
  if (! fFitter) 
    {
      cout << "-E- CbmSttFitTracks: No track fitter selected!" << endl;
      return kERROR;
    }

  // Get and check FairRootManager
  FairRootManager
    *ioman = FairRootManager::Instance();

  if (! ioman) 
    {
      cout << "-E- CbmSttFitTracks::Init: "
	   << "RootManager not instantised!" << endl;
      return kFATAL;
    }

  // Get SttTrack array
  fTrackArray  = (TClonesArray*) ioman->GetObject("STTTrack"); //=>SG
  if ( ! fTrackArray) 
    {
      cout << "-E- CbmSttFitTracks::Init: No SttTrack array!"
	   << endl;
      return kERROR;
    }

  // Call the Init method of the track fitter
  fFitter->Init();
  
  return kSUCCESS;
}
// -------------------------------------------------------------------------



// -----   Public method Exec   --------------------------------------------
void CbmSttFitTracks::Exec(Option_t* opt) 
{
    AddAllCollections();

  if ( !fTrackArray ) 
    return; // =>SG
  
  Int_t nTracks = fTrackArray->GetEntriesFast();

  for (Int_t iTrack=0; iTrack<nTracks; iTrack++) 
    {
      CbmSttTrack* pTrack = (CbmSttTrack*)fTrackArray->At(iTrack);
      fFitter->DoFit(pTrack);
    }
}
// -------------------------------------------------------------------------



// -----   Public method Finish   ------------------------------------------
void CbmSttFitTracks::Finish() 
{ 
}
// -------------------------------------------------------------------------

void CbmSttFitTracks::AddHitCollectionName(char *hitCollectionName)
{
    string
	newHitName(hitCollectionName);

    fHitCollectionNames.push_back(newHitName);
}

void CbmSttFitTracks::AddHitCollection(char const *hitCollectionName)
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

    fFitter->AddHitCollection(fHitArray);
}

void CbmSttFitTracks::AddAllCollections() 
{
    if (!fCollectionsComplete)
    {
	for (int counter = 0; counter < fHitCollectionNames.size(); counter++)
	{
	    AddHitCollection(fHitCollectionNames[counter].c_str());
	}
	fCollectionsComplete = kTRUE;
    }
}



ClassImp(CbmSttFitTracks)
