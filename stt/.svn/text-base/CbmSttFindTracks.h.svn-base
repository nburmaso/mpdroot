// -------------------------------------------------------------------------
// -----                   CbmSttFindTracks header file                -----
// -----                  Created 29/03/06  by V. Friese               -----
// -------------------------------------------------------------------------


/** CbmSttFindTracks
 *@author V.Friese <v.friese@gsi.de>
 **
 ** Task class for track finding in the STT. 
 ** Input: TClonesArray of CbmSttHit 
 ** Output: TClonesArray of CbmSttTrack
 **
 ** Uses as track finding algorithm classes derived from CbmSttTrackFinder.
 **/


#ifndef CBMSTTFINDTRACKS
#define CBMSTTFINDTRACKS 1

#include <string>
#include <vector>

#include "FairTask.h"

using std::string;
using std::vector;

class CbmSttTrackFinder;


class CbmSttFindTracks : public FairTask
{

 public:

  /** Default constructor **/
  CbmSttFindTracks();


  /** Standard constructor
   *@param finder   Pointer to STT track finder concrete class
   *@param verbose  Verbosity level
   **/
  CbmSttFindTracks(CbmSttTrackFinder* finder, Int_t verbose = 1);


  /** Constructor with name and title
   *@param name     Name of class
   *@param title    Task title
   *@param finder   Pointer to STT track finder concrete class
   *@param verbose  Verbosity level
   **/
  CbmSttFindTracks(const char* name, const char* title = "FairTask",
		   CbmSttTrackFinder* finder = NULL, Int_t verbose = 1);


  /** Destructor **/
  virtual ~CbmSttFindTracks();


  /** Initialisation at beginning of each event **/
  virtual InitStatus Init();


  /** Task execution **/
  virtual void Exec(Option_t* opt);


  /** Finish at the end of each event **/
  virtual void Finish();


  /** Accessors **/
  CbmSttTrackFinder* GetFinder() { return fFinder; };
  Int_t GetNofTracks()           { return fNofTracks; };


  /** Set concrete track finder **/
  void UseFinder(CbmSttTrackFinder* finder) { fFinder = finder; };

  /** Add an hit collection to perform trackfinding on */
  void AddHitCollectionName(char *hitCollectionName, char *pointCollectionName);


 private:
  void AddAllCollections(); 
  void AddHitCollection(char const *collectionName, char const *pointCollectionName);

  CbmSttTrackFinder* fFinder;    // Pointer to TrackFinder concrete class
  TClonesArray* fTrackArray;     // Output array of CbmSttTracks 

  Int_t fNofTracks;              // Number of tracks created
  Int_t fVerbose;                // Verbosity level

  vector<string> fHitCollectionNames;
  vector<string> fPointCollectionNames;

  Bool_t fCollectionsComplete;

  ClassDef(CbmSttFindTracks,1);
};

#endif
