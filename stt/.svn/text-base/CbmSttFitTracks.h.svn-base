// -------------------------------------------------------------------------
// -----                    CbmSttFitTracks header file                -----
// -----                  Created 18/02/05  by V. Friese               -----
// -------------------------------------------------------------------------


/** CbmSttFitTracks
 *@author V.Friese <v.friese@gsi.de>
 **
 ** Task class for track fitting in the STT.
 ** Input: TClonesArray of CbmSttTrack
 ** Parameters of these objects are updated
 **
 ** Uses as track fitting algorithm classes derived from CbmSttTrackFitter.
 **/


#ifndef CBMSTTFITTRACKS
#define CBMSTTFITTRACKS 1


#include "FairTask.h"
#include <string>
#include <vector>

using std::vector;
using std::string;

class CbmSttTrackFitter;


class CbmSttFitTracks : public FairTask
{

 public:

  /** Default constructor **/
  CbmSttFitTracks();


  /** Standard constructor 
   **
   *@param name   Name of class
   *@param title  Task title
   *@param fitter Pointer to STT track fitter concrete class
   **/
  CbmSttFitTracks(const char* name, const char* title = "FairTask",
		  CbmSttTrackFitter* fitter = NULL);


  /** Destructor **/
  virtual ~CbmSttFitTracks();


  /** Initialisation at beginning of each event **/
  virtual InitStatus Init();


  /** Task execution **/
  virtual void Exec(Option_t* opt);


  /** Finish at the end of each event **/
  virtual void Finish();


  /** Accessors **/
  CbmSttTrackFitter* GetFitter() { return fFitter; };
  Int_t GetNofTracks()           { return fNofTracks; };


  /** Set concrete track finder **/
  void UseFitter(CbmSttTrackFitter* fitter) { fFitter = fitter; };

  void AddHitCollectionName(char *hitCollectionName);


 private:
  void AddAllCollections(); 
  void AddHitCollection(char const *collectionName);

  CbmSttTrackFitter* fFitter;    // Pointer to TrackFinder concrete class
  TClonesArray* fTrackArray;     // Input array of STT tracks
  Int_t fNofTracks;              // Number of tracks successfully fitted
  vector<string> fHitCollectionNames;
  Bool_t fCollectionsComplete;

  ClassDef(CbmSttFitTracks,1);

};

#endif
