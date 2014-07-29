// -------------------------------------------------------------------------
// -----                   CbmSttTrackFinder header file               -----
// -----                  Created 28/03/06  by R. Castelijns           -----
// -------------------------------------------------------------------------


/** CbmSttTrackFinder
 *@author R.Castelijns <r.castelijns@fz-jeulich.de>
 **
 ** Abstract base class for concrete STT track finding algorithm.
 ** Each derived class must implement the method DoFind. This has
 ** to operate on the two TClonesArrays of pixel and strip hits
 ** and to fill the CbmSttTrackArray.
 **/

#ifndef CBMSTTTRACKFINDER
#define CBMSTTTRACKFINDER 1

#include "TObject.h"
#include "TList.h"
#include <iostream>

class TClonesArray;

class CbmSttTrackFinder : public TObject
{
 public:

  /** Default constructor **/
  CbmSttTrackFinder() { }; 


  /** Destructor **/
  virtual ~CbmSttTrackFinder() { };


  /** Virtual method Init. If needed, to be implemented in the
   ** concrete class. Else no action.
   **/
  virtual void Init() { };


  /** Abstract method DoFind. To be implemented in the concrete class.
   ** Task: Read the pixel and strip hit arrays and fill the track array,
   ** pointers to which are given as arguments
   *@value Number of tracks created
   **/
  virtual Int_t DoFind(TClonesArray* trackArray) = 0;
  
  /** Virtual method Finish. If needed, to be implemented in the concrete
   ** class. Executed at the end of the run.
   **/
  virtual void Finish() { };

  virtual void AddHitCollection(TClonesArray* mHitArray, TClonesArray* mPointArray) { };

  /** Set verbosity 
   *@param verbose   Verbosity level
   **/
  void SetVerbose(Int_t verbose) { fVerbose = verbose; };


 private:

  Int_t fVerbose;      // Verbosity level

  ClassDef(CbmSttTrackFinder,1);
};

#endif
