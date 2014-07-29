// -------------------------------------------------------------------------
// -----                   CbmSttTrackMatch header file                -----
// -----                  Created 28/03/06  by V. Friese               -----
// -------------------------------------------------------------------------


/** CbmSttTrackMatch.h
 *@author R.Castelijns <r.castelijns@fz-juelich.de>
 **
 ** Data structure describing the matching of a reconstructed CbmSttTrack
 ** with a Monte Carlo FairMCTrack.
 **/


#ifndef CBMSTTTRACKMATCH_H
#define CBMSTTTRACKMATCH_H 1


#include "TObject.h"

class CbmSttTrackMatch : public TObject
{

 public:

  /** Default constructor **/
  CbmSttTrackMatch();


  /** Standard constructor 
  *@param mcTrackID   Index of matched MCTrack
  *@param nTrue       Good hits from matched MCTrack
  *@param nWrong      Good Hits from other MCTracks
  *@param nFake       Fake Hits
  *@param nTracks     Number of MCTracks with common points
  **/
  CbmSttTrackMatch(Int_t mcTrackID, Int_t nTrue, Int_t nWrong, 
		   Int_t nFake, Int_t nTracks);


  /** Destructor **/
  virtual ~CbmSttTrackMatch();


  /** Accessors **/
  Int_t GetMCTrackID()    { return fMCTrackID;    };
  Int_t GetNofTrueHits()  { return fNofTrueHits;  };
  Int_t GetNofWrongHits() { return fNofWrongHits; };
  Int_t GetNofFakeHits()  { return fNofFakeHits;  };
  Int_t GetNofMCTracks()  { return fNofMCTracks;  };


 private:

  /** Best matching FairMCTrack  **/
  Int_t fMCTrackID;

  /** Number of good hits belonging to the matched MCTrack **/
  Int_t fNofTrueHits;

  /** Number of good hits belonging to other MCTracks **/
  Int_t fNofWrongHits;

  /** Number of fake hits **/
  Int_t fNofFakeHits;

  /** Number of MCTracks with common points **/
  Int_t fNofMCTracks;


  ClassDef(CbmSttTrackMatch,1);

};


#endif
				 
