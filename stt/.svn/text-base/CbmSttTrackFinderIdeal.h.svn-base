// -------------------------------------------------------------------------
// -----                 CbmSttTrackFinderIdeal header file            -----
// -----                  Created 28/03/06  by R. Castelijns           -----
// -------------------------------------------------------------------------


/** CbmSttTrackFinderIdeal
 *@author R.Castelijns <r.castelijns@fz-juelich.de>
 **
 ** Ideal track finder in the STT for simulated data. 
 ** For each MCTrack having at least 3 SttPoints, a SttTrack is created
 ** and the corresponding SttHits are attached using the correspondence
 ** between SttHit and SttPoint.
 **/


#ifndef CBMSTTTRACKFINDERIDEAL
#define CBMSTTTRACKFINDERIDEAL 1

#include "CbmSttTrackFinder.h"
#include "FairMCTrack.h"

class TClonesArray;

class CbmSttTrackFinderIdeal : public CbmSttTrackFinder
{

 public:
    void GetTrackletCircular(Double_t firstX, Double_t firstY, Double_t firstR, 
			     Double_t secondX, Double_t secondY, Double_t secondR, 
			     Double_t thirdX, Double_t thirdY, Double_t thirdR, 
			     Double_t *circleRadii, Double_t *circleCentersX, 
			     Double_t *circleCentersY) const;
    
    //void ZoomTrack(Double_t &dSeed, Double_t &phiSeed, Double_t &rSeed, CbmSttTrack *track);
    void GetTrack(Double_t &dSeed, Double_t &phiSeed, Double_t &rSeed, Int_t mcTrackNo);
  
  /** Default constructor **/
  CbmSttTrackFinderIdeal();


  /** Standard constructor **/
  CbmSttTrackFinderIdeal(Int_t verbose);


  /** Destructor **/
  virtual ~CbmSttTrackFinderIdeal();


  /** Initialisation **/
  virtual void Init();


  /** Track finding algorithm
   ** This just reads MC truth (MCTracks and MCPoints), creates
   ** one StsTrack for each MCTrack and attaches the hits according
   ** to the MCTrack of the corresponding MCPoint
   **
   *@param mHitArray   Array of MAPS hits
   *@param trackArray  Array of CbmStsTrack
   **
   *@value Number of tracks created
   **/
 virtual Int_t DoFind(TClonesArray* mHitArray);
 virtual void AddHitCollection(TClonesArray* mHitArray, TClonesArray* mPointArray) {fHitCollectionList.Add(mHitArray); fPointCollectionList.Add(mPointArray);}
 void plotAllStraws();
 Bool_t putStraw(Double_t xpos, Double_t ypos, Double_t radius);



 private:

  /** Arrays of MC information **/
  TClonesArray* fMCTrackArray;

  Bool_t rootoutput;

  /** Verbosity level **/
  Int_t fVerbose;

  TList fHitCollectionList;
  TList fPointCollectionList;
  CbmSttHit* GetHitFromCollections(Int_t hitCounter);
  FairMCPoint* GetPointFromCollections(Int_t hitCounter);

  ClassDef(CbmSttTrackFinderIdeal,1);
};


#endif
