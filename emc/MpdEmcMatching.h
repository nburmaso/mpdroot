//--------------------------------------------------------------------
//
// Description:
//      MPD TPC-EMC Matching 
//
//
// Author List:
//      Alexander Zinchenko LHEP, JINR, Dubna - 1-June-2016
//
//--------------------------------------------------------------------

#ifndef MPDEMCMATCHING_H
#define MPDEMCMATCHING_H 1

#include "MpdEmcDigit.h"
#include "MpdEmcGeoPar.h"

#include "FairTask.h"
#include <iostream>
#include <map>

class MpdTpcKalmanTrack;
class TClonesArray;

class MpdEmcMatching : public FairTask 
{
 public:

  /** Default constructor **/
  MpdEmcMatching();
  
  
  /** Destructor **/
  ~MpdEmcMatching();
  
  
  /** Virtual method Init **/
  virtual InitStatus Init();
  
  
  /** Virtual method Exec **/
  virtual void Exec(Option_t* opt);
  void virtual Finish();
  
 private:

  /** Input array of MpdEmcPoints **/
  TClonesArray* fPointArray;
  /** Input array of MCTracks **/
  TClonesArray* fMcTrackArray;
  /** Input array of EmcRecPoints **/
  TClonesArray* fRecPointArray;
  /** Input array of TpcKalmanTracks **/
  TClonesArray* fTpcTracks;
  /** Output array of EmcMatches **/
  TClonesArray* fMatchArray;
  
  /** Output array of MpdEmcHit **/
  TClonesArray* fDigiArray;
  
  void DoMatching(Int_t itrack);
  
  MpdEmcDigit* SearchHit(TString tower);
  
  MpdEmcGeoPar* fGeoPar;
  std::vector<std::multimap<Double_t,Int_t> > fRecPoints; 
  
  ClassDef(MpdEmcMatching, 1);
  
};

#endif
