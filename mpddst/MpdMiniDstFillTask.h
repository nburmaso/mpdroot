/**
 * \class MpdMiniDstFillTrask
 * \brief Class that reads event, track, and detector parameters and fills MpdMiniDst
 *
 * The MpdMiniDstFillTask reads information from the reconstructed (and/or simulated) event, track, vertex
 * and detectors and fill MpdMiniDst structure.
 *
 * \author Pavel Batyuk (JINR), Grigory Nigmatkulov (NRNU MEPhI)
 * \email pbatyuk@jinr.ru ; nigmatkulov@gmail.com ; ganigmatkulov@mephi.ru
 * \date August 10, 2019
 **/

#ifndef MPDMINIDSTFILLTASK_H
#define MPDMINIDSTFILLTASK_H

// ROOT and FAIRROOT headers
#include "TNamed.h"
#include "TClass.h"
#include "TChain.h"
#include "TTree.h"
#include "TString.h"
#include "TClonesArray.h"
#include "TFile.h"

#include <FairTask.h>
#include <FairRunAna.h>
#include <FairField.h>
#include <FairRootManager.h>
#include <FairMCEventHeader.h>
#include <FairEventHeader.h>
#include <MpdMCTrack.h>
#include <MpdGenTrack.h>

// MpdRoot headers
#include <MpdEvent.h>
#include <MpdVertex.h>
#include <MpdTrack.h>
#include <MpdTpcKalmanTrack.h>
#include <MpdKalmanFilter.h>
#include <MpdKalmanGeoScheme.h>
#include <MpdTofHit.h>
#include <MpdEmcDigitKI.h>
#include <MpdEmcClusterKI.h>
#include <MpdTofMatchingData.h>


#include "MpdMiniDst.h"
#include "MpdMiniArrays.h"
#include "MpdMiniEvent.h"
#include "MpdMiniTrack.h"
#include "MpdMiniMcTrack.h"
#include "MpdMiniMessMgr.h"
#include "MpdMiniTrackCovMatrix.h"
#include "MpdMiniBTofHit.h"
#include "MpdMiniBTofPidTraits.h"
#include "MpdMiniBECalHit.h"
#include "MpdMiniBECalPidTraits.h"
#include "MpdMiniMcEvent.h"

//_________________
class MpdMiniDstFillTask : public FairTask {

 public:
  /// Default constructor
  MpdMiniDstFillTask();

  /// Constructor with output file name for miniDST and IO-modes (read or write)
  MpdMiniDstFillTask(TString);
  /// Destructor
  virtual ~MpdMiniDstFillTask();

  /// Init
  virtual InitStatus Init();
  /// Execute
  virtual void Exec(Option_t* option);
  /// Finish
  virtual void Finish();

  /// Fill track covariant matrix
  /// \param true store covariant matrix (default)
  /// \param false do not store covariant matrix information
  void isUseCovMatrix(Bool_t flag) { fIsUseCovMatrix = flag; }

 private:

  /// Store/Not store track covariant matrix information
  Bool_t fIsUseCovMatrix;

  /// Pointer to event header
  FairMCEventHeader* fEventHeaders;
  /// Pointer to MpdEvent
  MpdEvent* fEvents;
  /// Pointer to primary vertieces
  TClonesArray* fVertices;
  /// Pointer to TPC tracks
  TClonesArray* fTpcTracks;
  /// Pointer to TOF hits
  TClonesArray* fTofHits;
  /// Pointer to TOF-matching information
  TClonesArray* fTofMatching;
  /// Pointer to MC tracks
  TClonesArray* fMCTracks;
  // Pointer to GenTracks (Primaries from MC-generator)
  TClonesArray* fGenTracks;
  // Pointer to EMC digits 
  TClonesArray* fEmcDigits;
  // Pointer to EMC clusters
  TObjArray* fEmcClusters;
  
  /// A pointer to the main input/outpur miniDst structure containing all `TObjArray`s
  MpdMiniDst* mMiniDst;

  /// Magnetic field of the current event
  Float_t mBField;

  /// Output file name
  TString mOutputFileName;
  /// Pointer to the output file
  TFile* mOutputFile;

  /// Pointer to the TTree with miniDst
  TTree* mTTree;

  /// Splitting level of ROOT file
  Int_t mSplit;
  /// Compression level
  Int_t mCompression;
  /// Size of the buffer
  Int_t mBufferSize;

  /// Pointer to the mini arrays
  TClonesArray** mMiniArrays;

  /// Turn-off ROOT streamers
  void streamerOff();

  /// Create arrays
  void createArrays();

  /// Fill event information
  void fillEvent();

  /// Fill track information
  void fillTracks();

  /// Fill BTOF information
  void fillBTofHits();

  /// Fill ECal information
  void fillECalHits();
 
  /* Below is given a set of auxiliary functions */
  /// Compute matrices of derivatives
  void ComputeAandB(TMatrixD&, const MpdKalmanTrack*, const MpdKalmanTrack&,
		    TMatrixD&, TMatrixD&, TMatrixD&);

  /// Adjust track parameters
  void Proxim(const MpdKalmanTrack&, MpdKalmanTrack&);
  
  /// Refit function for momenta
  void RefitToVp(MpdMiniTrack*, Int_t, MpdVertex*); // A function to be used when doing refit of tracks considered as primaries to the primary vertex aimed at precising of their momenta

  // Fill covariance matrix if needed
  void fillCovMatrix(MpdTpcKalmanTrack*, MpdMiniTrackCovMatrix*); // A function used for filling of cov. track matrix
  
  // Tof matching 
  void DoTofMatching(Int_t, Int_t, MpdMiniTrack*, MpdMiniBTofPidTraits*);
  
  // Ecal matching
  void DoEcalMathching(Int_t, Int_t, MpdMiniTrack*, MpdMiniBECalPidTraits*);
  
  ClassDef(MpdMiniDstFillTask, 0)
};

#endif
