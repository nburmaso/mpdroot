
#ifndef MPD_ECTTRACKFOLLOW2TPC_H
#define MPD_ECTTRACKFOLLOW2TPC_H

//#include "MpdKalmanHitZ.h"

#include "FairTask.h"
//#include "FairStsKFTrackFitter.h"
//#include "TLorentzVector.h"
#include "TH1.h"

class MpdKalmanHitZ;
class MpdKalmanHit;
class MpdKalmanTrack;
class MpdEtofPoint;
class TpcLheHit;
class MpdEctKalmanTrack;
class TClonesArray;
class TVector2;

class MpdEctTrackFollow2Tpc :public FairTask
{
 public:

  /** Constructor **/
  MpdEctTrackFollow2Tpc(const char *name="MpdEctTrackFollow2Tpc", Int_t iVerbose = 1 );
  
  /** Destructor **/
  virtual ~MpdEctTrackFollow2Tpc();
  
  /// * FairTask methods
  
  /** Intialisation at begin of run. To be implemented in the derived class.
   *@value  Success   If not kSUCCESS, task will be set inactive.
   **/
  InitStatus Init();
  
  /** Reinitialisation.
   *@value  Success   If not kSUCCESS, task will be set inactive.
   **/
  InitStatus ReInit();

  /** Intialise parameter containers.
   **/
  void SetParContainers();

  void Exec(Option_t * option);

  /** Action after each event. **/
  void Finish();
  void Reset();

 private:

  Int_t GetNofHitsInLayer(Int_t lay) { return (Int_t)fhLays->GetCellContent(lay+1,0); }
  Int_t GetHitsInLayer(Int_t lay) { return fLayPointers[lay]; } ///< first index of hits in layer
  void FollowInTPC(); // propagate tracks into TPC
  void MakeKalmanHits(); // create Kalman hits                                  
  void GetTpcHits(); // get TPC hits near zMax
  Int_t RunKalmanFilter(MpdEctKalmanTrack *track, Int_t layBeg); ///< run Kalman filter       
  void RemoveDoubles(); ///< remove double tracks
  Int_t NofCommonHits(MpdEctKalmanTrack* track, MpdEctKalmanTrack* track1); ///< get number of common hits
  void AddHits(); // add hit objects to tracks                                  
  void SetTrackID(MpdEctKalmanTrack *track); // set track ID from IDs of its hits
  void StoreTracks(); // transfer tracks from fTrackCand to fTracks             
  void ExcludeHits() { }; // exclude used hits
  void Write();
  void Writedir2current( TObject *obj );
  Double_t Proxim(MpdKalmanTrack *track, const MpdKalmanHit *hit); ///< adjust R-Phi coord. for continuity

  Int_t fNPass; 
  TDirectory *fHistoDir;
  TH1F *fhNBranches;

  TClonesArray *fEctPoints; //! ECT points
  TClonesArray *fTpcPoints; //! TPC points
  TClonesArray *fEctTracks; //! ECT tracks
  TClonesArray *fTpcHits; //! TPC hits
  TClonesArray *fKHits; //! TPC Kalman hits
  TClonesArray *fMCTracks; //!< array of MC tracks
  TClonesArray *fTpcTracks; //! TPC tracks
  TClonesArray *fTracks; //! ECT tracks
  Int_t *fLayPointers; //!< locations of hits from different layers             
  TH1F *fhLays; //!< histo with layer hit multiplicities                        

  //FairVertex *fPrimVtx;
  //FairStsKFTrackFitter fStsFitter;

 private:
  // Some constants                                                             
  static const Double_t fgkChi2Cut; ///< max accepted Chi2 of hit for track     
  static const Double_t fgkLayCut; ///< max accepted TPC layer

  ClassDef(MpdEctTrackFollow2Tpc,1);

};

#endif
