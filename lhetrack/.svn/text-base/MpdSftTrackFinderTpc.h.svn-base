
#ifndef MPD_SFTTRACKFINDERTPC_H
#define MPD_SFTTRACKFINDERTPC_H

//#include "MpdKalmanHitZ.h"

#include "FairTask.h"
//#include "TLorentzVector.h"
#include "TH1.h"

class MpdKalmanHit;
class MpdKalmanTrack;
//class MpdEtofPoint;
class MpdEctKalmanTrack;
class TClonesArray;
class TVector2;
class TArrayI;

class MpdSftTrackFinderTpc :public FairTask
{
 public:

  /** Constructor **/
  MpdSftTrackFinderTpc(const char *name="MpdSftTrackFinderTpc", Int_t iVerbose = 1 );
  
  /** Destructor **/
  virtual ~MpdSftTrackFinderTpc();
  
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
  void GetTrackSeeds(Int_t iPass); // build track seeds                         
  void DoTracking(Int_t iPass); // run tracking                                 
  void FollowInTPC(); // follow tracks in TPC
  void MakeKalmanHitsSft(); // create Kalman hits for SFT
  //Double_t EvalPt(const MpdKalmanHitZ *hit1, const MpdKalmanHitZ *hit2); // evaluate Pt
  Double_t EvalPt(MpdKalmanTrack *track, const MpdKalmanHit *hit1, const MpdKalmanHit *hit2); // evaluate Pt
  //void EvalCovar(const MpdKalmanHitZ *hitOut, const MpdKalmanHitZ *hitIn,
  void EvalCovar(const MpdKalmanHit *hitOut, const MpdKalmanHit *hitIn,
		 MpdEctKalmanTrack *track); // eval. covariance
  Int_t RunKalmanFilter(MpdEctKalmanTrack *track, Int_t layBeg); ///< run Kalman filter       
  Int_t RunKalmanFilterTPC(MpdEctKalmanTrack *track, Int_t layBeg); ///< run Kalman filter in TPC
  void RemoveDoubles(); ///< remove double tracks
  Int_t NofCommonHits(MpdEctKalmanTrack* track, MpdEctKalmanTrack* track1); ///< get number of common hits
  void AddHits(); // add hit objects to tracks                                  
  void SetTrackID(MpdEctKalmanTrack *track); // set track ID from IDs of its hits
  void StoreTracks(); // transfer tracks from fTrackCand to fTracks             
  void ExcludeHits() { }; // exclude used hits
  void Write();
  void Writedir2current( TObject *obj );
  Double_t Proxim(MpdKalmanTrack *track, const MpdKalmanHit *hit); ///< adjust R-Phi coord. for continuity
  //TVector2 GetDistance(MpdEctKalmanTrack *track, MpdKalmanHitZ *hit); ///< hit-track distance
  TVector2 GetDistance(MpdEctKalmanTrack *track, MpdKalmanHit *hit); ///< hit-track distance
  void GoToVertex(); // propagate tracks to vertex
  void PassInnerShell(MpdKalmanTrack *track); ///< propagate thru TPC inner shell
  void RemoveTpcTracks(); ///< remove TPC tracks which are parts of the combined ones

  Int_t fNPass; 
  TDirectory *fHistoDir;
  TH1F *fhNBranches;

  TClonesArray *fSftPoints; //! SFT points
  TClonesArray *fTpcPoints; //! TPC points
  TClonesArray *fTpcHits; //! TPC hits
  TClonesArray *fKHits; //!< array of Kalman hits
  TClonesArray *fMCTracks; //!< array of MC tracks
  //TClonesArray *fSTSTrackMatch;
  TClonesArray *fTracks; //! ECT tracks
  Int_t *fLayPointers; //!< locations of hits from different layers             
  TH1F *fhLays; //!< histo with layer hit multiplicities                        
  Int_t fLayMinMax[2][2]; //!< first and last layer numbers for modules at Z > 0 and Z < 0

  //FairVertex *fPrimVtx;

 private:
  // Some constants                                                             
  static const Double_t fgkChi2Cut; ///< max accepted Chi2 of hit for track     

  ClassDef(MpdSftTrackFinderTpc,1);

};

#endif
