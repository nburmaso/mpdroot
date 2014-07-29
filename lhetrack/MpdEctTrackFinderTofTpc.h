
#ifndef MPD_ECTTRACKFINDERTOFTPC_H
#define MPD_ECTTRACKFINDERTOFTPC_H

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

class MpdEctTrackFinderTofTpc :public FairTask
{
 public:

  /** Constructor **/
  MpdEctTrackFinderTofTpc(const char *name="MpdEctTrackFinderTofTpc", Int_t iVerbose = 1 );
  
  /** Destructor **/
  virtual ~MpdEctTrackFinderTofTpc();
  
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
  void MakeKalmanHits(); // create Kalman hits                                  
  void GetTpcPoints(); // get TPC points near max Z
  void EvalParams(const MpdEtofPoint *tof, const TpcLheHit *tpc, MpdEctKalmanTrack *track); // evaluate params 
  void EvalCovar(const MpdEtofPoint *tof, const TpcLheHit *tpc, MpdEctKalmanTrack *track); // eval. covariance
  Int_t RunKalmanFilter(MpdEctKalmanTrack *track, Int_t layBeg); ///< run Kalman filter       
  void RemoveDoubles(); ///< remove double tracks
  Int_t NofCommonHits(MpdEctKalmanTrack* track, MpdEctKalmanTrack* track1); ///< get number of common hits
  void AddHits(); // add hit objects to tracks                                  
  void SetTrackID(MpdEctKalmanTrack *track); // set track ID from IDs of its hits
  void StoreTracks(); // transfer tracks from fTrackCand to fTracks             
  void ExcludeHits() { }; // exclude used hits
  void Write();
  void Writedir2current( TObject *obj );
  Double_t Proxim(Double_t phi0, Double_t phi); // adjust angle for continuity  
  Double_t Proxim(MpdKalmanTrack *track, const MpdKalmanHit *hit); ///< adjust R-Phi coord. for continuity
  TVector2 GetDistance(MpdEctKalmanTrack *track, MpdKalmanHitZ *hit); ///< hit-track distance

  Int_t fNPass; 
  TDirectory *fHistoDir;
  TH1F *fhNBranches;

  TClonesArray *fTpcHits; //! TPC hits
  TClonesArray *fEctHits; //! ECT hits
  TClonesArray *fTofHits; //! TOF hits
  TClonesArray *fKHits; //!< array of Kalman hits
  TClonesArray *fTpcTracks; //!< array of TPC tracks
  TClonesArray *fMCTracks; //!< array of MC tracks
  //TClonesArray *fSTSTrackMatch;
  TClonesArray *fTracks; //! ECT tracks
  Int_t *fLayPointers; //!< locations of hits from different layers             
  TH1F *fhLays; //!< histo with layer hit multiplicities                        
  TObjArray *fTPC; //! TPC point near Zmax

  //FairVertex *fPrimVtx;
  //FairStsKFTrackFitter fStsFitter;

 private:
  // Some constants                                                             
  static const Double_t fgkChi2Cut; ///< max accepted Chi2 of hit for track     

  ClassDef(MpdEctTrackFinderTofTpc,1);

};

#endif
