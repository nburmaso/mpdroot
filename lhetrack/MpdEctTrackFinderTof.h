
#ifndef MPD_ECTTRACKFINDERTOF_H
#define MPD_ECTTRACKFINDERTOF_H

#include "FairTask.h"
//#include "FairStsKFTrackFitter.h"
//#include "TLorentzVector.h"
#include "TH1.h"
#include <map>

class MpdKalmanHit;
class MpdKalmanTrack;
class MpdEtofHit;
class MpdEctKalmanTrack;
class FairGeoTransform;
class TClonesArray;
class TVector2;

class MpdEctTrackFinderTof :public FairTask
{
 public:

  /** Constructor **/
  MpdEctTrackFinderTof(const char *name="MpdEctTrackFinderTof", Int_t iVerbose = 1 );
  
  /** Destructor **/
  virtual ~MpdEctTrackFinderTof();
  
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

  Bool_t Tpc() { return fTpc; } ///< TPC flag (if TRUE - reco after TPC to ECT reco)
  void SetTpc(Bool_t tpc) { fTpc = tpc; } ///< set TPC flag (if TRUE - reco after TPC to ECT reco)

 private:
  // Some constants
  static const Double_t fgkChi2Cut; ///< max accepted Chi2 of hit for track
  static const Int_t fgkNlays = 60 * 2, fgkNtube = 700, fgkNlays2 = 60; // nLaysMax, nTubesMax

 private:

  void InitGeo(); ///< initialize detector geometry                                  
  Int_t GetNofHitsInLayer(Int_t lay) { return (Int_t)fhLays->GetCellContent(lay+1,0); }
  Int_t GetHitsInLayer(Int_t lay) { return fLayPointers[lay]; } ///< first index of hits in layer
  void MatchEtof(); ///< match tracks with ETOF hits                         
  void GetTrackSeeds(Int_t iPass); // build track seeds                         
  void DoTracking(Int_t iPass); // run tracking                                 
  void MakeKalmanHits(); // create Kalman hits                                  
  void EvalParams(const MpdEtofHit *tof, const MpdKalmanHit *ect, MpdEctKalmanTrack *track, 
		  Double_t rEct, Double_t phEct); // evaluate params 
  void EvalCovar(const MpdEtofHit *tof, const MpdKalmanHit *ect, MpdEctKalmanTrack *track,
		 Double_t rEct, Double_t phEct); // eval. covariance
  Int_t RunKalmanFilter(MpdEctKalmanTrack *track, Int_t layBeg); ///< run Kalman filter       
  void RemoveDoubles(); ///< remove double tracks
  Int_t NofCommonHits(MpdEctKalmanTrack* track, MpdEctKalmanTrack* track1); ///< get number of common hits
  void AddHits(); // add hit objects to tracks                                  
  Int_t HitMotherId(MpdKalmanHit* hit, Int_t idM, Int_t &id1); // check if hit has the same mother ID as idM
  void SetTrackID(MpdEctKalmanTrack *track); // set track ID from IDs of its hits
  void SelectTracks(Int_t iPass); ///< do track selection and compute shared hit multiplicities
  void StoreTracks(Int_t iPass); // transfer tracks from fTrackCand to fTracks             
  void ExcludeHits() { }; // exclude used hits
  void Write();
  void Writedir2current( TObject *obj );
  Double_t Proxim(MpdKalmanTrack *track, const MpdKalmanHit *hit); ///< adjust R-Phi coord. for continuity
  TVector2 GetDistance(MpdEctKalmanTrack *track, MpdKalmanHit *hit); ///< hit-track distance
  void MatchTpc(); ///< match tracks with TPC tracks                         
  void PassWall(MpdEctKalmanTrack *track, Double_t thick); ///< pass TPC end-plate
  void GoToBeamLine(); // propagate tracks to the beam line
  Double_t CorrectForLoss(Double_t pt, Double_t the, Int_t id); // correct for dE loss in pipe
  Int_t GetParticleId(Int_t id); ///< particle ID for track id
  void MergeWithTpc(MpdEctKalmanTrack *track); ///< merge track with TPC track
  void Smooth(); ///< primary vertex constraints
  void GoOutward(); ///< go outward

  Int_t fNPass; 
  Bool_t fTpc;
  //Int_t fNofLays; //!< actual number of detector layers
  Int_t fNofTubes; //!< actual number of tubes in a layer
  Double_t frMinMax[3]; //!< min / max radii of ECT and tube radius
  Double_t fZplanes[fgkNlays];
  TDirectory *fHistoDir;
  TH1F *fhNBranches;
  Int_t fExact; //!< exact match flag (if 1 match IDs)

  TClonesArray *fEctHits; //! ECT hits
  TClonesArray *fTofHits; //! TOF hits
  TClonesArray *fTofPoints; //! TOF points
  TClonesArray *fKHits; //!< array of Kalman hits
  TClonesArray *fMCTracks; //!< array of MC tracks
  //TClonesArray *fSTSTrackMatch;
  TClonesArray *fTracks; //! ECT tracks
  TClonesArray *fTpcTracks; //! TPC tracks
  TClonesArray *fTrackCand; //! ECT track candidates
  TClonesArray *fPrimVtx; //! event vertices
  Int_t *fLayPointers; //!< locations of hits from different layers             
  TH1F *fhLays; //!< histo with layer hit multiplicities                        

  FairGeoTransform *fTransf[fgkNlays][fgkNtube];
  std::map<Int_t,std::pair<Double_t,Double_t> > fWireMap;
  //FairVertex *fPrimVtx;
  //FairStsKFTrackFitter fStsFitter;

  ClassDef(MpdEctTrackFinderTof,1);

};

#endif
