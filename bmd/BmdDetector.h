//-----------------------------------------------------------
// File and Version Information:
// $Id$
//
// Description:
//      Detector class - sensitive detector for VMC
//
// Environment:
//      Software developed for the PANDA Detector at FAIR.
//
// Author List:
//     Pedro Gonzalez Zamora (original author)
//
//-----------------------------------------------------------

#ifndef BMDDETECTOR_HH
#define BMDDETECTOR_HH

// Base Class Headers ----------------
#include "FairDetector.h"

// Collaborating Class Headers -------
#include "TGeoMedium.h"
#include "TVector3.h"
#include <TLorentzVector.h>
#include <map>
#include <ostream> // remove if you do not need streaming op

using namespace std;

// Collaborating Class Declarations --
class TClonesArray;
class FairVolume;
class BmdPoint;

class BmdDetector : public FairDetector {

 public:
  // Constructors/Destructors ---------

  /**      default constructor    */
  BmdDetector();

  /**      Name :  Detector Name
           Active: kTRUE for active detectors
           (ProcessHits() will be called)
           kFALSE for inactive detectors     */
  BmdDetector(const char *Name, Bool_t Active);

  /**       destructor     */
  virtual ~BmdDetector();

  /**      Initialization of the detector is done here    */
  // virtual void   Initialize();

  /**       this method is called for each step
            during simulation (see FairMCApplication::Stepping())    */
  virtual Bool_t ProcessHits(FairVolume *v = 0);

  /**       Registers the produced collections in CBMRootManager.     */
  virtual void Register();

  virtual void Print() const;

  /** Gets the produced collections */
  virtual TClonesArray *GetCollection(Int_t iColl) const;

  /**      has to be called after each event
           to reset the containers        */
  virtual void Reset();

  /**      Create the detector geometry        */
  virtual void ConstructGeometry();

  // Construct the geometry from an ASCII geometry file
  virtual void ConstructAsciiGeometry();

  // Construct the geometry from a GDML geometry file
  virtual void ConstructGDMLGeometry();
  void ExpandNodeForGdml(TGeoNode *node);
  map<TString, TGeoMedium *>
    fFixedMedia; // List of media "repaired" after importing GMDL

  // Check whether a volume is sensitive.
  // The decision is based on the volume name. Only used in case
  // of GDML and ROOT geometry.
  // @param name    Volume name
  // @value         kTRUE if volume is sensitive, else kFALSE
  virtual Bool_t CheckIfSensitive(std::string name);

  void EndOfEvent();

  BmdPoint *AddHit(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom,
                   Double_t time, Double_t length, Double_t eLoss, Int_t BmdID, Int_t RingID, Int_t CellID);

  Int_t GetVolumeID(TString volname);
  Int_t GetBmdID(TString volname);
  Int_t GetRingID(TString volname);
  Int_t GetCellID(TString volname);

 private:

  TClonesArray *fBmdPointCollection;
  Int_t fTrackID;       //!  track index
  Int_t fVolumeID;      //!  volume id
  Int_t fBmdID;          //!  number of detector 1 or 2
  Int_t fRingID;         //!  ring id
  Int_t fCellID;         //!  cell id in ring
  TLorentzVector fPos;  //!  position
  TLorentzVector fMom;  //!  momentum
  Double_t fTime;       //!  time
  Double_t fLength;     //!  length
  Double_t fELoss;      //!  energy loss
  Int_t currentTrackID; //!
  Int_t currentEvent;   //! current event
  Bool_t fNewTrack;
  //  const double nan; //!

  void ResetParameters();

  ClassDef(BmdDetector, 1)
};

//--------------------------------------------------------------------
inline void BmdDetector::ResetParameters() {
  fTrackID = fVolumeID = 0;
  /* fPos.SetXYZM(nan, nan, nan, nan); */
  /* fMom.SetXYZM(nan, nan, nan, nan); */

  fPos.SetXYZM(0., 0., 0., 0.);
  fMom.SetXYZM(0., 0., 0., 0.);
  fTime = fLength = fELoss = 0.;
  // fPosIndex = 0;
};
  //-------------------------

#endif

