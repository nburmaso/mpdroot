/**
 * Holds information about ECal tower
 *
 * The class holds information about the tower from
 * the Barrel Electromagnetic Calorimeter (ECal)
 *
 */

#ifndef MpdMiniBECalHit_h
#define MpdMiniBECalHit_h

// ROOT headers
#include "TObject.h"
#include "TMath.h"

//_________________
class MpdMiniBECalHit : public TObject {

 public:
  /// Default constructor
  MpdMiniBECalHit();
  /// Copy constructor
  MpdMiniBECalHit(const MpdMiniBECalHit &hit);
  /// Destructor
  virtual ~MpdMiniBECalHit();
  /// Print tower information
  virtual void Print(const Char_t* option = "") const;

  //
  // Getters
  //

  /// Return sector index
  UShort_t sector() const      { return fSectorId; }
  /// Return row index
  UShort_t row() const         { return fRowId; }
  /// Return module index
  UShort_t module() const      { return fModuleId; }
  /// Return energy of the tower
  Float_t energy() const       { return fEnergy; }
  /// Return hit mean time
  Float_t time() const         { return fTime; }
  /// Return associated track index. -1 in several.
  Short_t trackId() const      { return fTrackId; }
  /// Return flag
  Int_t flag() const           { return (Int_t)fFlag; }
  /// Return number of tracks that deposited energy in the tower
  Int_t numberOfTracks() const { return (Int_t)fNumOfTracks; }
  /// Return rho-position of the module center
  Float_t rhoCenter() const    { return fRhoCenter; }
  /// Return z-position of the module center
  Float_t zCenter() const      { return fZCenter; }
  /// Return phi-position of the module center
  Float_t phiCenter() const    { return TMath::RadToDeg() * fPhiCenter; }
  /// Return theta-position of the module center
  Float_t thetaCenter() const  { return fThetaCenter; }

  //
  // Setters
  //

  /// Set sector ID
  void setSectorId(Int_t id)      { fSectorId = id; }
  /// Set row ID
  void setRowId(Int_t id)         { fRowId = id; }
  /// Set module ID
  void setModuleId(Int_t id)      { fModuleId = id; }
  /// Set tower energy
  void setEnergy(Float_t energy)  { fEnergy = energy; }
  /// Set hit mean time
  void setTime(Float_t t)         { fTime = t; }
  /// Set associated track index. -1 if several
  void setTrackId(Int_t id);
  /// Set flag
  void setFlag(Int_t flag);
  /// Set number of associated tracks
  void setNumberOfTracks(Int_t num);
  /// Set rho-coordinate of the module center
  void setRhoCenter(Float_t rho)  { fRhoCenter = rho; }
  /// Set z-coordinate of the module center
  void setZCenter(Float_t z)      { fZCenter = z; }
  /// Set phi-coordinate of the module center
  void setPhiCenter(Float_t phi)  { fPhiCenter = TMath::DegToRad() * phi; }
  /// Set theta-coordinate of the module center
  void setThetaCenter(Float_t t)  { fThetaCenter = t; }

 protected:

  // The three indices below are useless and should be replaced
  // with normal encoding when the numbering scheme will be known

  /// Sector ID (0-25)
  UShort_t fSectorId;
  /// Row ID (0-300)
  UShort_t fRowId;
  /// Module ID (0-128)
  UShort_t fModuleId;

  /// Energy deposited in the tower
  Float16_t fEnergy;
  /// Hit mean time
  Float16_t fTime;

  /// Associated track index (only reconstructed one).
  /// -1 if more than one track in module.
  Short_t fTrackId;
  /// Flag for general purposes [TDC, event tagging...]
  Char_t fFlag;
  /// Number of tracks deposited energy in the tower
  UChar_t fNumOfTracks;

  /// Rho-coordinate of the module center
  Float16_t fRhoCenter;
  /// Z-coordinate of the module center
  Float16_t fZCenter;
  /// Phi-angle of the module center (in radians)
  Float16_t fPhiCenter;  //[-pi,pi,20]
  /// Theta-angle of the module center
  Float16_t fThetaCenter;

  ClassDef(MpdMiniBECalHit, 2)
};

#endif // #define MpdMiniBECalHit_h
