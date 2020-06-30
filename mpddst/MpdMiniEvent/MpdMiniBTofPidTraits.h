/**
 * Hold information about BTOF-matched tracks
 *
 * The class stores information about tracks that matched
 * the Barrel Time-of-Flight detector
 * 
 * \author Grigory Nigmatkulov (NRNU MEPhI), Pavel Batyuk (JINR)
 * \email ganigmatkulov@mephi.ru ; nigmatkulov@gmail.com ; pavel.batyuk@jinr.ru
 * \date May 01, 2020
 */

#ifndef MpdMiniBTofPidTraits_h
#define MpdMiniBTofPidTraits_h

// C++ headers
#include <limits>

// ROOT headers
#include "TObject.h"
#include "TVector3.h"

//_________________
class MpdMiniBTofPidTraits : public TObject {

 public:
  /// Default constructor
  MpdMiniBTofPidTraits();
  /// Copy constructor
  MpdMiniBTofPidTraits(const MpdMiniBTofPidTraits &traits);
  /// Destructor
  virtual ~MpdMiniBTofPidTraits();
  /// Print TOF PID traits information
  virtual void Print(const Char_t* option = "") const;

  //
  // Getters
  //

  /// Return index of the associated track
  Int_t   trackIndex() const     { return mTrackIndex; }
  /// Return index of the associated hit
  Int_t   hitIndex() const     { return mHitIndex; }
  /// Return beta (compression = beta * 20000)
  Float_t btofBeta() const       { return (Float_t)mBTofBeta / 20000.f; }
  /// Return length
  Float_t length() const   { return fLength; }
  
  //
  // Setters
  //

  /// Set associated track index
  void setTrackIndex(Int_t idx) { mTrackIndex = (idx > std::numeric_limits<short>::max()) ? -1 : (Short_t)idx; }
  /// Set associated tof hit index
  void setHitIndex(Int_t idx) { mHitIndex = (idx > std::numeric_limits<short>::max()) ? -1 : (Short_t)idx; }
  /// Set beta
  void setBeta(Float_t beta);
  /// Set length
  void setLength(Float_t length) { fLength = length; }
  
 private:

  /// Index to the associated track in the event
  Short_t  mTrackIndex; 
  /// Index to the associated hit in the event
  Short_t  mHitIndex;   
  /// Beta * 20000
  UShort_t  mBTofBeta;
  /// Track length
  Float_t fLength;
  
  ClassDef(MpdMiniBTofPidTraits, 1);
};

#endif // #define MpdMiniBTofPidTraits_h
