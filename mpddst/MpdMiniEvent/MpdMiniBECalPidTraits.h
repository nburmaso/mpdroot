/**
 * Holds information about ECal-matched tracks
 *
 * Keep information about Barrel ElectroMagnetic Calorimeter (ECal)
 * matched tracks.
 *
 * \author Grigory Nigmatkulov (NRNU MEPhI), Pavel Batyuk (JINR)
 * \email nigmatkulov@gmail.com; ganigmatkulov@mephi.ru; pavel.batyuk@jinr.ru
 * \date April 9, 2020
 **/

#ifndef MpdMiniBECalPidTraits_h
#define MpdMiniBECalPidTraits_h

// C++ headers
#include <limits>

// ROOT headers
#include <TObject.h>

//_________________
class MpdMiniBECalPidTraits: public TObject {

 public:
  /// Default constructor
  MpdMiniBECalPidTraits();
  /// Constructor that fills the parameters accordingly to the input
  MpdMiniBECalPidTraits(Int_t index);
  /// Copy constructor
  MpdMiniBECalPidTraits(const MpdMiniBECalPidTraits &traits);
  /// Destructor
  virtual ~MpdMiniBECalPidTraits();
  /// Print BEMC PID traits information
  virtual void Print(const Char_t* option = "") const;

  //
  // Getters
  //

  /// Return track index
  Int_t   trackIndex() const     { return (Int_t)mTrackIndex; }

  //
  // Setters
  //

  /// Set track index of the assiciated track
  void setTrackIndex(Int_t idx)  { mTrackIndex = (idx > std::numeric_limits<short>::max()) ? -1 : (Short_t)idx; }

 private:

  /// Index to the associated track in the event
  Short_t  mTrackIndex;

  ClassDef(MpdMiniBECalPidTraits, 2);
};

#endif // #define MpdMiniBECalPidTraits_h
