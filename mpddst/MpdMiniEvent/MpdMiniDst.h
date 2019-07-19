/**
 * \class MpdMiniDst
 * \brief Main class that keeps TClonesArrays with objects
 *
 * The MpdMiniDst class holds pointers to the miniArrays with all data objects.
 *
 * \author Grigory Nigmatkulov
 * \email nigmatkulov@gmail.com ; ganigmatkulov@mephi.ru
 * \date July 11, 2019
 */

#ifndef MpdMiniDst_h
#define MpdMiniDst_h

// ROOT headers
#include "TClonesArray.h"

// MiniDst headers
#include "MpdMiniArrays.h"

// Forward declarations
class MpdMiniEvent;
class MpdMiniTrack;
class MpdMiniBTofHit;
class MpdMiniBTofPidTraits;
class MpdMiniBECalHit;
class MpdMiniBECalPidTraits;
class MpdMiniTrackCovMatrix;
class MpdMiniMcEvent;
class MpdMiniMcTrack;

//_________________
class MpdMiniDst {

 public:

  /// Default constructor
  MpdMiniDst() { /* emtpy */}
  /// Destructor
  ~MpdMiniDst() { /* empty*/ }

  /// Set the pointers to the TClonesArrays
  static void set(TClonesArray**);
  /// Reset the pointers to the TClonesArrays to 0
  static void unset();
  /// Return pointer to the n-th TClonesArray
  static TClonesArray* miniArray(Int_t type) { return miniArrays[type]; }

  /// Return pointer to current MpdMiniEvent (class holding the event wise information)
  static MpdMiniEvent* event() { return (MpdMiniEvent*)miniArrays[MpdMiniArrays::Event]->UncheckedAt(0); }
  /// Return pointer to i-th track
  static MpdMiniTrack* track(Int_t i) { return (MpdMiniTrack*)miniArrays[MpdMiniArrays::Track]->UncheckedAt(i); }
  /// Return pointer to i-th btof hit
  static MpdMiniBTofHit* btofHit(Int_t i) { return (MpdMiniBTofHit*)miniArrays[MpdMiniArrays::BTofHit]->UncheckedAt(i); }
  /// Return pointer to i-th btof pidTraits
  static MpdMiniBTofPidTraits* btofPidTraits(Int_t i) { return (MpdMiniBTofPidTraits*)miniArrays[MpdMiniArrays::BTofPidTraits]->UncheckedAt(i); }
  /// Return pointer to i-th ECal (barrel electromagnetic calorimeter) tower (hit)
  static MpdMiniBECalHit* becalHit(Int_t i) { return (MpdMiniBECalHit*)miniArrays[MpdMiniArrays::BECalHit]->UncheckedAt(i); }
  /// Return pointer to i-th emc pidTraits
  static MpdMiniBECalPidTraits* becalPidTraits(Int_t i) { return (MpdMiniBECalPidTraits*)miniArrays[MpdMiniArrays::BECalPidTraits]->UncheckedAt(i); }
  /// Return pointer to i-th track covariance matrix
  static MpdMiniTrackCovMatrix* trackCovMatrix(Int_t i) { return (MpdMiniTrackCovMatrix*)miniArrays[MpdMiniArrays::TrackCovMatrix]->UncheckedAt(i); }
  /// Return pointer to current MpdMiniMcEvent (class holding the event wise information of the orignial MC event)
  static MpdMiniMcEvent* mcEvent() { return ( (MpdMiniMcEvent*)miniArrays[MpdMiniArrays::McEvent]->UncheckedAt(0) ) ? 
                                              (MpdMiniMcEvent*)miniArrays[MpdMiniArrays::McEvent]->UncheckedAt(0) : nullptr; }
  /// Return pointer to i-th MC track
  static MpdMiniMcTrack* mcTrack(Int_t i) { return ( (MpdMiniMcTrack*)miniArrays[MpdMiniArrays::McTrack]->UncheckedAt(i) ) ? 
                                                     (MpdMiniMcTrack*)miniArrays[MpdMiniArrays::McTrack]->UncheckedAt(i) : nullptr; }

  /// Return number of tracks
  static UInt_t numberOfTracks() { return miniArrays[MpdMiniArrays::Track]->GetEntries(); }
  /// Return number of BTof hits
  static UInt_t numberOfBTofHits() { return miniArrays[MpdMiniArrays::BTofHit]->GetEntries(); }
  /// Return number of BTof PID traits
  static UInt_t numberOfBTofPidTraits() { return miniArrays[MpdMiniArrays::BTofPidTraits]->GetEntries(); }
  /// Return number of ECal hits
  static UInt_t numberOfBECalHits() { return miniArrays[MpdMiniArrays::BECalHit]->GetEntries(); }
  /// Return number of ECal PID traits
  static UInt_t numberOfBECalPidTraits() { return miniArrays[MpdMiniArrays::BECalPidTraits] ->GetEntries(); }
  /// Return number of track covariant matrices
  static UInt_t numberOfTrackCovMatrices() { return miniArrays[MpdMiniArrays::TrackCovMatrix]->GetEntries(); }
  /// Return number of tracks
  static UInt_t numberOfMcTracks() { return miniArrays[MpdMiniArrays::McTrack]->GetEntries(); }

  /// Print information
  void printEvent() const;
  /// Print track info
  static void printTracks();
  /// Print BTOF hit info
  static void printBTofHits();
  /// Print BTOF PID trait info
  static void printBTofPidTraits();
  /// Print ECal hit info
  static void printBECalHits();
  /// Print BEMC PID trait info
  static void printBECalPidTraits();
  /// Print track covariance matrix info
  static void printTrackCovMatrices();
  /// Print MC track info
  static void printMcTracks();

 private:

  /// Array of TClonesArrays
  static TClonesArray** miniArrays;
};

#endif // #define MpdMiniDst_h
