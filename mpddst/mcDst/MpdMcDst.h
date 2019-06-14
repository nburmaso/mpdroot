/**
 * \class McDst
 * \brief Monte Carlo DST format holder
 *
 * The class McDst holds the Monte Carlo data structure and access
 * methods to the branches
 */

#ifndef MpdMcDst_h
#define MpdMcDst_h

// ROOT headers
#include "TClonesArray.h"

// McDst headers
#include "MpdMcArrays.h"

// Forward declarations
class MpdMcEvent;
class MpdMcParticle;

//________________
class MpdMcDst {

 public:
  /// Default constructor
  MpdMcDst()  { /* emtpy */ }
  /// Destructor
  virtual ~MpdMcDst();

  /// Set pointers to the TClonesArrays
  static void set(TClonesArray** array);
  /// Reset pointers to the TClonesArrays to 0
  static void unset();
  /// Return pointer to the i-th TClonesArray
  static TClonesArray* mcArray(Int_t type) { return mcArrays[type]; }
  /// Return pointer to the McEvent information
  static MpdMcEvent* event() { return (MpdMcEvent*)mcArrays[MpdMcArrays::Event]->UncheckedAt(0); }
  /// Return pointer to the i-th particle
  static MpdMcParticle* particle(Int_t i) { return (MpdMcParticle*)mcArrays[MpdMcArrays::Particle]->UncheckedAt(i); }
  /// Return number of particles in the current events
  static UInt_t numberOfParticles() { return mcArrays[MpdMcArrays::Particle]->GetEntries(); }

  /// Print information
  static void print();
  /// Print event information
  static void printEventInfo();
  /// Print particle information
  static void printParticles();

 private:
  /// Array of TClonesArrays
  static TClonesArray** mcArrays;

//#ifdef __ROOT__
  ClassDef(MpdMcDst, 0)
//#endif
};

#endif // #define McDst_h
