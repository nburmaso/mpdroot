/**
 * \class MpdFemtoEventCutMonitor
 * \brief Event cut monitor for basic analysis
 *
 * The class provides histograms for monitoring event cuts
 */

#ifndef MpdFemtoEventCutMonitor_h
#define MpdFemtoEventCutMonitor_h

// MpdFemtoMaker headers
// Base
#include "MpdFemtoBaseCutMonitor.h"
// Infrastructure
#include "MpdFemtoTypes.h"
#include "MpdFemtoEvent.h"

// ROOT headers
#include "TH1F.h"
#include "TH2F.h"
#include "TProfile.h"

//_________________
class MpdFemtoEventCutMonitor : public MpdFemtoBaseCutMonitor{

 public:
  /// Constructor
  MpdFemtoEventCutMonitor();
  /// Constructor with parameters
  MpdFemtoEventCutMonitor(const char* TitCutMoni, const char* title);
  /// Copy constructor
  MpdFemtoEventCutMonitor(const MpdFemtoEventCutMonitor& c);
  /// Assignment operator
  MpdFemtoEventCutMonitor operator=(const MpdFemtoEventCutMonitor& c);
  /// Destructor
  virtual ~MpdFemtoEventCutMonitor();

  /// Construct report
  virtual MpdFemtoString report();
  /// Fill information with event
  virtual void fill(const MpdFemtoEvent* event);
  /// Finish
  virtual void finish();

  /// Write all histograms
  void writeOutHistos();

  /// Ouput list with histograms
  virtual TList* getOutputList();

  // These dummy fill() functions were introduced to remove a compiler
  // warning related to overloaded base-class Fill() functions being
  // hidden by a single version of fill() in this derived class
  void fill(const MpdFemtoTrack* /* d */) {;}
  void fill(const MpdFemtoV0* /* d */)    {;}
  void fill(const MpdFemtoXi* /* xi */)   {;}
  void fill(const MpdFemtoKink* /* d */)  {;}
  void fill(const MpdFemtoPair* /* d */)  {;}
  void fill(const MpdFemtoParticleCollection* /* d */) {;}
  void fill(const MpdFemtoEvent* /* d1 */, const MpdFemtoParticleCollection* /* d2 */) {;}
  void fill(const MpdFemtoParticleCollection* /* d1 */, const MpdFemtoParticleCollection* /* d2 */) {;}

  /// Return y vs. x position of the primary vertex
  TH2F* vertexYvsVertexX() { return (mVertexYvsVertexX) ? mVertexYvsVertexX : nullptr; }
  /// Return y vs. x position of the primary vertex
  TH2F* VertexYvsVertexX() { return vertexYvsVertexX(); }
  /// Return z position of the primary vertex
  TH1F* vertexZ()          { return (mVertexZ) ? mVertexZ : nullptr; }
  /// Return z position of the primary vertex
  TH1F* VertexZ()          { return vertexZ(); }
  /// Return reference multiplicity
  TH1F* refMult()          { return (mRefMult) ? mRefMult : nullptr; }
  /// Return reference multiplicity
  TH1F* RefMult()          { return refMult(); }
  /// Return difference between Vz reconstructed by TPC and by VPD
  TH1F* vpdVzDiff()        { return (mVpdVzDiff) ? mVpdVzDiff : nullptr; }
  /// Return difference between Vz reconstructed by TPC and by VPD
  TH1F* VpdVzDiff()        { return vpdVzDiff(); }
  /// Return BTof tray multiplicity vs. reference multiplicity
  TH2F* btofMultVsRefMult()  { return (mBTofTrayMultVsRefMult) ? mBTofTrayMultVsRefMult : nullptr; }
  /// Return BTof tray multiplicity vs. reference multiplicity
  TH2F* BTofMultVsRefMult()  { return btofMultVsRefMult(); }
  /// Return TOF-matched multiplicity vs. reference multiplicity
  TH2F* btofMatchedVsRefMult() { return (mBTofMatchedVsRefMult) ? mBTofMatchedVsRefMult : nullptr; }
  /// Return TOF-matched multiplicity vs. reference multiplicity
  TH2F* BTofMatchedVsRefMult() { return btofMatchedVsRefMult(); }

 private:

  /// Primary vertex z
  TH1F* mVertexZ;
  /// Reference multiplicity
  TH1F* mRefMult;
  /// Primary vertex Y vs. X
  TH2F* mVertexYvsVertexX;
  /// Primary vertex Z: Vz(TPC)-Vz(VPD)
  TH1F* mVpdVzDiff;
  /// Barrel TOF tray multiplicity vs. reference multiplicity
  TH2F* mBTofTrayMultVsRefMult;
  /// TOF-matched multiplicity vs. reference multiplicity
  TH2F* mBTofMatchedVsRefMult;

#ifdef __ROOT__
 ClassDef(MpdFemtoEventCutMonitor, 2)
#endif
};

#endif // #define MpdFemtoEventCutMonitor_h
