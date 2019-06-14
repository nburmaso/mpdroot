/**
 * \class MpdFemtoBasicEventCut
 * \brief The basic cut for events
 *
 * Cuts on event multiplicity, z-vertex position, trigger and other event variables
 */

#ifndef MpdFemtoBasicEventCut_h
#define MpdFemtoBasicEventCut_h

// C++ headers
#include <vector>

// MpdFemtoMaker headers
// Base
#include "MpdFemtoBaseEventCut.h"
// Infrastructure
#include "MpdFemtoEvent.h"
#include "MpdFemtoString.h"

// ROOT headers
#include "TList.h"
#include "TString.h"

//_________________
class MpdFemtoBasicEventCut : public MpdFemtoBaseEventCut {

 public:
  /// Constructor
  MpdFemtoBasicEventCut();
  /// Copy contructor
  MpdFemtoBasicEventCut(const MpdFemtoBasicEventCut& c);
  /// Assignment operator
  MpdFemtoBasicEventCut& operator=(const MpdFemtoBasicEventCut& c);
  /// Destructor
  virtual ~MpdFemtoBasicEventCut();

  /// Check bad run flag
  void setCheckBadRun(bool check) { mCheckBadRun = check; }
  /// Set min and max acceptable event multiplicity
  void setEventMult(const int& lo, const int& hi)     { mRefMult[0] = lo; mRefMult[1] = hi; }
  /// Set min and max acceptable vertex z-coordinate
  void setVertZPos(const float& lo, const float& hi) { mVertZPos[0] = lo; mVertZPos[1] = hi; }
  /// Set min and max acceptable (Vz-VpdVz) values
  void setVpdVzDiff(const float& lo, const float& hi){ mVpdVzDiff[0] = lo; mVpdVzDiff[1] = hi; }
  /// Set x shift of primary vertex, min and max acceptrable
  /// radial position of the vertex
  void setVertXShift(const float& shift)             { mVertXShift = shift; }
  /// Set y shift of primary vertex, min and max acceptrable
  /// radial position of the vertex
  void setVertYShift(const float& shift)             { mVertYShift = shift; }
  void setVertRPos(const float& lo, const float& hi) { mVertRPos[0] = lo; mVertRPos[1] = hi; }
  /// Set min and max acceptable values of sphericity
  void setSphericity(const float& lo, const float& hi) { mSphericity[0] = lo; mSphericity[1] = hi; }
  /// Set min and max acceptable TOF tray multiplicity
  void setBTofTrayMult(const int& lo, const int& hi) { mBTofTrayMult[0] = lo; mBTofTrayMult[1] = hi; }
  /// Set min and max acceptable TOF-matched tracks
  void setBTofMatchMult(const int& lo, const int& hi){ mBTofMatch[0] = lo; mBTofMatch[1] = hi; }
  /// Number of events passed
  int nEventsPassed() const                          { return mNEventsPassed; }
  /// Number of events failed
  int nEventsFailed() const                          { return mNEventsFailed; }
  /// Add trigger to select
  void addTriggerId(const unsigned int& id);
  /// Add trigger to select
  void setTriggerId(const unsigned int& id)          { addTriggerId( id ); }
  /// Set the min and max allowed event plane angle
  void setEventPlaneAngle(const float& lo, const float& hi)  { mPsiEP[0] = lo; mPsiEP[1] = hi; }
  /// Set the min and max allowed event plane angle
  void setEPAngle(const float& lo, const float& hi)  { setEventPlaneAngle( lo, hi ); }

  virtual TList* appendSettings(TList*, const TString& prefix="") const;
  virtual MpdFemtoString report();
  virtual bool pass(const MpdFemtoEvent* event);

  virtual MpdFemtoBasicEventCut* clone() const
  { MpdFemtoBasicEventCut* c = new MpdFemtoBasicEventCut(*this); return c; }

 private:

  /// Check bad run flag. By default it is true (see constructor for more info)
  bool mCheckBadRun;
  /// Range of multiplicity
  short mRefMult[2];
  /// Range of z-position of vertex
  float mVertZPos[2];
  /// Range of (Vz - VpdVz) of vertex
  float mVpdVzDiff[2];
  /// Range of transverse positions of vertex and their shifts
  float mVertXShift;
  float mVertYShift;
  float mVertRPos[2];
  /// Range of min and max values of event sphericity
  float mSphericity[2];
  /// Range of TOF tray multiplicity
  short mBTofTrayMult[2];
  /// Range of TOF matched tracks (multplicity)
  short mBTofMatch[2];
  // Range of centralities
  short mCent9[2];
  /// Range of vzero ep angle
  float mPsiEP[2];
  /// Number of events checked by this cut that passed
  unsigned int mNEventsPassed;
  /// Number of events checked by this cut that failed
  unsigned int mNEventsFailed;
  /// If set, only given trigger will be selected
  std::vector<unsigned int> mTriggersToSelect;

  /// Method that looks at the bad run lists
  bool isInBadRunList(int runNumber);

  static const std::vector<unsigned int> bad_run_list_7gev;
  static const std::vector<unsigned int> bad_run_list_11gev;
  static const std::vector<unsigned int> bad_run_list_14gev;
  static const std::vector<unsigned int> bad_run_list_19gev;
  static const std::vector<unsigned int> bad_run_list_27gev;
  static const std::vector<unsigned int> bad_run_list_39gev;
  static const std::vector<unsigned int> bad_run_list_62gev;
  static const std::vector<unsigned int> bad_run_list_200gev;

#ifdef __ROOT__
  ClassDef(MpdFemtoBasicEventCut, 1);
#endif
};

#endif // MpdFemtoBasicEventCut_h
