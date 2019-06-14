/**
 * \class MpdFemtoQinvCorrFctnKt
 * \brief One-dimensional correlation function with kT-binning
 *
 * One-dimensional correlation function with correlation functions binned
 * according to the user request
 */

#ifndef MpdFemtoQinvCorrFctnKt_h
#define MpdFemtoQinvCorrFctnKt_h

// MpdFemtoMaker headers
#include "MpdFemtoBaseCorrFctn.h"
#include "MpdFemtoBasePairCut.h"

// ROOT headers
#include <TH1F.h>
#include <TString.h>

// C++ headers
#include <cstdio>
#include <sstream>

//_________________
class MpdFemtoQinvCorrFctnKt : public MpdFemtoBaseCorrFctn {

 public:
  /// Constructor
  MpdFemtoQinvCorrFctnKt(char* title,
		 const int& nbins=80, const float& QinvLo=0., const float& QinvHi=0.8,
		 const int& nCFs=20, const float& KtLo=0., const float& KtHi=1.);
  /// Copy constructor
  MpdFemtoQinvCorrFctnKt(const MpdFemtoQinvCorrFctnKt& copy);
  /// Assignment operator
  MpdFemtoQinvCorrFctnKt& operator=(const MpdFemtoQinvCorrFctnKt& copy);
  /// Destructor
  virtual ~MpdFemtoQinvCorrFctnKt();

  /// Make report
  virtual MpdFemtoString report();
  /// Add real pair
  virtual void addRealPair(MpdFemtoPair* pair);
  /// Add mixed pair
  virtual void addMixedPair(MpdFemtoPair* pair);

  /// Begin event
  virtual void eventBegin(const MpdFemtoEvent* event);
  /// Event end
  virtual void eventEnd(const MpdFemtoEvent* event);
  /// Finish
  virtual void finish();

  /// Write histograms
  void writeOutHistos();

  /// Return output list
  virtual TList* getOutputList();

  /// Clone correlation function
  virtual MpdFemtoQinvCorrFctnKt* clone() const { return new MpdFemtoQinvCorrFctnKt(*this); }

 private:

  // TODO: probably move arrays to STL vectors

  /// Numerators
  TH1F* mNumerator[20];
  /// Denominators
  TH1F* mDenominator[20];

  /// Number of kT bins to study
  int mNumberKt;
  /// Low kT of the pair value
  float mKtMin;
  /// High kT of the pair value
  float mKtMax;

  /// Pointer to the current histogram
  int* mIndexKt;
  /// kT bin width
  float mDeltaKt;

#ifdef __ROOT__
  ClassDef(MpdFemtoQinvCorrFctnKt, 1)
#endif
};

#endif // #define MpdFemtoQinvCorrFctnKt_h
