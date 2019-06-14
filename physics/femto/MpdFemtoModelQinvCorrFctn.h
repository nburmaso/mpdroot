/**
 * \class MpdFemtoModelQinvCorrFctn
 * \brief One-dimensional correlation function for the model estimations
 *
 * One-dimensional correlation function which will store numerators (from real events)
 * and denominatros (from mixed events) with and without femtoscopic weights
 */

#ifndef MpdFemtoModelQinvCorrFctn_h
#define MpdFemtoModelQinvCorrFctn_h

// MpdFemtoMaker headers
// Base
#include "MpdFemtoBaseCorrFctn.h"

// Forward declarations
class MpdFemtoPair;
class MpdFemtoModelManager;
class TH1D;

//_________________
class MpdFemtoModelQinvCorrFctn: public MpdFemtoBaseCorrFctn {

 public:
  /// Default constructor
  MpdFemtoModelQinvCorrFctn();
  /// Parametrized constructor
  MpdFemtoModelQinvCorrFctn(const char *title, const Int_t& nbins,
                         const Double_t& qInvLow, const Double_t& qInvHi);
  /// Copy constructor
  MpdFemtoModelQinvCorrFctn(const MpdFemtoModelQinvCorrFctn& corrFctn);
  /// Assignment operator
  MpdFemtoModelQinvCorrFctn& operator=(const MpdFemtoModelQinvCorrFctn& corrFctn);
  /// Destructor
  virtual ~MpdFemtoModelQinvCorrFctn();

  /// Method that allows front-loading model manager
  virtual void connectToManager(MpdFemtoModelManager *manager);

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
  virtual void writeOutHistos();

  /// Return output list
  virtual TList* getOutputList();
  /// Clone correlation function
  virtual MpdFemtoModelQinvCorrFctn* clone() const { return new MpdFemtoModelQinvCorrFctn(*this); }

 protected:

  /// Pointer to the model manager that performs femtoscopic
  /// weight calculation
  MpdFemtoModelManager *mManager;

  /// Numerator made with pairs from the same event with femtoscopic weight
  TH1D *mNumeratorWeighted;
  /// Numerator made with pairs from the same event without weights
  TH1D *mNumerator;
  /// Denominator made with mixed pairs with femtoscopic weight
  TH1D *mDenominatorWeighted;
  /// Denominator made with mixed pairs without weights
  TH1D *mDenominator;

#ifdef __ROOT__
  ClassDef(MpdFemtoModelQinvCorrFctn, 1);
#endif
};

#endif // MpdFemtoModelQinvCorrFctn_h
