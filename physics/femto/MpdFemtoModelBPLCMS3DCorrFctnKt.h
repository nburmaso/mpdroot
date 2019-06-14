/**
 * \class MpdFemtoModelBPLCMS3DCorrFctnKt
 * \brief Three-dimensional Bertsch-Pratt correlation function in LCMS for the model estimations
 *
 * Three-dimensional Bertsch-Pratt correlation function which will store
 * numerators (from same event) and denominatros (from mixed events) with
 * and without femtoscopic weights
 */

#ifndef MpdFemtoModelBPLCMS3DCorrFctnKt_h
#define MpdFemtoModelBPLCMS3DCorrFctnKt_h

// MpdFemtoMaker headers
// Base
#include "MpdFemtoBaseCorrFctn.h"

// ROOT headers
#include "TH3.h"
#include <TGraphErrors.h>

// Forward declarations
class MpdFemtoPair;
class MpdFemtoModelManager;

//_________________

class MpdFemtoModelBPLCMS3DCorrFctnKt : public MpdFemtoBaseCorrFctn {
public:
    /// Parametrized constructor
    ///
    /// \param title  Name of the histogram
    /// \param nBins  Number of bins (will be used for out, side and long projections)
    /// \param qLo    Minimum value of the q
    /// \param qHi    Maximum value of the q
    /// \param ktBins Number of kT bins used in the analysis
    /// \param ktLo   Minimum value of kT
    /// \param ktHi   Maximum value of kT
    ///
    MpdFemtoModelBPLCMS3DCorrFctnKt(const char* title = "hBPLCMSCorrFctn",
            const int& nBins = 80, const double& qLo = -0.4, const double& qHi = 0.4,
            const int& ktBins = 10, const double& ktLo = 0.05, const double& ktHi = 1.05);
    /// Copy constructor
    MpdFemtoModelBPLCMS3DCorrFctnKt(const MpdFemtoModelBPLCMS3DCorrFctnKt& corrFctn);
    /// Assignment operator
    MpdFemtoModelBPLCMS3DCorrFctnKt& operator=(const MpdFemtoModelBPLCMS3DCorrFctnKt& corrFctn);
    /// Destructor
    virtual ~MpdFemtoModelBPLCMS3DCorrFctnKt();

    /// Method that allows front-loading model manager
    virtual void connectToManager(MpdFemtoModelManager *manager);

    /// Set histogram parameters (nbins, qLow, qHi)
    void setHistoParameters(const int& nBins = 80, const float& qLo = -0.4, const float& qHi = 0.4);
    /// Set kT range (nbins, ktLow, ktHi)
    void setKtRange(const int& nKtBins = 10, const float& kTLow = 0.05, const float& kTHi = 1.05);

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

    /// Return i-th histogram for numerator

    TH3F* numerator(int i) {
        return (mNumerator[i]) ? mNumerator[i] : nullptr;
    }
    /// Return i-th histogram for denominator

    TH3F* denominator(int i) {
        return (mDenominator[i]) ? mDenominator[i] : nullptr;
    }
    /// Return i-th weighted numerator

    TH3F* numeratorWeighted(int i) {
        return (mNumeratorWeighted[i]) ? mNumeratorWeighted[i] : nullptr;
    }
    /// Return i-th weighted numerator

    TH3F* denominatorWeighted(int i) {
        return (mDenominatorWeighted[i]) ? mDenominatorWeighted[i] : nullptr;
    }

    /// Write histograms
    virtual void writeOutHistos();

    /// Return output list
    virtual TList* getOutputList();
    /// Clone correlation function

    virtual MpdFemtoModelBPLCMS3DCorrFctnKt* clone() const {
        return new MpdFemtoModelBPLCMS3DCorrFctnKt(*this);
    }

    TGraphErrors* errs;

protected:

    /// Pointer to the model manager that performs femtoscopic
    /// weight calculation
    MpdFemtoModelManager *mManager;

    /// Numerator made with pairs from the same event with femtoscopic weight
    TH3F *mNumeratorWeighted[20];
    /// Numerator made with pairs from the same event without weights
    TH3F *mNumerator[20];
    /// Denominator made with mixed pairs with femtoscopic weight
    TH3F *mDenominatorWeighted[20];
    /// Denominator made with mixed pairs without weights
    TH3F *mDenominator[20];

    /// Number of bins in the outward direction
    int mNQbins;
    /// qOut range
    float mQRange[2];

    /// Number of the kT bins
    int mNKtBins;
    /// kT step
    float mKtStep;
    /// kT range
    float mKtRange[2];

    ClassDef(MpdFemtoModelBPLCMS3DCorrFctnKt, 1);
};

#endif // MpdFemtoModelBPLCMS3DCorrFctnKt_h
