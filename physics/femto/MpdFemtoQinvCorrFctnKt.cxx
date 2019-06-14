//
// One-dimensional correlation function with kT-binning
//

// MpdFemtoMakerUser headers
#include "MpdFemtoQinvCorrFctnKt.h"

// C++ headers
#include <iostream>

ClassImp(MpdFemtoQinvCorrFctnKt)

//_________________
MpdFemtoQinvCorrFctnKt::MpdFemtoQinvCorrFctnKt(char* title,
        const int &nbins, const float &QLo, const float &QHi,
        const int& ktBin, const float& KtLo, const float& KtHi) :
MpdFemtoBaseCorrFctn() {

    // Constructor
    mNumberKt = ktBin;
    mKtMin = KtLo;
    mKtMax = KtHi;
    mDeltaKt = (mKtMax - mKtMin) / mNumberKt;


    std::stringstream TitNum, TitDen, TitCurrent;
    TitNum.str("");
    TitDen.str("");
    TitCurrent.str("");
    TitNum << title << "_Num";
    TitDen << title << "_Den";

    TString hist_title = TString::Format("%s;q_{inv} (GeV/c);dN/dq_{inv}", title);

    // Histogram loop
    for (int i = 0; i < mNumberKt; i++) {
        TitCurrent.str("");
        TitCurrent << TitNum.str() << "_kt_" << i;
        mNumerator[i] = new TH1F(TitCurrent.str().c_str(),
                hist_title,
                nbins, QLo, QHi);
        mNumerator[i]->Sumw2();

        TitCurrent.str("");
        TitCurrent << TitDen.str() << "_kt_" << i;
        mDenominator[i] = new TH1F(TitCurrent.str().c_str(),
                hist_title,
                nbins, QLo, QHi);
        mDenominator[i]->Sumw2();
    } // for(int i=0; i<mNumberKt; i++)
}
//_________________

MpdFemtoQinvCorrFctnKt::MpdFemtoQinvCorrFctnKt(const MpdFemtoQinvCorrFctnKt& c) :
MpdFemtoBaseCorrFctn(c),
mNumberKt(c.mNumberKt),
mKtMin(c.mKtMin),
mKtMax(c.mKtMax),
mIndexKt(nullptr),
mDeltaKt(c.mDeltaKt) {
    // Copy constructor
    for (int iBin = 0; iBin < mNumberKt; iBin++) {
        mNumerator[iBin] = (c.mNumerator[iBin]) ? new TH1F(*c.mNumerator[iBin]) : nullptr;
        mNumerator[iBin] = (c.mNumerator[iBin]) ? new TH1F(*c.mNumerator[iBin]) : nullptr;
    } // for ( int iBin=0; iBin<mNumberKt; iBin++ )
}

//_________________

MpdFemtoQinvCorrFctnKt& MpdFemtoQinvCorrFctnKt::operator=(const MpdFemtoQinvCorrFctnKt& c) {
    // Assignment operator
    if (this != &c) {
        mNumberKt = c.mNumberKt;
        mKtMin = c.mKtMin;
        mKtMax = c.mKtMax;
        mIndexKt = nullptr;
        mDeltaKt = c.mDeltaKt;
        for (int iBin = 0; iBin < mNumberKt; iBin++) {
            mNumerator[iBin] = (c.mNumerator[iBin]) ? new TH1F(*c.mNumerator[iBin]) : nullptr;
            mNumerator[iBin] = (c.mNumerator[iBin]) ? new TH1F(*c.mNumerator[iBin]) : nullptr;
        } // for ( int iBin=0; iBin<mNumberKt; iBin++ )
    } // if (this != &c)

    return *this;
}

//_________________

MpdFemtoQinvCorrFctnKt::~MpdFemtoQinvCorrFctnKt() {
    // Destructor
    for (int i = 0; i < mNumberKt; i++) {
        if (mNumerator[i]) delete mNumerator[i];
        if (mDenominator[i]) delete mDenominator[i];
    }
}

//_________________

void MpdFemtoQinvCorrFctnKt::writeOutHistos() {
    // Write histograms to the file
    for (int i = 0; i < mNumberKt; i++) {
        mNumerator[i]->Write();
        mDenominator[i]->Write();
    }
}

//_________________

TList* MpdFemtoQinvCorrFctnKt::getOutputList() {
    // Prepare the list of objects to be written to the output
    TList *outputList = new TList();
    for (int i = 0; i < mNumberKt; i++) {
        outputList->Add(mNumerator[i]);
        outputList->Add(mDenominator[i]);
    }
    return outputList;
}

//_________________

void MpdFemtoQinvCorrFctnKt::eventBegin(const MpdFemtoEvent* /* event */) {
    /* empty */
}

//_________________

void MpdFemtoQinvCorrFctnKt::eventEnd(const MpdFemtoEvent* /* event */) {
    /* empty */
}

//_________________

void MpdFemtoQinvCorrFctnKt::finish() {
    /* empty */
}

//_________________

void MpdFemtoQinvCorrFctnKt::addRealPair(MpdFemtoPair* pair) {

    // Check if pair passes front-loaded cut if exists
    if (mPairCut && !mPairCut->pass(pair)) {
        return;
    }

    int mIndexKt = (int) (((pair->kT() - mKtMin) / mDeltaKt));

    if ((mIndexKt >= 0) && (mIndexKt < mNumberKt)) {
        if (mNumerator[mIndexKt]) {
            mNumerator[mIndexKt]->Fill(fabs(pair->qInv()));
        } else {
            std::cout << "[ERROR] void MpdFemtoQinvCorrFctnKt::addRealPair(MpdFemtoPair* pair) - "
                    << "No correlation function with index: " << mIndexKt << " was found"
                    << std::endl;
        }
    } // if ( ( mIndexKt>=0 ) && ( mIndexKt<mNumberKt ) )
}

//_________________

void MpdFemtoQinvCorrFctnKt::addMixedPair(MpdFemtoPair* pair) {

    // Check if pair passes front-loaded cut if exists
    if (mPairCut && !mPairCut->pass(pair)) {
        return;
    }

    int mIndexKt = (int) (((fabs(pair->kT()) - mKtMin) / mDeltaKt));

    if ((mIndexKt >= 0) && (mIndexKt < mNumberKt)) {
        if (mDenominator[mIndexKt]) {
            mDenominator[mIndexKt]->Fill(fabs(pair->qInv()));
        } else {
            std::cout << "[ERROR] void MpdFemtoQinvCorrFctnKt::addMixedPair(MpdFemtoPair* pair) - "
                    << "No correlation function with index: " << mIndexKt << " was found"
                    << std::endl;
        }
    } // if ( ( mIndexKt>=0 ) && ( mIndexKt<mNumberKt ) )
}

//_________________

MpdFemtoString MpdFemtoQinvCorrFctnKt::report() {

    // Make a report
    MpdFemtoString tStr = "MpdFemtoQinvCorrFctnKt report";

    int mNumeratorEntries = 0;
    int mDenominatorEntries = 0;

    for (int i = 0; i < mNumberKt; i++) {
        mNumeratorEntries += (int) mNumerator[i]->GetEntries();
        mDenominatorEntries += (int) mDenominator[i]->GetEntries();
    } // for (int i=0; i<mNumberKt; i++)

    tStr += TString::Format("Total number of pairs in the numerator  :\t%d\n", mNumeratorEntries);
    tStr += TString::Format("Total number of pairs in the denominator:\t%d\n", mDenominatorEntries);
    for (int i = 0; i < mNumberKt; i++) {
        tStr += TString::Format("Total number of pairs in %d-th numerator :\t%E\n",
                i, mNumerator[i]->GetEntries());
        tStr += TString::Format("Total number of pairs in %d-th numerator :\t%E\n",
                i, mDenominator[i]->GetEntries());

    }

    if (mPairCut) {
        tStr += "Here is the PairCut specific to this CorrFctn\n";
        tStr += mPairCut->report();
    } else {
        tStr += "No PairCut specific to this CorrFctn\n";
    }

    return tStr;
}
