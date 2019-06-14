//
// The basic track cut
//

// C++ headers
#include <iostream>

// MpdFemtoMakerUser headers
#include "MpdFemtoBasicTrackCut.h"

// ROOT headers
#include "TString.h"
#include "TLorentzVector.h"

ClassImp(MpdFemtoBasicTrackCut)

//_________________
MpdFemtoBasicTrackCut::MpdFemtoBasicTrackCut() :
MpdFemtoBaseTrackCut(),
mType(true),
mCharge(1),
mNHitsRat(0),
mDetSelection(3),
mPidSelection(HbtPID::Pion),
mNTracksPassed(0),
mNTracksFailed(0) {
    // Default constructor

    mNHits[0] = 10;
    mNHits[1] = 100;
    mPt[0] = 0.05;
    mPt[1] = 2.;
    mP[0] = 0.1;
    mP[1] = 2.;
    mRapidity[0] = -1e6;
    mRapidity[1] = 1e6;
    mEta[0] = -1.;
    mEta[1] = 1.;
    mDCA[0] = -0.1;
    mDCA[1] = 3.;

    mNSigmaElectron[0] = -100.;
    mNSigmaElectron[1] = 100.;
    mNSigmaPion[0] = -100.0;
    mNSigmaPion[1] = 100.0;
    mNSigmaKaon[0] = -100.0;
    mNSigmaKaon[1] = 100.0;
    mNSigmaProton[0] = -100.0;
    mNSigmaProton[1] = 100.0;
    mNSigmaOther[0] = 0.;
    mNSigmaOther[1] = 0.;
    mTpcMom[0] = 0.1;
    mTpcMom[1] = 0.55;

    mTofMassSqr[0] = -0.15;
    mTofMassSqr[1] = 1.2;
    mTofMom[0] = 0.1;
    mTofMom[1] = 1.45;

    mTnTNSigmaElectron[0] = -100.;
    mTnTNSigmaElectron[1] = 100.;
    mTnTNSigmaPion[0] = -100.;
    mTnTNSigmaPion[1] = 100.;
    mTnTNSigmaKaon[0] = -100.;
    mTnTNSigmaKaon[1] = 100.;
    mTnTNSigmaProton[0] = -100.;
    mTnTNSigmaProton[1] = 100.;

    mIsTheory = false;
    // Positive pion
    mPdgId = 211;
}

//_________________

MpdFemtoBasicTrackCut::MpdFemtoBasicTrackCut(const MpdFemtoBasicTrackCut& c) :
MpdFemtoBaseTrackCut(c),
mType(c.mType),
mCharge(c.mCharge),
mNHitsRat(c.mNHitsRat),
mDetSelection(c.mDetSelection),
mPidSelection(c.mPidSelection),
mNTracksPassed(0),
mNTracksFailed(0) {
    // Copy constructor

    mNHits[0] = c.mNHits[0];
    mNHits[1] = c.mNHits[1];
    mPt[0] = c.mPt[0];
    mPt[1] = c.mPt[1];
    mP[0] = c.mP[0];
    mP[1] = c.mP[1];
    mRapidity[0] = c.mRapidity[0];
    mRapidity[1] = c.mRapidity[1];
    mEta[0] = c.mEta[0];
    mEta[1] = c.mEta[1];
    mDCA[0] = c.mDCA[0];
    mDCA[1] = c.mDCA[1];

    mNSigmaElectron[0] = c.mNSigmaElectron[0];
    mNSigmaElectron[1] = c.mNSigmaElectron[1];
    mNSigmaPion[0] = c.mNSigmaPion[0];
    mNSigmaPion[1] = c.mNSigmaPion[1];
    mNSigmaKaon[0] = c.mNSigmaKaon[0];
    mNSigmaKaon[1] = c.mNSigmaKaon[1];
    mNSigmaProton[0] = c.mNSigmaProton[0];
    mNSigmaProton[1] = c.mNSigmaProton[1];
    mNSigmaOther[0] = c.mNSigmaOther[0];
    mNSigmaOther[1] = c.mNSigmaOther[1];
    mTpcMom[0] = c.mTpcMom[0];
    mTpcMom[1] = c.mTpcMom[1];

    mTofMassSqr[0] = c.mTofMassSqr[0];
    mTofMassSqr[1] = c.mTofMassSqr[1];
    mTofMom[0] = c.mTofMom[0];
    mTofMom[1] = c.mTofMom[1];

    mTnTNSigmaElectron[0] = c.mTnTNSigmaElectron[0];
    mTnTNSigmaElectron[1] = c.mTnTNSigmaElectron[1];
    mTnTNSigmaPion[0] = c.mTnTNSigmaPion[0];
    mTnTNSigmaPion[1] = c.mTnTNSigmaPion[1];
    mTnTNSigmaKaon[0] = c.mTnTNSigmaKaon[0];
    mTnTNSigmaKaon[1] = c.mTnTNSigmaKaon[1];
    mTnTNSigmaProton[0] = c.mTnTNSigmaProton[0];
    mTnTNSigmaProton[1] = c.mTnTNSigmaProton[1];

    mIsTheory = c.mIsTheory;
    mPdgId = c.mPdgId;
}

//_________________

MpdFemtoBasicTrackCut& MpdFemtoBasicTrackCut::operator=(const MpdFemtoBasicTrackCut& c) {
    // Assignment operator
    if (this != &c) {
        MpdFemtoBaseTrackCut::operator=(c);
        mType = c.mType;
        mCharge = c.mCharge;
        mNHits[0] = c.mNHits[0];
        mNHits[1] = c.mNHits[1];
        mNHitsRat = c.mNHitsRat;
        mPt[0] = c.mPt[0];
        mPt[1] = c.mPt[1];
        mP[0] = c.mP[0];
        mP[1] = c.mP[1];
        mRapidity[0] = c.mRapidity[0];
        mRapidity[1] = c.mRapidity[1];
        mEta[0] = c.mEta[0];
        mEta[1] = c.mEta[1];
        mDCA[0] = c.mDCA[0];
        mDCA[1] = c.mDCA[1];

        mDetSelection = c.mDetSelection;

        mNSigmaElectron[0] = c.mNSigmaElectron[0];
        mNSigmaElectron[1] = c.mNSigmaElectron[1];
        mNSigmaPion[0] = c.mNSigmaPion[0];
        mNSigmaPion[1] = c.mNSigmaPion[1];
        mNSigmaKaon[0] = c.mNSigmaKaon[0];
        mNSigmaKaon[1] = c.mNSigmaKaon[1];
        mNSigmaProton[0] = c.mNSigmaProton[0];
        mNSigmaProton[1] = c.mNSigmaProton[1];
        mNSigmaOther[0] = c.mNSigmaOther[0];
        mNSigmaOther[1] = c.mNSigmaOther[1];
        mTpcMom[0] = c.mTpcMom[0];
        mTpcMom[1] = c.mTpcMom[1];

        mTofMassSqr[0] = c.mTofMassSqr[0];
        mTofMassSqr[1] = c.mTofMassSqr[1];
        mTofMom[0] = c.mTofMom[0];
        mTofMom[1] = c.mTofMom[1];

        mTnTNSigmaElectron[0] = c.mTnTNSigmaElectron[0];
        mTnTNSigmaElectron[1] = c.mTnTNSigmaElectron[1];
        mTnTNSigmaPion[0] = c.mTnTNSigmaPion[0];
        mTnTNSigmaPion[1] = c.mTnTNSigmaPion[1];
        mTnTNSigmaKaon[0] = c.mTnTNSigmaKaon[0];
        mTnTNSigmaKaon[1] = c.mTnTNSigmaKaon[1];
        mTnTNSigmaProton[0] = c.mTnTNSigmaProton[0];
        mTnTNSigmaProton[1] = c.mTnTNSigmaProton[1];

        mPidSelection = c.mPidSelection;

        mIsTheory = c.mIsTheory;
        mPdgId = c.mPdgId;

        mNTracksPassed = 0;
        mNTracksFailed = 0;
    }

    return *this;
}

//_________________

MpdFemtoBasicTrackCut::~MpdFemtoBasicTrackCut() {
    /* emtpy */
}

//_________________

bool MpdFemtoBasicTrackCut::pass(const MpdFemtoTrack* t) {
    // Test the particle and return true if it meets all the criteria
    // false if it doesn't meet at least one of the criteria

    const bool goodType = (mType == t->type());
    const bool primTrk = (goodType && t->isPrimary());

    TLorentzVector vec;
    if (primTrk) {
        vec.SetXYZM(t->pMom().X(), t->pMom().Y(), t->pMom().Z(), mMass);
    } else {
        vec.SetXYZM(t->gMom().X(), t->gMom().Y(), t->gMom().Z(), mMass);
    }

    // For theoretical estimations
    if (mIsTheory) {
        if (!t->hiddenInfo()) {
            std::cout << "[WARNING] MpdFemtoBasicTrackCut::pass(const MpdFemtoTrack* t) "
                    << "- Track does not have hidden info" << std::endl;
            return false;
        }
        if (mPdgId != t->hiddenInfo()->pdgId()) {
            return false;
        }
    }

    const bool goodCharge = (mCharge == (char) t->charge());
    const bool goodKine = ((mNHits[0] <= t->nHits()) && (t->nHits() <= mNHits[1]) &&
            (t->nHitsFit2PossRatio() >= mNHitsRat) &&
            (mPt[0] <= vec.Pt()) && (vec.Pt() <= mPt[1]) &&
            (mP[0] <= vec.P()) && (vec.P() <= mP[1]) &&
            (mRapidity[0] <= vec.Rapidity()) && (vec.Rapidity() <= mRapidity[1]) &&
            (mEta[0] <= vec.Eta()) && (vec.Eta() <= mEta[1]) &&
            (mDCA[0] <= t->gDCA().Mag()) && (t->gDCA().Mag() <= mDCA[1]));

    // Check just first particles cuts withou PID (fasten track processing)
    if (!goodType || !goodCharge || !goodKine) {
        mNTracksFailed++;
        return false;
    }

    bool goodPID = false;

    // Choose identification scheme
    if (mDetSelection == 0) { // TPC selection

        if (mPidSelection == HbtPID::Electron) { /// Electron
            goodPID = ((mNSigmaElectron[0] <= t->nSigmaElectron()) &&
                    (t->nSigmaElectron() <= mNSigmaElectron[1]) &&
                    (mTpcMom[0] <= vec.P()) && (vec.P() <= mTpcMom[1]) &&
                    ((t->nSigmaPion() <= mNSigmaOther[0]) || (t->nSigmaPion() >= mNSigmaOther[1])) &&
                    ((t->nSigmaKaon() <= mNSigmaOther[0]) || (t->nSigmaKaon() >= mNSigmaOther[1])) &&
                    ((t->nSigmaProton() <= mNSigmaOther[0]) || (t->nSigmaProton() >= mNSigmaOther[1])));
        } else if (mPidSelection == HbtPID::Pion) { // Pion
            goodPID = ((mNSigmaPion[0] <= t->nSigmaPion()) &&
                    (t->nSigmaPion() <= mNSigmaPion[1]) &&
                    (mTpcMom[0] <= vec.P()) && (vec.P() <= mTpcMom[1]) &&
                    ((t->nSigmaElectron() <= mNSigmaOther[0]) || (t->nSigmaElectron() >= mNSigmaOther[1])) &&
                    ((t->nSigmaKaon() <= mNSigmaOther[0]) || (t->nSigmaKaon() >= mNSigmaOther[1])) &&
                    ((t->nSigmaProton() <= mNSigmaOther[0]) || (t->nSigmaProton() >= mNSigmaOther[1])));
        } else if (mPidSelection == HbtPID::Kaon) { // Kaon
            goodPID = ((mNSigmaKaon[0] <= t->nSigmaKaon()) &&
                    (t->nSigmaKaon() <= mNSigmaKaon[1]) &&
                    (mTpcMom[0] <= vec.P()) && (vec.P() <= mTpcMom[1]) &&
                    ((t->nSigmaElectron() <= mNSigmaOther[0]) || (t->nSigmaElectron() >= mNSigmaOther[1])) &&
                    ((t->nSigmaPion() <= mNSigmaOther[0]) || (t->nSigmaPion() >= mNSigmaOther[1])) &&
                    ((t->nSigmaProton() <= mNSigmaOther[0]) || (t->nSigmaProton() >= mNSigmaOther[1])));
        } else if (mPidSelection == HbtPID::Proton) { // Proton
            goodPID = ((mNSigmaProton[0] <= t->nSigmaProton()) &&
                    (t->nSigmaProton() <= mNSigmaProton[1]) &&
                    (mTpcMom[0] <= vec.P()) && (vec.P() <= mTpcMom[1]) &&
                    ((t->nSigmaElectron() <= mNSigmaOther[0]) || (t->nSigmaElectron() >= mNSigmaOther[1])) &&
                    ((t->nSigmaPion() <= mNSigmaOther[0]) || (t->nSigmaPion() >= mNSigmaOther[1])) &&
                    ((t->nSigmaKaon() <= mNSigmaOther[0]) || (t->nSigmaKaon() >= mNSigmaOther[1])));
        } else {
            std::cout << "[ERROR] MpdFemtoBasicTrackCut: Wrong HbtPID " << mPidSelection << std::endl;
            return false;
        }

    } else if (mDetSelection == 1) { // TOF selection
        // Must be a TOF-matched track
        if (t->isTofTrack()) {
            goodPID = ((mTofMassSqr[0] <= t->massSqr()) && (t->massSqr() <= mTofMassSqr[1]) &&
                    (mTofMom[0] <= vec.P()) && (vec.P() <= mTofMom[1]));
        } // if( t->isTofTrack() )
    } else if (mDetSelection == 2) { // TPC+TOF selection
        // Must be a TOF-matched track
        if (t->isTofTrack()) {

            if (mPidSelection == HbtPID::Electron) { // Electron
                goodPID = ((mTofMassSqr[0] <= t->massSqr()) && (t->massSqr() <= mTofMassSqr[1]) &&
                        (mTofMom[0] <= vec.P()) && (vec.P() <= mTofMom[1]) &&
                        (mTnTNSigmaElectron[0] <= t->nSigmaElectron()) &&
                        (t->nSigmaElectron() <= mTnTNSigmaElectron[1]));
            } else if (mPidSelection == HbtPID::Pion) { // Pion
                goodPID = ((mTofMassSqr[0] <= t->massSqr()) && (t->massSqr() <= mTofMassSqr[1]) &&
                        (mTofMom[0] <= vec.P()) && (vec.P() <= mTofMom[1]) &&
                        (mTnTNSigmaPion[0] <= t->nSigmaPion()) &&
                        (t->nSigmaPion() <= mTnTNSigmaPion[1]));
            } else if (mPidSelection == HbtPID::Kaon) { // Kaon
                goodPID = ((mTofMassSqr[0] <= t->massSqr()) && (t->massSqr() <= mTofMassSqr[1]) &&
                        (mTofMom[0] <= vec.P()) && (vec.P() <= mTofMom[1]) &&
                        (mTnTNSigmaKaon[0] <= t->nSigmaKaon()) &&
                        (t->nSigmaKaon() <= mTnTNSigmaKaon[1]));
            } else if (mPidSelection == HbtPID::Proton) { // Proton
                goodPID = ((mTofMassSqr[0] <= t->massSqr()) && (t->massSqr() <= mTofMassSqr[1]) &&
                        (mTofMom[0] <= vec.P()) && (vec.P() <= mTofMom[1]) &&
                        (mTnTNSigmaProton[0] <= t->nSigmaProton()) &&
                        (t->nSigmaProton() <= mTnTNSigmaProton[1]));
            } else {
                std::cout << "[ERROR] MpdFemtoBasicTrackCut: Wrong HbtPID " << mPidSelection << std::endl;
                return false;
            }
        } // if( t->isTofTrack() )
    } else if (mDetSelection == 3) { // if(TOF) {TPC+TOF} else {TPC}

        // If there is a TOF hit, then use TPC+TOF identification
        if (t->isTofTrack()) {
            if (mPidSelection == HbtPID::Electron) { // Electron
                goodPID = ((mTofMassSqr[0] <= t->massSqr()) && (t->massSqr() <= mTofMassSqr[1]) &&
                        (mTofMom[0] <= vec.P()) && (vec.P() <= mTofMom[1]) &&
                        (mTnTNSigmaElectron[0] <= t->nSigmaElectron()) &&
                        (t->nSigmaElectron() <= mTnTNSigmaElectron[1]));
            } else if (mPidSelection == HbtPID::Pion) { // Pion
                goodPID = ((mTofMassSqr[0] <= t->massSqr()) && (t->massSqr() <= mTofMassSqr[1]) &&
                        (mTofMom[0] <= vec.P()) && (vec.P() <= mTofMom[1]) &&
                        (mTnTNSigmaPion[0] <= t->nSigmaPion()) &&
                        (t->nSigmaPion() <= mTnTNSigmaPion[1]));
            } else if (mPidSelection == HbtPID::Kaon) { // Kaon
                goodPID = ((mTofMassSqr[0] <= t->massSqr()) && (t->massSqr() <= mTofMassSqr[1]) &&
                        (mTofMom[0] <= vec.P()) && (vec.P() <= mTofMom[1]) &&
                        (mTnTNSigmaKaon[0] <= t->nSigmaKaon()) &&
                        (t->nSigmaKaon() <= mTnTNSigmaKaon[1]));
            } else if (mPidSelection == HbtPID::Proton) { // Proton
                goodPID = ((mTofMassSqr[0] <= t->massSqr()) && (t->massSqr() <= mTofMassSqr[1]) &&
                        (mTofMom[0] <= vec.P()) && (vec.P() <= mTofMom[1]) &&
                        (mTnTNSigmaProton[0] <= t->nSigmaProton()) &&
                        (t->nSigmaProton() <= mTnTNSigmaProton[1]));
            } else {
                std::cout << "[ERROR] MpdFemtoBasicTrackCut: Wrong HbtPID " << mPidSelection << std::endl;
                return false;
            }
        }// if( t->isTofTrack() )
        else { // When no TOF hit is available check TPC only
            if (mPidSelection == HbtPID::Electron) { // Electron
                goodPID = ((mNSigmaElectron[0] <= t->nSigmaElectron()) &&
                        (t->nSigmaElectron() <= mNSigmaElectron[1]) &&
                        (mTpcMom[0] <= vec.P()) && (vec.P() <= mTpcMom[1]) &&
                        ((t->nSigmaPion() <= mNSigmaOther[0]) || (t->nSigmaPion() >= mNSigmaOther[1])) &&
                        ((t->nSigmaKaon() <= mNSigmaOther[0]) || (t->nSigmaKaon() >= mNSigmaOther[1])) &&
                        ((t->nSigmaProton() <= mNSigmaOther[0]) || (t->nSigmaProton() >= mNSigmaOther[1])));
            } else if (mPidSelection == HbtPID::Pion) { // Pion
                goodPID = ((mNSigmaPion[0] <= t->nSigmaPion()) &&
                        (t->nSigmaPion() <= mNSigmaPion[1]) &&
                        (mTpcMom[0] <= vec.P()) && (vec.P() <= mTpcMom[1]) &&
                        ((t->nSigmaElectron() <= mNSigmaOther[0]) || (t->nSigmaElectron() >= mNSigmaOther[1])) &&
                        ((t->nSigmaKaon() <= mNSigmaOther[0]) || (t->nSigmaKaon() >= mNSigmaOther[1])) &&
                        ((t->nSigmaProton() <= mNSigmaOther[0]) || (t->nSigmaProton() >= mNSigmaOther[1])));
            } else if (mPidSelection == HbtPID::Kaon) { // Kaon
                goodPID = ((mNSigmaKaon[0] <= t->nSigmaKaon()) &&
                        (t->nSigmaKaon() <= mNSigmaKaon[1]) &&
                        (mTpcMom[0] <= vec.P()) && (vec.P() <= mTpcMom[1]) &&
                        ((t->nSigmaElectron() <= mNSigmaOther[0]) || (t->nSigmaElectron() >= mNSigmaOther[1])) &&
                        ((t->nSigmaPion() <= mNSigmaOther[0]) || (t->nSigmaPion() >= mNSigmaOther[1])) &&
                        ((t->nSigmaProton() <= mNSigmaOther[0]) || (t->nSigmaProton() >= mNSigmaOther[1])));
            } else if (mPidSelection == HbtPID::Proton) { // Proton
                goodPID = ((mNSigmaProton[0] <= t->nSigmaProton()) &&
                        (t->nSigmaProton() <= mNSigmaProton[1]) &&
                        (mTpcMom[0] <= vec.P()) && (vec.P() <= mTpcMom[1]) &&
                        ((t->nSigmaElectron() <= mNSigmaOther[0]) || (t->nSigmaElectron() >= mNSigmaOther[1])) &&
                        ((t->nSigmaPion() <= mNSigmaOther[0]) || (t->nSigmaPion() >= mNSigmaOther[1])) &&
                        ((t->nSigmaKaon() <= mNSigmaOther[0]) || (t->nSigmaKaon() >= mNSigmaOther[1])));
            } else {
                std::cout << "[ERROR] MpdFemtoBasicTrackCut: Wrong HbtPID " << mPidSelection << std::endl;
                return false;
            }
        } // else
    } else {
        std::cout << "[ERROR] MpdFemtoBasicTrackCut: Wrong particle identification scheme "
                << mDetSelection << std::endl;
        return false;
    }

    // Final calculation
    const bool goodTrack = goodCharge && goodType && goodKine && goodPID;

    // Choose your destiny
    goodTrack ? mNTracksPassed++ : mNTracksFailed++;
    return goodTrack;
}

//_________________

MpdFemtoString MpdFemtoBasicTrackCut::report() {
    // Construct report
    TString report;

    report += TString::Format("Particle mass:\t%E\n", mMass);
    report += TString::Format("Particle type:\t%d\n", mType);
    report += TString::Format("Particle charge:\t%d\n", mCharge);
    report += TString::Format("Particle nhits:\t%d - %d\n", mNHits[0], mNHits[1]);
    report += TString::Format("Particle nHitsFit/nHitsPossible:\t%E\n", mNHitsRat);
    report += TString::Format("Particle pT:\t%E - %E\n", mPt[0], mPt[1]);
    report += TString::Format("Particle p:\t%E - %E\n", mP[0], mP[1]);
    report += TString::Format("Particle rapidity:\t%E - %E\n", mRapidity[0], mRapidity[1]);
    report += TString::Format("Particle pseudoRapidity:\t%E - %E\n", mEta[0], mEta[1]);
    report += TString::Format("Particle DCA:\t%E - %E\n", mDCA[0], mDCA[1]);
    report += TString::Format("Particle nSigma from electron:\t%E - %E\n", mNSigmaElectron[0], mNSigmaElectron[1]);
    report += TString::Format("Particle nSigma from pion:\t%E - %E\n", mNSigmaPion[0], mNSigmaPion[1]);
    report += TString::Format("Particle nSigma from kaon:\t%E - %E\n", mNSigmaKaon[0], mNSigmaKaon[1]);
    report += TString::Format("Particle nSigma from proton:\t%E - %E\n", mNSigmaProton[0], mNSigmaProton[1]);
    report += TString::Format("Particle nSigma from other:\t%E - %E\n", mNSigmaOther[0], mNSigmaOther[1]);
    report += TString::Format("Particle TPC momentum:\t%E - %E\n", mTpcMom[0], mTpcMom[1]);
    report += TString::Format("Particle TOF mass square:\t%E - %E\n", mTofMassSqr[0], mTofMassSqr[1]);
    report += TString::Format("Particle TOF momentum:\t%E - %E\n", mTofMom[0], mTofMom[1]);
    report += TString::Format("Particle TPC+TOF nSigma for electron:\t%E - %E\n", mTnTNSigmaElectron[0], mTnTNSigmaElectron[1]);
    report += TString::Format("Particle TPC+TOF nSigmat for pion:\t%E - %E\n", mTnTNSigmaPion[0], mTnTNSigmaPion[1]);
    report += TString::Format("Particle TPC+TOF nSigmat for kaon:\t%E - %E\n", mTnTNSigmaKaon[0], mTnTNSigmaKaon[1]);
    report += TString::Format("Particle TPC+TOF nSigmat for proton:\t%E - %E\n", mTnTNSigmaProton[0], mTnTNSigmaProton[1]);
    if (mPidSelection == HbtPID::Electron) {
        report += TString::Format("Particle PID to select: electron");
    } else if (mPidSelection == HbtPID::Pion) {
        report += TString::Format("Particle PID to select: pion");
    } else if (mPidSelection == HbtPID::Kaon) {
        report += TString::Format("Particle PID to select: kaon");
    } else if (mPidSelection == HbtPID::Proton) {
        report += TString::Format("Particle PID to select: proton");
    } else {
        report += TString::Format("Particle PID to select: unknown");
    }
    report += TString::Format("Number of tracks which passed:\t%u  Number which failed:\t%u\n",
            mNTracksPassed, mNTracksFailed);

    return MpdFemtoString((const char *) report);
}

//_________________

TList *MpdFemtoBasicTrackCut::listSettings() {

    // Return a list of settings in a writable form
    TList *settings_list = new TList();

    settings_list->AddVector(
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.mass=%f", mMass)),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.type=%u", mType)),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.charge=%i", mCharge)),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.nhits.min=%u", mNHits[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.nhits.max=%u", mNHits[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.nhits.nhitsratio=%f", mNHitsRat)),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.pt.min=%f", mPt[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.pt.max=%f", mPt[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.p.min=%f", mP[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.p.max=%f", mP[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.rapidity.min=%f", mRapidity[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.rapidity.max=%f", mRapidity[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.pseudorapidity.min=%f", mEta[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.pseudorapidity.max=%f", mEta[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.dca.min=%f", mDCA[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.dca.max=%f", mDCA[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.nsigmaelectron.min=%f", mNSigmaElectron[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.nsigmaelectron.max=%f", mNSigmaElectron[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.nsigmapion.min=%f", mNSigmaPion[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.nsigmapion.max=%f", mNSigmaPion[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.nsigmakaon.min=%f", mNSigmaKaon[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.nsigmakaon.max=%f", mNSigmaKaon[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.nsigmaproton.min=%f", mNSigmaProton[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.nsigmaproton.max=%f", mNSigmaProton[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.nsigmaother.min=%f", mNSigmaOther[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.nsigmaother.max=%f", mNSigmaOther[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tpcmom.min=%f", mTpcMom[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tpcmom.max=%f", mTpcMom[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tofmasssqr.min=%f", mTofMassSqr[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tofmasssqr.max=%f", mTofMassSqr[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tofmom.min=%f", mTofMom[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tofmom.max=%f", mTofMom[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tntnsigmaelectron.min=%f", mTnTNSigmaElectron[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tntnsigmaelectron.max=%f", mTnTNSigmaElectron[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tntnsigmapion.min=%f", mTnTNSigmaPion[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tntnsigmapion.max=%f", mTnTNSigmaPion[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tntnsigmakaon.min=%f", mTnTNSigmaKaon[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tntnsigmakaon.max=%f", mTnTNSigmaKaon[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tntnsigmaproton.min=%f", mTnTNSigmaProton[0])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.tntnsigmaproton.max=%f", mTnTNSigmaProton[1])),
            new TObjString(TString::Format("MpdFemtoBasicTrackCut.hbtpid=%d", mPidSelection)),
            NULL
            );

    return settings_list;
}
