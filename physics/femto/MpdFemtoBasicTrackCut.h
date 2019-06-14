/**
 * \class MpdFemtoBasicTrackCut
 * \brief The basic track cut
 *
 * Cuts on particle identification, transverse momentum, rapidity, distance
 * of closest approach to primary vertex and charge. Charge defaults to 1 (positive)
 */

#ifndef MpdFemtoBasicTrackCut_h
#define MpdFemtoBasicTrackCut_h

// MpdFemtoMaker headers
// Base
#include "MpdFemtoBaseParticleCut.h"
#include "MpdFemtoBaseTrackCut.h"

// C++ headers
#include <limits>

//_________________

class MpdFemtoBasicTrackCut : public MpdFemtoBaseTrackCut {
public:
    /// Default constructor
    MpdFemtoBasicTrackCut();
    /// Copy constructor
    MpdFemtoBasicTrackCut(const MpdFemtoBasicTrackCut& copy);
    /// Assignment operator
    MpdFemtoBasicTrackCut& operator=(const MpdFemtoBasicTrackCut& copy);
    /// Destructor
    virtual ~MpdFemtoBasicTrackCut();

    /// Test the particle and return true if it meets all criteria. false otherwise.
    virtual bool pass(const MpdFemtoTrack* tr);

    /// Prepare report
    virtual MpdFemtoString report();
    /// Prepare list of settings
    virtual TList *listSettings();

    /// Enumeration of PID

    enum HbtPID {
        Electron = 1, Pion, Kaon, Proton
    };

    /// Select track type: false (0) - global, true (1) - primary

    void selectPrimary(const bool& type) {
        setType(type);
    }

    void setType(const bool& type) {
        mType = type;
    }
    /// Set charge of the track to select

    void setCharge(const short& charge) {
        mCharge = (char) charge;
    }
    /// Set min and max range of nhits

    void setNHits(const short& lo, const short& hi) {
        mNHits[0] = (unsigned char) lo;
        mNHits[1] = (unsigned char) hi;
    }
    /// Set min and max range of nHitsFit/nHitsPossible

    void setNHitsFitOverNHitsPoss(const float& lo) {
        mNHitsRat = lo;
    }

    void setAntiSplit(const float& lo) {
        setNHitsFitOverNHitsPoss(lo);
    }
    /// Set min and max ranges of transverse momentum

    void setPt(const float& lo, const float& hi) {
        mPt[0] = lo;
        mPt[1] = hi;
    }

    void setP(const float& lo, const float& hi) {
        mP[0] = lo;
        mP[1] = hi;
    }

    void setTranverseMomentum(const float& lo, const float& hi) {
        setPt(lo, hi);
    }
    /// Set min and max ranges of rapidity

    void setRapidity(const float& lo, const float& hi) {
        mRapidity[0] = lo;
        mRapidity[1] = hi;
    }
    /// Set min and max ranges of pseudorapidity

    void setEta(const float& lo, const float& hi) {
        mEta[0] = lo;
        mEta[1] = hi;
    }

    void setPseudoRapidity(const float& lo, const float& hi) {
        setEta(lo, hi);
    }
    /// Set min and max range of DCA

    void setDCA(const float& lo, const float& hi) {
        mDCA[0] = lo;
        mDCA[1] = hi;
    }

    /// Set type of detector selection:
    /// 0 - TPC, 1 - TOF, 2 - TPC+TOF, 3 - if(TOF){TPC+TOF} else{TPC}

    void setDetectorSelection(const int& selection) {
        if (selection < 0 || selection > 3) {
            std::cout << "[WARNING] MpdFemtoBasicTrackCut: wrong detector seletction: " << selection
                    << " reset to TPC (0)" << std::endl;
            mDetSelection = 0;
        } else {
            mDetSelection = (unsigned char) selection;
        }
    }

    /// Set min and max range of electron nSigma to select

    void setNSigmaElectron(const float& lo, const float& hi) {
        mNSigmaElectron[0] = lo;
        mNSigmaElectron[1] = hi;
    }
    /// Set min and max range of pion nSigma to select

    void setNSigmaPion(const float& lo, const float& hi) {
        mNSigmaPion[0] = lo;
        mNSigmaPion[1] = hi;
    }
    /// Set min and max range of kaon nSigma to select

    void setNSigmaKaon(const float& lo, const float& hi) {
        mNSigmaKaon[0] = lo;
        mNSigmaKaon[1] = hi;
    }
    /// Set min and max range of proton nSigma to select

    void setNSigmaProton(const float& lo, const float& hi) {
        mNSigmaProton[0] = lo;
        mNSigmaProton[1] = hi;
    }
    /// Set low and hight values for the exclusion cut

    void setNSigmaOther(const float& lo, const float& hi) {
        mNSigmaOther[0] = lo;
        mNSigmaOther[1] = hi;
    }
    /// Set min and max momentum of the track for TPC identification

    void setTpcP(const float& lo, const float& hi) {
        mTpcMom[0] = lo;
        mTpcMom[1] = hi;
    }

    void setTpcMomentum(const float& lo, const float& hi) {
        setTpcP(lo, hi);
    }

    void setTpcMom(const float& lo, const float& hi) {
        setTpcP(lo, hi);
    }

    /// Set min and max mass square estimated using TOF

    void setMassSqr(const float& lo, const float& hi) {
        mTofMassSqr[0] = lo;
        mTofMassSqr[1] = hi;
    }
    /// Set min and max momentum for TOF or TPC+TOF identification

    void setTofP(const float& lo, const float& hi) {
        mTofMom[0] = lo;
        mTofMom[1] = hi;
    }

    void setTofMomentum(const float& lo, const float& hi) {
        setTofP(lo, hi);
    }

    void setTofMom(const float& lo, const float& hi) {
        setTofP(lo, hi);
    }

    /// Set min and max values of electron nSigma for TPC+TOF identification

    void setTnTNSigmaElectron(const float& lo, const float& hi) {
        mTnTNSigmaElectron[0] = lo;
        mTnTNSigmaElectron[1] = hi;
    }
    /// Set min and max values of pion nSigma for TPC+TOF identification

    void setTnTNSigmaPion(const float& lo, const float& hi) {
        mTnTNSigmaPion[0] = lo;
        mTnTNSigmaPion[1] = hi;
    }
    /// Set min and max values of kaon nSigma for TPC+TOF identification

    void setTnTNSigmaKaon(const float& lo, const float& hi) {
        mTnTNSigmaKaon[0] = lo;
        mTnTNSigmaKaon[1] = hi;
    }
    /// Set min and max values of proton nSigma for TPC+TOF identification

    void setTnTNSigmaProton(const float& lo, const float& hi) {
        mTnTNSigmaProton[0] = lo;
        mTnTNSigmaProton[1] = hi;
    }

    /// Set PID to select: 1-electron, 2-pion, 3-kaon, 4-proton

    void setHbtPid(const HbtPID& pid) {
        mPidSelection = pid;
    }

    /// Is theoretical work

    void setIsTheory(const bool& isTheory) {
        mIsTheory = isTheory;
    }
    /// Set PDD ID to select (will be checked if IsTheory set to true)

    void setPdgId(const int& pdgId) {
        mPdgId = pdgId;
    }


protected:

    /// Track type: 0 - global, 1 - primary
    bool mType;
    /// Charge of the track - if 0 the charge is not checked
    char mCharge;
    /// Bounds for number of hits
    unsigned char mNHits[2];
    /// nHitsFit/nHitsPossible
    float mNHitsRat;
    /// Bounds for transverse momentum
    float mPt[2];
    /// Bounds for full momentum
    float mP[2];
    /// Bounds for rapidity
    float mRapidity[2];
    /// Bounds for pseudorapidity
    float mEta[2];
    /// Bounds for DCA to primary vertex
    float mDCA[2];

    /// Type of detector selection:
    /// 0 - TPC, 1 - TOF, 2 - TPC+TOF, 3 - if(TOF){TPC+TOF}else{TPC}
    unsigned char mDetSelection;

    // TPC identification

    /// Bounds for nsigma dEdx from electron band
    float mNSigmaElectron[2];
    /// Bounds for nsigma dEdx from pion band
    float mNSigmaPion[2];
    /// Bounds for nsigma dEdx from kaon band
    float mNSigmaKaon[2];
    /// Bounds for nsigma dEdx from proton band
    float mNSigmaProton[2];
    /// Bounds for exclusion cuts
    float mNSigmaOther[2];
    /// Momentum of the track for pure TPC identification
    float mTpcMom[2];


    // TOF identification

    /// TOF mass square
    float mTofMassSqr[2];
    /// Momentum of the track for the pure TOF identification
    float mTofMom[2];


    // TPC && TOF identification

    /// Bounds for nsigma dEdx from pion band
    float mTnTNSigmaElectron[2];
    /// Bounds for nsigma dEdx from pion band
    float mTnTNSigmaPion[2];
    /// Bounds for nsigma dEdx from kaon band
    float mTnTNSigmaKaon[2];
    /// Bounds for nsigma dEdx from proton band
    float mTnTNSigmaProton[2];

    /// PID selection: 1-electron, 2-pion, 3-kaon, 4-proton
    HbtPID mPidSelection;

    /// Passed tracks counter
    unsigned int mNTracksPassed;
    /// Falied tracks counter
    unsigned int mNTracksFailed;

    /// If work with theory
    bool mIsTheory;
    /// PDG pid
    int mPdgId;

    ClassDef(MpdFemtoBasicTrackCut, 2);
};

#endif // MpdFemtoBasicTrackCut_h
