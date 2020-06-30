
#ifndef MpdFemtoFemtoDstReader_h
#define MpdFemtoFemtoDstReader_h

// MpdFemtoMaker headers
// Base
#include "MpdFemtoBaseEventReader.h"
// Infrastructure
#include "MpdFemtoEvent.h"
#include "MpdFemtoTrack.h"
#include "MpdFemtoV0.h"
#include "MpdFemtoKink.h"
#include "MpdFemtoXi.h"
#include "MpdFemtoEnumeration.h"
#include "MpdFemtoString.h"

// StFemtoDst headers
#include "StFemtoDst.h"
#include "StFemtoDstReader.h"
#include "StFemtoEvent.h"
#include "StFemtoTrack.h"
#include "StFemtoArrays.h"

// ROOT haders
#include "TSystem.h"
#include "TChain.h"
#include "TTree.h"
#include "TMatrixTLazy.h"

//_________________

class MpdFemtoFemtoDstReader : public MpdFemtoBaseEventReader {
public:
    /// Default constructor
    MpdFemtoFemtoDstReader();
    /// Constructor
    MpdFemtoFemtoDstReader(StFemtoDstReader *femtoDstReader, int debug = 1);
    /// Copy constructor
    MpdFemtoFemtoDstReader(const MpdFemtoFemtoDstReader& copy);
    /// Assignment operator
    MpdFemtoFemtoDstReader& operator=(const MpdFemtoFemtoDstReader& copy);
    /// Destructor
    virtual ~MpdFemtoFemtoDstReader();

    /// Return MpdFemtoEvent (back to the analysis)
    virtual MpdFemtoEvent *returnHbtEvent();
    /// Make report
    virtual MpdFemtoString report();

    /// Set femtoDst reader

    void setFemtoDstReader(StFemtoDstReader *femtoDstReader) {
        mFemtoDstReader = femtoDstReader;
    }

private:

    /// Pointer to the StFemtoDstReader
    StFemtoDstReader *mFemtoDstReader;
    /// Pointer to MpdFemtoEvent
    MpdFemtoEvent *mHbtEvent;

    /// Sphericity esitmated by tracks with  |eta|<0.5, pT>0.15, DCA<3 and nHits>10
    float mSphericity;
    /// Sphericity esitmated by tracks with  |eta|<1, pT>0.15, DCA<3 and nHits>10
    float mSphericity2;

    /// Number of events passed
    long int mEventsPassed;

    /// Matrix for sphericity estimation (pT>0.15 && |eta|<0.5)
    TMatrixTSym<double> *mMatrix;
    /// Matrix for sphericity2 estimation (pT>0.15 && |eta|<1.0)
    TMatrixTSym<double> *mMatrix2;

    ClassDef(MpdFemtoFemtoDstReader, 3)
};

#endif // #define MpdFemtoFemtoDstReader_h
