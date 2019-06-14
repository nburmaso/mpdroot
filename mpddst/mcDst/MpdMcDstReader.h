/**
 * \class MpdMcDstReader
 * \brief Allows to read mcDst file or a list of files
 *
 * This class allows to read mcDst.root file or a list of files
 * that contain mcDst and sets up pointers to the mcDst, and
 * certain TClonesArrays that keep Event, Particle, etc...
 * One can also turn on or off certain branches using the
 * SetStatus method.
 */

#ifndef MpdMcDstReader_h
#define MpdMcDstReader_h

// ROOT headers
#include "TChain.h"
#include "TTree.h"
#include "TFile.h"
#include "TString.h"
#include "TClonesArray.h"

// McDst headers
#include "MpdMcDst.h"
#include "MpdMcRun.h"
#include "MpdMcArrays.h"

//_________________

class MpdMcDstReader : public TObject {
public:
    // Empty constructor 

    MpdMcDstReader() {
    }
    /// Constructor that takes either mcDst file or file that
    /// contains a list of mcDst.root files
    MpdMcDstReader(const Char_t* inFileName);
    /// Destructor
    virtual ~MpdMcDstReader();

    /// Return a pointer to mcDst (return nullptr if no dst is found)

    MpdMcDst *mcDst() {
        return mMcDst;
    }
    /// Return a pointer to Run info (return nullptr if not set)
    /// Return pointer to the chain of .mcDst.root files

    TChain *chain() {
        return mChain;
    }
    /// Return pointer to the current TTree

    TTree *tree() {
        return mTree;
    }
    /// Return Run information

    MpdMcRun *run() const {
        return mMcRun;
    }

    /// Set enable/disable branch matching when reading uDst

    void SetStatus(const Char_t* branchNameRegex, Int_t enable) {
        setStatus(branchNameRegex, enable);
    }
    /// Set enable/disable branch matching when reading uDst
    void setStatus(const Char_t* branchNameRegex, Int_t enable);

    /// Calls openRead()
    void Init();
    /// Read next event in the chain
    Bool_t loadEntry(Long64_t iEntry);
    /// Close files and finilize
    void Finish();

private:

    /// Name of the inputfile (or of the inputfiles.list)
    TString mInputFileName;

    /// Turn off streamers
    void streamerOff();

    /// Create TClonesArray of McDst classes and set them to uDst
    void createArrays();
    /// Clear all TClonesArrays with McDst classes
    void clearArrays();
    /// Set adresses of mcArrays and their statuses (enable/disable) to chain
    void setBranchAddresses(TChain *chain);

    /// Pointer to the input/output McDst structure
    MpdMcDst *mMcDst;
    /// Pointer to the Run information
    MpdMcRun *mMcRun;
    /// Pointer to the chain
    TChain *mChain;
    /// Pointer to the current tree
    TTree *mTree;

    /// Event counter
    Int_t mEventCounter;

    /// Pointer to the TClonesArray with the data
    TClonesArray *mMcArrays[MpdMcArrays::NAllMpdMcArrays];
    /// Status of the TClonesArray
    Char_t mStatusArrays[MpdMcArrays::NAllMpdMcArrays];

//#ifdef __ROOT__
    ClassDef(MpdMcDstReader, 0)
//#endif
};

#endif // MpdMcDstReader_h
