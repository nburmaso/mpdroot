#ifndef MPDMINIDSTFILLTASK_H
#define MPDMINIDSTFILLTASK_H

// ROOT and FAIRROOT headers
#include <TNamed.h>
#include <TClass.h>
#include <TChain.h>
#include <TTree.h>
#include <TString.h>
#include <TClonesArray.h>
#include <TFile.h>
#include <FairTask.h>
#include <FairRunAna.h>
#include <FairField.h>
#include <FairRootManager.h>
#include <FairMCEventHeader.h>
#include <FairEventHeader.h>
#include <FairMCTrack.h>

// MpdRoot headers
#include <MpdEvent.h>
#include <MpdVertex.h>
#include <MpdTrack.h>
#include <MpdTpcKalmanTrack.h>
#include <MpdTofHit.h>
#include <MpdTofMatchingData.h>
#include <MpdMiniDst.h>
#include <MpdMiniArrays.h>
#include <MpdMiniEvent.h>
#include <MpdMiniTrack.h>
#include <MpdMiniMcTrack.h>
#include <MpdMiniMessMgr.h>
#include <MpdMiniTrackCovMatrix.h>
#include <MpdMiniBTofHit.h>
#include <MpdMiniBTofPidTraits.h>
#include <MpdMiniBECalHit.h>
#include <MpdMiniBECalPidTraits.h>
#include <MpdMiniMcEvent.h>

using namespace std;

class MpdMiniDstFillTask : public FairTask {
public:
    
    // Default constructor
    MpdMiniDstFillTask();

    // Constructor with output file name for miniDST and IO-modes (read or write)
    MpdMiniDstFillTask(TString);

    virtual ~MpdMiniDstFillTask();

    virtual InitStatus Init();
    virtual void Exec(Option_t* option);
    virtual void Finish();
    
    // Public modifiers to include / exclude a det. system from MiniDST
    void isUseTpc(Bool_t flag) {
        fIsUseTpc = flag;
    }
    
    void isUseTof(Bool_t flag) {
        fIsUseToF = flag;
    }
    
    void isUseEcal(Bool_t flag) {
        fIsUseEcal = flag;
    }
     
    void isUseMcTracks(Bool_t flag) {
        fIsUseMcTracks = flag;
    }

private:
    Bool_t fIsUseTpc;
    Bool_t fIsUseToF;
    Bool_t fIsUseEcal;
    Bool_t fIsUseMcTracks;
        
    FairMCEventHeader* fEventHeaders;
    MpdEvent* fEvents;
    TClonesArray* fVertices;
    TClonesArray* fTpcTracks;
    TClonesArray* fTofHits;
    TClonesArray* fTofMatching;
    TClonesArray* fMCTracks;

    /// A pointer to the main input/outpur miniDst structure containing all `TObjArray`s
    MpdMiniDst* mMiniDst;

    /// Magnetic field of the current event
    Float_t mBField;

    /// Output file name
    TString mOutputFileName;
    /// Pointer to the output file
    TFile* mOutputFile;

    /// Pointer to the TTree with miniDst
    TTree* mTTree;

       /// Splitting level of ROOT file
    Int_t mSplit;
    /// Compression level
    Int_t mCompression;
    /// Size of the buffer
    Int_t mBufferSize;

    /// Pointer to the mini arrays
    TClonesArray** mMiniArrays;
   
    /// Turn-off ROOT streamers
    void streamerOff();

    /// Create arrays
    void createArrays();

    /// Fill event information
    void fillEvent();

    /// Fill track information
    void fillTracks();

    /// Fill BTOF information
    void fillBTofHits();

    ClassDef(MpdMiniDstFillTask, 1)
};
#endif