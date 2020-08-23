/*
 * NicaMiniDstSource.cxx
 *
 *  Created on: 17 kwi 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMiniDstSource.h"
#include "FairRootManager.h"
#include "FairLogger.h"
#include <TList.h>

NicaMiniDstSource::NicaMiniDstSource():NicaMiniDstSource("data.root") {

}

NicaMiniDstSource::NicaMiniDstSource(TString inFile):fFileName(inFile),fChain(nullptr),
        fEvent(nullptr),fTracks(nullptr),fTofInfo(nullptr),fEmcInfo(nullptr),
        fMcEvent(nullptr),fMcTracks(nullptr){
}

Bool_t NicaMiniDstSource::Init() {
    FairRootManager *mngr = FairRootManager::Instance();
    fChain= new TChain("MiniDst");
    if(fFileName.EndsWith(".root")){
        LOG(DEBUG3)<<"NicaMiniDstSource: opening single file"<<fFileName;
        fChain->Add(fFileName);
    }else{// this is long list
        std::ifstream list;
        list.open(fFileName);
        do{
            TString temp;
            list>>temp;
            if(temp.Length()>1){
                fChain->Add(temp);
            }else{
                break;
            }
            LOG(DEBUG3)<<"Adding file "<<temp<<" to chain";
        }while(!list.eof());
        list.close();
    }

    fChain->SetBranchStatus("Event", 1);
    fChain->SetBranchStatus("Track", 1);
    fChain->SetBranchStatus("BTofPidTraits",1);
//    fChain->SetBranchStatus("")
    fChain->SetBranchStatus("McEvent",1);
    fChain->SetBranchStatus("McTrack",1);
    fEvent = new TClonesArray("MpdMiniEvent");
    fMcEvent = new TClonesArray("MpdMiniMcEvent");
    fTracks = new TClonesArray("MpdMiniTrack");
    fTofInfo = new TClonesArray("MpdMiniBTofPidTraits");
    fEmcInfo = new TClonesArray("MpdMiniBECalPidTraits");
    fMcTracks = new TClonesArray("MpdMiniMcTrack");

    fChain->SetBranchAddress("Event", &fEvent);
    fChain->SetBranchAddress("Track", &fTracks);
    fChain->SetBranchAddress("BTofPidTraits",&fTofInfo);
    fChain->SetBranchAddress("McEvent",&fMcEvent);
    fChain->SetBranchAddress("McTrack",&fMcTracks);

    mngr->SetInChain(fChain,-1);\
    mngr->Register("Event","DST",fEvent,kFALSE);
    mngr->Register("Track","DST",fTracks,kFALSE);
    mngr->Register("BTofPidTraits","TOF",fTofInfo,kFALSE);
    mngr->Register("McEvent","MC",fMcEvent,kFALSE);
    mngr->Register("McTrack","MC",fMcTracks,kFALSE);
    return kTRUE;
}

Int_t NicaMiniDstSource::ReadEvent(UInt_t unsignedInt) {
    fChain->GetEntry(unsignedInt);
    return 0;
}

void NicaMiniDstSource::Close() {
}

Int_t NicaMiniDstSource::CheckMaxEventNo(Int_t int1) {
    return fChain->GetEntriesFast();
}

NicaMiniDstSource::NicaMiniDstSource(const NicaMiniDstSource &other) {
    fFileName = other.fFileName;
    if(!other.fChain){
        Init();
    }
}

NicaMiniDstSource::~NicaMiniDstSource() {
    if(fChain) delete fChain;
}

