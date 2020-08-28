/*
 * NicaMpdMiniDstEvent.cxx
 *
 *  Created on: 17 kwi 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdMiniDstEvent.h"
#include "NicaMpdMiniDstEventInterface.h"
#include "MpdMiniTrack.h"
#include "NicaMpdMiniDstTrack.h"
#include "MpdMiniBTofPidTraits.h"
#include "FairRootManager.h"

NicaMpdMiniDstEvent::NicaMpdMiniDstEvent(eMode mode):NicaExpEventHelix("NicaMpdMiniDstTrack"),fMode(mode) {

}

void NicaMpdMiniDstEvent::ShallowCopyEvent(NicaEvent *event) {
    NicaExpEventHelix::ShallowCopyEvent(event);
}

NicaMpdMiniDstEvent::NicaMpdMiniDstEvent(TString trackname) :NicaExpEventHelix(trackname),fMode(kGlobalTrack){
}

void NicaMpdMiniDstEvent::CreateSource() {
    fSource = new NicaMpdMiniDstEventInterface();
}

void NicaMpdMiniDstEvent::Update() {
    fTracks->Clear();
    NicaMpdMiniDstEventInterface *interface = (NicaMpdMiniDstEventInterface*)fSource;
    MpdMiniEvent *event =(MpdMiniEvent*) interface->GetMiniEvent();
    TVector3 vec = event->primaryVertex();
    fVertex->SetXYZT(vec.X(),vec.Y(),vec.Z(),0);
    vec = event->primaryVertexError();
    fVertexError->SetXYZT(vec.X(),vec.Y(),vec.Z(),0);
    fTotalTracksNo = interface->GetTotalTrackNo();
    fRunInfoId = event->runId();
    fMagField->SetXYZ(0,0,event->bField()*0.1);
    NicaHelix::SetMagField(event->bField()*0.1);
    NicaTrackClones *prim = interface->fTracks;
    fTotalTracksNo  = prim->GetEntriesFast();
    fTracks->ExpandCreateFast(fTotalTracksNo);

    for(int i=0;i<fTotalTracksNo;i++){
        NicaMpdMiniDstTrack *mpd_track = (NicaMpdMiniDstTrack*)fTracks->UncheckedAt(i);
        mpd_track->SetEvent(this);
        MpdMiniTrack *mini_track = (MpdMiniTrack*)prim->UncheckedAt(i);
        mpd_track->Update((MpdMiniTrack*)prim->UncheckedAt(i),fMode);
        mpd_track->GetLink()->Clear();
        mpd_track->GetLink()->SetLink(0, i);
        mpd_track->SetID(i);
    }
    TClonesArray *tof_info = interface->fTofInfo->GetArray();
    // fill tof info
    for(int i=0;i<tof_info->GetEntriesFast();i++){
        MpdMiniBTofPidTraits *tof = (MpdMiniBTofPidTraits*)tof_info->UncheckedAt(i);
        if(tof->trackIndex()<0) continue;
        NicaMpdMiniDstTrack *mpd_track = (NicaMpdMiniDstTrack*)fTracks->UncheckedAt(tof->trackIndex());
        NicaToFTrack *tof_track = mpd_track->GetToFTrack();

        tof_track->SetBeta(tof->beta());
        Double_t p = mpd_track->GetMomentum()->P();
        Double_t p2 = p*p;
        Double_t beta2 = tof->beta()*tof->beta();
        Double_t factor = beta2-beta2*beta2;
        if(factor==0){
            tof_track->SetMass2(-1);
        }else {
            tof_track->SetMass2(tof->massSqr());
        }
        tof_track->SetFlag(+1);
    }
}

Bool_t NicaMpdMiniDstEvent::ExistInTree() const {
    FairRootManager *manager = FairRootManager::Instance();
    manager->Print();
    if(manager->CheckBranch("Event")){
        return kTRUE;
    }
    return kFALSE;
}

TString NicaMpdMiniDstEvent::GetFormatName() const {
    switch(fMode){
    case kGlobalTrack:
        return "NicaMpdMiniDstEventGlobal";
        break;
    case kPrimaryTrack:
        return "NicaMpdMiniDstEventPrimary";
        break;
    }
    return "";
}

NicaMpdMiniDstEvent::~NicaMpdMiniDstEvent() {

}


