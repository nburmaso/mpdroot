/*
 * NicaMpdMiniDstTrack.cxx
 *
 *  Created on: 17 kwi 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdMiniDstTrack.h"

NicaMpdMiniDstTrack::NicaMpdMiniDstTrack() {
}

void NicaMpdMiniDstTrack::Update(MpdMiniTrack *track, NicaMpdMiniDstEvent::eMode mode ){
    TVector3 mom;
    switch(mode){
    case NicaMpdMiniDstEvent::eMode::kGlobalTrack:{
        mom = track->gMom();
    }break;
    case NicaMpdMiniDstEvent::eMode::kPrimaryTrack:{
        mom = track->pMom();
    }break;
    }
    GetMomentum()->SetPxPyPzE(mom.X(), mom.Y(), mom.Z(), 0);

    SetID(track->id());
    SetCharge(track->charge());
    SetNHits(track->nHits());
    SetPrimary(track->isPrimary());
    SetChi2(track->chi2());
    fTpcTrack->SetNHits(track->nHits());//! FIXME
    fTpcTrack->SetDeDx(track->dEdx());
    fTpcTrack->SetSigma(track->nSigmaPion(),track->nSigmaKaon(),track->nSigmaProton(),track->nSigmaProton());


    fToFTrack->SetBeta(NicaToFTrack::DummyVal());
    fToFTrack->SetMass2(NicaToFTrack::DummyVal());
    fToFTrack->SetFlag(0);

    TVector3 origin = track->origin();
    fFirstPoint->SetXYZ(origin.X(), origin.Y(), origin.Z());
    Double_t vx = GetEvent()->GetVertex()->X();
    Double_t vy = GetEvent()->GetVertex()->Y();
    Double_t vz = GetEvent()->GetVertex()->Z();
    GetDCA()->SetXYZ(origin.X()-vx, origin.Y()-vy,origin.Z());
    switch(mode){
    case NicaMpdMiniDstEvent::eMode::kGlobalTrack:{
        GetDCA()->SetXYZ(0, 0, 0);
    }break;
    case NicaMpdMiniDstEvent::eMode::kPrimaryTrack:{
        GetDCA()->SetXYZ(0, 0, 0);
        GetDCA()->SetXYZ(origin.X()-vx, origin.Y()-vy,origin.Z());
    }break;
    }
   // fSharedHitsMap = track->topologyMap(0);
   // fHitsMap = track->topologyMap(1);
    NicaHelix *helix = GetHelix();
    helix->SetParams(*fFirstPoint,mom, fCharge);
}

NicaMpdMiniDstTrack::~NicaMpdMiniDstTrack() {
    // TODO Auto-generated destructor stub
}

//==================================================
NicaMpdMiniDstTrackInterface::NicaMpdMiniDstTrackInterface() {
    // TODO Auto-generated constructor stub

}

NicaMpdMiniDstTrackInterface::~NicaMpdMiniDstTrackInterface() {
    // TODO Auto-generated destructor stub
}

