/*
 * NicaMpdMiniDstMcEvent.cxx
 *
 *  Created on: 17 kwi 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaMpdMiniDstMcEvent.h"
#include <TList.h>
#include "FairRootManager.h"
#include "MpdMiniMcTrack.h"
#include "NicaMpdMiniDstMcEventInterface.h"
#include "NicaMpdMiniDstMcTrack.h"

NicaMpdMiniDstMcEvent::NicaMpdMiniDstMcEvent()
    : NicaMCEvent("NicaMpdMiniDstMcTrack") {
  // TODO Auto-generated constructor stub
}

NicaMpdMiniDstMcEvent::NicaMpdMiniDstMcEvent(const NicaMpdMiniDstMcEvent &other)
    : NicaMCEvent(other) {}

void NicaMpdMiniDstMcEvent::CreateSource() {
  fSource = new NicaMpdMiniDstMcEventInterface();
}

void NicaMpdMiniDstMcEvent::Update() {
  MpdMiniMcEvent *event =
      static_cast<NicaMpdMiniDstMcEventInterface *>(fSource)->GetEvent();
  TClonesArray *tracks = static_cast<NicaMpdMiniDstMcEventInterface *>(fSource)
                             ->fTracks->GetArray();
  fTracks->Clear();
  fTracks->ExpandCreateFast(tracks->GetEntriesFast());
  fTotalTracksNo = fTracks->GetEntriesFast();
  fVertex->SetXYZT(event->primaryVertexX(), event->primaryVertexY(),
                   event->primaryVertexZ(), 0);
  SetPhi(event->reactionPlaneAngle(), 0);
  fB = (event->impactParameter());
  for (int i = 0; i < tracks->GetEntriesFast(); i++) {
    MpdMiniMcTrack *track = (MpdMiniMcTrack *)tracks->UncheckedAt(i);
    NicaMpdMiniDstMcTrack *mc =
        (NicaMpdMiniDstMcTrack *)fTracks->UncheckedAt(i);
    mc->GetMomentum()->SetPxPyPzE(track->px(), track->py(), track->pz(),
                                  track->energy());
    mc->SetPrimary();
    mc->SetCharge(CalculateCharge(track->pdgId()));
    mc->SetPdg(track->pdgId());
    mc->GetStartPosition()->SetXYZT(track->x(), track->y(), track->z(),
                                    track->t());
    mc->GetLink()->Clear();
    mc->GetLink()->SetLink(0, i);
    mc->SetID(i);
  }
}

void NicaMpdMiniDstMcEvent::Clear(Option_t *opt) { fTracks->Clear(opt); }

void NicaMpdMiniDstMcEvent::Print() { NicaMCEvent::Print(); }

Bool_t NicaMpdMiniDstMcEvent::ExistInTree() const {
  FairRootManager *manager = FairRootManager::Instance();
  Int_t header = manager->CheckBranch("McEvent");
  if (header > 1) header = 1;
  Int_t tracks = manager->CheckBranch("McTrack");
  if ((header + tracks) >= 2) {
    return kTRUE;
  }
  return kTRUE;
  return kFALSE;
}

TString NicaMpdMiniDstMcEvent::GetFormatName() const {
  return "MpdMiniDstMcEvent";
}

NicaMpdMiniDstMcEvent::~NicaMpdMiniDstMcEvent() { delete fTracks; }
