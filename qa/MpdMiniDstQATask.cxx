/*
 * MpdQATask.cxx
 *
 *  Created on: 10 paź 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdMiniDstQATask.h"
#include "MpdMiniDstEventQA.h"
#include "NicaPackage.h"
#include "NicaTrackClones.h"
#include "TDatabasePDG.h"

#include "MpdMiniDstTrackQA.h"
MpdMiniDstQATask::MpdMiniDstQATask()
    : NicaSimpleQATask("QA", 1, 9),
      fRecoEvent(new NicaTrackClones("MpdMiniEvent", "Event", "")),
      fRecoTracks(new NicaTrackClones("MpdMiniTrack", "Track", "")),
      fRecoTofInfo(
          new NicaTrackClones("MpdMiniBTofPidTraits", "BTofPidTraits", "")),
      fMcEvent(new NicaTrackClones("McEvent", "McEvent", "")),
      fMcTracks(new NicaTrackClones("MpdMiniMcTrack", "McTrack", "")) {
  SetTrackCollectionName(kProton, "Proton");
  SetTrackCollectionName(kAntiproton, "Antiproton");
  SetTrackCollectionName(kPionPlus, "Pion Plus");
  SetTrackCollectionName(kPionMinus, "Pion MInus");
  SetTrackCollectionName(kKaonPlus, "Kaon Plus");
  SetTrackCollectionName(kKaonMinus, "Kaon Minus");
  SetTrackCollectionName(kElectron, "Electron");
  SetTrackCollectionName(kAntielectron, "Antielectron");
  SetTrackCollectionName(kUnmatched, "Unmatched");
}

MpdMiniDstBaseQAEvent *MpdMiniDstQATask::EventPlots(Int_t no) const {
  return (MpdMiniDstBaseQAEvent *)GetQAEvent(no);
}

MpdMiniDstBaseQATrack *MpdMiniDstQATask::TrackPlots(Int_t no) const {
  return (MpdMiniDstBaseQATrack *)GetQATrack(no);
}

MpdMiniDstQATask::~MpdMiniDstQATask() {}

InitStatus MpdMiniDstQATask::Init() {
  InitStatus stat = NicaSimpleQATask::Init();
  if (stat == kFATAL) return stat;
  fRecoEvent->GetFromTree();
  fRecoTracks->GetFromTree();
  fRecoTofInfo->GetFromTree();
  fMcEvent->GetFromTree();
  fMcTracks->GetFromTree();
  return kSUCCESS;
}

void MpdMiniDstQATask::Exec(Option_t *opt) {
  EventPlots(0)->FillPair((MpdMiniEvent *)fRecoEvent->UncheckedAt(0),
                          (MpdMiniMcEvent *)fMcEvent->UncheckedAt(0));
  for (int i = 0; i < 9; i++) {
    TrackPlots(i)->FillMcEvent((MpdMiniMcEvent *)fMcEvent->UncheckedAt(0));
    TrackPlots(i)->FillRecoEvent((MpdMiniEvent *)fRecoEvent->UncheckedAt(0));
  }
  for (int i = 0; i < fMcTracks->GetEntriesFast(); i++) {
    MpdMiniMcTrack *mc = (MpdMiniMcTrack *)fMcTracks->UncheckedAt(i);
    for (int j = 0; j < 9; j++) {
      TrackPlots(j)->FillSim(mc);
    }
  }
  for (int i = 0; i < fRecoTracks->GetEntriesFast(); i++) {
    MpdMiniTrack *reco = (MpdMiniTrack *)fRecoTracks->UncheckedAt(i);
    Int_t parent_id = reco->mcTrackIndex();
    Int_t tof_id = reco->bTofPidTraitsIndex();
    MpdMiniBTofPidTraits *tof = nullptr;
    MpdMiniMcTrack *mc = nullptr;
    if (parent_id >= 0)
      mc = (MpdMiniMcTrack *)fMcTracks->UncheckedAt(parent_id);
    if (tof_id >= 0) {
      tof = (MpdMiniBTofPidTraits *)fRecoTofInfo->UncheckedAt(tof_id);
    }
    if (mc == nullptr) {
      TrackPlots(kUnmatched)->FillTriple(reco, tof, mc);
    } else {
      switch (mc->pdgId()) {
        case 211:
          TrackPlots(kPionPlus)->FillTriple(reco, tof, mc);
          break;
        case -211:
          TrackPlots(kPionMinus)->FillTriple(reco, tof, mc);
          break;
        case 2212:
          TrackPlots(kProton)->FillTriple(reco, tof, mc);
          break;
        case -2212:
          TrackPlots(kAntiproton)->FillTriple(reco, tof, mc);
          break;
        case 321:
          TrackPlots(kKaonPlus)->FillTriple(reco, tof, mc);
          break;
        case -321:
          TrackPlots(kKaonMinus)->FillTriple(reco, tof, mc);
          break;
        case 11:
          TrackPlots(kElectron)->FillTriple(reco, tof, mc);
          break;
        case -11:
          TrackPlots(kAntielectron)->FillTriple(reco, tof, mc);
          break;
      }
    }
  }
}
