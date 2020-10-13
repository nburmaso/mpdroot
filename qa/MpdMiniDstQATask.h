/*
 * MpdQATask.h
 *
 *  Created on: 10 paź 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_QA_MPDMINIDSTQATASK_H_
#define MPDROOT_QA_MPDMINIDSTQATASK_H_

#include <FairTask.h>

#include "MpdMiniDstTrackQA.h"
#include "NicaSimpleQATask.h"
class TDatabasePDG;
class NicaTrackClones;
class MpdMiniDstEventQA;

class MpdMiniDstQATask : public NicaSimpleQATask {
  NicaTrackClones *fRecoEvent;
  NicaTrackClones *fRecoTracks;
  NicaTrackClones *fRecoTofInfo;

  NicaTrackClones *fMcEvent;
  NicaTrackClones *fMcTracks;

  enum {
    kProton,
    kAntiproton,
    kPionPlus,
    kPionMinus,
    kKaonPlus,
    kKaonMinus,
    kElectron,
    kAntielectron,
    kUnmatched
  };

 protected:
  virtual InitStatus Init();
  MpdMiniDstBaseQAEvent *EventPlots(Int_t no) const;
  MpdMiniDstBaseQATrack *TrackPlots(Int_t no) const;

 public:
  MpdMiniDstQATask();
  virtual void Exec(Option_t *opt);
  virtual ~MpdMiniDstQATask();
  ClassDef(MpdMiniDstQATask, 1)
};

#endif /* MPDROOT_QA_MPDMINIDSTQATASK_H_ */
