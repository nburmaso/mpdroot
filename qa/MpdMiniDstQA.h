/*
 * MpdMiniDstQA.h
 *
 *  Created on: 11 paź 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_QA_MPDMINIDSTQA_H_
#define MPDROOT_QA_MPDMINIDSTQA_H_

#include "MpdMiniBTofPidTraits.h"
#include "MpdMiniEvent.h"
#include "MpdMiniMcEvent.h"
#include "MpdMiniMcTrack.h"
#include "MpdMiniTrack.h"
#include "NicaQAPlot.h"

class MpdMiniDstBaseQAEvent : public NicaQAPlot {
 protected:
  MpdMiniDstBaseQAEvent(TString name, Int_t dim1, Int_t dim2, Int_t dim3);

 public:
  MpdMiniDstBaseQAEvent();
  virtual void FillPair(MpdMiniEvent *reco, MpdMiniMcEvent *sim) = 0;
  virtual ~MpdMiniDstBaseQAEvent(){};
  ClassDef(MpdMiniDstBaseQAEvent, 1)
};

class MpdMiniDstBaseQATrack : public NicaQAPlot {
 protected:
  MpdMiniEvent *fRecoEvent;
  MpdMiniMcEvent *fSimEvent;
  MpdMiniDstBaseQATrack(TString name, Int_t dim1, Int_t dim2, Int_t dim3);

 public:
  MpdMiniDstBaseQATrack();
  void FillMcEvent(MpdMiniMcEvent *sim) { fSimEvent = sim; };
  void FillRecoEvent(MpdMiniEvent *reco) { fRecoEvent = reco; };
  virtual void FillTriple(MpdMiniTrack *reco, MpdMiniBTofPidTraits *tof,
                          MpdMiniMcTrack *sim) = 0;
  virtual void FillSim(MpdMiniMcTrack *sim) = 0;
  virtual ~MpdMiniDstBaseQATrack(){};
  ClassDef(MpdMiniDstBaseQATrack, 1)
};
#endif /* MPDROOT_QA_MPDMINIDSTQA_H_ */
