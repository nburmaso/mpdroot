/*
 * MpdMiniDstEventQA.h
 *
 *  Created on: 10 paź 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_QA_MPDMINIDSTEVENTQA_H_
#define MPDROOT_QA_MPDMINIDSTEVENTQA_H_

#include <TH1D.h>
#include <TH2D.h>
#include <TObject.h>
#include "MpdMiniDstQA.h"
#include "MpdMiniEvent.h"
#include "MpdMiniMcEvent.h"
#include "NicaPackage.h"
#include "NicaQAPlot.h"

class MpdMiniDstEventQA : public MpdMiniDstBaseQAEvent {
 public:
  enum TwoDim {
    kMultiB = 0,
    kTofTpc = 1,
    kVertexSimRecoZ = 2,
    kVertexResoZMult = 3,
    kVertexResoXYMult = 4,
    kVertexResoZZ = 5
  };
  MpdMiniDstEventQA(TString name = "");
  void FillPair(MpdMiniEvent *reco, MpdMiniMcEvent *sim);
  virtual NicaQAPlot *MakeCopy() const { return new MpdMiniDstEventQA(*this); }
  virtual ~MpdMiniDstEventQA();
  ClassDef(MpdMiniDstEventQA, 1)
};

#endif /* MPDROOT_QA_MPDMINIDSTEVENTQA_H_ */
