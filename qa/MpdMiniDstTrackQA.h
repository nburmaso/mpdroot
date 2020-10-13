/*
 * MpdSimTrackQA.h
 *
 *  Created on: 10 paź 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_QA_MPDMINIDSTTRACKQA_H_
#define MPDROOT_QA_MPDMINIDSTTRACKQA_H_

#include <TH1D.h>
#include <TH2D.h>
#include <TNamed.h>
#include "MpdMiniBTofPidTraits.h"
#include "MpdMiniDstQA.h"
#include "MpdMiniEvent.h"
#include "MpdMiniMcEvent.h"
#include "MpdMiniMcTrack.h"
#include "MpdMiniTrack.h"
#include "NicaHistogramManager.h"
#include "NicaPackage.h"
#include "NicaQAPlot.h"
class MpdMiniDstTrackQA : public MpdMiniDstBaseQATrack {
 public:
  enum TwoDimHist {
    kDedx = 0,
    kPtEtaReco = 1,
    kPtEtaSim = 2,
    kPM2 = 3,
    kPtMomReso = 4,
    ktPzMomReso = 5,
  };
  enum OneDimHist { kTpcHits = 0 };

  MpdMiniDstTrackQA(TString name = "");
  MpdMiniDstTrackQA *MakeCopy() const { return new MpdMiniDstTrackQA(*this); };
  void Set2DimAxes(TwoDimHist no, TVector3 xAxis, TVector3 xYaxis);
  void Set1DimAxis(OneDimHist no, TVector3 xAxis);
  void FillTriple(MpdMiniTrack *reco, MpdMiniBTofPidTraits *tof,
                  MpdMiniMcTrack *sim);
  void FillSim(MpdMiniMcTrack *sim);
  virtual ~MpdMiniDstTrackQA();
  ClassDef(MpdMiniDstTrackQA, 1)
};

#endif /* MPDROOT_QA_MPDMINIDSTTRACKQA_H_ */
