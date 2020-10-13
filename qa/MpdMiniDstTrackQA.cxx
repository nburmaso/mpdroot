/*
 * MpdSimTrackQA.cxx
 *
 *  Created on: 10 paź 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdMiniDstTrackQA.h"

#include <TNamed.h>
#include <TString.h>
#include "NicaParameter.h"
#include "NicaStd.h"

MpdMiniDstTrackQA::MpdMiniDstTrackQA(TString name)
    : MpdMiniDstBaseQATrack(name, 1, 7, 0) {
  SetAxis1D(kTpcHits, 56, 0.5, 55.5);
  SetAxis2D(kDedx, 200, 0, 4, 200, 0, 5E+4);
  SetAxis2D(kPtEtaReco, 200, -4, 4, 200, 0, 4);
  SetAxis2D(kPtEtaSim, 200, -4, 4, 200, 0, 4);
  SetAxis2D(kPM2, 200, 0, 4, 200, -.1, 1.5);
  SetAxis2D(kPtMomReso, 200, 0, 4, 100, -0.1, .1);
  SetAxis2D(ktPzMomReso, 200, 0, 4, 100, -0.1, .1);
  SetAxis2D(6, 200, -4, 4, 200, 0, 4);
  SetNameTitle1D(kTpcHits, "NTpcHits", "NTpcHits;N;N");

  SetNameTitle2D(kDedx, "dEdX", "dEdX;p[GeV/c];dEdX");
  SetNameTitle2D(kPM2, "mTof", "mTof;p[GeV/c];m^{2}_{Tof} [GeV^{2}/c^{4}]");
  SetNameTitle2D(kPtEtaReco, "kinReco",
                 "kinReco;p_{T reco}[GeV/c];#eta_{reco}");
  SetNameTitle2D(kPtEtaSim, "kinSim", "kinSim;p_{T sim}[GeV/c];#eta_{sim}");
  SetNameTitle2D(kPtMomReso, "ptReso",
                 "ptReso;p_{T reco}[GeV/c];(p_{T reco}-p_{T sim})/p_{T reco}");
  SetNameTitle2D(ktPzMomReso, "pzReso",
                 "pzReso;p_{z reco}[GeV/c];(p_{z reco}-p_{z sim})/p_{z reco}");
  SetNameTitle2D(6, "Efficiency", "Efficiency;p_{T reco}[GeV/c];#eta_{reco}");
  // divide to get efficiency
  SetFlag2D(6, '/', kPtEtaReco, kPtEtaSim);
}

MpdMiniDstTrackQA::~MpdMiniDstTrackQA() {}

void MpdMiniDstTrackQA::FillSim(MpdMiniMcTrack *sim) {
  TVector3 mom(sim->px(), sim->py(), sim->pz());
  Get2D(kPtEtaSim)->Fill(mom.Eta(), mom.Pt());
}

void MpdMiniDstTrackQA::FillTriple(MpdMiniTrack *reco,
                                   MpdMiniBTofPidTraits *tof,
                                   MpdMiniMcTrack *sim) {
  Get1D(kTpcHits)->Fill(reco->nHits());
  Get2D(kDedx)->Fill(reco->pPtot(), reco->dEdx());
  Get2D(kPtEtaReco)->Fill(reco->pMom().Eta(), reco->pPt());

  if (tof) {
    Get2D(kPM2)->Fill(reco->pPtot(), tof->massSqr());
  }
  if (sim) {
    TVector3 momReco = reco->pMom();
    TVector3 simReco = sim->p();
    Double_t pt = momReco.Pt();
    Double_t pz = momReco.Pz();
    Double_t dpt = momReco.Pt() - simReco.Pt();
    Double_t dpz = momReco.Z() - simReco.Z();
    Get2D(kPtMomReso)->Fill(pt, (dpt / pt));
    Get2D(ktPzMomReso)->Fill(pz, (dpz / pz));
  }
}

void MpdMiniDstTrackQA::Set2DimAxes(TwoDimHist no, TVector3 xAxis,
                                    TVector3 xYaxis) {
  SetAxis2D(no, xAxis.X(), xAxis.Y(), xAxis.Z(), xYaxis.X(), xYaxis.Y(),
            xYaxis.Z());
}

void MpdMiniDstTrackQA::Set1DimAxis(OneDimHist no, TVector3 xAxis) {
  SetAxis1D(no, xAxis.X(), xAxis.Y(), xAxis.Z());
}
