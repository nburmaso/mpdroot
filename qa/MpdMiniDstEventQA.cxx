/*
 * MpdMiniDstEventQA.cxx
 *
 *  Created on: 10 paź 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdMiniDstEventQA.h"
#include "NicaParameter.h"

MpdMiniDstEventQA::MpdMiniDstEventQA(TString name)
    : MpdMiniDstBaseQAEvent(name, 0, 6, 0) {
  SetAxis2D(kMultiB, 200, 0, 1200, 200, 0, 20);
  SetNameTitle2D(kMultiB, "Multi_B", "MultiB;multi[N];B[fm]");
  SetAxis2D(kTofTpc, 200, 0, 1000, 200, 0, 100);
  SetNameTitle2D(kTofTpc, "Tof_Tpc", "Tof vs Tpc;tof htis;tpc tracks");
  SetAxis2D(kVertexSimRecoZ, 200, -60, 60, 200, -60, 60);
  SetNameTitle2D(kVertexSimRecoZ, "Vzsim_Vzreco",
                 "VzVz;V_{z reco} [cm];V_{z sim} [cm]");
  SetAxis2D(kVertexResoZMult, 200, 0, 5, 200, 0, 1200);
  SetAxis2D(kVertexResoXYMult, 200, 0, 5, 200, 0, 1200);
  SetNameTitle2D(kVertexResoXYMult, "DVertexXY_Multi",
                 "DVertexXY_Multi:#DeltaV_{xy} [cm]; nGlob");
  SetAxis2D(kVertexResoZMult, 200, 0, 5, 200, -60, 60);
  SetNameTitle2D(kVertexResoZMult, "VertexZ_Multi",
                 "Vertex_ZMulti:#DeltaV_{z} [cm]; nGlob");
  SetAxis2D(kVertexResoZZ, 200, 0, 5, 200, -60, 60);
  SetNameTitle2D(kVertexResoZZ, "DVertexZ_VZ",
                 "DVertexZ_VertexZi:#DeltaV_{z} [cm]; V_{z reco} [cm]");
}

MpdMiniDstEventQA::~MpdMiniDstEventQA() {}

void MpdMiniDstEventQA::FillPair(MpdMiniEvent *reco, MpdMiniMcEvent *sim) {
  Get2D(kMultiB)->Fill(reco->numberOfGlobalTracks(), sim->b());
  Get2D(kTofTpc)->Fill(reco->btofTrayMultiplicity(),
                       reco->numberOfGlobalTracks());
  Get2D(kVertexSimRecoZ)
      ->Fill(reco->primaryVertex().Z(), sim->primaryVertexZ());
  Double_t deltaZ =
      TMath::Abs(reco->primaryVertex().Z() - sim->primaryVertexZ());
  Double_t deltaXY = (reco->primaryVertex() - sim->primaryVertex()).Pt();
  Get2D(kVertexResoXYMult)->Fill(deltaXY, reco->numberOfGlobalTracks());
  Get2D(kVertexResoZMult)->Fill(deltaZ, reco->numberOfGlobalTracks());
  Get2D(kVertexResoZZ)->Fill(deltaZ, reco->primaryVertex().Z());
}
