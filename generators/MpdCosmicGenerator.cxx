/*
 * MpdCosmicGenerator.cxx
 *
 *  Created on: 27 kwi 2021
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdCosmicGenerator.h"

#include <FairLogger.h>
#include <FairPrimaryGenerator.h>
#include <TFile.h>
#include <TH1D.h>
#include <TNtuple.h>

MpdCosmicGenerator::MpdCosmicGenerator(TString filename, Int_t multi) :
  fFile(nullptr), fTree(nullptr), fMultiplicity(multi), fFileName(filename), fData(nullptr), fGeneratedTracks(0) {}

Bool_t MpdCosmicGenerator::ReadEvent(FairPrimaryGenerator* primGen) {
  for (int iTrack = 0; iTrack < fMultiplicity; iTrack++) {
    if (fGeneratedTracks == fIDs.size()) return kFALSE;
    Int_t pos = fIDs[fGeneratedTracks];
    fTree->GetEntry(pos);
    Int_t pdgid = fData[0];
    // walliD
    Double_t vx = fData[2];
    Double_t vy = fData[3];
    Double_t vz = fData[4];
    Double_t px = fData[5];
    Double_t py = fData[6];
    Double_t pz = fData[7];

    primGen->AddTrack(pdgid, px, py, pz, vx, vy, vz);
    ++fGeneratedTracks;
  }
  return kTRUE;
}

Bool_t MpdCosmicGenerator::Init() {
  fFile = new TFile(fFileName);
  fTree = (TNtuple*) fFile->Get("CosmicCube");
  if (fTree == nullptr) return kFALSE;
  TH1D* h          = (TH1D*) fFile->Get("SimulationParameters");
  Double_t simTime = h->GetBinContent(1);
  Double_t edge    = h->GetBinContent(2);
  LOG(info) << "MpdCosmicGenerator: simulation time: " << simTime << " s";
  LOG(info) << "MpdCosmicGenerator: edge size: " << edge << " m";
  Double_t m           = fMultiplicity;
  Double_t tracks      = fTree->GetEntriesFast();
  Double_t simPerEvent = m / tracks * simTime;
  LOG(info) << "MpdCosmicGenerator: simulation time per event: " << simPerEvent << " s";
  fIDs.resize(fTree->GetEntriesFast());
  fTotalEvents = fTree->GetEntriesFast() / fMultiplicity;
  int i        = 0;
  for (auto el : fIDs)
    el = i++;
  std::random_shuffle(fIDs.begin(), fIDs.end());
  fData = fTree->GetArgs();
  return kTRUE;
}

MpdCosmicGenerator::~MpdCosmicGenerator() {
  if (fFile) delete fFile;
}
