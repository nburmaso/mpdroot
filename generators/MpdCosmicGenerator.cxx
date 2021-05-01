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
#include <TTree.h>
#include <iostream>

MpdCosmicGenerator::MpdCosmicGenerator(TString filename, Int_t multi) :
  fFile(nullptr),
  fTree(nullptr),
  fMultiplicity(multi),
  fFileName(filename),
  fPID(0),
  fPosX(0),
  fPosY(0),
  fPosZ(0),
  fPx(0),
  fPy(0),
  fPz(0),
  fShift(0),
  fGeneratedTracks(0) {}

Bool_t MpdCosmicGenerator::ReadEvent(FairPrimaryGenerator* primGen) {
  if (fGeneratedTracks + fMultiplicity >= fIDs.size()) return kFALSE;
  std::vector<int>::const_iterator first = fIDs.begin() + fGeneratedTracks;
  std::vector<int>::const_iterator last  = fIDs.begin() + fGeneratedTracks + fMultiplicity;
  std::vector<int> newVec(first, last);
  std::sort(newVec.begin(), newVec.end());

  for (int iTrack = 0; iTrack < fMultiplicity; iTrack++) {
    Int_t pos = newVec[iTrack];
    fTree->GetEntry(pos);
    primGen->AddTrack(fPID, fPx, fPy, fPz, fPosX * 100.0, fPosY * 100.0 - fShift, fPosZ * 100.0);
    ++fGeneratedTracks;
  }
  return kTRUE;
}

Bool_t MpdCosmicGenerator::Init() {
  fFile = new TFile(fFileName);
  fTree = (TTree*) fFile->Get("CosmicCube");
  if (fTree == nullptr) return kFALSE;
  TBranch* b_PID;     //!
  TBranch* b_wallID;  //!
  TBranch* b_posX;    //!
  TBranch* b_posY;    //!
  TBranch* b_posZ;    //!
  TBranch* b_px;      //!
  TBranch* b_py;      //!
  TBranch* b_pz;      //!
  TBranch* b_pr;      //!
  TBranch* b_ptheta;  //!
  TBranch* b_pphi;    //!
  fTree->SetBranchAddress("PID", &fPID, &b_PID);
  fTree->SetBranchAddress("posX", &fPosX, &b_posX);
  fTree->SetBranchAddress("posY", &fPosY, &b_posY);
  fTree->SetBranchAddress("posZ", &fPosZ, &b_posZ);
  fTree->SetBranchAddress("px", &fPx, &b_px);
  fTree->SetBranchAddress("py", &fPy, &b_py);
  fTree->SetBranchAddress("pz", &fPz, &b_pz);

  TH1D* h          = (TH1D*) fFile->Get("SimulationParameters");
  Double_t simTime = h->GetBinContent(1);
  Double_t edge    = h->GetBinContent(2);
  fShift           = edge * 50.0;
  LOG(info) << "MpdCosmicGenerator: simulation time: " << simTime << " s";
  LOG(info) << "MpdCosmicGenerator: edge size: " << edge << " m";
  Double_t m           = fMultiplicity;
  Double_t tracks      = fTree->GetEntriesFast();
  Double_t simPerEvent = m / tracks * simTime;
  LOG(info) << "MpdCosmicGenerator: simulation time per event: " << simPerEvent << " s";
  fIDs.resize(fTree->GetEntriesFast());
  int i = 0;
  for (auto el : fIDs)
    fIDs[i] = i++;
  std::random_shuffle(fIDs.begin(), fIDs.end());
  return kTRUE;
}

MpdCosmicGenerator::~MpdCosmicGenerator() {
  if (fFile) delete fFile;
}

