/*
 * MpdCosmicGenerator.h
 *
 *  Created on: 27 kwi 2021
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_GENERATORS_MPDCOSMICGENERATOR_H_
#define MPDROOT_GENERATORS_MPDCOSMICGENERATOR_H_

#include "FairGenerator.h"

#include <TString.h>
#include <vector>

class TFile;
class TNtuple;

class MpdCosmicGenerator : public FairGenerator {
  TFile* fFile;
  TNtuple* fTree;
  Int_t fMultiplicity;
  Int_t fGeneratedTracks;
  Float_t* fData;
  TString fFileName;
  std::vector<int> fIDs;

public:
  MpdCosmicGenerator(TString filename = "", Int_t multi = 1000);
  virtual Bool_t Init();
  virtual Bool_t ReadEvent(FairPrimaryGenerator* primGen);
  virtual ~MpdCosmicGenerator();
  ClassDef(MpdCosmicGenerator, 1)
};


#endif /* MPDROOT_GENERATORS_MPDCOSMICGENERATOR_H_ */
