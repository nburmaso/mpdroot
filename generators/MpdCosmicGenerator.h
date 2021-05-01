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
class TTree;

class MpdCosmicGenerator : public FairGenerator {
  TFile* fFile;  //!
  TTree* fTree;  //!
  Int_t fMultiplicity;
  Int_t fGeneratedTracks;
  Int_t fPID;
  Float_t fPosX;
  Float_t fPosY;
  Float_t fPosZ;
  Float_t fPx;
  Float_t fPy;
  Float_t fPz;
  Float_t fShift;
  TString fFileName;
  std::vector<int> fIDs;  //!

public:
  MpdCosmicGenerator(TString filename = "", Int_t multi = 1000);
  virtual Bool_t Init();
  virtual Bool_t ReadEvent(FairPrimaryGenerator* primGen);
  virtual ~MpdCosmicGenerator();
  ClassDef(MpdCosmicGenerator, 1)
};


#endif /* MPDROOT_GENERATORS_MPDCOSMICGENERATOR_H_ */

