#ifndef CBMSTTPARROOTFILEIO_H
#define CBMSTTPARROOTFILEIO_H

#include "FairDetParRootFileIo.h"
#include "TFile.h"
#include "TArrayI.h"

class FairParRootFile;
class FairParSet;
class CbmSttDigiPar;

class CbmSttParRootFileIo : public FairDetParRootFileIo
{
public:
  CbmSttParRootFileIo(FairParRootFile* f);
  ~CbmSttParRootFileIo();
  Bool_t init(FairParSet*,Int_t*);
  Bool_t read(CbmSttDigiPar*,Int_t*);
  ClassDef(CbmSttParRootFileIo,0) // Class for STT parameter I/O from ROOT file
};

#endif  /* !CBMSTTPARROOTFILEIO_H */










