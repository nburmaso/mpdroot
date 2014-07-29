#ifndef MPDSFTGEO_H
#define MPDSFTGEO_H

#include "FairGeoSet.h"

class  MpdSftGeo : public FairGeoSet {

protected:
  char modName[20];  // name of module
  char eleName[20];  // substring for elements in module

public:
  MpdSftGeo();
  ~MpdSftGeo() {}
  const char* getModuleName(Int_t);
  const char* getEleName(Int_t);

  ClassDef(MpdSftGeo,0) // Class for Sft
};

#endif  /* !MPDSFTGEO_H */
