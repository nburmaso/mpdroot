#ifndef MPDFSAGEO_H
#define MPDFSAGEO_H

#include "FairGeoSet.h"

class  MpdFsaGeo : public FairGeoSet {

protected:
  char modName[20];  // name of module
  char eleName[20];  // substring for elements in module

public:
  MpdFsaGeo();
  ~MpdFsaGeo() {}
  const char* getModuleName(Int_t);
  const char* getEleName(Int_t);

  ClassDef(MpdFsaGeo,0) // Class for Fsa
};

#endif  /* !MPDFSAGEO_H */
