#ifndef MPDDCHGEO_H
#define MPDDCHGEO_H

#include "FairGeoSet.h"

class  MpdDchGeo : public FairGeoSet {

protected:
  char modName[20];  // name of module
  char eleName[20];  // substring for elements in module

public:
  MpdDchGeo();
  ~MpdDchGeo() {}
  const char* getModuleName(Int_t);
  const char* getEleName(Int_t);

  ClassDef(MpdDchGeo,0) // Class for Dch
};

#endif  /* !MPDDCHGEO_H */
