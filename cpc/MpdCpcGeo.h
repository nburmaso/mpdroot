#ifndef MPDCPCGEO_H
#define MPDCPCGEO_H

#include "FairGeoSet.h"

class  MpdCpcGeo : public FairGeoSet {

protected:
  char modName[20];  // name of module
  char eleName[20];  // substring for elements in module

public:
  MpdCpcGeo();
  ~MpdCpcGeo() {}
  const char* getModuleName(Int_t);
  const char* getEleName(Int_t);

  ClassDef(MpdCpcGeo,0) // Class for Cpc
};

#endif  /* !MPDCPCGEO_H */
