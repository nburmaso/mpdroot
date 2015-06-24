#ifndef MPDStrawECTGEO_H
#define MPDStrawECTGEO_H

#include "FairGeoSet.h"

class  MpdStrawECTGeo : public FairGeoSet {

protected:
  char modName[20];  // name of module
  char eleName[20];  // substring for elements in module

public:
  MpdStrawECTGeo();
  ~MpdStrawECTGeo() {}
  const char* getModuleName(Int_t);
  const char* getEleName(Int_t);

  ClassDef(MpdStrawECTGeo,0) // Class for StrawECT
};

#endif  /* !MPDStrawECTGEO_H */
