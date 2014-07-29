#ifndef MPDTgemGEO_H
#define MPDTgemGEO_H

#include "FairGeoSet.h"

class  MpdTgemGeo : public FairGeoSet {

protected:
  char modName[20];  // name of module
  char eleName[20];  // substring for elements in module

public:
  MpdTgemGeo();
  ~MpdTgemGeo() {}
  const char* getModuleName(Int_t);
  const char* getEleName(Int_t);

  ClassDef(MpdTgemGeo,0) // Class for Tgem
};

#endif  /* !MPDTgemGEO_H */
