#ifndef MPDStrawendcapGEO_H
#define MPDStrawendcapGEO_H

#include "FairGeoSet.h"

class  MpdStrawendcapGeo : public FairGeoSet {

protected:
  char modName[20];  // name of module
  char eleName[20];  // substring for elements in module

public:
  MpdStrawendcapGeo();
  ~MpdStrawendcapGeo() {}
  const char* getModuleName(Int_t);
  const char* getEleName(Int_t);

  ClassDef(MpdStrawendcapGeo,0) // Class for Strawendcap
};

#endif  /* !MPDStrawendcapGEO_H */
