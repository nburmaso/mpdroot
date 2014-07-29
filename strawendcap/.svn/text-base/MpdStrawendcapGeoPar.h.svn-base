#ifndef MPDStrawendcapGEOPAR_H
#define MPDStrawendcapGEOPAR_H

#include "FairParGenericSet.h"
#include "TH1F.h"
#include "TObjArray.h"

class MpdStrawendcapGeoPar : public FairParGenericSet {
public:
  TObjArray            *fGeoSensNodes; // List of FairGeoNodes for sensitive volumes
  TObjArray            *fGeoPassNodes; // List of FairGeoNodes for sensitive volumes

  MpdStrawendcapGeoPar(const char* name="MpdStrawendcapGeoPar",
             const char* title="Strawendcap Geometry Parameters",
             const char* context="TestDefaultContext");
  ~MpdStrawendcapGeoPar(void);
  void clear(void);
  void putParams(FairParamList*);
  Bool_t getParams(FairParamList*);
  TObjArray             *GetGeoSensitiveNodes(){return fGeoSensNodes;}
  TObjArray             *GetGeoPassiveNodes(){return fGeoPassNodes;}

  ClassDef(MpdStrawendcapGeoPar,1)
};

#endif /* !MPDStrawendcapGEOPAR_H */
