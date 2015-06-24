#ifndef MPDStrawECTGEOPAR_H
#define MPDStrawECTGEOPAR_H

#include "FairParGenericSet.h"
#include "TH1F.h"
#include "TObjArray.h"

class MpdStrawECTGeoPar : public FairParGenericSet {
public:
  TObjArray            *fGeoSensNodes; // List of FairGeoNodes for sensitive volumes
  TObjArray            *fGeoPassNodes; // List of FairGeoNodes for sensitive volumes

  MpdStrawECTGeoPar(const char* name="MpdStrawECTGeoPar",
             const char* title="StrawECT Geometry Parameters",
             const char* context="TestDefaultContext");
  ~MpdStrawECTGeoPar(void);
  void clear(void);
  void putParams(FairParamList*);
  Bool_t getParams(FairParamList*);
  TObjArray             *GetGeoSensitiveNodes(){return fGeoSensNodes;}
  TObjArray             *GetGeoPassiveNodes(){return fGeoPassNodes;}

  ClassDef(MpdStrawECTGeoPar,1)
};

#endif /* !MPDStrawECTGEOPAR_H */
