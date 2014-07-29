#ifndef MPDSFTGEOPAR_H
#define MPDSFTGEOPAR_H

#include "FairParGenericSet.h"
#include "TH1F.h"
#include "TObjArray.h"

class MpdSftGeoPar : public FairParGenericSet {
public:
  TObjArray            *fGeoSensNodes; // List of FairGeoNodes for sensitive volumes
  TObjArray            *fGeoPassNodes; // List of FairGeoNodes for sensitive volumes

  MpdSftGeoPar(const char* name="MpdSftGeoPar",
             const char* title="SFT Geometry Parameters",
             const char* context="TestDefaultContext");
  ~MpdSftGeoPar(void);
  void clear(void);
  void putParams(FairParamList*);
  Bool_t getParams(FairParamList*);
  TObjArray             *GetGeoSensitiveNodes(){return fGeoSensNodes;}
  TObjArray             *GetGeoPassiveNodes(){return fGeoPassNodes;}

  ClassDef(MpdSftGeoPar,1)
};

#endif /* !MPDSFTGEOPAR_H */
