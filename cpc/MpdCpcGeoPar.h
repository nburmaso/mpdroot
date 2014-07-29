#ifndef MPDCPCGEOPAR_H
#define MPDCPCGEOPAR_H

#include "FairParGenericSet.h"
#include "TH1F.h"

class MpdCpcGeoPar : public FairParGenericSet {
public:
  TObjArray            *fGeoSensNodes; // List of FairGeoNodes for sensitive volumes
  TObjArray            *fGeoPassNodes; // List of FairGeoNodes for sensitive volumes

  MpdCpcGeoPar(const char* name="MpdCpcGeoPar",
             const char* title="Cpc Geometry Parameters",
             const char* context="TestDefaultContext");
  ~MpdCpcGeoPar(void);
  void clear(void);
  void putParams(FairParamList*);
  Bool_t getParams(FairParamList*);
  TObjArray             *GetGeoSensitiveNodes(){return fGeoSensNodes;}
  TObjArray             *GetGeoPassiveNodes(){return fGeoPassNodes;}

  ClassDef(MpdCpcGeoPar,1)
};

#endif /* !MPDCPCGEOPAR_H */
