#ifndef MPDDCHGEOPAR_H
#define MPDDCHGEOPAR_H

#include "FairParGenericSet.h"
#include "TH1F.h"
#include "TObjArray.h"

class MpdDchGeoPar : public FairParGenericSet {
public:
  TObjArray            *fGeoSensNodes; // List of FairGeoNodes for sensitive volumes
  TObjArray            *fGeoPassNodes; // List of FairGeoNodes for sensitive volumes

  MpdDchGeoPar(const char* name="MpdDchGeoPar",
             const char* title="Dch Geometry Parameters",
             const char* context="TestDefaultContext");
  ~MpdDchGeoPar(void);
  void clear(void);
  void putParams(FairParamList*);
  Bool_t getParams(FairParamList*);
  TObjArray             *GetGeoSensitiveNodes(){return fGeoSensNodes;}
  TObjArray             *GetGeoPassiveNodes(){return fGeoPassNodes;}

  ClassDef(MpdDchGeoPar,1)
};

#endif /* !MPDDCHGEOPAR_H */
