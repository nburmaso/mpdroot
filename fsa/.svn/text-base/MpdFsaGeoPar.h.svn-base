#ifndef MPDFSAGEOPAR_H
#define MPDFSAGEOPAR_H

#include "FairParGenericSet.h"
#include "TH1F.h"

class MpdFsaGeoPar : public FairParGenericSet {
public:
  TObjArray            *fGeoSensNodes; // List of FairGeoNodes for sensitive volumes
  TObjArray            *fGeoPassNodes; // List of FairGeoNodes for sensitive volumes

  MpdFsaGeoPar(const char* name="MpdFsaGeoPar",
             const char* title="Fsa Geometry Parameters",
             const char* context="TestDefaultContext");
  ~MpdFsaGeoPar(void);
  void clear(void);
  void putParams(FairParamList*);
  Bool_t getParams(FairParamList*);
  TObjArray             *GetGeoSensitiveNodes(){return fGeoSensNodes;}
  TObjArray             *GetGeoPassiveNodes(){return fGeoPassNodes;}

  ClassDef(MpdFsaGeoPar,1)
};

#endif /* !MPDFSAGEOPAR_H */
