#ifndef MBBGEOPAR_H
#define MBBGEOPAR_H

#include "FairParGenericSet.h"
#include "TH1F.h"

class MbbGeoPar : public FairParGenericSet {
public:
  TObjArray            *fGeoSensNodes; // List of FairGeoNodes for sensitive volumes
  TObjArray            *fGeoPassNodes; // List of FairGeoNodes for sensitive volumes

  MbbGeoPar(const char* name="MbbGeoPar",
	    const char* title="Mbb Geometry Parameters",
	    const char* context="MbbDefaultContext");
  ~MbbGeoPar(void);
  void clear(void);
  void putParams(FairParamList*);
  Bool_t getParams(FairParamList*);
  TObjArray             *GetGeoSensitiveNodes(){return fGeoSensNodes;}
  TObjArray             *GetGeoPassiveNodes(){return fGeoPassNodes;}

  ClassDef(MbbGeoPar,1)
};

#endif
