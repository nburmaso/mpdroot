#ifndef CBMGEOSTTPAR_H
#define CBMGEOSTTPAR_H

#include "FairParGenericSet.h"
#include "TH1F.h"
#include "TObjArray.h"

class CbmGeoSttPar : public FairParGenericSet
{
public:
  TObjArray            *fGeoSensNodes; // List of FairGeoNodes for sensitive volumes
  TObjArray            *fGeoPassNodes; // List of FairGeoNodes for sensitive volumes

  CbmGeoSttPar(const char* name="CbmGeoSttPar",
	       const char* title="Stt Geometry Parameters",
	       const char* context="TestDefaultContext");
  ~CbmGeoSttPar(void);
  void clear(void);
  void putParams(FairParamList*);
  Bool_t getParams(FairParamList*);
  TObjArray             *GetGeoSensitiveNodes(){return fGeoSensNodes;}
  TObjArray             *GetGeoPassiveNodes(){return fGeoPassNodes;}

  ClassDef(CbmGeoSttPar,1)
};

#endif /* !CBMGEOSTTPAR_H */
