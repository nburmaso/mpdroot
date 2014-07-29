#ifndef MPDTgemGEOPAR_H
#define MPDTgemGEOPAR_H

#include "FairParGenericSet.h"
#include "TH1F.h"
#include "TObjArray.h"

class MpdTgemGeoPar : public FairParGenericSet {
public:
  TObjArray            *fGeoSensNodes; // List of FairGeoNodes for sensitive volumes
  TObjArray            *fGeoPassNodes; // List of FairGeoNodes for sensitive volumes

  MpdTgemGeoPar(const char* name="MpdTgemGeoPar",
             const char* title="Tgem Geometry Parameters",
             const char* context="TestDefaultContext");
  ~MpdTgemGeoPar(void);
  void clear(void);
  void putParams(FairParamList*);
  Bool_t getParams(FairParamList*);
  TObjArray             *GetGeoSensitiveNodes(){return fGeoSensNodes;}
  TObjArray             *GetGeoPassiveNodes(){return fGeoPassNodes;}

  ClassDef(MpdTgemGeoPar,1)
};

#endif /* !MPDTgemGEOPAR_H */
