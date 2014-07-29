#ifndef MPDEMCDETGEOPAR_H
#define MPDEMCDETGEOPAR_H

#include "FairParGenericSet.h"

class TObjArray;
class FairParamList;

class MpdEmcGeoPar       : public FairParGenericSet {
public:
 
  /** List of FairGeoNodes for sensitive  volumes */
  TObjArray      *fGeoSensNodes; 

  /** List of FairGeoNodes for sensitive  volumes */
  TObjArray      *fGeoPassNodes; 

  MpdEmcGeoPar(const char* name="MpdEmcGeoPar",
		       const char* title="MpdEmc Geometry Parameters",
		       const char* context="TestDefaultContext");
  ~MpdEmcGeoPar(void);
  void clear(void);
  void putParams(FairParamList*);
  Bool_t getParams(FairParamList*);
  TObjArray* GetGeoSensitiveNodes() {return fGeoSensNodes;}
  TObjArray* GetGeoPassiveNodes()   {return fGeoPassNodes;}

  ClassDef(MpdEmcGeoPar,1)
};

#endif /* MPDEMCDETGEOPAR_H */
