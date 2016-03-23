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

  MpdEmcGeoPar(const char* name, const char* title, const char* context);
  MpdEmcGeoPar();
  ~MpdEmcGeoPar(void);
  void clear(void);
  void putParams(FairParamList*);
  Bool_t getParams(FairParamList*);
  TObjArray* GetGeoSensitiveNodes() {return fGeoSensNodes;}
  TObjArray* GetGeoPassiveNodes()   {return fGeoPassNodes;}
  
  Float_t GetLength() const {return length;}
  Float_t GetRmin() const {return rMin;}
  Float_t GetRmax() const {return rMax;}
  
  UInt_t GetNsec() const {return nSec;}
  UInt_t GetNrows() const {return nRows;}
  UInt_t GetNmod() const {return nMod;}
  
private:
    
  Float_t length;
  Float_t rMin;
  Float_t rMax;
  
  UInt_t nSec;
  UInt_t nRows;
  UInt_t nMod;  

  ClassDef(MpdEmcGeoPar,1)
};

#endif /* MPDEMCDETGEOPAR_H */
