#ifndef MPDEMCDETGEOPARAMS_H
#define MPDEMCDETGEOPARAMS_H

#include "FairParGenericSet.h"
#include "TMath.h"

class TObjArray;
class FairParamList;

using namespace std;
using namespace TMath;

class MpdEmcGeoParams       : public FairParGenericSet {
public:
 
  /** List of FairGeoNodes for sensitive  volumes */
  TObjArray      *fGeoSensNodes; 

  /** List of FairGeoNodes for sensitive  volumes */
  TObjArray      *fGeoPassNodes; 

  MpdEmcGeoParams(const char* name, const char* title, const char* context);
  MpdEmcGeoParams();
  ~MpdEmcGeoParams(void);
  void clear(void);
  void putParams(FairParamList*);
  Bool_t getParams(FairParamList*);
  TObjArray* GetGeoSensitiveNodes() {return fGeoSensNodes;}
  TObjArray* GetGeoPassiveNodes()   {return fGeoPassNodes;}
  
  const Double_t GetLength() const {return length;}
  const Double_t GetRmin() const {return rMin;}
  const Double_t GetRmax() const {return rMax;}
  
  const UInt_t GetNsec() const {return nSec;}
  const UInt_t GetNrows() const {return nRows;}
  const UInt_t GetNmod() const {return nMod;}

  const Double_t GetSizeAngleSector1() const {return fAngleSector1;}
  const Double_t GetSizeAngleSector2() const {return fAngleSector2;}

  const Double_t GetSizeLowBox() const {return sizeLowBox;}
  const Double_t GetSizeHighBox() const {return sizeHighBox;}
  const Double_t GetLengthBox() const {return lengthBox;}
 
  vector<Double_t> GetPhiSector() {return phiSector;}
  vector<Double_t> GetPhiRow() {return phiRow;}
  vector<Double_t> GetThetaBox() {return thetaBox;}
  vector<Double_t> GetRhoCenterBox() {return rhoCenterBox;}
  vector<Double_t> GetZCenterBox() {return zCenterBox;}

  UInt_t GetNsupMod()const {return nSupMod;}


private:

    
  Double_t length; // Total EMC length
  Double_t rMin; // Barell minimal radius 
  Double_t rMax; // Barell maximal radius

  UInt_t nSec;  // number of sectors in EMC
  UInt_t nRows; // number of rows in one sector
  UInt_t nMod;  // number of modules in one rows

  UInt_t  nSupMod;
  
// Static module parameters 

  Double_t fAngleSector1; // big sector angle size
  Double_t fAngleSector2; // small sector angle size

  Double_t sizeLowBox; // low XY-size of module 
  Double_t sizeHighBox; // high XY-size of module
  Double_t lengthBox; // module Z - length

  vector<Double_t> phiSector; // phi - central angle of each sector
  vector<Double_t> phiRow; // phi - central angle of each row
  vector<Double_t> thetaBox; // theta - angle of each module in row
  vector<Double_t> rhoCenterBox; // XY - radius of module (center)
  vector<Double_t> zCenterBox; // Z - coodinate of module (center)

  ClassDef(MpdEmcGeoParams, 1)
};

#endif /* MPDEMCDETGEOPARAMS_H */
