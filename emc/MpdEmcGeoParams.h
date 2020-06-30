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
  const UInt_t GetNmod() const {return nTowers;}

  const Double_t GetSizeAngleSector() const {return fAngleSector;}
  const Double_t GetSizeAngleCrate() const {return fAngleCrate;}

  const Double_t GetSizeLowBox() const {return sizeLowBox;}
  const Double_t GetSizeHighBox() const {return sizeHighBox;}
  const Double_t GetLengthBox() const {return lengthBox;}
 
  vector<Double_t> GetPhiSector() {return phiSector;}
  vector<Double_t> GetPhiRow() {return phiRow;}
  vector<Double_t> GetXRow() {return xRow;}
  vector<Double_t> GetYRow() {return yRow;}

  vector<Double_t> GetXBox() {return xBox;}
  vector<Double_t> GetYBox() {return yBox;}
  vector<Double_t> GetZBox() {return zBox;}

  vector<Double_t> GetRhoCenterBox() {return rhoBox;}
  vector<Double_t> GetZCenterBox() {return zBox;}

  vector<Double_t> GetThetaBox() {return thetaBox;}
  vector<Double_t> GetPhiBox() {return phiBox;}


  UInt_t GetNsupMod()const {return nSupMod;}


private:

  UInt_t geoVersion; // version number (2 or 3)
    
  Double_t length; // Total EMC length
  Double_t rMin; // Barell minimal radius 
  Double_t rMax; // Barell maximal radius

  UInt_t nSec;  // number of sectors in EMC
  UInt_t nRows; // number of rows in one sector
  UInt_t nTowers;  // number of towers in one rows

  UInt_t  nSupMod;
  
// Static module parameters 

  Double_t fAngleSector; // sector phi angle size
  Double_t fAngleCrate; // crate phi angle 

  Double_t sizeLowBox; // low XY-size of module 
  Double_t sizeHighBox; // high XY-size of module
  Double_t lengthBox; // module Z - length

  vector<Double_t> phiSector; // phi - central angle of each sector
  vector<Double_t> phiRow; // phi - central angle of each row
  vector<Double_t> xRow, yRow; // x and y of row in the local system

  vector<Double_t> xBox, yBox, zBox; // X, Y, Z - coordinate of tower (center)
  vector<Double_t> rhoBox; // tower radius (center)
  vector<Double_t> phiBox, thetaBox; // phi, theta - angles of each tower

  ClassDef(MpdEmcGeoParams, 1)
};

#endif /* MPDEMCDETGEOPARAMS_H */
