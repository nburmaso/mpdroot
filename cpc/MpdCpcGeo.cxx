/////////////////////////////////////////////////////////////
// MpdCpcGeo
//
// Class for geometry of CPC
//
/////////////////////////////////////////////////////////////

#include "MpdCpcGeo.h"
#include "FairGeoNode.h"

ClassImp(MpdCpcGeo)

MpdCpcGeo::MpdCpcGeo() {
  // Constructor
  fName="cpc";
  maxSectors=0;
  maxModules=4;
}

const char* MpdCpcGeo::getModuleName(Int_t m) {
  // Returns the module name of cpc number m
  sprintf(modName,"cpc0%i",m+1);
  return modName;
}

const char* MpdCpcGeo::getEleName(Int_t m) {
  // Returns the element name of cpc number m
  sprintf(eleName,"cpc0%i",m+1);
  return eleName;
}
