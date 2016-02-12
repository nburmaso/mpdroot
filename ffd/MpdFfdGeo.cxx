/////////////////////////////////////////////////////////////
// MpdFfdGeo
//
// Class for geometry of FFD
//
/////////////////////////////////////////////////////////////

#include "MpdFfdGeo.h"
#include "FairGeoNode.h"

ClassImp(MpdFfdGeo)

MpdFfdGeo::MpdFfdGeo() {
  // Constructor
  fName="FFD";
  maxSectors=0;
  maxModules=4;
}

const char* MpdFfdGeo::getModuleName(Int_t m) {
  // Returns the module name of FFD number m
  sprintf(modName,"ffd0%i",m+1);
  return modName;
}

const char* MpdFfdGeo::getEleName(Int_t m) {
  // Returns the element name of FFD number m
  sprintf(eleName,"ffd0%i",m+1);
  return eleName;
}
