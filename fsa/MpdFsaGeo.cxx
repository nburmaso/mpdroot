/////////////////////////////////////////////////////////////
// MpdFsaGeo
//
// Class for geometry of FSA
//
/////////////////////////////////////////////////////////////

#include "MpdFsaGeo.h"
#include "FairGeoNode.h"

ClassImp(MpdFsaGeo)

MpdFsaGeo::MpdFsaGeo() {
  // Constructor
  fName="fsa";
  maxSectors=0;
  maxModules=4;
}

const char* MpdFsaGeo::getModuleName(Int_t m) {
  // Returns the module name of fsa number m
  sprintf(modName,"fsa0%i",m+1);
  return modName;
}

const char* MpdFsaGeo::getEleName(Int_t m) {
  // Returns the element name of fsa number m
  sprintf(eleName,"fsa0%i",m+1);
  return eleName;
}
