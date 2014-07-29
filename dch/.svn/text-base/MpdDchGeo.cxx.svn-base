/////////////////////////////////////////////////////////////
// MpdDchGeo
//
// Class for geometry of Dch
//
/////////////////////////////////////////////////////////////

#include "MpdDchGeo.h"
#include "FairGeoNode.h"

ClassImp(MpdDchGeo)

MpdDchGeo::MpdDchGeo() {
  // Constructor
  fName="Dch";
  maxSectors=0;
  maxModules=24;
}

const char* MpdDchGeo::getModuleName(Int_t m) {
  // Returns the module name of Dch number m
  //  std::cout << "Dch module: " << m << "\n";
  if (m < 9) {
    sprintf(modName,"dch0%i",m+1);
  } else {
    sprintf(modName,"dch%i",m+1);
  }
  return modName;
}

const char* MpdDchGeo::getEleName(Int_t m) {
  // Returns the element name of Dch number m
  //  std::cout << "Dch element: " << m << "\n";
  if (m < 9) {
    sprintf(eleName,"dch0%i",m+1);
  } else {
    sprintf(eleName,"dch%i",m+1);
  }
  return eleName;
}


