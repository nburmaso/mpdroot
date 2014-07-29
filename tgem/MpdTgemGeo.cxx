/////////////////////////////////////////////////////////////
// MpdTgemGeo
//
// Class for geometry of Tgem
//
/////////////////////////////////////////////////////////////

#include "MpdTgemGeo.h"
#include "FairGeoNode.h"

ClassImp(MpdTgemGeo)

MpdTgemGeo::MpdTgemGeo() {
  // Constructor
  fName="Tgem";
  maxSectors=0;
  maxModules=24;
}

const char* MpdTgemGeo::getModuleName(Int_t m) {
  // Returns the module name of Tgem number m
  //  std::cout << "Tgem module: " << m << "\n";
  if (m < 9) {
    sprintf(modName,"tgem0%i",m+1);
  } else {
    sprintf(modName,"tgem%i",m+1);
  }
  return modName;
}

const char* MpdTgemGeo::getEleName(Int_t m) {
  // Returns the element name of Tgem number m
  //  std::cout << "Tgem element: " << m << "\n";
  if (m < 9) {
    sprintf(eleName,"tgem0%i",m+1);
  } else {
    sprintf(eleName,"tgem%i",m+1);
  }
  return eleName;
}


