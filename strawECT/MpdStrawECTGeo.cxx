/////////////////////////////////////////////////////////////
// MpdStrawECTGeo
//
// Class for geometry of StrawECT
//
/////////////////////////////////////////////////////////////

#include "MpdStrawECTGeo.h"
#include "FairGeoNode.h"

ClassImp(MpdStrawECTGeo)

MpdStrawECTGeo::MpdStrawECTGeo() {
  // Constructor
  fName="StrawECT";
  maxSectors=0;
  maxModules=24;
}

const char* MpdStrawECTGeo::getModuleName(Int_t m) {
  // Returns the module name of StrawECT number m
  //  std::cout << "StrawECT module: " << m << "\n";
  if (m < 9) {
    sprintf(modName,"stt0%i",m+1);
  } else {
    sprintf(modName,"stt%i",m+1);
  }
  return modName;
}

const char* MpdStrawECTGeo::getEleName(Int_t m) {
  // Returns the element name of StrawECT number m
  //  std::cout << "StrawECT element: " << m << "\n";
  if (m < 9) {
    sprintf(eleName,"stt0%i",m+1);
  } else {
    sprintf(eleName,"stt%i",m+1);
  }
  return eleName;
}


