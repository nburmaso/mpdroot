/////////////////////////////////////////////////////////////
// MpdStrawendcapGeo
//
// Class for geometry of Strawendcap
//
/////////////////////////////////////////////////////////////

#include "MpdStrawendcapGeo.h"
#include "FairGeoNode.h"

ClassImp(MpdStrawendcapGeo)

MpdStrawendcapGeo::MpdStrawendcapGeo() {
  // Constructor
  fName="Strawendcap";
  maxSectors=0;
  maxModules=24;
}

const char* MpdStrawendcapGeo::getModuleName(Int_t m) {
  // Returns the module name of Strawendcap number m
  //  std::cout << "Strawendcap module: " << m << "\n";
  if (m < 9) {
    sprintf(modName,"stt0%i",m+1);
  } else {
    sprintf(modName,"stt%i",m+1);
  }
  return modName;
}

const char* MpdStrawendcapGeo::getEleName(Int_t m) {
  // Returns the element name of Strawendcap number m
  //  std::cout << "Strawendcap element: " << m << "\n";
  if (m < 9) {
    sprintf(eleName,"stt0%i",m+1);
  } else {
    sprintf(eleName,"stt%i",m+1);
  }
  return eleName;
}


