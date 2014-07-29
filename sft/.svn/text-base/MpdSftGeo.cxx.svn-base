/////////////////////////////////////////////////////////////
// MpdSftGeo
//
// Class for geometry of Silicon Forward Tracker
//
/////////////////////////////////////////////////////////////

#include "MpdSftGeo.h"
#include "FairGeoNode.h"

ClassImp(MpdSftGeo)

MpdSftGeo::MpdSftGeo() {
  // Constructor
  fName="Sft";
  maxSectors=0;
  maxModules=24;
}

const char* MpdSftGeo::getModuleName(Int_t m) {
  // Returns the module name of RHEndcap number m
  // std::cout << "RHEndcap module: " << m << "\n";
  if (m < 9) {
    sprintf(modName,"sft0%i",m+1);
  } else {
    sprintf(modName,"sft%i",m+1);
  }
  return modName;
}

const char* MpdSftGeo::getEleName(Int_t m) {
  // Returns the element name of RHEndcap number m
  // std::cout << "RHEndcap element: " << m << "\n";
  if (m < 9) {
    sprintf(eleName,"sft0%i",m+1);
  } else {
    sprintf(eleName,"sft%i",m+1);
  }
  return eleName;
}


