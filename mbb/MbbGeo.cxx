//-----------------------------------------------------------
// File and Version Information:
// $Id$
//
// Description:
//      Implementation of class MbbGeo
//      see MbbGeo.hh for details
//
// Environment:
//      Software developed for the PANDA Detector at FAIR.
//
// Author List:
//      Pedro González    TUM            (original author)
//
//
//-----------------------------------------------------------
// Panda Headers --------------------------------------------

// This Class' Header ------------------
#include "MbbGeo.h"

// C/C++ Headers ----------------------


// Collaborating Class Headers --------
#include "FairGeoNode.h"

// Class Member definitions -----------

ClassImp(MbbGeo)

// -----   Default constructor   -------------------------------------------
MbbGeo::MbbGeo() {
  // Constructor
  fName="mbb";
  maxSectors=1;
  maxModules=320; //Adapt for MBB.
}

// -------------------------------------------------------------------------

const char* MbbGeo::getModuleName(Int_t m) {

  // Returns the module name of Mbb number m

  // Setting tpc here means that all modules names in the ASCII file should start with tpc otherwise they will not be constructed


  //sprintf(modName,"bmdChamber%i",m+1);
  sprintf(modName,"MBB");
  return modName;
}

const char* MbbGeo::getEleName(Int_t m) {

    printf("Entering to MbbGeo");
  // Returns the element name of Det number m
  if( m == 0 )
  sprintf(eleName,"BMDPLUSA");
  else if ( m == 1 )
  sprintf(eleName,"BMDPLUSC");
  
  return eleName;

}
