//-----------------------------------------------------------
// File and Version Information:
// $Id$
//
// Description:
//      Mbb geometry module
//
//
// Environment:
//      Software developed for the PANDA Detector at FAIR.
//
// Author List:
//      Pedro Gonzalez    TUM            (original author)
//      Luis Valenzuela-Cazares
//
//-----------------------------------------------------------

#ifndef MBBGEO_HH
#define MBBGEO_HH

// Base Class Headers ----------------
#include "FairGeoSet.h"

// Collaborating Class Headers -------

// Collaborating Class Declarations --



class MbbGeo : public FairGeoSet {
public:

  // Constructors/Destructors ---------
   MbbGeo();
  ~MbbGeo(){}

  // Operators


  // Accessors -----------------------
  const char* getModuleName(Int_t);
  const char* getEleName(Int_t);
  //inline Int_t getModNumInMod(const TString&);


  // Modifiers -----------------------


  // Operations ----------------------
protected:
  char modName[20];  // name of module
  char eleName[20];  // substring for elements in module
private:

  // Private Data Members ------------


  // Private Methods -----------------

public:
  ClassDef(MbbGeo,1)

};

#endif

//--------------------------------------------------------------
// $Log$
//--------------------------------------------------------------
