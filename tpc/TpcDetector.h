//-----------------------------------------------------------
// File and Version Information:
// $Id$
//
// Description:
//      Detector class - sensitive detector for VMC
//
//
// Environment:
//      Software developed for the PANDA Detector at FAIR.
//
// Author List:
//      Sebastian Neubert    TUM            (original author)
//
//
//-----------------------------------------------------------

#ifndef TPCDETECTOR_HH
#define TPCDETECTOR_HH

// Base Class Headers ----------------
#include "FairDetector.h"

// Collaborating Class Headers -------
#include <ostream> // remove if you do not need streaming op
#include "TVector3.h"

// Collaborating Class Declarations --
class TClonesArray;
class FairVolume;
class TpcPoint;


class TpcDetector : public FairDetector{
public:	
  // Constructors/Destructors ---------
  /**      Name :  Detector Name
	   Active: kTRUE for active detectors  
	   (ProcessHits() will be called)
	   kFALSE for inactive detectors     */
  TpcDetector(const char * Name, Bool_t Active);
  
  /**      default constructor    */
  TpcDetector();
  /**       destructor     */
  virtual ~TpcDetector();
  
  /**      Initialization of the detector is done here    */
  //virtual void   Initialize();
  
  /**       this method is called for each step 
            during simulation (see FairMCApplication::Stepping())    */
  virtual Bool_t ProcessHits( FairVolume *v=0);
  
  /**       Registers the produced collections in CBMRootManager.     */
  virtual void   Register();

  virtual void Print() const;
  
  /** Gets the produced collections */
  virtual TClonesArray* GetCollection(Int_t iColl) const ;
  
  /**      has to be called after each event 
	   to reset the containers        */
  virtual void   Reset();
  
  /**      Create the detector geometry        */
  void ConstructGeometry();
  
  void EndOfEvent();
  
  TpcPoint* AddHit(Int_t trackID, Int_t detID, TVector3 pos,
		   TVector3 mom, Double_t time, Double_t length,
		   Double_t eLoss);
  // Operators
  
  
  // Accessors -----------------------
  
  
  // Modifiers -----------------------
  
  
  // Operations ----------------------
  
private:

  // Private Data Members ------------
  TClonesArray  *fTpcPointCollection;
  
  // Private Methods -----------------
  
public:
  
  ClassDef(TpcDetector,1)
    
};

#endif

//--------------------------------------------------------------
// $Log$
//--------------------------------------------------------------
