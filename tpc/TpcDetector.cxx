//-----------------------------------------------------------
// File and Version Information:
// $Id$
//
// Description:
//      Implementation of class TpcDetector
//      see TpcDetector.hh for details
//
// Environment:
//      Software developed for the PANDA Detector at FAIR.
//
// Author List:
//      Sebastian Neubert    TUM            (original author)
//
//
//-----------------------------------------------------------

// Panda Headers ----------------------

// This Class' Header ------------------
#include "TpcDetector.h"

// C/C++ Headers ----------------------
#include <iostream>
using namespace std;

// Collaborating Class Headers --------
#include "TClonesArray.h"
#include "FairRootManager.h"
#include "TpcPoint.h"
#include "TVirtualMC.h"
#include "TLorentzVector.h"
#include "TpcGeo.h"
#include "FairGeoLoader.h"
#include "FairGeoInterface.h"
#include "TList.h"
#include "FairRun.h"
#include "FairRuntimeDb.h"
#include "TpcGeoPar.h"
#include "TObjArray.h"
#include <TSystem.h>
#include "FairGeoNode.h"
#include "FairGeoVolume.h"
#include "FairVolume.h"
#include "FairStack.h"



// Class Member definitions -----------


TpcDetector::TpcDetector(const char * Name, Bool_t Active)
  : FairDetector(Name, Active)
{
  
  fTpcPointCollection= new TClonesArray("TpcPoint");

  fVerboseLevel = 1;
}

TpcDetector::TpcDetector()
{
  fTpcPointCollection= new TClonesArray("TpcPoint");

  fVerboseLevel = 1;
}

TpcDetector::~TpcDetector()
{
  if (fTpcPointCollection) {
      fTpcPointCollection->Delete();
      delete fTpcPointCollection;
   }
}




void TpcDetector::EndOfEvent() 
{
   if(fVerboseLevel) Print();
   fTpcPointCollection->Delete();

   //AZ - memory monitor
   ProcInfo_t proc;
   gSystem->GetProcInfo(&proc);
   cout << " User CPU time: " << proc.fCpuUser << " Memory: resident " << proc.fMemResident << ", virtual " << proc.fMemVirtual << endl;
}



void TpcDetector::Register() {

/** This will create a branch in the output tree called  TpcDetectorPoint, setting the last parameter to kFALSE means:

    this collection will not be written to the file, it will exist only during the simulation. */
 
   FairRootManager::Instance()->Register("TpcPoint", "Tpc", fTpcPointCollection, kTRUE);

}

Bool_t 
TpcDetector::ProcessHits( FairVolume *v)
{
  // create Hit for every MC step
  Double_t time   = gMC->TrackTime() * 1.0e09;
  Double_t length = gMC->TrackLength();
  TLorentzVector pos;
  gMC->TrackPosition(pos);
  //  pos.Print();
  TLorentzVector mom;
  gMC->TrackMomentum(mom);
  //  mom.Print();
  Double_t eLoss = gMC->Edep();
  Int_t trackID  = gMC->GetStack()->GetCurrentTrackNumber();
  Int_t volumeID = v->getMCid();

  if (eLoss > 0) { //AZ
    TpcPoint* p=AddHit(trackID, volumeID, pos.Vect(), mom.Vect(), time, length, eLoss);
    p->SetStep(gMC->TrackStep());

    ((FairStack*)gMC->GetStack())->AddPoint(kTPC);

  }
  //  p->Print("");

  return kTRUE;
}


TClonesArray* TpcDetector::GetCollection(Int_t iColl) const {
  if (iColl == 0) return fTpcPointCollection;
  else return NULL;
}


TpcPoint* TpcDetector::AddHit(Int_t trackID, Int_t detID, TVector3 pos,
    TVector3 mom, Double_t time, Double_t length,
    Double_t eLoss) {
  TClonesArray& clref = *fTpcPointCollection;
  Int_t size = clref.GetEntriesFast();
  return new(clref[size]) TpcPoint(trackID, detID, pos, mom,
      time, length, eLoss);
}

void TpcDetector::Reset() {
  fTpcPointCollection->Delete();
  //ResetParameters();
}

void TpcDetector::Print() const
{
        Int_t nHits = fTpcPointCollection->GetEntriesFast();
        cout<<"-I- MpdTpc: " << nHits << " points registered in this event." << endl;

        if(fVerboseLevel > 1)
                for(Int_t i=0; i<nHits; i++) (*fTpcPointCollection)[i]->Print();
}

void 
TpcDetector::ConstructGeometry() {
  /** If you are using the standard ASCII input for the geometry just copy this and use it for your detector, otherwise you can
      
  implement here you own way of constructing the geometry. */
  
  std::cout<<" --- Building TPC Geometry ---"<<std::endl;

  
  FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
  FairGeoInterface* geoFace = geoLoad->getGeoInterface();
  TpcGeo*       Geo  = new TpcGeo();
  Geo->setGeomFile(GetGeometryFileName());
  geoFace->addGeoModule(Geo);

  Bool_t rc = geoFace->readSet(Geo);
  if (rc) Geo->create(geoLoad->getGeoBuilder());
  else std::cerr<<"TpcDetector:: geometry could not be read!"<<std::endl;

  TList* volList = Geo->getListOfVolumes();

  // store geo parameter
  FairRun *fRun = FairRun::Instance();
  FairRuntimeDb *rtdb= FairRun::Instance()->GetRuntimeDb();
  TpcGeoPar* par=(TpcGeoPar*)(rtdb->getContainer("TpcGeoPar"));
  TObjArray *fSensNodes = par->GetGeoSensitiveNodes();
  TObjArray *fPassNodes = par->GetGeoPassiveNodes();

  TListIter iter(volList);
  FairGeoNode* node   = NULL;
  FairGeoVolume *aVol=NULL;
  
  
  while( (node = (FairGeoNode*)iter.Next()) ) {
      aVol = dynamic_cast<FairGeoVolume*> ( node );
       if ( node->isSensitive()  ) {
           fSensNodes->AddLast( aVol );
       }else{
           fPassNodes->AddLast( aVol );
       }
  }
  par->setChanged();
  par->setInputVersion(fRun->GetRunId(),1);

  ProcessNodes ( volList );
}

ClassImp(TpcDetector)
