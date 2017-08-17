//-----------------------------------------------------------
// File and Version Information:
// $Id$
//
// Description:
//      Implementation of class BmdDetector
//      see BmdDetector.hh for details
//
// Environment:
//      Software developed for the PANDA Detector at FAIR.
//
// Author List:
//    Pedro Gonzalez, Mario Rodriguez
//
//
//-----------------------------------------------------------
// Panda Headers ----------------------
// This Class' Header ------------------
#include "BmdDetector.h"
// C/C++ Headers ----------------------
#include <iostream>
using namespace std;

// Collaborating Class Headers --------
#include "TClonesArray.h"
#include "FairRootManager.h"
#include "BmdPoint.h"
#include "TVirtualMC.h"
#include "TLorentzVector.h"
#include "BmdGeo.h"
#include "FairGeoLoader.h"
#include "FairGeoInterface.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "TList.h"
#include "FairRun.h"
#include "FairRuntimeDb.h"
#include "BmdGeoPar.h"
#include "TObjArray.h"
#include <TSystem.h>
#include "FairGeoNode.h"
#include "FairGeoVolume.h"
#include "FairVolume.h"
#include "FairStack.h"

#include "TGeoManager.h"
#include "TGDMLParse.h"
#include "FairGeoMedia.h"

// Class Member definitions -----------


BmdDetector::BmdDetector(const char * Name, Bool_t Active)
  : FairDetector(Name, Active), nan(std::numeric_limits<double>::quiet_NaN())
{

  fBmdPointCollection= new TClonesArray("BmdPoint");

  fVerboseLevel = 1;
  fNewTrack = kFALSE;
  currentTrackID = -1;
}

//BmdDetector::BmdDetector()
//{
  //fBmdPointCollection= new TClonesArray("BmdPoint");

  //fVerboseLevel = 1;
  //nan=(std::numeric_limits<double>::quiet_NaN());
//}

BmdDetector::~BmdDetector()
{
  if (fBmdPointCollection) {
      fBmdPointCollection->Delete();
      delete fBmdPointCollection;
   }
}




void BmdDetector::EndOfEvent()
{
   if(fVerboseLevel) Print();
   fBmdPointCollection->Delete();

   //AZ - memory monitor
   ProcInfo_t proc;
   gSystem->GetProcInfo(&proc);
   cout << " User CPU time: " << proc.fCpuUser << " Memory: resident " << proc.fMemResident << ", virtual " << proc.fMemVirtual << endl;
}



void BmdDetector::Register() {

/** This will create a branch in the output tree called  BmdDetectorPoint, setting the last parameter to kFALSE means:

    this collection will not be written to the file, it will exist only during the simulation. */

    FairRootManager::Instance()->Register("BmdPoint", "Bmd", fBmdPointCollection, kTRUE);

}

Bool_t BmdDetector::ProcessHits( FairVolume *v)
{

  
    const char* volName = v->getRealName();
    Int_t copyNo;
    Int_t volIDMC = gMC->CurrentVolID(copyNo);
    TString volID = gMC->CurrentVolName();
    Int_t maxStep = gMC->GetMaxNStep();
    TLorentzVector	currentPos; 
    gMC->TrackPosition(currentPos);
    Int_t cTrackID = gMC->GetStack()->GetCurrentTrackNumber();
    
    
    
     //cout<<" ** "<<gMC->GetStack()->GetCurrentTrackNumber()<<" "<<gMC->IsTrackEntering()<<" "<<gMC->IsTrackExiting()<<" "<<gMC->IsTrackStop()<<" "<<gMC->IsTrackDisappeared()<<" "<<gMC->TrackStep()<<" "<<gMC->IsTrackOut()<<" "<<volID<<" "<<maxStep;
    
  
      // Set parameters at entrance of volume. Reset ELoss.
     if( gMC->IsTrackEntering() && fNewTrack  == kFALSE && cTrackID != currentTrackID ) 
     {
		fELoss  = 0.;
		fTime   = gMC->TrackTime() * 1.0e09;
		fLength = gMC->TrackLength();
		
		gMC->TrackMomentum(fMom);
		gMC->TrackPosition(fPos);
		cout<<fPos.Vect().X()<<" " <<fPos.Vect().Y()<<" "<<fPos.Vect().Z();
		fNewTrack = kTRUE;
		currentTrackID = cTrackID;
      }
  
      // Sum energy loss for all steps in the active volume
      fELoss += gMC->Edep();
      
      //cout<<" ** "<<gMC->GetStack()->GetCurrentTrackNumber()<<" "<<gMC->IsTrackEntering()<<" "<<gMC->IsTrackExiting()<<" "<<gMC->IsTrackStop()<<" "<<gMC->IsTrackDisappeared()<<" "<<gMC->TrackStep()<<" "<<gMC->IsTrackOut()<<" "<<volID<<" "<<maxStep<<" "<<fELoss;
     
      
       
      if(  ( gMC->IsTrackExiting() || gMC->IsTrackStop() || gMC->IsTrackDisappeared() )  && (TMath::Abs (currentPos.Vect().Z() ) > 201.25 ) && fNewTrack == kTRUE )
      {	
	       
	        if( fELoss == 0. ) {
		 
		  ResetParameters();
		  fNewTrack = kFALSE;
		  return kFALSE;
		  
		}
		
		
		
		fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
		fVolumeID = v->getMCid();
		BmdPoint* p=AddHit(fTrackID, fVolumeID, fPos.Vect(), fMom.Vect(), fTime, fLength, fELoss);
		p->SetStep(gMC->TrackStep());
		
		((FairStack*)gMC->GetStack())->AddPoint(kBMD);
		TLorentzVector fPosFin;
		
		gMC->TrackPosition(fPosFin);
		
		//cout<<" ** "<<fELoss<<" Salio "<<fPosFin.Vect().X()<<" "<<fPosFin.Vect().Y()<<" "<<fPosFin.Vect().Z()<<endl;
		
    		ResetParameters();
		fNewTrack = kFALSE;
		
      }

return kTRUE;

}


TClonesArray* BmdDetector::GetCollection(Int_t iColl) const {
  if (iColl == 0) return fBmdPointCollection;
  else return NULL;
}



BmdPoint* BmdDetector::AddHit(Int_t trackID, Int_t detID, TVector3 pos,
    TVector3 mom, Double_t time, Double_t length,
    Double_t eLoss) {
  TClonesArray& clref = *fBmdPointCollection;
  Int_t size = clref.GetEntriesFast();
  return new(clref[size]) BmdPoint(trackID, detID, pos, mom,
      time, length, eLoss);
}

void BmdDetector::Reset() {
  
  
   fBmdPointCollection->Delete();
   ResetParameters();
  
  
  
}

void BmdDetector::Print() const
{
 
    Int_t nHits = fBmdPointCollection->GetEntriesFast();
        cout<<"-I- MpdBmd: " << nHits << " points registered in this event." << endl;

        if(fVerboseLevel > 1)
                for(Int_t i=0; i<nHits; i++) (*fBmdPointCollection)[i]->Print();
  
  
}

void BmdDetector::ConstructGeometry() {
  
    TString fileName = GetGeometryFileName();

    if ( fileName.EndsWith(".root") ) {
      gLogger->Info(MESSAGE_ORIGIN,
                  "Constructing BMD geometry from ROOT file %s",
                  fileName.Data());
      ConstructRootGeometry();
    }
    else if ( fileName.EndsWith(".geo") ) {
      gLogger->Info(MESSAGE_ORIGIN,
                    "Constructing BMD geometry from ASCII file %s",
                    fileName.Data());
      ConstructAsciiGeometry();
    }
    else if ( fileName.EndsWith(".gdml") )
    {
      gLogger->Info(MESSAGE_ORIGIN,
            "Constructing BMD geometry from GDML file %s",
            fileName.Data());
      //ConstructGDMLGeometry();
    }
    else
    {
      gLogger->Fatal(MESSAGE_ORIGIN,
             "Geometry format of BMD file %s not supported.",
             fileName.Data());
    }
}

// -----   ConstructAsciiGeometry   -------------------------------------------
void BmdDetector::ConstructAsciiGeometry() {

  FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
  FairGeoInterface* geoFace = geoLoad->getGeoInterface();
  
  BmdGeo*       BMDGeo  = new BmdGeo();
  BMDGeo->setGeomFile(GetGeometryFileName());
  geoFace->addGeoModule(BMDGeo);

  Bool_t rc = geoFace->readSet(BMDGeo);
  if (rc) BMDGeo->create(geoLoad->getGeoBuilder());
  TList* volList = BMDGeo->getListOfVolumes();
  // store geo parameter
  FairRun *fRun = FairRun::Instance();
  FairRuntimeDb *rtdb= FairRun::Instance()->GetRuntimeDb();
  BmdGeoPar* par=(BmdGeoPar*)(rtdb->getContainer("BmdGeoPar"));
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
  ProcessNodes( volList );
  
}
// ----------------------------------------------------------------------------

// -----   ConstructGDMLGeometry   -------------------------------------------
void BmdDetector::ConstructGDMLGeometry()
{/*
    TFile *old = gFile;
    TGDMLParse parser;
    TGeoVolume* gdmlTop;

    // Before importing GDML
    Int_t maxInd = gGeoManager->GetListOfMedia()->GetEntries() - 1;

    gdmlTop = parser.GDMLReadFile(GetGeometryFileName());

    // Cheating - reassigning media indices after GDML import (need to fix this in TGDMLParse class!!!)
    //   for (Int_t i=0; i<gGeoManager->GetListOfMedia()->GetEntries(); i++)
    //      gGeoManager->GetListOfMedia()->At(i)->Dump();
    // After importing GDML
    Int_t j = gGeoManager->GetListOfMedia()->GetEntries() - 1;
    Int_t curId;
    TGeoMedium* m;
    do {
        m = (TGeoMedium*)gGeoManager->GetListOfMedia()->At(j);
        curId = m->GetId();
        m->SetId(curId+maxInd);
        j--;
    } while (curId > 1);
    //   LOG(DEBUG) << "====================================================================" << FairLogger::endl;
    //   for (Int_t i=0; i<gGeoManager->GetListOfMedia()->GetEntries(); i++)
    //      gGeoManager->GetListOfMedia()->At(i)->Dump();

    Int_t newMaxInd = gGeoManager->GetListOfMedia()->GetEntries() - 1;

    gGeoManager->GetTopVolume()->AddNode(gdmlTop, 1, 0);
    ExpandNodeForGdml(gGeoManager->GetTopVolume()->GetNode(gGeoManager->GetTopVolume()->GetNdaughters()-1));

    for (Int_t k = maxInd+1; k < newMaxInd+1; k++) {
        TGeoMedium* medToDel = (TGeoMedium*)(gGeoManager->GetListOfMedia()->At(maxInd+1));
        LOG(DEBUG) << "    removing media " << medToDel->GetName() << " with id " << medToDel->GetId() << " (k=" << k << ")" << FairLogger::endl;
        gGeoManager->GetListOfMedia()->Remove(medToDel);
    }
    gGeoManager->SetAllIndex();

    gFile = old;*/
}

void BmdDetector::ExpandNodeForGdml(TGeoNode* node)
{
    LOG(DEBUG) << "----------------------------------------- ExpandNodeForGdml for node " << node->GetName() << FairLogger::endl;

    TGeoVolume* curVol = node->GetVolume();

    LOG(DEBUG) << "    volume: " << curVol->GetName() << FairLogger::endl;

    if (curVol->IsAssembly()) {
        LOG(DEBUG) << "    skipping volume-assembly" << FairLogger::endl;
    }
    else
    {
        TGeoMedium* curMed = curVol->GetMedium();
        TGeoMaterial* curMat = curVol->GetMaterial();
        TGeoMedium* curMedInGeoManager = gGeoManager->GetMedium(curMed->GetName());
        TGeoMaterial* curMatOfMedInGeoManager = curMedInGeoManager->GetMaterial();
        TGeoMaterial* curMatInGeoManager = gGeoManager->GetMaterial(curMat->GetName());

        // Current medium and material assigned to the volume from GDML
        LOG(DEBUG2) << "    curMed\t\t\t\t" << curMed << "\t" << curMed->GetName() << "\t" << curMed->GetId() << FairLogger::endl;
        LOG(DEBUG2) << "    curMat\t\t\t\t" << curMat << "\t" << curMat->GetName() << "\t" << curMat->GetIndex() << FairLogger::endl;

        // Medium and material found in the gGeoManager - either the pre-loaded one or one from GDML
        LOG(DEBUG2) << "    curMedInGeoManager\t\t" << curMedInGeoManager
                 << "\t" << curMedInGeoManager->GetName() << "\t" << curMedInGeoManager->GetId() << FairLogger::endl;
        LOG(DEBUG2) << "    curMatOfMedInGeoManager\t\t" << curMatOfMedInGeoManager
                 << "\t" << curMatOfMedInGeoManager->GetName() << "\t" << curMatOfMedInGeoManager->GetIndex() << FairLogger::endl;
        LOG(DEBUG2) << "    curMatInGeoManager\t\t" << curMatInGeoManager
                 << "\t" << curMatInGeoManager->GetName() << "\t" << curMatInGeoManager->GetIndex() << FairLogger::endl;

        TString matName = curMat->GetName();
        TString medName = curMed->GetName();

        if (curMed->GetId() != curMedInGeoManager->GetId()) {
            if (fFixedMedia.find(medName) == fFixedMedia.end()) {
                LOG(DEBUG) << "    Medium needs to be fixed" << FairLogger::endl;
                fFixedMedia[medName] = curMedInGeoManager;
                Int_t ind = curMat->GetIndex();
                gGeoManager->RemoveMaterial(ind);
                LOG(DEBUG) << "    removing material " << curMat->GetName()
                    << " with index " << ind << FairLogger::endl;
                for (Int_t i=ind; i<gGeoManager->GetListOfMaterials()->GetEntries(); i++) {
                    TGeoMaterial* m = (TGeoMaterial*)gGeoManager->GetListOfMaterials()->At(i);
                    m->SetIndex(m->GetIndex()-1);
                }

                LOG(DEBUG) << "    Medium fixed" << FairLogger::endl;
            }
            else
            {
                LOG(DEBUG) << "    Already fixed medium found in the list    " << FairLogger::endl;
            }
        }
        else
        {
            if (fFixedMedia.find(medName) == fFixedMedia.end()) {
                LOG(DEBUG) << "    There is no correct medium in the memory yet" << FairLogger::endl;

                FairGeoLoader* geoLoad = FairGeoLoader::Instance();
                FairGeoInterface* geoFace = geoLoad->getGeoInterface();
                FairGeoMedia* geoMediaBase =  geoFace->getMedia();
                FairGeoBuilder* geobuild = geoLoad->getGeoBuilder();

                FairGeoMedium* curMedInGeo = geoMediaBase->getMedium(medName);
                if (curMedInGeo == 0)
                {
                    LOG(FATAL) << "    Media not found in Geo file: " << medName << FairLogger::endl;
                    //! This should not happen.
                    //! This means that somebody uses material in GDML that is not in the media.geo file.
                    //! Most probably this is the sign to the user to check materials' names in the CATIA model.
                }
                else
                {
                    LOG(DEBUG) << "    Found media in Geo file" << medName << FairLogger::endl;
                    Int_t nmed = geobuild->createMedium(curMedInGeo);
                    fFixedMedia[medName] = (TGeoMedium*)gGeoManager->GetListOfMedia()->Last();
                    gGeoManager->RemoveMaterial(curMatOfMedInGeoManager->GetIndex());
                    LOG(DEBUG) << "    removing material " << curMatOfMedInGeoManager->GetName()
                        << " with index " << curMatOfMedInGeoManager->GetIndex() << FairLogger::endl;
                    for (Int_t i=curMatOfMedInGeoManager->GetIndex(); i<gGeoManager->GetListOfMaterials()->GetEntries(); i++) {
                        TGeoMaterial* m = (TGeoMaterial*)gGeoManager->GetListOfMaterials()->At(i);
                        m->SetIndex(m->GetIndex()-1);
                    }
                }

                if (curMedInGeo->getSensitivityFlag()) {
                    LOG(DEBUG) << "    Adding sensitive  " << curVol->GetName() << FairLogger::endl;
                    AddSensitiveVolume(curVol);
                }
            }
            else
            {
                LOG(DEBUG) << "    Already fixed medium found in the list" << FairLogger::endl;
                LOG(DEBUG) << "!!! Sensitivity: " << fFixedMedia[medName]->GetParam(0) << FairLogger::endl;
                if (fFixedMedia[medName]->GetParam(0) == 1) {
                    LOG(DEBUG) << "    Adding sensitive  " << curVol->GetName() << FairLogger::endl;
                    AddSensitiveVolume(curVol);
                }
            }
        }

        curVol->SetMedium(fFixedMedia[medName]);
        gGeoManager->SetAllIndex();

  //      gGeoManager->GetListOfMaterials()->Print();
  //      gGeoManager->GetListOfMedia()->Print();

    }

    //! Recursevly go down the tree of nodes
    if (curVol->GetNdaughters() != 0)
    {
        TObjArray* NodeChildList = curVol->GetNodes();
        TGeoNode* curNodeChild;
        for (Int_t j=0; j<NodeChildList->GetEntriesFast(); j++)
        {
            curNodeChild = (TGeoNode*)NodeChildList->At(j);
            ExpandNodeForGdml(curNodeChild);
        }
    }
}
//-----------------------------------------------------------------------------

//Check if Sensitive-----------------------------------------------------------
Bool_t BmdDetector::CheckIfSensitive(std::string name) {
    TString tsname = name;
    cout<<tsname.Data()<<endl;
   
    if (tsname.Contains("BMDPlusA1Sec") || tsname.Contains("BMDPlusA2Sec") ||
        tsname.Contains("BMDPlusA3Sec") || tsname.Contains("BMDPlusA4Sec") ||
        tsname.Contains("BMDPlusA5Sec") || 
        tsname.Contains("BMDPlusC1Sec") || tsname.Contains("BMDPlusC2Sec") ||
        tsname.Contains("BMDPlusC3Sec") || tsname.Contains("BMDPlusC4Sec") ||
        tsname.Contains("BMDPlusC5Sec") ) 
    {
      return kTRUE;
    }
    return kFALSE;

}
//---------------------------------------------------------

ClassImp(BmdDetector)
