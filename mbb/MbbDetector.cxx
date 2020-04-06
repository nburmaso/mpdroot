//-----------------------------------------------------------
// File and Version Information:
// $Id$
//
// Description:
//      Implementation of class MbbDetector
//      see MbbDetector.h for details
//
// Environment:
//      Software developed for the PANDA Detector at FAIR.
//
// Author List:
//    Pedro Gonzalez Zamora, (original author)
//    Luis Valenzuela-Cázares
//
//-----------------------------------------------------------
// Panda Headers ----------------------
// This Class' Header ------------------
#include "MbbDetector.h"
// C/C++ Headers ----------------------
#include <iostream>
using namespace std;

// Collaborating Class Headers --------
#include "TClonesArray.h"
#include "FairRootManager.h"
#include "MbbPoint.h"
#include "TVirtualMC.h"
#include "TLorentzVector.h"
#include "MbbGeo.h"
#include "FairGeoLoader.h"
#include "FairGeoInterface.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "TList.h"
#include "FairRun.h"
#include "FairRuntimeDb.h"
#include "MbbGeoPar.h"
#include "TObjArray.h"
#include <TSystem.h>
#include "FairGeoNode.h"
#include "FairGeoVolume.h"
#include "FairVolume.h"
#include "MpdStack.h"

#include "TGeoManager.h"
#include "TGDMLParse.h"
#include "FairGeoMedia.h"
#include "TParticle.h"

// Class Member definitions -----------


MbbDetector::MbbDetector(const char * Name, Bool_t Active)
  : FairDetector(Name, Active), nan(std::numeric_limits<double>::quiet_NaN())
{

  fMbbPointCollection= new TClonesArray("MbbPoint");

  fVerboseLevel = 1;
  fNewTrack = kFALSE;
  currentTrackID = -1;
  currentEvent   = -1;
}

MbbDetector::MbbDetector():MbbDetector("Mbebe",kFALSE){

}

MbbDetector::~MbbDetector()
{
  if (fMbbPointCollection) {
      fMbbPointCollection->Delete();
      delete fMbbPointCollection;
   }
}




void MbbDetector::EndOfEvent()
{
   if(fVerboseLevel) Print();
   fMbbPointCollection->Delete();

   //AZ - memory monitor
   ProcInfo_t proc;
   gSystem->GetProcInfo(&proc);
   cout << " User CPU time: " << proc.fCpuUser << " Memory: resident " << proc.fMemResident << ", virtual " << proc.fMemVirtual << endl;
}



void MbbDetector::Register() {

/** This will create a branch in the output tree called  MbbDetectorPoint, setting the last parameter to kFALSE means:

    this collection will not be written to the file, it will exist only during the simulation. */

    FairRootManager::Instance()->Register("MbbPoint", "Mbb", fMbbPointCollection, kTRUE);

}

Bool_t MbbDetector::ProcessHits( FairVolume *v)
{

  // create Hit for every MC step

  const char* volName = v->getRealName();
  Int_t eventNumber = gMC->CurrentEvent();



    if( gMC->IsTrackEntering() )
     {
		fELoss   = 0.;
		fTime    = gMC->TrackTime() * 1.0e09;
		fLength  = gMC->TrackLength();
                fTrackID = gMC->GetStack()->GetCurrentTrackNumber();

		gMC->TrackMomentum(fMom);
		gMC->TrackPosition(fPos);


                TString volID = gMC->CurrentVolName();
                fVolumeID = GetVolumeID(volID);
      }

   // Sum energy loss for all steps in the active volume
   fELoss += gMC->Edep();

   TParticle* currentParticle = (TParticle*)gMC->GetStack()->GetCurrentTrack();


  if( fELoss > 0. && gMC->TrackCharge() )  {

        Int_t statusCode = currentParticle->GetStatusCode();

        MbbPoint* p=AddHit(fTrackID, fVolumeID, fPos.Vect(), fMom.Vect(), fTime, fLength, fELoss,statusCode);
        p->SetStep(gMC->TrackStep());
        ((MpdStack*)gMC->GetStack())->AddPoint(kMBB);
        ResetParameters();

  }

  return kTRUE;

}


TClonesArray* MbbDetector::GetCollection(Int_t iColl) const {
  if (iColl == 0) return fMbbPointCollection;
  else return NULL;
}



MbbPoint* MbbDetector::AddHit(Int_t trackID, Int_t detID, TVector3 pos,
    TVector3 mom, Double_t time, Double_t length,
    Double_t eLoss,Int_t statusCode) {
  TClonesArray& clref = *fMbbPointCollection;
  Int_t size = clref.GetEntriesFast();
  return new(clref[size]) MbbPoint(trackID, detID, pos, mom,
      time, length, eLoss,statusCode);
}

void MbbDetector::Reset() {


   fMbbPointCollection->Delete();
   ResetParameters();



}

void MbbDetector::Print() const
{

    Int_t nHits = fMbbPointCollection->GetEntriesFast();
        cout<<"-I- MpdMbb: " << nHits << " points registered in this event." << endl;

        if(fVerboseLevel > 1)
                for(Int_t i=0; i<nHits; i++) (*fMbbPointCollection)[i]->Print();


}

void MbbDetector::ConstructGeometry() {

    TString fileName = GetGeometryFileName();

    if ( fileName.EndsWith(".root") ) {
      gLogger->Info(MESSAGE_ORIGIN,
                  "Constructing MBB geometry from ROOT file %s",
                  fileName.Data());
      ConstructRootGeometry();
    }
    else if ( fileName.EndsWith(".geo") ) {
      gLogger->Info(MESSAGE_ORIGIN,
                    "Constructing MBB geometry from ASCII file %s",
                    fileName.Data());
      ConstructAsciiGeometry();
    }
    else if ( fileName.EndsWith(".gdml") )
    {
      gLogger->Info(MESSAGE_ORIGIN,
            "Constructing MBB geometry from GDML file %s",
            fileName.Data());
      //ConstructGDMLGeometry();
    }
    else
    {
      gLogger->Fatal(MESSAGE_ORIGIN,
             "Geometry format of MBB file %s not supported.",
             fileName.Data());
    }
}

// -----   ConstructAsciiGeometry   -------------------------------------------
void MbbDetector::ConstructAsciiGeometry() {

  FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
  FairGeoInterface* geoFace = geoLoad->getGeoInterface();

  MbbGeo*       MBBGeo  = new MbbGeo();
  MBBGeo->setGeomFile(GetGeometryFileName());
  geoFace->addGeoModule(MBBGeo);

  Bool_t rc = geoFace->readSet(MBBGeo);
  if (rc) MBBGeo->create(geoLoad->getGeoBuilder());
  TList* volList = MBBGeo->getListOfVolumes();
  // store geo parameter
  FairRun *fRun = FairRun::Instance();
  FairRuntimeDb *rtdb= FairRun::Instance()->GetRuntimeDb();
  MbbGeoPar* par=(MbbGeoPar*)(rtdb->getContainer("MbbGeoPar"));
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
void MbbDetector::ConstructGDMLGeometry()
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

void MbbDetector::ExpandNodeForGdml(TGeoNode* node)
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
Bool_t MbbDetector::CheckIfSensitive(std::string name) {
    TString tsname = name;

    // cout<<tsname.Data()<<endl;

    if(tsname.Contains("centSensitive") )

    {
       return kTRUE;
    }


    return kFALSE;

}

Int_t MbbDetector::GetVolumeID(TString volname) {


 Int_t     detectorID = -1;

  TObjArray *svolnameArr = volname.Tokenize("_");
  if(svolnameArr->GetEntries()<3){
      cout << "ERROR: invalid name of volume'" << endl;
      return -1;
  }
  TObjString* ringNameObjStr;
  TObjString* cellNameObjStr;

  ringNameObjStr = (TObjString*)svolnameArr->At(1);
  cellNameObjStr = (TObjString*)svolnameArr->At(2);

  TString ringNameStr = ringNameObjStr->GetString();
  TString cellNameStr = cellNameObjStr->GetString();

  Int_t   offSet = 0;

  

  detectorID = (ringNameStr.Atoi()-1)*20 +cellNameStr.Atoi();

  //detectorID = offSet + cellNameStr.Atoi();



  return detectorID;

}


//---------------------------------------------------------

ClassImp(MbbDetector)
