//-----------------------------------------------------------
// File and Version Information:
// $Id$
//
// Description:
//      Implementation of class BmdDetector
//      see BmdDetector.h for details
//
// Environment:
//      Software developed for the MPD Detector at NICA.
//
// Author List:
//    Pedro Gonzalez, Mario Rodriguez
//-----------------------------------------------------------
// This Class' Header ------------------

#include "TClonesArray.h"
#include "TList.h"
#include "TLorentzVector.h"
#include "TObjArray.h"
#include "TVirtualMC.h"
#include <TSystem.h>

#include "FairGeoInterface.h"
#include "FairGeoLoader.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "FairGeoVolume.h"
#include "FairRootManager.h"
#include "FairRun.h"
#include "FairRuntimeDb.h"
#include "MpdStack.h"
#include "FairVolume.h"

#include "FairGeoMedia.h"
#include "TGDMLParse.h"
#include "TGeoManager.h"
#include "TParticle.h"

#include "BmdDetector.h"
#include "BmdGeo.h"
#include "BmdGeoPar.h"
#include "BmdPoint.h"

#include <iostream>

using namespace std;

// Class Member definitions -----------

//const double BmdDetector::nan = numeric_limits<double>::quiet_NaN();

//_________________________________________________________
BmdDetector::BmdDetector() {
  //  nan = std::numeric_limits<double>::quiet_NaN();
  fBmdPointCollection = new TClonesArray("BmdPoint");
  fVerboseLevel = 1;
  fNewTrack = kFALSE;
  currentTrackID = -1;
  currentEvent = -1;
}

//_______________________________________________________
BmdDetector::BmdDetector(const char *Name, Bool_t Active)
  : FairDetector(Name, Active)
    //, nan(std::numeric_limits<double>::quiet_NaN())
{

  fBmdPointCollection = new TClonesArray("BmdPoint");

  fVerboseLevel = 1;
  fNewTrack = kFALSE;
  currentTrackID = -1;
  currentEvent = -1;
}

BmdDetector::~BmdDetector() {
  if (fBmdPointCollection) {
    fBmdPointCollection->Delete();
    delete fBmdPointCollection;
  }
}

void BmdDetector::EndOfEvent() {
  if (fVerboseLevel)
    Print();
  fBmdPointCollection->Delete();

  ProcInfo_t proc;
  gSystem->GetProcInfo(&proc);
  cout << " User CPU time: " << proc.fCpuUser << " Memory: resident "
       << proc.fMemResident << ", virtual " << proc.fMemVirtual << endl;
}

void BmdDetector::Register() {

  /** This will create a branch in the output tree called  BmdDetectorPoint,
     setting the last parameter to kFALSE means:

      this collection will not be written to the file, it will exist only during
     the simulation. */

  FairRootManager::Instance()->Register("BmdPoint", "Bmd", fBmdPointCollection,
                                        kTRUE);
}

//_________________________________________________________________
Bool_t BmdDetector::ProcessHits(FairVolume *v) {

  // create Hit for every MC step

  const char *volName = v->getRealName();
  Int_t eventNumber = gMC->CurrentEvent();

  if (gMC->IsTrackEntering()) {
    fELoss = 0.;
    fTime = gMC->TrackTime() * 1.0e09;
    fLength = gMC->TrackLength();
    fTrackID = gMC->GetStack()->GetCurrentTrackNumber();

    gMC->TrackMomentum(fMom);
    gMC->TrackPosition(fPos);

    TString volID = gMC->CurrentVolName();
    fBmdID = GetBmdID(volID);
    fRingID = GetRingID(volID);
    fCellID = GetCellID(volID);
    fVolumeID = GetVolumeID(volID);
    //cout << "volID  " << volID << "  fBmdID  " << fBmdID << "  fRingID  " << fRingID << "  fCellID  " << fCellID << endl;
  }

  // Sum energy loss for all steps in the active volume
  fELoss += gMC->Edep();

  TParticle *currentParticle = (TParticle *)gMC->GetStack()->GetCurrentTrack();

  if (fELoss > 0. &&
      (gMC->IsTrackExiting() || gMC->IsTrackStop() ||
       gMC->IsTrackDisappeared()) &&
      gMC->TrackCharge()) {

    BmdPoint *p = AddHit(fTrackID, fVolumeID, fPos.Vect(), fMom.Vect(), fTime,
			 fLength, fELoss, fBmdID, fRingID, fCellID);
    p->SetStep(gMC->TrackStep());
    ((MpdStack *)gMC->GetStack())->AddPoint(kBMD);
    ResetParameters();
  }

  return kTRUE;
}

//_________________________________________________________________
TClonesArray *BmdDetector::GetCollection(Int_t iColl) const {
  if (iColl == 0)
    return fBmdPointCollection;
  else
    return NULL;
}

//_________________________________________________________________
BmdPoint *BmdDetector::
AddHit(Int_t trackID, Int_t detID, TVector3 pos,
       TVector3 mom, Double_t time, Double_t length,
       Double_t eLoss, Int_t BmdID, Int_t RingID, Int_t CellID) {
  TClonesArray &clref = *fBmdPointCollection;
  Int_t size = clref.GetEntriesFast();
  return new (clref[size])
      BmdPoint(trackID, detID, pos, mom, time, length, eLoss, BmdID, RingID, CellID);
}

//_________________________________________________________________
void BmdDetector::Reset() {

  fBmdPointCollection->Delete();
  ResetParameters();
}

//_________________________________________________________________
void BmdDetector::Print() const {

  Int_t nHits = fBmdPointCollection->GetEntriesFast();
  cout << "-I- MpdBmd: " << nHits << " points registered in this event."
       << endl;

  if (fVerboseLevel > 1)
    for (Int_t i = 0; i < nHits; i++)
      (*fBmdPointCollection)[i]->Print();
}

//_________________________________________________________________
void BmdDetector::ConstructGeometry() {

  TString fileName = GetGeometryFileName();

  if (fileName.EndsWith(".root")) {
    LOG(info) << "Constructing BMD geometry from ROOT file " << fileName.Data()
              << endl;
    ConstructRootGeometry();
  } else if (fileName.EndsWith(".geo")) {
    LOG(info) << "Constructing BMD geometry from ASCII file " << fileName.Data()
              << endl;
    ConstructAsciiGeometry();
  } else if (fileName.EndsWith(".gdml")) {
    LOG(info) << "Constructing BMD geometry from GDML file " << fileName.Data()
              << endl;
    // ConstructGDMLGeometry();
  } else {
    LOG(info) << "Geometry format of BMD file not supported " << fileName.Data()
              << endl;
  }
}

//_________________________________________________________________
void BmdDetector::ConstructAsciiGeometry() {

  FairGeoLoader *geoLoad = FairGeoLoader::Instance();
  FairGeoInterface *geoFace = geoLoad->getGeoInterface();

  BmdGeo *BMDGeo = new BmdGeo();
  BMDGeo->setGeomFile(GetGeometryFileName());
  geoFace->addGeoModule(BMDGeo);

  Bool_t rc = geoFace->readSet(BMDGeo);
  if (rc)
    BMDGeo->create(geoLoad->getGeoBuilder());
  TList *volList = BMDGeo->getListOfVolumes();
  // store geo parameter
  FairRun *fRun = FairRun::Instance();
  FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
  BmdGeoPar *par = (BmdGeoPar *)(rtdb->getContainer("BmdGeoPar"));
  TObjArray *fSensNodes = par->GetGeoSensitiveNodes();
  TObjArray *fPassNodes = par->GetGeoPassiveNodes();

  TListIter iter(volList);
  FairGeoNode *node = NULL;
  FairGeoVolume *aVol = NULL;

  while ((node = (FairGeoNode *)iter.Next())) {
    aVol = dynamic_cast<FairGeoVolume *>(node);
    if (node->isSensitive()) {
      fSensNodes->AddLast(aVol);
    } else {
      fPassNodes->AddLast(aVol);
    }
  }
  par->setChanged();
  par->setInputVersion(fRun->GetRunId(), 1);
  ProcessNodes(volList);
}
// ----------------------------------------------------------------------------

// -----   ConstructGDMLGeometry   -------------------------------------------
void BmdDetector::
    ConstructGDMLGeometry() {
  /*
    TFile *old = gFile;
    TGDMLParse parser;
    TGeoVolume* gdmlTop;

    // Before importing GDML
    Int_t maxInd =
    gGeoManager->GetListOfMedia()->GetEntries() - 1;

    gdmlTop =
    parser.GDMLReadFile(GetGeometryFileName());

    // Cheating - reassigning media indices after
    GDML import (need to fix this in TGDMLParse
    class!!!)
    //   for (Int_t i=0;
    i<gGeoManager->GetListOfMedia()->GetEntries();
    i++)
    //
    gGeoManager->GetListOfMedia()->At(i)->Dump();
    // After importing GDML
    Int_t j =
    gGeoManager->GetListOfMedia()->GetEntries() -
    1; Int_t curId; TGeoMedium* m; do { m =
    (TGeoMedium*)gGeoManager->GetListOfMedia()->At(j);
    curId = m->GetId();
    m->SetId(curId+maxInd);
    j--;
    } while (curId > 1);
    //   LOG(DEBUG) <<
    "===================================================================="
    << FairLogger::endl;
    //   for (Int_t i=0;
    i<gGeoManager->GetListOfMedia()->GetEntries();
    i++)
    //
    gGeoManager->GetListOfMedia()->At(i)->Dump();

    Int_t newMaxInd =
    gGeoManager->GetListOfMedia()->GetEntries() -
    1;

    gGeoManager->GetTopVolume()->AddNode(gdmlTop,
    1, 0);
    ExpandNodeForGdml(gGeoManager->GetTopVolume()->GetNode(gGeoManager->GetTopVolume()->GetNdaughters()-1));

    for (Int_t k = maxInd+1; k < newMaxInd+1; k++)
    { TGeoMedium* medToDel =
    (TGeoMedium*)(gGeoManager->GetListOfMedia()->At(maxInd+1));
    LOG(DEBUG) << "    removing media " <<
    medToDel->GetName() << " with id " <<
    medToDel->GetId() << " (k=" << k << ")" <<
    FairLogger::endl;
    gGeoManager->GetListOfMedia()->Remove(medToDel);
    }
    gGeoManager->SetAllIndex();

    gFile = old;
*/
}

//_________________________________________________________________
void BmdDetector::ExpandNodeForGdml(TGeoNode *node) {
  /*
  LOG(DEBUG)
    << "----------------------------------------- ExpandNodeForGdml for node "
    << node->GetName() << FairLogger::endl;

  TGeoVolume *curVol = node->GetVolume();

  LOG(DEBUG) << "    volume: " << curVol->GetName() << FairLogger::endl;

  if (curVol->IsAssembly()) {
    LOG(DEBUG) << "    skipping volume-assembly" << FairLogger::endl;
  } else {
    TGeoMedium *curMed = curVol->GetMedium();
    TGeoMaterial *curMat = curVol->GetMaterial();
    TGeoMedium *curMedInGeoManager = gGeoManager->GetMedium(curMed->GetName());
    TGeoMaterial *curMatOfMedInGeoManager = curMedInGeoManager->GetMaterial();
    TGeoMaterial *curMatInGeoManager =
        gGeoManager->GetMaterial(curMat->GetName());

    // Current medium and material assigned to the volume from GDML
    LOG(DEBUG2) << "    curMed\t\t\t\t" << curMed << "\t" << curMed->GetName()
                << "\t" << curMed->GetId() << FairLogger::endl;
    LOG(DEBUG2) << "    curMat\t\t\t\t" << curMat << "\t" << curMat->GetName()
                << "\t" << curMat->GetIndex() << FairLogger::endl;

    // Medium and material found in the gGeoManager - either the pre-loaded one
    // or one from GDML
    LOG(DEBUG2) << "    curMedInGeoManager\t\t" << curMedInGeoManager << "\t"
                << curMedInGeoManager->GetName() << "\t"
                << curMedInGeoManager->GetId() << FairLogger::endl;
    LOG(DEBUG2) << "    curMatOfMedInGeoManager\t\t" << curMatOfMedInGeoManager
                << "\t" << curMatOfMedInGeoManager->GetName() << "\t"
                << curMatOfMedInGeoManager->GetIndex() << FairLogger::endl;
    LOG(DEBUG2) << "    curMatInGeoManager\t\t" << curMatInGeoManager << "\t"
                << curMatInGeoManager->GetName() << "\t"
                << curMatInGeoManager->GetIndex() << FairLogger::endl;

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
        for (Int_t i = ind; i < gGeoManager->GetListOfMaterials()->GetEntries();
             i++) {
          TGeoMaterial *m =
              (TGeoMaterial *)gGeoManager->GetListOfMaterials()->At(i);
          m->SetIndex(m->GetIndex() - 1);
        }

        LOG(DEBUG) << "    Medium fixed" << FairLogger::endl;
      } else {
        LOG(DEBUG) << "    Already fixed medium found in the list    "
                   << FairLogger::endl;
      }
    } else {
      if (fFixedMedia.find(medName) == fFixedMedia.end()) {
        LOG(DEBUG) << "    There is no correct medium in the memory yet"
                   << FairLogger::endl;

        FairGeoLoader *geoLoad = FairGeoLoader::Instance();
        FairGeoInterface *geoFace = geoLoad->getGeoInterface();
        FairGeoMedia *geoMediaBase = geoFace->getMedia();
        FairGeoBuilder *geobuild = geoLoad->getGeoBuilder();

        FairGeoMedium *curMedInGeo = geoMediaBase->getMedium(medName);
        if (curMedInGeo == 0) {
          LOG(FATAL) << "    Media not found in Geo file: " << medName
                     << FairLogger::endl;
          //! This should not happen.
          //! This means that somebody uses material in GDML that is not in the
          //! media.geo file. Most probably this is the sign to the user to
          //! check materials' names in the CATIA model.
        } else {
          LOG(DEBUG) << "    Found media in Geo file" << medName
                     << FairLogger::endl;
          Int_t nmed = geobuild->createMedium(curMedInGeo);
          fFixedMedia[medName] =
              (TGeoMedium *)gGeoManager->GetListOfMedia()->Last();
          gGeoManager->RemoveMaterial(curMatOfMedInGeoManager->GetIndex());
          LOG(DEBUG) << "    removing material "
                     << curMatOfMedInGeoManager->GetName() << " with index "
                     << curMatOfMedInGeoManager->GetIndex() << FairLogger::endl;
          for (Int_t i = curMatOfMedInGeoManager->GetIndex();
               i < gGeoManager->GetListOfMaterials()->GetEntries(); i++) {
            TGeoMaterial *m =
                (TGeoMaterial *)gGeoManager->GetListOfMaterials()->At(i);
            m->SetIndex(m->GetIndex() - 1);
          }
        }

        if (curMedInGeo->getSensitivityFlag()) {
          LOG(DEBUG) << "    Adding sensitive  " << curVol->GetName()
                     << FairLogger::endl;
          AddSensitiveVolume(curVol);
        }
      } else {
        LOG(DEBUG) << "    Already fixed medium found in the list"
                   << FairLogger::endl;
        LOG(DEBUG) << "!!! Sensitivity: " << fFixedMedia[medName]->GetParam(0)
                   << FairLogger::endl;
        if (fFixedMedia[medName]->GetParam(0) == 1) {
          LOG(DEBUG) << "    Adding sensitive  " << curVol->GetName()
                     << FairLogger::endl;
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
  if (curVol->GetNdaughters() != 0) {
    TObjArray *NodeChildList = curVol->GetNodes();
    TGeoNode *curNodeChild;
    for (Int_t j = 0; j < NodeChildList->GetEntriesFast(); j++) {
      curNodeChild = (TGeoNode *)NodeChildList->At(j);
      ExpandNodeForGdml(curNodeChild);
    }
  }
  */
}

// Check if Sensitive-----------------------------------------------------------
Bool_t BmdDetector::CheckIfSensitive(std::string name) {
  TString tsname = name;

  // cout<<tsname.Data()<<endl;

  if (
      // tsname.Contains("A1_") || tsname.Contains("C1_") ||
      // tsname.Contains("A2_") || tsname.Contains("C2_") ||
      // tsname.Contains("A3_") || tsname.Contains("C3_") ||
      // tsname.Contains("A4_") || tsname.Contains("C4_") ||
      // tsname.Contains("A5_") || tsname.Contains("C5_") ||
      // tsname.Contains("A6_") || tsname.Contains("C6_") ||
      // tsname.Contains("A7_") || tsname.Contains("C7_"))
      tsname.Contains("Hexagon")) {
    return kTRUE;
  }
  return kFALSE;
}

//_________________________________________________________________
Int_t BmdDetector::GetVolumeID(TString volname) {

  Int_t detectorID = -1;

  TObjArray *svolnameArr = volname.Tokenize("Hexagon_cell_");
  if (svolnameArr->GetEntries() < 2) {
    cout << "ERROR: invalid name of volume'" << endl;
    return -1;
  }
  TObjString *ringNameObjStr;
  TObjString *cellNameObjStr;

  ringNameObjStr = (TObjString *)svolnameArr->At(0);
  cellNameObjStr = (TObjString *)svolnameArr->At(1);

  TString ringNameStr = ringNameObjStr->GetString();
  TString cellNameStr = cellNameObjStr->GetString();

  Int_t offSet = 0;

  detectorID = offSet + cellNameStr.Atoi();
//  cout << " Ring " <<  ringNameObjStr <<
//    " cell " << cellNameStr << " detectorID " << detectorID << endl;

  return detectorID;
}
Int_t BmdDetector::GetBmdID(TString volname) {

  Int_t BmdID = 0;

  TObjArray *svolnameArr = volname.Tokenize("Hexagon_cell_");
  if (svolnameArr->GetEntries() < 3) {
    cout << "ERROR: invalid name of volume'" << endl;
    return 0;
  }
  TObjString *BmdNameObjStr;
  TObjString *cellNameObjStr;

  BmdNameObjStr = (TObjString *)svolnameArr->At(0);

  TString BmdNameStr = BmdNameObjStr->GetString();

  Int_t offSet = 0;

  BmdID = offSet + BmdNameStr.Atoi();

  return BmdID;
}
Int_t BmdDetector::GetRingID(TString volname) {

  Int_t RingID = 0;

  TObjArray *svolnameArr = volname.Tokenize("Hexagon_cell_");
  if (svolnameArr->GetEntries() < 3) {
    cout << "ERROR: invalid name of volume'" << endl;
    return 0;
  }
  TObjString *ringNameObjStr;

  ringNameObjStr = (TObjString *)svolnameArr->At(1);


  TString ringNameStr = ringNameObjStr->GetString();

  Int_t offSet = 0;

  RingID = offSet + ringNameStr.Atoi();

  return RingID;
}
Int_t BmdDetector::GetCellID(TString volname) {

  Int_t detectorID = 0;

  TObjArray *svolnameArr = volname.Tokenize("Hexagon_cell_");
  if (svolnameArr->GetEntries() < 3) {
    cout << "ERROR: invalid name of volume'" << endl;
    return 0;
  }
  TObjString *cellNameObjStr;

  cellNameObjStr = (TObjString *)svolnameArr->At(2);

  TString cellNameStr = cellNameObjStr->GetString();

  Int_t offSet = 0;

  detectorID = offSet + cellNameStr.Atoi();

  return detectorID;
}

//---------------------------------------------------------

ClassImp(BmdDetector)
