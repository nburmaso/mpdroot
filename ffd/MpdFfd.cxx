//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                       MpdFfd source file                      -----
// -------------------------------------------------------------------------

#include "iostream"

#include "MpdMCTrack.h"
#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TParticle.h"
#include "TVirtualMC.h"

#include "FairGeoInterface.h"
#include "FairGeoLoader.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "FairRootManager.h"
#include "FairRun.h"
#include "FairRuntimeDb.h"
#include "FairVolume.h"
#include "MpdStack.h"
#include "TObjArray.h"

#include "MpdFfd.h"
#include "MpdFfdGeo.h"
#include "MpdFfdGeoPar.h"
#include "MpdFfdPoint.h"

class FairVolume;

//------------------------------------------------------------------------------------------------------------------------
MpdFfd::MpdFfd() : FairDetector("FFD", kTRUE) {
  fFfdCollection = new TClonesArray("MpdFfdPoint");
  fPosIndex = 0;
  fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdFfd::MpdFfd(const char *name, Bool_t active) : FairDetector(name, active) {
  fFfdCollection = new TClonesArray("MpdFfdPoint");
  fPosIndex = 0;
  fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdFfd::~MpdFfd() {
  if (fFfdCollection) {
    fFfdCollection->Delete();
    delete fFfdCollection;
  }
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t MpdFfd::ProcessHits(FairVolume *vol) {
  //	Int_t gap, cell, module, region;
  //	TString Volname;

  // Set parameters at entrance of volume. Reset ELoss.
  if (gMC->IsTrackEntering()) {
    fELoss = 0.;
    fTime = gMC->TrackTime() * 1.0e09;
    fLength = gMC->TrackLength();
    gMC->TrackPosition(fPos);
    gMC->TrackMomentum(fMom);
  }

  // Sum energy loss for all steps in the active volume
  fELoss += gMC->Edep();

  // Create MpdFfdPoint at ENTER of active volume; fELoss INVALID!!!
  // AZ if(gMC->IsTrackEntering())

  // Create MpdFfdPoint at exit of active volume
  if ((gMC->IsTrackExiting() || gMC->IsTrackStop() ||
       gMC->IsTrackDisappeared()) &&
      fELoss > 0) {
    fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
    // Volname = vol->getRealName();         // EL
    // region = Volname[5] - '0';   //?????????????????????????
    // gMC->CurrentVolID(gap);
    // gMC->CurrentVolOffID(1, cell);
    // gMC->CurrentVolOffID(2, module);

    // fVolumeID = ((region-1)<<24);////////////// + ((module-1)<<14) +
    // ((cell-1)<<4) + (gap-1);
    fVolumeID = vol->getMCid();

    AddHit(fTrackID, fVolumeID, TVector3(fPos.X(), fPos.Y(), fPos.Z()),
           TVector3(fMom.Px(), fMom.Py(), fMom.Pz()), fTime, fLength, fELoss);

    ((MpdStack *)gMC->GetStack())->AddPoint(kFFD);

    ResetParameters();
  }

  return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFfd::EndOfEvent() {
  if (fVerboseLevel)
    Print();
  fFfdCollection->Delete();
  fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFfd::Register() {
  FairRootManager::Instance()->Register("FFDPoint", "Ffd", fFfdCollection,
                                        kTRUE);
}
//------------------------------------------------------------------------------------------------------------------------
TClonesArray *MpdFfd::GetCollection(Int_t iColl) const {
  if (iColl == 0)
    return fFfdCollection;

  return NULL;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFfd::Print() const {
  Int_t nHits = fFfdCollection->GetEntriesFast();
  cout << "-I- MpdFfd: " << nHits << " points registered in this event."
       << endl;

  if (fVerboseLevel > 1)
    for (Int_t i = 0; i < nHits; i++)
      (*fFfdCollection)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFfd::Reset() {
  fFfdCollection->Delete();
  ResetParameters();
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFfd::CopyClones(TClonesArray *cl1, TClonesArray *cl2, Int_t offset) {
  Int_t nEntries = cl1->GetEntriesFast();
  cout << "-I- MpdFfd: " << nEntries << " entries to add." << endl;
  TClonesArray &clref = *cl2;
  MpdFfdPoint *oldpoint = NULL;

  for (Int_t i = 0; i < nEntries; i++) {
    oldpoint = (MpdFfdPoint *)cl1->At(i);
    Int_t index = oldpoint->GetTrackID() + offset;
    oldpoint->SetTrackID(index);
    new (clref[fPosIndex]) MpdFfdPoint(*oldpoint);
    fPosIndex++;
  }

  cout << "-I- MpdFfd: " << cl2->GetEntriesFast() << " merged entries." << endl;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFfd::ConstructGeometry() {
  TString fileName = GetGeometryFileName();

  if (fileName.EndsWith(".root")) {
    LOG(INFO) << "Constructing FFD geometry from ROOT file " << fileName.Data()
              << endl;
    ConstructRootGeometry();
  } else if (fileName.EndsWith(".geo")) {
    LOG(INFO) << "Constructing FFD geometry from ASCII file " << fileName.Data()
              << endl;
    ConstructAsciiGeometry();
  }
  /*else if ( fileName.EndsWith(".gdml") )
  {
          LOG(INFO) << "Constructing CPC geometry from GDML file " <<
  fileName.Data() << endl; ConstructGDMLGeometry();
  }*/
  else {
    LOG(FATAL) << "Geometry format of FFD file " << fileName.Data()
               << " not supported." << endl;
  }
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFfd::ConstructAsciiGeometry() {

  int count = 0;
  int count_tot = 0;

  FairGeoLoader *geoLoad = FairGeoLoader::Instance();
  FairGeoInterface *geoFace = geoLoad->getGeoInterface();
  MpdFfdGeo *Geo = new MpdFfdGeo();
  Geo->setGeomFile(GetGeometryFileName());
  geoFace->addGeoModule(Geo);

  Bool_t rc = geoFace->readSet(Geo);
  if (rc)
    Geo->create(geoLoad->getGeoBuilder());
  else
    std::cerr << "FfdDetector:: geometry could not be read!" << std::endl;

  TList *volList = Geo->getListOfVolumes();

  // store geo parameter
  FairRun *fRun = FairRun::Instance();
  FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
  MpdFfdGeoPar *par = (MpdFfdGeoPar *)(rtdb->getContainer("MpdFfdGeoPar"));
  TObjArray *fSensNodes = par->GetGeoSensitiveNodes();
  TObjArray *fPassNodes = par->GetGeoPassiveNodes();

  TListIter iter(volList);
  FairGeoNode *node = NULL;
  FairGeoVolume *aVol = NULL;

  while ((node = (FairGeoNode *)iter.Next())) {
    aVol = dynamic_cast<FairGeoVolume *>(node);
    if (node->isSensitive()) {
      fSensNodes->AddLast(aVol);
      count++;
    } else
      fPassNodes->AddLast(aVol);
    count_tot++;
  }

  par->setChanged();
  par->setInputVersion(fRun->GetRunId(), 1);

  ProcessNodes(volList);
}
//------------------------------------------------------------------------------------------------------------------------
MpdFfdPoint *MpdFfd::AddHit(Int_t trackID, Int_t detID, TVector3 pos,
                            TVector3 mom, Double_t time, Double_t length,
                            Double_t eLoss) {
  TClonesArray &clref = *fFfdCollection;
  Int_t size = clref.GetEntriesFast();

  return new (clref[size])
      MpdFfdPoint(trackID, detID, pos, mom, time, length, eLoss);
}
//------------------------------------------------------------------------------------------------------------------------
// Check if Sensitive-----------------------------------------------------------
Bool_t MpdFfd::CheckIfSensitive(std::string name) {
  TString tsname = name;
  if (tsname.Contains("Active")) {
    return kTRUE;
  }
  return kFALSE;
}
//------------------------------------------------------------------------------------------------------------------------

ClassImp(MpdFfd)
