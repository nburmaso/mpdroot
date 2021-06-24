#include "FairConverter.h"
#include "FairGeoConverter.h"
#include "FairGeoLoader.h"
#include "FairGeoInterface.h"
#include "FairGeoRootBuilder.h"
#include "FairGeoNode.h"
#include "FairRuntimeDb.h"
#include "FairGeoPassivePar.h"
#include "FairRun.h"
#include "FairGeoVolume.h" // for FairGeoVolume
#include "TObjArray.h"

void FairConverter::ConstructGeometry()
{
  TString fileName = GetGeometryFileName();
  if (fileName.EndsWith(".geo")) {
    ConstructASCIIGeometry();
  } else if (fileName.EndsWith(".root")) {
    ConstructRootGeometry();
  } else {
    LOG(fatal) << "Geometry format not supported";
  }
}

void FairConverter::ConstructASCIIGeometry()
{
  FairGeoLoader* loader = FairGeoLoader::Instance();
  FairGeoInterface* GeoInterface = loader->getGeoInterface();
  auto* MGeo = new FairGeoConverter();
  MGeo->setGeomFile(GetGeometryFileName());
  GeoInterface->addGeoModule(MGeo);
  Bool_t rc = GeoInterface->readSet(MGeo);
  if (rc) {
    MGeo->create(loader->getGeoBuilder());
  }

  TList* volList = MGeo->getListOfVolumes();
  // store geo parameter
  FairRun* fRun = FairRun::Instance();
  FairRuntimeDb* rtdb = FairRun::Instance()->GetRuntimeDb();
  auto* par = (FairGeoPassivePar*)(rtdb->getContainer("FairGeoPassivePar"));
  TObjArray* fSensNodes = par->GetGeoSensitiveNodes();
  TObjArray* fPassNodes = par->GetGeoPassiveNodes();

  TListIter iter(volList);
  FairGeoNode* node = nullptr;
  FairGeoVolume* aVol = nullptr;

  while ((node = (FairGeoNode*)iter.Next())) {
    aVol = dynamic_cast<FairGeoVolume*>(node);
    if (node->isSensitive()) {
      fSensNodes->AddLast(aVol);
    } else {
      fPassNodes->AddLast(aVol);
    }
  }
  ProcessNodes(volList);
  par->setChanged();
  par->setInputVersion(fRun->GetRunId(), 1);
}

ClassImp(FairConverter)
