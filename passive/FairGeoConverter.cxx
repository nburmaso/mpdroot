#include "FairGeoConverter.h"
#include "FairGeoInterface.h" // for FairGeoInterface
#include "FairGeoLoader.h"    // for FairGeoLoader

#include "TString.h" // for TString

class FairGeoBuilder;
class TObject;

ClassImp(FairGeoConverter);

// Constructor
FairGeoConverter::FairGeoConverter()
  : FairGeoSet()
{
  fName = "converter";
  strcpy(modName, "p");
  strcpy(eleName, "p");
  maxSectors = 0;
  maxModules = 1;
}

Bool_t FairGeoConverter::create(FairGeoBuilder* build)
{
  Bool_t rc = FairGeoSet::create(build);
  if (rc) {
    FairGeoLoader* loader = FairGeoLoader::Instance();
    FairGeoInterface* GeoInterface = loader->getGeoInterface();
    GeoInterface->getMasterNodes()->Add(static_cast<TObject*>(getVolume("pipeCentral")));
  }
  return rc;
}
