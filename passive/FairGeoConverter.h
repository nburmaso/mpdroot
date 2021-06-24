#ifndef PNDGEOCONVERTER_H
#define PNDGEOCONVERTER_H

#include "FairGeoSet.h" // for FairGeoSet
#include "Rtypes.h" // for FairGeoPipe::Class, Int_t, etc

class FairGeoBuilder;

class FairGeoConverter : public FairGeoSet
{
 public:
  FairGeoConverter();
  ~FairGeoConverter() override = default;
  const char* getModuleName(Int_t) override { return modName; }
  const char* getEleName(Int_t) override { return eleName; }
  Bool_t create(FairGeoBuilder*) override;

 protected:
  char modName[2]{}; // name of module
  char eleName[2]{}; // substring for elements in module

  ClassDef(FairGeoConverter, 0) // Class for geometry of beam pipe
};

#endif /* !PNDGEOCONVERTER_H */
