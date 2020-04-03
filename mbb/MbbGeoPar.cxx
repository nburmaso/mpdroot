using namespace std;
#include "TObjArray.h"
#include "MbbGeoPar.h"
#include "FairParamList.h"
#include <iostream>
#include <iomanip>

ClassImp(MbbGeoPar)

MbbGeoPar::MbbGeoPar(const char* name,const char* title,const char* context)
           : FairParGenericSet(name,title,context)
{
  fGeoSensNodes = new TObjArray();
  fGeoPassNodes = new TObjArray();
}

MbbGeoPar::~MbbGeoPar(void) 
{
  if(fGeoSensNodes) delete fGeoSensNodes;
  if(fGeoPassNodes) delete fGeoPassNodes;
}

void MbbGeoPar::clear(void)
{
  if(fGeoSensNodes) 
    delete fGeoSensNodes;
  if(fGeoPassNodes) 
    delete fGeoPassNodes;
  fGeoSensNodes = fGeoPassNodes = 0x0; //AZ
}

void MbbGeoPar::putParams(FairParamList* l)
{
  if (!l) 
    return;
  l->addObject("FairGeoNodes Sensitive List", fGeoSensNodes);
  l->addObject("FairGeoNodes Passive List", fGeoPassNodes);
}

Bool_t MbbGeoPar::getParams(FairParamList* l)
{
  if (!l) 
    return kFALSE;
   if (!l->fillObject("FairGeoNodes Sensitive List", fGeoSensNodes))
    return kFALSE;
   if (!l->fillObject("FairGeoNodes Passive List",   fGeoPassNodes))
    return kFALSE;

  return kTRUE;
}
