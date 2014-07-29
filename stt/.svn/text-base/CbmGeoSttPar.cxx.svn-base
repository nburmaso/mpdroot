//*-- AUTHOR : Denis Bertini
//*-- Created : 21/06/2005

using namespace std;
#include "CbmGeoSttPar.h"
#include "FairParamList.h"
#include <iostream>
#include <iomanip>

ClassImp(CbmGeoSttPar)

CbmGeoSttPar::CbmGeoSttPar(const char* name,const char* title,const char* context)
    : FairParGenericSet(name,title,context)
{
    fGeoSensNodes = new TObjArray();
    fGeoPassNodes = new TObjArray();
}

CbmGeoSttPar::~CbmGeoSttPar(void) 
{
}

void CbmGeoSttPar::clear(void) 
{
    if(fGeoSensNodes) delete fGeoSensNodes;
    if(fGeoPassNodes) delete fGeoPassNodes;
}

void CbmGeoSttPar::putParams(FairParamList* l)
{
  if (!l) return;
   //l->addBinary("FairGeoNodes Sensitive List", fGeoSensNodes);
   //l->addBinary("FairGeoNodes Passive List", fGeoPassNodes);
   l->addObject("FairGeoNodes Sensitive List", fGeoSensNodes);
   l->addObject("FairGeoNodes Passive List", fGeoPassNodes);
}

Bool_t CbmGeoSttPar::getParams(FairParamList* l)
{
    if (!l) return kFALSE;
    //if (!l->fillBinary("FairGeoNodes Sensitive List", fGeoSensNodes)) return kFALSE;
    //if (!l->fillBinary("FairGeoNodes Passive List", fGeoPassNodes)) return kFALSE;
    if (!l->fillObject("FairGeoNodes Sensitive List", fGeoSensNodes)) return kFALSE;
    if (!l->fillObject("FairGeoNodes Passive List", fGeoPassNodes)) return kFALSE;

  return kTRUE;
}
