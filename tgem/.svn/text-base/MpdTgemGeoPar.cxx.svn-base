using namespace std;
#include "MpdTgemGeoPar.h"
#include "FairParamList.h"
#include <iostream>
#include <iomanip>

ClassImp(MpdTgemGeoPar)

MpdTgemGeoPar::MpdTgemGeoPar(const char* name,const char* title,const char* context)
           : FairParGenericSet(name,title,context) {

               fGeoSensNodes = new TObjArray();
               fGeoPassNodes = new TObjArray();
}

MpdTgemGeoPar::~MpdTgemGeoPar(void) {
}

void MpdTgemGeoPar::clear(void) {
    if(fGeoSensNodes) delete fGeoSensNodes;
    if(fGeoPassNodes) delete fGeoPassNodes;
    fGeoSensNodes = fGeoPassNodes = 0x0; //AZ
}

void MpdTgemGeoPar::putParams(FairParamList* l) {
  if (!l) return;
   //l->addBinary("FairGeoNodes Sensitive List", fGeoSensNodes);
   //l->addBinary("FairGeoNodes Passive List", fGeoPassNodes);
   l->addObject("FairGeoNodes Sensitive List", fGeoSensNodes);
   l->addObject("FairGeoNodes Passive List", fGeoPassNodes);
}

Bool_t MpdTgemGeoPar::getParams(FairParamList* l) {
    if (!l) return kFALSE;
    //if (!l->fillBinary("FairGeoNodes Sensitive List", fGeoSensNodes)) return kFALSE;
    //if (!l->fillBinary("FairGeoNodes Passive List", fGeoPassNodes)) return kFALSE;
    if (!l->fillObject("FairGeoNodes Sensitive List", fGeoSensNodes)) return kFALSE;
    if (!l->fillObject("FairGeoNodes Passive List", fGeoPassNodes)) return kFALSE;

  return kTRUE;
}
