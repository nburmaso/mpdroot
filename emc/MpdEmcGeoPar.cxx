#include "MpdEmcGeoPar.h"
#include "FairParamList.h"
#include "TObjArray.h"
#include "emc.h"
#include <iostream>

ClassImp(MpdEmcGeoPar)

MpdEmcGeoPar ::MpdEmcGeoPar(const char* name, const char* title, const char* context)
  : FairParGenericSet(name,title,context) {

  fGeoSensNodes = new TObjArray();
  fGeoPassNodes = new TObjArray();

}

MpdEmcGeoPar::MpdEmcGeoPar() {
    //TODO!!! correct parameters;
    length = emc1Chamber_z * 0.1; //0.1 - mm->cm
    rMax = outr;
    rMin = inr;
    nSec = NSector;
    nRows = NmodZ;
    nMod = NmodPHY / nSec;
}

MpdEmcGeoPar::~MpdEmcGeoPar(void) {
}

void MpdEmcGeoPar::clear(void) {
  if(fGeoSensNodes) delete fGeoSensNodes;
  if(fGeoPassNodes) delete fGeoPassNodes;
}

void MpdEmcGeoPar::putParams(FairParamList* l) {
  if (!l) return;
  l->addObject("FairGeoNodes Sensitive List", fGeoSensNodes);
  l->addObject("FairGeoNodes Passive List", fGeoPassNodes);
}

Bool_t MpdEmcGeoPar::getParams(FairParamList* l) {
  if (!l) return kFALSE;
  if (!l->fillObject("FairGeoNodes Sensitive List", fGeoSensNodes)) return kFALSE;
  if (!l->fillObject("FairGeoNodes Passive List", fGeoPassNodes)) return kFALSE;
  return kTRUE;
}
