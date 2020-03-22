/*************************************************************************************
 *
 *         BmdDigiPar
 *    Container class for Bmd digitisation parameters
 *
 *  Author:   RO
 *  e-mail:   rogachevsky@jinr.ru
 *  Version:  04-Apr-2008
 *
 ************************************************************************************/

#include "BmdDigiPar.h"
#include "FairParamList.h"

ClassImp(BmdDigiPar)

    BmdDigiPar::BmdDigiPar(const char *name, const char *title,
                           const char *context)
    : FairParGenericSet(name, title, context) {}

void BmdDigiPar::putParams(FairParamList *list) {
  if (!list)
    return;
  list->add("ADCBits", fADCBits);
  list->add("ADCResolution", fADCResolution);
  list->add("EnergyDigiThreshold", fEnergyDigiThreshold);
}
Bool_t BmdDigiPar::getParams(FairParamList *list) {
  if (!list)
    return kFALSE;
  if (!list->fill("ADCBits", &fADCBits))
    return kFALSE;
  if (!list->fill("ADCResolution", &fADCResolution))
    return kFALSE;
  if (!list->fill("EnergyDigiThreshold", &fEnergyDigiThreshold))
    return kFALSE;
  return kTRUE;
}
