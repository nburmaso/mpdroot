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

#ifndef MPDCPCDIGIPAR_H
#define MPDCPCDIGIPAR_H

#include "FairParGenericSet.h"
#include "TObjArray.h"

class CpcDigiPar : public FairParGenericSet {

public:
  CpcDigiPar(const char *name = "CpcDigiPar",
             const char *title = "CPC Digi Parameters",
             const char *context = "TestDefaultContext");
  virtual ~CpcDigiPar(){};

  void putParams(FairParamList *list);
  Bool_t getParams(FairParamList *list);

  inline Int_t GetADCBits() { return fADCBits; };
  inline Double_t GetADCResolution() { return fADCResolution; };
  inline Double_t GetEnergyDigiThreshold() { return fEnergyDigiThreshold; };

private:
  Int_t fADCBits;          // Hardware parameter
  Double_t fADCResolution; // Hardware parameter
  Double_t fEnergyDigiThreshold;

  ClassDef(CpcDigiPar, 1);
};
#endif // MPDBMDDIGIPAR_H
