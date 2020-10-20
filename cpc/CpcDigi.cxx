
/*************************************************************************************
 *
 *            BmdDigi
 *    Class for digital data taken from Bmd detector
 *
 *  Author:   RO
 *  e-mail:   rogachevsky@jinr.ru
 *  Version:  19-May-2019
 *
 ************************************************************************************/

#include "CpcDigi.h"
#include "CpcDigiScheme.h"

#include <iostream>
using std::cout;
using std::endl;

static const Int_t kADCBits = 30;               // build-in constant
static const Double_t kADCResolution = 1e-9;    // build-in constant
static const Double_t kEnergyDigiThreshold = 0; // build-in constant

char CpcDigi::fWasInitialized = 0;

Int_t CpcDigi::fADCBits = kADCBits;
Double_t CpcDigi::fADCResolution = kADCResolution;
Double_t CpcDigi::fEnergyDigiThreshold = kEnergyDigiThreshold;

// -------------------------------------------------------------------------

CpcDigi::CpcDigi() {
  fDetectorID = -1;
  fModuleID = -1;
  fChannelID = -1;
  fELoss = 0;
  fELossDigi = 0;
  fIsPsd = kFALSE;
}

// -------------------------------------------------------------------------

CpcDigi::CpcDigi(Int_t pfDetectorID, Int_t pfModuleID, Int_t pfChannelID,
                 Double_t pfELoss, Bool_t pIsPsd) {
  fDetectorID = pfDetectorID;
  fModuleID = pfModuleID;
  fChannelID = pfChannelID;
  fELossDigi = 0;

  InitStatic(pIsPsd);
  fELoss = pfELoss;
}

// -------------------------------------------------------------------------

CpcDigi::CpcDigi(MpdCpcPoint *p, Bool_t pIsPsd) {
  fDetectorID = -1;
  fModuleID = -1;
  fChannelID = -1;
  fELoss = 0;
  fELossDigi = 0;

  CpcDigiScheme *pDigiScheme = CpcDigiScheme::Instance();

  if ((pDigiScheme) && (p)) {

    Int_t detID, modID, chanID;
    pDigiScheme->SplitDigiID(
        pDigiScheme->GetDigiIdFromCoords(p->GetX(), p->GetY(), p->GetZ()),
        detID, modID, chanID);

    fDetectorID = detID;
    fModuleID = modID;
    fChannelID = chanID;

    fELoss = p->GetEnergyLoss();
  }

  InitStatic(pIsPsd);
}

// -------------------------------------------------------------------------

CpcDigi::~CpcDigi() {}

// -------------------------------------------------------------------------

void CpcDigi::Clear() {
  fWasInitialized = 0;
  fIsPsd = kFALSE;
  fADCBits = kADCBits;
  fADCResolution = kADCResolution;
  fEnergyDigiThreshold = kEnergyDigiThreshold;

  fELossDigi = 0;
  fELoss = 0;
}

// -------------------------------------------------------------------------
// -------------------------------------------------------------------------

void CpcDigi::InitStatic(Bool_t pIsPsd) {
  Bool_t do_init = ((fIsPsd != pIsPsd) || (!fWasInitialized));
  if (do_init) {
    Clear();
    fIsPsd = pIsPsd;
    fWasInitialized = 1;
    CpcDigiScheme *pDigiScheme = CpcDigiScheme::Instance();

    if (pDigiScheme) {

      CpcDigiPar *pCpcDigiPar = 0;

      if (!pIsPsd)
        pCpcDigiPar = pDigiScheme->GetCpcDigiPar();

      if (pCpcDigiPar) {
        fADCBits = pCpcDigiPar->GetADCBits();
        fADCResolution = pCpcDigiPar->GetADCResolution();
        fEnergyDigiThreshold = pCpcDigiPar->GetEnergyDigiThreshold();
        fWasInitialized = 2;
      }
    }
  }
}

// -------------------------------------------------------------------------

void CpcDigi::Print(const Option_t *opt) {
  cout << " CpcDigi  DetID:" << fDetectorID << " ModuleID:" << fModuleID
       << " ChanID:" << fChannelID << " ELossDigi:" << fELossDigi
       << " ELoss: " << fELoss << "  IsPsd: " << fIsPsd
       << "  [Bits:" << fADCBits << ", Resol:" << fADCResolution
       << ", Thresh:" << fEnergyDigiThreshold << ", Init:" << fWasInitialized
       << "]" << endl;
}

// -------------------------------------------------------------------------

UInt_t CpcDigi::AddCpcPoint(MpdCpcPoint *p) {
  CpcDigiScheme *pDigiScheme = CpcDigiScheme::Instance();

  if ((pDigiScheme) && (p)) {

    Int_t detID, modID, chanID;
    pDigiScheme->SplitDigiID(
        pDigiScheme->GetDigiIdFromCoords(p->GetX(), p->GetY(), p->GetZ()),
        detID, modID, chanID);

    if ((fDetectorID == detID) && (fModuleID == modID) &&
        (fChannelID == chanID))
      fELoss += p->GetEnergyLoss();
  }

  return 0;
}

// -------------------------------------------------------------------------

UInt_t CpcDigi::ADC(Double_t pfELoss) {
  //  if (pfELoss<=fEnergyDigiThreshold) {
  if (pfELoss > 0) {
    UInt_t tmp = 0;
    tmp = (UInt_t)(pfELoss / fADCResolution);
    return (tmp & ((2 << (fADCBits - 1)) - 1));
  } else
    return 0;
}

// -------------------------------------------------------------------------

ClassImp(CpcDigi)
