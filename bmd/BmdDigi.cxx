
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

#include "BmdDigi.h"
#include "BmdDigiScheme.h"

#include <iostream>
using std::cout;
using std::endl;

static const Int_t kADCBits = 30;               // build-in constant
static const Double_t kADCResolution = 1e-9;    // build-in constant
static const Double_t kEnergyDigiThreshold = 0; // build-in constant

char BmdDigi::fWasInitialized = 0;

Int_t BmdDigi::fADCBits = kADCBits;
Double_t BmdDigi::fADCResolution = kADCResolution;
Double_t BmdDigi::fEnergyDigiThreshold = kEnergyDigiThreshold;

// -------------------------------------------------------------------------

BmdDigi::BmdDigi() {
  fDetectorID = -1;
  fModuleID = -1;
  fChannelID = -1;
  fELoss = 0;
  fELossDigi = 0;
  fIsPsd = kFALSE;
}

// -------------------------------------------------------------------------

BmdDigi::BmdDigi(Int_t pfDetectorID, Int_t pfModuleID, Int_t pfChannelID,
                 Double_t pfELoss, Bool_t pIsPsd) {
  fDetectorID = pfDetectorID;
  fModuleID = pfModuleID;
  fChannelID = pfChannelID;
  fELossDigi = 0;

  InitStatic(pIsPsd);
  fELoss = pfELoss;
}

// -------------------------------------------------------------------------

BmdDigi::BmdDigi(BmdPoint *p, Bool_t pIsPsd) {
  fDetectorID = -1;
  fModuleID = -1;
  fChannelID = -1;
  fELoss = 0;
  fELossDigi = 0;

  BmdDigiScheme *pDigiScheme = BmdDigiScheme::Instance();

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

BmdDigi::~BmdDigi() {}

// -------------------------------------------------------------------------

void BmdDigi::Clear() {
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

void BmdDigi::InitStatic(Bool_t pIsPsd) {
  Bool_t do_init = ((fIsPsd != pIsPsd) || (!fWasInitialized));
  if (do_init) {
    Clear();
    fIsPsd = pIsPsd;
    fWasInitialized = 1;
    BmdDigiScheme *pDigiScheme = BmdDigiScheme::Instance();

    if (pDigiScheme) {

      BmdDigiPar *pBmdDigiPar = 0;

      if (!pIsPsd)
        pBmdDigiPar = pDigiScheme->GetBmdDigiPar();

      if (pBmdDigiPar) {
        fADCBits = pBmdDigiPar->GetADCBits();
        fADCResolution = pBmdDigiPar->GetADCResolution();
        fEnergyDigiThreshold = pBmdDigiPar->GetEnergyDigiThreshold();
        fWasInitialized = 2;
      }
    }
  }
}

// -------------------------------------------------------------------------

void BmdDigi::Print(const Option_t *opt) {
  cout << " BmdDigi  DetID:" << fDetectorID << " ModuleID:" << fModuleID
       << " ChanID:" << fChannelID << " ELossDigi:" << fELossDigi
       << " ELoss: " << fELoss << "  IsPsd: " << fIsPsd
       << "  [Bits:" << fADCBits << ", Resol:" << fADCResolution
       << ", Thresh:" << fEnergyDigiThreshold << ", Init:" << fWasInitialized
       << "]" << endl;
}

// -------------------------------------------------------------------------

UInt_t BmdDigi::AddBmdPoint(BmdPoint *p) {
  BmdDigiScheme *pDigiScheme = BmdDigiScheme::Instance();

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

UInt_t BmdDigi::ADC(Double_t pfELoss) {
  //  if (pfELoss<=fEnergyDigiThreshold) {
  if (pfELoss > 0) {
    UInt_t tmp = 0;
    tmp = (UInt_t)(pfELoss / fADCResolution);
    return (tmp & ((2 << (fADCBits - 1)) - 1));
  } else
    return 0;
}

// -------------------------------------------------------------------------

ClassImp(BmdDigi)
