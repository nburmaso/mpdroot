/////////////////////////////////////////////////////////////
//
//  MpdEmcHitProducer
//
//  Filler of MpdEmcHit
//
/////////////////////////////////////////////////////////////// 


#include "TClonesArray.h"
#include "FairRootManager.h"
#include "TMath.h"
#include "TH2F.h"
//*
#include "FairRunAna.h"

#include "MpdEmcHitProducer.h"
#include "MpdEmcGeoPar.h"
#include "MpdEmcHit.h"
#include "MpdEmcPoint.h"

using namespace std;
using namespace TMath;

// -----   Default constructor   -------------------------------------------

MpdEmcHitProducer::MpdEmcHitProducer() :
FairTask("Ideal EMC hit Producer") {

}


// -----   Destructor   ----------------------------------------------------

MpdEmcHitProducer::~MpdEmcHitProducer() {
}
// -------------------------------------------------------------------------



// -----   Public method Init   --------------------------------------------

InitStatus MpdEmcHitProducer::Init() {

    cout << "******************* EMC INIT *********************" << endl;

    // Get RootManager
    FairRootManager* ioman = FairRootManager::Instance();
    if (!ioman) {
        cout << "-E- MpdEmcHitProducer::Init: "
                << "RootManager not instantiated!" << endl;
        return kFATAL;
    }

    fGeoPar = new MpdEmcGeoPar();

    // Get input array
    fPointArray = (TClonesArray*) ioman->GetObject("MpdEmcPoint");
    if (!fPointArray) {
        cout << "-W- MpdEmcHitProducer::Init: " << "No EmcPoint array!" << endl;
        return kERROR;
    }

    // Create and register output array
    fDigiArray = new TClonesArray("MpdEmcHit", 100);
    ioman->Register("MpdEmcHit", "EMC", fDigiArray, kTRUE);

    cout << "-I- MpdEmcHitProducer: Intialization successfull" << endl;

    return kSUCCESS;

}

void MpdEmcHitProducer::Finish() {
    //---

    Int_t nSec = fGeoPar->GetNsec();
    Int_t nMod = fGeoPar->GetNmod();
    Int_t nRow = fGeoPar->GetNrows();
    TH2F* h_map = new TH2F("tmp", "tmp", nRow * 2, -nRow, nRow, nSec * nMod, 0, nSec * nMod);
    for (UInt_t iDig = 0; iDig < fDigiArray->GetEntriesFast(); ++iDig) {
        MpdEmcHit* dig = (MpdEmcHit*) fDigiArray->At(iDig);
        Int_t sec = dig->GetSec();
        Int_t row = dig->GetRow();
        Int_t mod = dig->GetMod();
        Float_t E = dig->GetE();

        Int_t globModNumber;
        if (sec > nSec) {
            row *= -1;
            globModNumber = (sec - nSec) * 4 + mod;
        } else {
            globModNumber = sec * 4 + mod;
        }
        
        h_map->Fill(row, globModNumber, E);
    }

    h_map->Write(); //Draw("colz");

    cout << "-I- MpdEmcHitProducer: Finish" << endl;

}

void MpdEmcHitProducer::Exec(Option_t* opt) {
    cout << "MpdEmcHitProducer::Exec started" << endl;

    // Reset output Array
    if (!fDigiArray) Fatal("MpdEmcHitProducer::Exec)", "No array of digits");
    fDigiArray->Delete();

    for (UInt_t iPnt = 0; iPnt < fPointArray->GetEntriesFast(); ++iPnt) {
        FairMCPoint* pnt = (FairMCPoint*) fPointArray->At(iPnt);
        Float_t x = pnt->GetX();
        Float_t y = pnt->GetY();
        Float_t z = pnt->GetZ();
        UInt_t sec = GetSecId(x, y, z);
        UInt_t row = GetRowId(z);
        UInt_t mod = GetModId(x, y, z, sec);
        Float_t e = pnt->GetEnergyLoss();

        MpdEmcHit* hit = SearchHit(sec, row, mod);
        if (hit == NULL) {
            new((*fDigiArray)[fDigiArray->GetEntriesFast()]) MpdEmcHit(sec, row, mod, e);
        } else {
            hit->IncreaseEnergy(e);
        }
    }
}

UInt_t MpdEmcHitProducer::GetSecId(Float_t x, Float_t y, Float_t z) {
    UInt_t id = 1000;
    Float_t ang = ATan2(y, x);
    if (ang < 0) ang += TwoPi();
    ang *= RadToDeg();
    Int_t nSec = fGeoPar->GetNsec();
    if (z > 0.0)
        return UInt_t(ang / 360 * nSec);
    else
        return nSec + UInt_t(ang / 360 * nSec);
}

UInt_t MpdEmcHitProducer::GetModId(Float_t x, Float_t y, Float_t z, UInt_t sec) {
    UInt_t id = 1000;
    Float_t ang = ATan2(y, x);
    if (ang < 0) ang += TwoPi();
    ang *= RadToDeg();
    Float_t nDegreesInOneSector = 360.0 / fGeoPar->GetNsec();
    if (z < 0.0)
        sec -= fGeoPar->GetNsec();

    Float_t secStartAng = sec * nDegreesInOneSector;
    Float_t secFinishAng = secStartAng + nDegreesInOneSector;
    Float_t localAng = ang - secStartAng;

    return UInt_t(localAng * fGeoPar->GetNmod() / nDegreesInOneSector);
}

UInt_t MpdEmcHitProducer::GetRowId(Float_t z) {
    UInt_t id = 1000;
    return UInt_t(Abs(z) / fGeoPar->GetLength() * fGeoPar->GetNrows());
}

MpdEmcHit* MpdEmcHitProducer::SearchHit(UInt_t sec, UInt_t row, UInt_t mod) {
    MpdEmcHit* foundHit = NULL;
    for (Int_t i = 0; i < fDigiArray->GetEntriesFast(); ++i) {
        MpdEmcHit* hit = (MpdEmcHit*) fDigiArray->At(i);
        if (hit->GetSec() != sec) continue;
        if (hit->GetRow() != row) continue;
        if (hit->GetMod() != mod) continue;
        foundHit = hit;
    }
    return foundHit;
}

// -------------------------------------------------------------------------

ClassImp(MpdEmcHitProducer)
