#include <TStyle.h>
#include "MpdFemtoHistos.h"

//--------------------------------------------------------------------------

MpdFemtoHistos::MpdFemtoHistos(Float_t qInv, const Char_t* out) {
    fQinv = qInv;
    fOut = new TFile(out, "RECREATE");
    _hCFQinvNomBase = new TH1F("_hCFQinvNomBase", "_hCFQinvNomBase", 50, 0., qInv);
    _hCFQinvNom = new TH1F("_hCFQinvNom", "_hCFQinvNom", 50, 0., qInv);
    _hCFQinvDenom = new TH1F("_hCFQinvDenom", "_hCFQinvDenom", 50, 0., qInv);
    _hCF = new TH1F("_hCF", "_hCF", 50, 0., qInv);
    _hCFBase = new TH1F("_hCFBase", "_hCFBase", 50, 0., qInv);
    
    _hCFNom3D   = new TH3F("_hCFNom3D", "_hCFNom3D", 10, 0., qInv, 10, 0., qInv, 10, 0., qInv);
    _hCFDenom3D = new TH3F("_hCFDenom3D", "_hCFDenom3D", 10, 0., qInv, 10, 0., qInv, 10, 0., qInv);
    _hCF3D      = new TH3F("_hCF3D", "_hCF3D", 10, 0., qInv, 10, 0., qInv, 10, 0., qInv);
}

//--------------------------------------------------------------------------

MpdFemtoHistos::~MpdFemtoHistos() {
    _hCFQinvNomBase->Write();
    _hCFQinvNom->Write();
    _hCFQinvDenom->Write();
    _hCF->Write();
    _hCFBase->Write();
    _hCFNom3D->Write();
    _hCFDenom3D->Write();
    _hCF3D->Write();

    delete _hCFQinvNomBase;
    delete _hCFQinvNom;
    delete _hCFQinvDenom;
    delete _hCF;
    delete _hCFBase;
    delete _hCFNom3D;
    delete _hCFDenom3D;
    delete _hCF3D;

    delete fOut;
}

//--------------------------------------------------------------------------

Double_t* MpdFemtoHistos::GetFitParams1D() {
    // Normalization:
    _hCFQinvNom->Sumw2();
    // fHisto->GetNominatorBase()->Sumw2();
    _hCFQinvDenom->Sumw2();
    _hCF->Sumw2();
    _hCFBase->Sumw2();

    _hCF->Divide(_hCFQinvNom, _hCFQinvDenom, 1., 1.);
    _hCFBase->Divide(_hCFQinvNomBase, _hCFQinvDenom, 1., 1.);

    Float_t normDenomFactor = _hCFQinvDenom->Integral(0.25 * _hCFQinvDenom->GetNbinsX(), 0.75 * _hCFQinvDenom->GetNbinsX());
    Float_t normNominFactor = _hCFQinvNom->Integral(0.25 * _hCFQinvNom->GetNbinsX(), 0.75 * _hCFQinvNom->GetNbinsX());
    Float_t normNominBaseFactor = _hCFQinvNomBase->Integral(0.25 * _hCFQinvNomBase->GetNbinsX(), 0.75 * _hCFQinvNomBase->GetNbinsX());

    if (normNominFactor > 0. && normNominBaseFactor > 0.) {
        _hCF->Scale(normDenomFactor / normNominFactor);
        _hCFBase->Scale(normDenomFactor / normNominBaseFactor);
    }
       
    //Fitting: fit QS only: C2 = N * [1 + lambda * exp(-q^2r0^2/h/h)], h=0.1973 GeV/fm
    TF1* fqs = new TF1("fqs", "[2] * (1 + [1] * exp(-x * x * [0] * [0] / 0.1973 / 0.1973))", 0, fQinv);
    fqs->SetParName(0, "r_{0}");
    fqs->SetParName(1, "#lambda");
    fqs->SetParName(2, "Norm");
    fqs->SetParameters(10, 1, 1);

    _hCF->Fit(fqs, "SRQ", " ", 0., fQinv); 
      
    return fqs->GetParameters();
}

//--------------------------------------------------------------------------

Double_t* MpdFemtoHistos::GetFitParams3D() {
    _hCFNom3D->Sumw2();
    _hCFDenom3D->Sumw2();
    _hCF3D->Sumw2();
    _hCF3D->Divide(_hCFNom3D, _hCFDenom3D, 1., 1.);

    TF3* fitc = new TF3("fitc", "1 + [3] * exp(-25.76578 * (x * x * [0] * [0] + y * y * [1] * [1] + z * z * [2] * [2]))");
    fitc->SetParameters(3.0, 3.0, 3.0, 1.0);

    _hCF3D->Fit(fitc, "SRQ", " ", 0., fQinv);

    return fitc->GetParameters();
}
