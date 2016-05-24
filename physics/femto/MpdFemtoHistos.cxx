#include "MpdFemtoHistos.h"

//--------------------------------------------------------------------------

MpdFemtoHistos::MpdFemtoHistos() :
_hCFQinvNomBase(NULL),
_hCFQinvNom(NULL),
_hCFQinvDenom(NULL),
_hCF(NULL),
_hCFBase(NULL) {
    _hCFQinvNomBase = new TH1F("_hCFQinvNomBase", "_hCFQinvNomBase", 50, 0., 0.15);
    _hCFQinvNom = new TH1F("_hCFQinvNom", "_hCFQinvNom", 50, 0., 0.15);
    _hCFQinvDenom = new TH1F("_hCFQinvDenom", "_hCFQinvDenom", 50, 0., 0.15);
    _hCF = new TH1F("_hCF", "_hCF", 50, 0., 0.15);
    _hCFBase = new TH1F("_hCFBase", "_hCFBase", 50, 0., 0.15);
}

//--------------------------------------------------------------------------

MpdFemtoHistos::~MpdFemtoHistos() {
    delete _hCFQinvNomBase;
    delete _hCFQinvNom;
    delete _hCFQinvDenom;
}

//--------------------------------------------------------------------------
void MpdFemtoHistos::MakeNorm_1D() {
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
}