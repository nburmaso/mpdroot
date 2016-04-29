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