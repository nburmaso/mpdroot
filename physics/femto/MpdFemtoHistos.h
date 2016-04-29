#ifndef MPDFEMTOHISTOS_H
#define MPDFEMTOHISTOS_H 1

#include <iostream>
#include <TNamed.h>
#include <TH1.h>

using namespace std;

class MpdFemtoHistos : public TNamed {
public:
    MpdFemtoHistos();
    // MpdFemtoHistos(Int_t, Float_t);

    virtual ~MpdFemtoHistos();

    // Getters  
    TH1F* GetNominator() {
        return _hCFQinvNom;
    }

    TH1F* GetNominatorBase() {
        return _hCFQinvNomBase;
    }

    TH1F* GetDenominator() {
        return _hCFQinvDenom;
    }
    
    TH1F* GetCF() {
        return _hCF;
    }
    
    TH1F* GetCFBase() {
        return _hCFBase;
    }
    
    // Setters
    void SetNominator(TH1F* h) {
        _hCFQinvNom = h;
    }

    void SetNominatorBase(TH1F* h) {
        _hCFQinvNomBase = h;
    }

    void SetDenominator(TH1F* h) {
        _hCFQinvDenom = h;
    }
    
    void SetCF(TH1F* h) {
    _hCF = h;
    }

    void SetCFBase(TH1F* h) {
    _hCFBase = h;
    }
    
private:

    Int_t fBins;
    Float_t fxUp;
    
    TH1F* _hCFQinvNomBase;
    TH1F* _hCFQinvNom;
    TH1F* _hCFQinvDenom;
    TH1F* _hCF;
    TH1F* _hCFBase;
    
    ClassDef(MpdFemtoHistos, 1)
};

#endif