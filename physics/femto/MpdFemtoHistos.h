#ifndef MPDFEMTOHISTOS_H
#define MPDFEMTOHISTOS_H 1

#include <iostream>
#include <TNamed.h>
#include <TH1.h>
#include <TF1.h>
#include <TF3.h>
#include <TFitResult.h>
#include <TFitResultPtr.h>
#include <TVector3.h>
#include <TH3.h>
#include <TFile.h>

using namespace std;

class MpdFemtoHistos : public TNamed {
public:
    MpdFemtoHistos() {}
    MpdFemtoHistos(Float_t, const Char_t*);

    virtual ~MpdFemtoHistos();
    
    void SetQinv(Float_t val) {
        fQinv = val;
    }
    
    // Getters and Setters for 1D-analysis
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
    
    // Getters and Setters for 3D-analysis
    TH3F* GetNominator3D() {
        return _hCFNom3D;
    }

    TH3F* GetDenominator3D() {
        return _hCFDenom3D;
    }
    
    TH3F* GetCF3D() {
        return _hCF3D;
    }
        
    void SetNominator3D(TH3F* h) {
        _hCFNom3D = h;
    }

    void SetDenominator3D(TH3F* h) {
        _hCFDenom3D = h;
    }
    
    void SetCF3D(TH3F* h) {
    _hCF3D = h;
    }
     
    Double_t* GetFitParams1D();
    Double_t* GetFitParams3D();
       
private:
    TFile* fOut;
    
    Int_t fBins;
    Float_t fxUp;
    
    Float_t fQinv;
    
    TH1F* _hCFQinvNomBase;
    TH1F* _hCFQinvNom;
    TH1F* _hCFQinvDenom;
    TH1F* _hCF;
    TH1F* _hCFBase;
    
    TH3F* _hCFNom3D;
    TH3F* _hCFDenom3D;
    TH3F* _hCF3D;
    
    ClassDef(MpdFemtoHistos, 1)
};

#endif