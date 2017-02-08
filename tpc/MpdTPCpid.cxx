//-----------------------------------------------------------
// File and Version Information:
// $Id$
//
// Description:
//      Implementation of class ParticleIdentification
//      see ParticleIdentification.h for details
//
// Environment:
//      Software developed for MPD at NICA.
//
// Author List:
//      Gyulnara Eyyubova
//
//-----------------------------------------------------------

// C/C++ Headers ----------------------
#include <iostream>
#include <TMath.h>
#include <TF1.h>
#include <TH1.h>
#include <TH2.h>
// This Class' Header ------------------
#include "MpdTPCpid.h"

using namespace std;
using namespace TMath;

// Class Member definitions -----------

MpdTPCpid::MpdTPCpid() :
ProtonPar(),
PionPar(),
KaonPar(),
ElectronPar(),
sigmasPi(),
sigmasPr(),
sigmasKa(),
sigmasEl(),
fCoefficients(NULL) {
    ReadTPCResponse();
}

MpdTPCpid::~MpdTPCpid() {
    delete fCoefficients;
    fCoefficients = 0;
}

Int_t MpdTPCpid::GetTpcProbs(Float_t P, Float_t dedx, Int_t nHits, Float_t& Ppi, Float_t& Pk, Float_t& Pp, Float_t& Pe, Int_t method) {


    const Int_t Ntypes = 4; //order: pion, kaon, proton, electron
    Float_t resultProb[Ntypes];
    const Int_t Nintervals = 10;

    Double_t P_int[Nintervals + 1] = {0.1, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1., 1.2, 3};

    //default sigmas
    Float_t sigma = 0.07 * dedx;

    Float_t sum = 0.0; // for normalizing

    /*A priori coefficients for Bayes approach */
    Float_t bayesAprioriCoefficients[Ntypes];

    /*A "measured" probabilities */
    Float_t gausProb[Ntypes];
    for (Int_t i = 0; i < Ntypes; i++) gausProb[i] = 0;

    Int_t i_p = 0; // momentum interval 

    switch (method) {

        case 0: //bethe-bloch approximation equal bayesian coefficients

            if (P > 3) i_p = 9;
            for (Int_t k = 1; k < Nintervals; k++) if (P >= P_int[k] && P < P_int[k + 1]) i_p = k;


            bayesAprioriCoefficients[0] = 1.0;
            bayesAprioriCoefficients[1] = 1.0;
            bayesAprioriCoefficients[2] = 1.0;
            bayesAprioriCoefficients[3] = 1.0;

            gausProb[0] = Gaus(dedx, BetheBlochFunction(P, PionPar), sigmasPi[i_p], kTRUE); // kTRUE = normilized by sqrt(2*Pi)*sigma.
            gausProb[1] = Gaus(dedx, BetheBlochFunction(P, KaonPar), sigmasKa[i_p], kTRUE);
            gausProb[2] = Gaus(dedx, BetheBlochFunction(P, ProtonPar), sigmasPr[i_p], kTRUE);
            gausProb[3] = Gaus(dedx, BetheBlochFunction(P, ElectronPar), sigmasEl[i_p], kTRUE);

            sum = gausProb[0] + gausProb[1] + gausProb[2] + gausProb[3];
            if (sum == 0.) return 1;
            gausProb[0] /= sum;
            gausProb[1] /= sum;
            gausProb[2] /= sum;
            gausProb[3] /= sum;
            break;

        case 1: //bethe-bloch approximation with special byesian coefficients

            if (P > 3) i_p = 9;
            for (Int_t k = 1; k < Nintervals; k++) if (P >= P_int[k] && P < P_int[k + 1]) i_p = k;

            if (P < 0.3) {
                bayesAprioriCoefficients[0] = 0.28;
                bayesAprioriCoefficients[1] = 0.25;
                bayesAprioriCoefficients[2] = 0.26;
                bayesAprioriCoefficients[3] = 0.21;
            } else if ((P >= 0.3) && (P < 0.4)) {
                bayesAprioriCoefficients[0] = 0.91;
                bayesAprioriCoefficients[1] = 0.02;
                bayesAprioriCoefficients[2] = 0.06;
                bayesAprioriCoefficients[3] = 0.01;
            } else if ((P >= 0.4) && (P < 0.5)) {
                bayesAprioriCoefficients[0] = 0.70;
                bayesAprioriCoefficients[1] = 0.11;
                bayesAprioriCoefficients[2] = 0.13;
                bayesAprioriCoefficients[3] = 0.06;
            } else if ((P >= 0.5) && (P < 0.6)) {
                bayesAprioriCoefficients[0] = 0.39;
                bayesAprioriCoefficients[1] = 0.19;
                bayesAprioriCoefficients[2] = 0.32;
                bayesAprioriCoefficients[3] = 0.10;
            } else if ((P >= 0.6) && (P < 0.7)) {
                bayesAprioriCoefficients[0] = 0.41;
                bayesAprioriCoefficients[1] = 0.17;
                bayesAprioriCoefficients[2] = 0.37;
                bayesAprioriCoefficients[3] = 0.5;
            } else if ((P >= 0.7) && (P < 0.8)) {
                bayesAprioriCoefficients[0] = 0.43;
                bayesAprioriCoefficients[1] = 0.16;
                bayesAprioriCoefficients[2] = 0.31;
                bayesAprioriCoefficients[3] = 0.10;
            } else if ((P >= 0.8) && (P < 0.9)) {
                bayesAprioriCoefficients[0] = 0.44;
                bayesAprioriCoefficients[1] = 0.11;
                bayesAprioriCoefficients[2] = 0.43;
                bayesAprioriCoefficients[3] = 0.02;
            } else if ((P >= 0.9) && (P < 1.0)) {
                bayesAprioriCoefficients[0] = 0.36;
                bayesAprioriCoefficients[1] = 0.21;
                bayesAprioriCoefficients[2] = 0.36;
                bayesAprioriCoefficients[3] = 0.07;
            } else if ((P >= 1.0) && (P < 1.2)) {
                bayesAprioriCoefficients[0] = 0.32;
                bayesAprioriCoefficients[1] = 0.32;
                bayesAprioriCoefficients[2] = 0.32;
                bayesAprioriCoefficients[3] = 0.04;
            } else if (P >= 1.2) {
                bayesAprioriCoefficients[0] = 0.34;
                bayesAprioriCoefficients[1] = 0.27;
                bayesAprioriCoefficients[2] = 0.31;
                bayesAprioriCoefficients[3] = 0.08;
            }

            gausProb[0] = Gaus(dedx, BetheBlochFunction(P, PionPar), sigmasPi[i_p], kTRUE);
            gausProb[1] = Gaus(dedx, BetheBlochFunction(P, KaonPar), sigmasKa[i_p], kTRUE);
            gausProb[2] = Gaus(dedx, BetheBlochFunction(P, ProtonPar), sigmasPr[i_p], kTRUE);
            gausProb[3] = Gaus(dedx, BetheBlochFunction(P, ElectronPar), sigmasEl[i_p], kTRUE);

            sum = gausProb[0] + gausProb[1] + gausProb[2] + gausProb[3];
            if (sum == 0.) return 1;
            gausProb[0] /= sum;
            gausProb[1] /= sum;
            gausProb[2] /= sum;
            gausProb[3] /= sum;
            break;



    }


    BayesFunction(gausProb, bayesAprioriCoefficients, resultProb, Ntypes);

    Ppi = resultProb[0];
    Pk = resultProb[1];
    Pp = resultProb[2];
    Pe = resultProb[3];

    return 0;
}

Float_t MpdTPCpid::BetheBlochFunction(Float_t x, Float_t *p) {
    Float_t b = 1 / (x / Sqrt(1 + x * x));
    return p[0] / Power(b, p[3]) * (p[1] - Power(b, p[3]) - Log(p[2] + Power(1 / x, p[4])));
}

/*Bayes function for probabilities calculation*/
Int_t MpdTPCpid::BayesFunction(Float_t *measProb, Float_t *aprioriProb, Float_t *bayesProb, Int_t N) {

    /*measProb    - measured probabilities from TPC/TOF/etc*/
    /*aprioriProb - a priori probabilities from some magic*/
    /*bayesProb   - result bayes probabilities */
    /*N           - number of types {pion, kaon, proton, electron} */

    Float_t sumBayes = 0.0;
    for (Int_t i = 0; i < N; ++i) {
        sumBayes += measProb[i] * aprioriProb[i];
    }

    if (sumBayes == 0.) return 1;

    for (Int_t i = 0; i < N; ++i) {
        bayesProb[i] = measProb[i] * aprioriProb[i] / sumBayes;
    }

    return 0;
}

void MpdTPCpid::ReadTPCResponse() {
    TString c;
    /* Accessing the DeDx response, obtained with VHLEE generator */
    fCoefficients = new TFile("$VMCWORKDIR/input/MPDTPCPidResponseVhelle.root");

    if (fCoefficients->IsZombie()) {
        printf("File MPDTPCPidResponseVhelle.root does not exist.\n");
    }

    TH2D *hdedx_pi = (TH2D*) fCoefficients->Get("dedx_pi");
    TH2D *hdedx_pr = (TH2D*) fCoefficients->Get("dedx_pr");
    TH2D *hdedx_ka = (TH2D*) fCoefficients->Get("dedx_ka");
    TH2D *hdedx_el = (TH2D*) fCoefficients->Get("dedx_el");

    TF1 *fBetheBlochPion = hdedx_pi->GetFunction("BetheBlochALEPH");
    PionPar[0] = fBetheBlochPion->GetParameter(0);
    PionPar[1] = fBetheBlochPion->GetParameter(1);
    PionPar[2] = fBetheBlochPion->GetParameter(2);
    PionPar[3] = fBetheBlochPion->GetParameter(3);
    PionPar[4] = fBetheBlochPion->GetParameter(4);

    TF1 *fBetheBlochProton = hdedx_pr->GetFunction("BetheBlochALEPH");
    ProtonPar[0] = fBetheBlochProton->GetParameter(0);
    ProtonPar[1] = fBetheBlochProton->GetParameter(1);
    ProtonPar[2] = fBetheBlochProton->GetParameter(2);
    ProtonPar[3] = fBetheBlochProton->GetParameter(3);
    ProtonPar[4] = fBetheBlochProton->GetParameter(4);

    TF1 *fBetheBlochKaon = hdedx_ka->GetFunction("BetheBlochALEPH");
    KaonPar[0] = fBetheBlochKaon->GetParameter(0);
    KaonPar[1] = fBetheBlochKaon->GetParameter(1);
    KaonPar[2] = fBetheBlochKaon->GetParameter(2);
    KaonPar[3] = fBetheBlochKaon->GetParameter(3);
    KaonPar[4] = fBetheBlochKaon->GetParameter(4);

    TF1 *fBetheBlochElectron = hdedx_el->GetFunction("BetheBlochALEPH");
    ElectronPar[0] = fBetheBlochElectron->GetParameter(0);
    ElectronPar[1] = fBetheBlochElectron->GetParameter(1);
    ElectronPar[2] = fBetheBlochElectron->GetParameter(2);
    ElectronPar[3] = fBetheBlochElectron->GetParameter(3);
    ElectronPar[4] = fBetheBlochElectron->GetParameter(4);


    const Int_t Nintervals = 10;
    Double_t P_int[Nintervals + 1] = {0.1, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1., 1.2, 3};

    for (Int_t k = 0; k < Nintervals; k++) {

        Int_t p_low_bin = hdedx_pi->GetXaxis()->FindBin(P_int[k]);
        Int_t p_up_bin = hdedx_pi->GetXaxis()->FindBin(P_int[k + 1]);

        c = Form("hpi%i", k);
        TH1D *hpi = (TH1D*) hdedx_pi->ProjectionY(c, p_low_bin, p_up_bin);
        hpi->Fit("gaus");
        sigmasPi[k] = hpi->GetFunction("gaus")->GetParameter(2);

        c = Form("hpr%i", k);
        TH1D *hpr = (TH1D*) hdedx_pr->ProjectionY("hpr", p_low_bin, p_up_bin);
        hpr->Fit("gaus");
        sigmasPr[k] = hpr->GetFunction("gaus")->GetParameter(2);

        c = Form("hka%i", k);
        TH1D *hka = (TH1D*) hdedx_ka->ProjectionY("hka", p_low_bin, p_up_bin);
        hka->Fit("gaus");
        sigmasKa[k] = hka->GetFunction("gaus")->GetParameter(2);

        c = Form("hel%i", k);
        TH1D *hel = (TH1D*) hdedx_el->ProjectionY("hel", p_low_bin, p_up_bin);
        hel->Fit("gaus");
        sigmasEl[k] = hel->GetFunction("gaus")->GetParameter(2);
    }

    //  f->Close();

}

ClassImp(MpdTPCpid);
