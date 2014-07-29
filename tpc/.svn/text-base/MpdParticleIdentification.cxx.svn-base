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
//      Sergey Merts
//
//-----------------------------------------------------------

// C/C++ Headers ----------------------
#include <iostream>
#include <TMath.h>

// This Class' Header ------------------
#include "MpdParticleIdentification.h"

using namespace std;
using namespace TMath;

// Class Member definitions -----------

MpdParticleIdentification::MpdParticleIdentification() {}

MpdParticleIdentification::~MpdParticleIdentification() {}

Int_t MpdParticleIdentification::GetTpcProbs(Float_t P, Float_t dedx, Int_t nHits, Float_t& Ppi, Float_t& Pk, Float_t& Pp, Float_t& Pe, Int_t method) {

    /*Parameters for fitting*/
    Float_t *ProtonPar, *PionPar, *KaonPar, *ElectronPar;
    /************************/

    dedx *= 1000000; // converting to keV/cm
    const Int_t Ntypes = 4; //order: pion, kaon, proton, electron
    const Int_t Nintervals = 10;
    Float_t sigmas[Ntypes][Nintervals] = {//array of sigmas for different particles and different intervals of momentum. They were got from 100000 BOX events
        {0.113, 0.107, 0.100, 0.107, 0.097, 0.111, 0.122, 0.105, 0.115, 0.118},
        {0.407, 0.276, 0.260, 0.159, 0.185, 0.154, 0.122, 0.105, 0.115, 0.118},
        {0.507, 0.488, 0.394, 0.330, 0.268, 0.244, 0.260, 0.172, 0.193, 0.118},
        {0.163, 0.160, 0.149, 0.159, 0.172, 0.205, 0.260, 0.172, 0.193, 0.156}
    };

    Float_t sigma = 1.0 / TMath::Sqrt(nHits); //for gaussian
    Float_t sum = 0.0; // for normalizing

    /*A priori coefficients for Bayes approach */
    Float_t bayesAprioriCoefficients[Ntypes];
    /*A "measured" probabilities */
    Float_t gausProb[Ntypes];

    Float_t sigPi, sigPr, sigKa, sigEl; //sigmas for gaussians

    switch (method) {

        case 0: //bethe-bloch approximation with "standard" sigmas and equal bayesian coefficients

            //parameters were got from Bethe-Bloch approximation for 100000 BOX events
            ProtonPar   = new Float_t[4];
            PionPar     = new Float_t[4];
            KaonPar     = new Float_t[4];
            ElectronPar = new Float_t[4];
            
            ProtonPar[0]   = -3.957;  ProtonPar[1]   = 3.525;  ProtonPar[2]   = 16.468;    ProtonPar[3]   = 0.815;  ProtonPar[4]   = 5.247;
            PionPar[0]     = -0.739;  PionPar[1]     = 7.373;  PionPar[2]     = 3904.790;  PionPar[3]     = 0.274;  PionPar[4]     = 5.497;
            KaonPar[0]     = -2.590;  KaonPar[1]     = 4.918;  KaonPar[2]     = 79.722;    KaonPar[3]     = 0.357;  KaonPar[4]     = 4.511;
            ElectronPar[0] = -1.552;  ElectronPar[1] = 1.748;  ElectronPar[2] = 7.425;     ElectronPar[3] = 0.980;  ElectronPar[4] = 1.604;

            sigma = 0.07 * dedx;
            sigPi = sigPr = sigKa = sigEl = sigma;
	    bayesAprioriCoefficients[0] = 1.0;
            bayesAprioriCoefficients[1] = 1.0;
            bayesAprioriCoefficients[2] = 1.0;
            bayesAprioriCoefficients[3] = 1.0;
            
            gausProb[0] = Gaus(dedx, BetheBlochFunction(P, PionPar),     sigPi, kTRUE);
            gausProb[1] = Gaus(dedx, BetheBlochFunction(P, KaonPar),     sigKa, kTRUE);
            gausProb[2] = Gaus(dedx, BetheBlochFunction(P, ProtonPar),   sigPr, kTRUE);
            gausProb[3] = Gaus(dedx, BetheBlochFunction(P, ElectronPar), sigEl, kTRUE);
            
            sum = gausProb[0] + gausProb[1] + gausProb[2] + gausProb[3];
            if (sum == 0.) return 1;
            gausProb[0] /= sum;
            gausProb[1] /= sum;
            gausProb[2] /= sum;
            gausProb[3] /= sum;
            break;

        case 1: //bethe-bloch approximation with special different sigmas and byesian coefficients

            //parameters were got from Bethe-Bloch approximation for 100000 BOX events
            ProtonPar   = new Float_t[4];
            PionPar     = new Float_t[4];
            KaonPar     = new Float_t[4];
            ElectronPar = new Float_t[4];
            
            ProtonPar[0]   = -3.957;  ProtonPar[1]   = 3.525;  ProtonPar[2]   = 16.468;    ProtonPar[3]   = 0.815;  ProtonPar[4]   = 5.247;
            PionPar[0]     = -0.739;  PionPar[1]     = 7.373;  PionPar[2]     = 3904.790;  PionPar[3]     = 0.274;  PionPar[4]     = 5.497;
            KaonPar[0]     = -2.590;  KaonPar[1]     = 4.918;  KaonPar[2]     = 79.722;    KaonPar[3]     = 0.357;  KaonPar[4]     = 4.511;
            ElectronPar[0] = -1.552;  ElectronPar[1] = 1.748;  ElectronPar[2] = 7.425;     ElectronPar[3] = 0.980;  ElectronPar[4] = 1.604;
            
            if (P < 0.3) {
                bayesAprioriCoefficients[0] = 0.28;
                bayesAprioriCoefficients[1] = 0.25;
                bayesAprioriCoefficients[2] = 0.26;
                bayesAprioriCoefficients[3] = 0.21;
                sigPi = sigmas[0][0];
                sigKa = sigmas[1][0];
                sigPr = sigmas[2][0];
                sigEl = sigmas[3][0];
            } else if ((P >= 0.3) && (P < 0.4)) {
                bayesAprioriCoefficients[0] = 0.91;
                bayesAprioriCoefficients[1] = 0.02;
                bayesAprioriCoefficients[2] = 0.06;
                bayesAprioriCoefficients[3] = 0.01;
                sigPi = sigmas[0][1];
                sigKa = sigmas[1][1];
                sigPr = sigmas[2][1];
                sigEl = sigmas[3][1];
            } else if ((P >= 0.4) && (P < 0.5)) {
                bayesAprioriCoefficients[0] = 0.70;
                bayesAprioriCoefficients[1] = 0.11;
                bayesAprioriCoefficients[2] = 0.13;
                bayesAprioriCoefficients[3] = 0.06;
                sigPi = sigmas[0][2];
                sigKa = sigmas[1][2];
                sigPr = sigmas[2][2];
                sigEl = sigmas[3][2];
            } else if ((P >= 0.5) && (P < 0.6)) {
                bayesAprioriCoefficients[0] = 0.39;
                bayesAprioriCoefficients[1] = 0.19;
                bayesAprioriCoefficients[2] = 0.32;
                bayesAprioriCoefficients[3] = 0.10;
                sigPi = sigmas[0][3];
                sigKa = sigmas[1][3];
                sigPr = sigmas[2][3];
                sigEl = sigmas[3][3];
            } else if ((P >= 0.6) && (P < 0.7)) {
                bayesAprioriCoefficients[0] = 0.41;
                bayesAprioriCoefficients[1] = 0.17;
                bayesAprioriCoefficients[2] = 0.37;
                bayesAprioriCoefficients[3] = 0.5;
                sigPi = sigmas[0][4];
                sigKa = sigmas[1][4];
                sigPr = sigmas[2][4];
                sigEl = sigmas[3][4];
            } else if ((P >= 0.7) && (P < 0.8)) {
                bayesAprioriCoefficients[0] = 0.43;
                bayesAprioriCoefficients[1] = 0.16;
                bayesAprioriCoefficients[2] = 0.31;
                bayesAprioriCoefficients[3] = 0.10;
                sigPi = sigmas[0][5];
                sigKa = sigmas[1][5];
                sigPr = sigmas[2][5];
                sigEl = sigmas[3][5];
            } else if ((P >= 0.8) && (P < 0.9)) {
                bayesAprioriCoefficients[0] = 0.44;
                bayesAprioriCoefficients[1] = 0.11;
                bayesAprioriCoefficients[2] = 0.43;
                bayesAprioriCoefficients[3] = 0.02;
                sigPi = sigmas[0][6];
                sigKa = sigmas[1][6];
                sigPr = sigmas[2][6];
                sigEl = sigmas[3][6];
            } else if ((P >= 0.9) && (P < 1.0)) {
                bayesAprioriCoefficients[0] = 0.36;
                bayesAprioriCoefficients[1] = 0.21;
                bayesAprioriCoefficients[2] = 0.36;
                bayesAprioriCoefficients[3] = 0.07;
                sigPi = sigmas[0][7];
                sigKa = sigmas[1][7];
                sigPr = sigmas[2][7];
                sigEl = sigmas[3][7];
            } else if ((P >= 1.0) && (P < 1.2)) {
                bayesAprioriCoefficients[0] = 0.32;
                bayesAprioriCoefficients[1] = 0.32;
                bayesAprioriCoefficients[2] = 0.32;
                bayesAprioriCoefficients[3] = 0.04;
                sigPi = sigmas[0][8];
                sigKa = sigmas[1][8];
                sigPr = sigmas[2][8];
                sigEl = sigmas[3][8];
            } else if (P >= 1.2) {
                bayesAprioriCoefficients[0] = 0.34;
                bayesAprioriCoefficients[1] = 0.27;
                bayesAprioriCoefficients[2] = 0.31;
                bayesAprioriCoefficients[3] = 0.08;
                sigPi = sigmas[0][9];
                sigKa = sigmas[1][9];
                sigPr = sigmas[2][9];
                sigEl = sigmas[3][9];
            }
            
            gausProb[0] = Gaus(dedx, BetheBlochFunction(P, PionPar),     sigPi, kTRUE);
            gausProb[1] = Gaus(dedx, BetheBlochFunction(P, KaonPar),     sigKa, kTRUE);
            gausProb[2] = Gaus(dedx, BetheBlochFunction(P, ProtonPar),   sigPr, kTRUE);
            gausProb[3] = Gaus(dedx, BetheBlochFunction(P, ElectronPar), sigEl, kTRUE);
            
            sum = gausProb[0] + gausProb[1] + gausProb[2] + gausProb[3];
            if (sum == 0.) return 1;
            gausProb[0] /= sum;
            gausProb[1] /= sum;
            gausProb[2] /= sum;
            gausProb[3] /= sum;
            break;

	case 2: //bethe-bloch approximation with special different sigmas and byesian coefficients

            //parameters were got from Bethe-Bloch approximation for 100000 BOX events
            ProtonPar   = new Float_t[4];
            PionPar     = new Float_t[4];
            KaonPar     = new Float_t[4];
            ElectronPar = new Float_t[4];
            
            ProtonPar[0]   = -3.957;  ProtonPar[1]   = 3.525;  ProtonPar[2]   = 16.468;    ProtonPar[3]   = 0.815;  ProtonPar[4]   = 5.247;
            PionPar[0]     = -0.739;  PionPar[1]     = 7.373;  PionPar[2]     = 3904.790;  PionPar[3]     = 0.274;  PionPar[4]     = 5.497;
            KaonPar[0]     = -2.590;  KaonPar[1]     = 4.918;  KaonPar[2]     = 79.722;    KaonPar[3]     = 0.357;  KaonPar[4]     = 4.511;
            ElectronPar[0] = -1.552;  ElectronPar[1] = 1.748;  ElectronPar[2] = 7.425;     ElectronPar[3] = 0.980;  ElectronPar[4] = 1.604;
            
	    sigma = 0.07 * dedx;
            sigPi = sigPr = sigKa = sigEl = sigma;
	    
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
            
            gausProb[0] = Gaus(dedx, BetheBlochFunction(P, PionPar),     sigPi, kTRUE);
            gausProb[1] = Gaus(dedx, BetheBlochFunction(P, KaonPar),     sigKa, kTRUE);
            gausProb[2] = Gaus(dedx, BetheBlochFunction(P, ProtonPar),   sigPr, kTRUE);
            gausProb[3] = Gaus(dedx, BetheBlochFunction(P, ElectronPar), sigEl, kTRUE);
            
            sum = gausProb[0] + gausProb[1] + gausProb[2] + gausProb[3];
            if (sum == 0.) return 1;
            gausProb[0] /= sum;
            gausProb[1] /= sum;
            gausProb[2] /= sum;
            gausProb[3] /= sum;
            break;
	    
        case 3: //parabolic approximation

            //parameters were got from parabolic approximation function for 2000 UrQMD events
            ProtonPar[0]   = 0.031;  ProtonPar[1]   = -0.124;  ProtonPar[2]   = 1.412;
            PionPar[0]     = 0.230;  PionPar[1]     =  0.088;  PionPar[2]     = 1.072;
            KaonPar[0]     = 0.676;  KaonPar[1]     =  0.621;  KaonPar[2]     = 0.831;
            ElectronPar[0] = 0.000;  ElectronPar[1] = -0.018;  ElectronPar[2] = 2.055;

            Float_t invP = 1.0 / P;

            gausProb[0] = Gaus(dedx, ParabolicFunction(invP, PionPar),     sigPi, kTRUE);
            gausProb[1] = Gaus(dedx, ParabolicFunction(invP, KaonPar),     sigKa, kTRUE);
            gausProb[2] = Gaus(dedx, ParabolicFunction(invP, ProtonPar),   sigPr, kTRUE);
            gausProb[3] = Gaus(dedx, ParabolicFunction(invP, ElectronPar), sigEl, kTRUE);

            sum = gausProb[0] + gausProb[1] + gausProb[2] + gausProb[3];
            if (sum == 0.) return 1;
            gausProb[0] /= sum;
            gausProb[1] /= sum;
            gausProb[2] /= sum;
            gausProb[3] /= sum;

            break;
    }
    Float_t *resultProb = new Float_t[Ntypes];
    BayesFunction(gausProb, bayesAprioriCoefficients, resultProb, Ntypes);

    Ppi = resultProb[0];
    Pk  = resultProb[1];
    Pp  = resultProb[2];
    Pe  = resultProb[3];

    return 0;
}

Int_t MpdParticleIdentification::GetTofProbs(Float_t P, Float_t m2, Int_t nHits, Float_t& Ppi, Float_t& Pk, Float_t& Pp, Float_t& Pe, Int_t method) {

    const Float_t m_proton   = 0.938;    //mass of proton (GeV)
    const Float_t m_pion     = 0.139;    //mass of pion (GeV)
    const Float_t m_kaon     = 0.494;    //mass of kaon (GeV)
    const Float_t m_electron = 0.000510; //mass of e (GeV)

    Float_t sigma = 1.0 / TMath::Sqrt(nHits); //for gaussian //FIXME PLEASE, they don't want me to be so strange!!!
    Float_t sum = 0.0; // for normalizing
    const Int_t Ntypes = 4; //order: pion, kaon, proton, electron

    Float_t gausProb[Ntypes] = {0.0, 0.0, 0.0, 0.0};

    switch (method) {

        case 0: //Hyperbolic (1/beta vs p)

            gausProb[0] = Gaus(HyperbolicFunction(P, m2), HyperbolicFunction(P, m_pion * m_pion),         sigma, kTRUE);
            gausProb[1] = Gaus(HyperbolicFunction(P, m2), HyperbolicFunction(P, m_kaon * m_kaon),         sigma, kTRUE);
            gausProb[2] = Gaus(HyperbolicFunction(P, m2), HyperbolicFunction(P, m_proton * m_proton),     sigma, kTRUE);
            gausProb[3] = Gaus(HyperbolicFunction(P, m2), HyperbolicFunction(P, m_electron * m_electron), sigma, kTRUE);
            sum = gausProb[0] + gausProb[1] + gausProb[2] + gausProb[3];
            if (sum == 0.) return 1;
            gausProb[0] /= sum;
            gausProb[1] /= sum;
            gausProb[2] /= sum;
            gausProb[3] /= sum;
            break;

        case 1:

            break;
    }
    //    for (Int_t i = 0; i < Ntypes; ++i) {
    //        if ((gausProb[i] > 1.0) || (gausProb[i] > 1.0)) {
    //            cout << "ERROR!!!!!! : i = " << i << " sum = " << sum << " prob = " << gausProb[i] << endl;
    //        }
    //    }

    Ppi = gausProb[0];
    Pk  = gausProb[1];
    Pp  = gausProb[2];
    Pe  = gausProb[3];

    return 0;
}

Int_t MpdParticleIdentification::GetCombinedProbs(Float_t *tofProbs, Float_t *tpcProbs, Float_t *resultProbs, Int_t N) {

    /*tofProbs    - measured probabilities from TOF*/
    /*tpcProbs    - measured probabilities from TPC*/
    /*resultProbs - combined probabilities */
    /*N           - number of types {pion, kaon, proton, electron} */

    Float_t sumBayes = 0.0;
    for (Int_t i = 0; i < N; ++i) {
        sumBayes += tofProbs[i] * tpcProbs[i];
    }

    for (Int_t i = 0; i < N; ++i) {
        if ((tpcProbs[i] != 0.0) && (tofProbs[i] != 0.0)) {
            resultProbs[i] = tofProbs[i] * tpcProbs[i] / sumBayes;
        } else if (tpcProbs[i] == 0.0) {
            resultProbs[i] = tofProbs[i];
        } else if (tofProbs[i] == 0.0) {
            resultProbs[i] = tpcProbs[i];
        }
    }
    return 0;
}

Float_t MpdParticleIdentification::BetheBlochFunction(Float_t x, Float_t *p) {
    Float_t b = 1 / (x / Sqrt(1 + x * x));
    return p[0] / Power(b, p[3]) * (p[1] - Power(b, p[3]) - Log(p[2] + Power(1 / x, p[4])));
}

Float_t MpdParticleIdentification::ParabolicFunction(Float_t x, Float_t *p) {
    return p[0] * x * x + p[1] * x + p[2];
}

Float_t MpdParticleIdentification::HyperbolicFunction(Float_t x, Float_t m2) {
    return Sqrt(1 + m2 / (x * x));
}

/*Bayes function for probabilities calculation*/
Int_t MpdParticleIdentification::BayesFunction(Float_t *measProb, Float_t *aprioriProb, Float_t *bayesProb, Int_t N) {

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

ClassImp(MpdParticleIdentification);
