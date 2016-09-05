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

MpdParticleIdentification::MpdParticleIdentification() {
}

MpdParticleIdentification::~MpdParticleIdentification() {
}

Int_t MpdParticleIdentification::GetTofProbs(Float_t P, Float_t beta, Float_t& Ppi, Float_t& Pk, Float_t& Pp, Float_t& Pe, Int_t method) {

    const Float_t m2_proton = 0.938 * 0.938; //square of proton mass (GeV)
    const Float_t m2_pion = 0.139 * 0.139; //square of pion mass (GeV)
    const Float_t m2_kaon = 0.494 * 0.494; //square of kaon mass (GeV)
    const Float_t m2_electron = 0.000511 * 0.000511; //square of e mass (GeV)

    if (Abs(beta) < 1e-6) return 1;
    const Float_t oneOverBeta = 1.0 / beta;

    Float_t sigma = 0.07 * oneOverBeta; //for gaussian 
    //    Float_t sigma = 0.05; //for gaussian 
    Float_t sum = 0.0; // for normalizing
    const Int_t Ntypes = 4; //order: pion, kaon, proton, electron

    Float_t gausProb[Ntypes] = {0.0, 0.0, 0.0, 0.0};

    switch (method) {

        case 0: //Hyperbolic (1/beta vs p)

            gausProb[0] = Gaus(oneOverBeta, HyperbolicFunction(P, m2_pion), sigma, kTRUE);
            gausProb[1] = Gaus(oneOverBeta, HyperbolicFunction(P, m2_kaon), sigma, kTRUE);
            gausProb[2] = Gaus(oneOverBeta, HyperbolicFunction(P, m2_proton), sigma, kTRUE);
            gausProb[3] = Gaus(oneOverBeta, HyperbolicFunction(P, m2_electron), sigma, kTRUE);
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

    Ppi = gausProb[0];
    Pk = gausProb[1];
    Pp = gausProb[2];
    Pe = gausProb[3];

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
