#include "MpdFemtoShareQualityPairCut.h"

MpdFemtoShareQualityPairCut::MpdFemtoShareQualityPairCut(TFile* f, Float_t ShareQualityMax, Float_t ShareFractionMax) {
    fShareQualityMax = ShareQualityMax;
    fShareFractionMax = ShareFractionMax;
    fFile = f;
}

MpdFemtoShareQualityPairCut::MpdFemtoShareQualityPairCut(TChain* ch) :
fTracks(NULL),
fHits_1st(NULL),
fHits_2nd(NULL),
fMinNoHits(0),
fZeroSharing(kFALSE) {
    fChain = ch;
    fChain->SetBranchAddress("TpcKalmanTrack", &fTracks);
}

void MpdFemtoShareQualityPairCut::CheckTwoTrackEffects() {

}

Float_t MpdFemtoShareQualityPairCut::Quality(Int_t id1, Int_t id2) {
    MpdTpcKalmanTrack* tr1 = (MpdTpcKalmanTrack*) fTracks->At(id1);
    MpdTpcKalmanTrack* tr2 = (MpdTpcKalmanTrack*) fTracks->At(id2);

    Int_t nHitsInPair = tr1->GetNofHits() + tr2->GetNofHits();
    Int_t denom = nHitsInPair;
    Int_t nomin = 0;

    fHits_1st = tr1->GetTrHits();
    fHits_2nd = tr2->GetTrHits();

    const Int_t size = 55;
    vector <Bool_t> layers1(size);
    vector <Bool_t> layers2(size);

    for (Int_t iHit1 = 0; iHit1 < tr1->GetNofHits(); iHit1++)
        layers1[GetHitLayer(fHits_1st, iHit1)] = kTRUE;

    for (Int_t iHit2 = 0; iHit2 < tr2->GetNofHits(); iHit2++)
        layers2[GetHitLayer(fHits_2nd, iHit2)] = kTRUE;

    for (Int_t iSize = 0; iSize < size; iSize++) {
        Int_t q = 0;
        if (layers1.at(iSize) && layers2.at(iSize))
            q = -1;
        else if (layers1.at(iSize) || layers2.at(iSize))
            q = 1;
        else
            q = 0;

        nomin += q;
    }
    return (1. * nomin / denom);
}

Float_t MpdFemtoShareQualityPairCut::Sharing(Int_t id1, Int_t id2) {
    // The function is called for a pair of particles
    Int_t nSharedHits = 0;
    Int_t totHitNum = 0;
    Int_t totHitNum1;
    Int_t totHitNum2;

    // Looking for a correspondance between reco(global)- and kalman(tpc)-tracks
    MpdTpcKalmanTrack* tr1 = (MpdTpcKalmanTrack*) fTracks->At(id1);
    if (tr1->GetNofHits() > fMinNoHits) {
        MpdTpcKalmanTrack* tr2 = (MpdTpcKalmanTrack*) fTracks->At(id2);
        if (tr2->GetNofHits() > fMinNoHits) {
            totHitNum = tr1->GetNofHits() + tr2->GetNofHits();
            totHitNum1 = tr1->GetNofHits();
            totHitNum2 = tr2->GetNofHits();
            fHits_1st = tr1->GetTrHits();
            fHits_2nd = tr2->GetTrHits();
            for (Int_t iHit1 = 0; iHit1 < tr1->GetNofHits(); iHit1++) {
                Int_t idx1 = GetHitIndex(fHits_1st, iHit1);
                for (Int_t iHit2 = 0; iHit2 < tr2->GetNofHits(); iHit2++) {
                    Int_t idx2 = GetHitIndex(fHits_2nd, iHit2);
                    if (idx1 == idx2)
                        nSharedHits++;
                }
            }
        }
    }
    return (1. * nSharedHits / totHitNum);
}

MpdFemtoShareQualityPairCut::~MpdFemtoShareQualityPairCut() {

    //  delete fDstTree;
}