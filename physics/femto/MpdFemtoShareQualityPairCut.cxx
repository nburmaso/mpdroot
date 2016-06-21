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
fMinNoHits(0){
    fChain = ch;
    fChain->SetBranchAddress("TpcKalmanTrack", &fTracks);
}

void MpdFemtoShareQualityPairCut::CheckTwoTrackEffects() {

}

void MpdFemtoShareQualityPairCut::CheckTwoTrackEffects(Int_t id1, Int_t id2) {
    
    Int_t nSharedHits = 0;
    Int_t totHitNum = 0;
    
    for (Int_t iTpcTrack1 = 0; iTpcTrack1 < fTracks->GetEntriesFast(); iTpcTrack1++) {
        MpdTpcKalmanTrack* tr1 = (MpdTpcKalmanTrack*) fTracks->At(iTpcTrack1);
        if (tr1->GetTrackID() == id1 && tr1->GetNofHits() > fMinNoHits) {
            for (Int_t iTpcTrack2 = 0; iTpcTrack2 < fTracks->GetEntriesFast(); iTpcTrack2++) {
                MpdTpcKalmanTrack* tr2 = (MpdTpcKalmanTrack*) fTracks->At(iTpcTrack2);
                if (tr2->GetTrackID() == id2 && tr2->GetNofHits() > fMinNoHits) {
                    totHitNum = tr1->GetNofHits() + tr2->GetNofHits();
                    fHits_1st = tr1->GetTrHits();
                    fHits_2nd = tr2->GetTrHits();
                    for (Int_t iHit1 = 0; iHit1 < tr1->GetNofHits(); iHit1++) {
                        Int_t idx1 = ((MpdKalmanHit*) (fHits_1st->UncheckedAt(iHit1)))->GetIndex(0);
                        Int_t lay1 = ((MpdKalmanHit*) (fHits_1st->UncheckedAt(iHit1)))->GetLayer();
                        for (Int_t iHit2 = 0; iHit2 < tr2->GetNofHits(); iHit2++) {
                            Int_t idx2 = ((MpdKalmanHit*) (fHits_2nd->UncheckedAt(iHit2)))->GetIndex(0);
                            Int_t lay2 = ((MpdKalmanHit*) (fHits_2nd->UncheckedAt(iHit2)))->GetLayer();
                            if (idx1 == idx2 && lay1 == lay2) 
                                nSharedHits++;
                        }
                    }
                }
            }
        }
    }
   if (nSharedHits != 0) 
       cout << nSharedHits << " " << totHitNum << " " << 1. * nSharedHits / totHitNum << endl;
   
}


MpdFemtoShareQualityPairCut::~MpdFemtoShareQualityPairCut() {

  //  delete fDstTree;


}