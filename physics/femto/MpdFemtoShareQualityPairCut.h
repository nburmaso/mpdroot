#ifndef MPDFEMTOSHAREQUALITYPAIRCUT_H
#define MPDFEMTOSHAREQUALITYPAIRCUT_H 1

#include <TMath.h>
#include <TNamed.h>
#include <TFile.h>
#include <TChain.h>
#include <TClonesArray.h>
#include <MpdTpcKalmanTrack.h>
#include <MpdTpcHit.h>
#include <MpdKalmanHit.h>

using namespace std;
using namespace TMath;

class MpdFemtoShareQualityPairCut : public TNamed {
public:

    MpdFemtoShareQualityPairCut();
    ;
    MpdFemtoShareQualityPairCut(TFile*, Float_t, Float_t);
    MpdFemtoShareQualityPairCut(TChain*);

    virtual ~MpdFemtoShareQualityPairCut();

    // Getters

    Float_t GetShareQualityMax() {
        return fShareQualityMax;
    }

    Float_t GetShareFractionMax() {
        return fShareFractionMax;
    }

    UInt_t GetNPairsPassed() {
        return fNPairsPassed;
    }

    UInt_t GetNPairsFailed() {
        return fNPairsFailed;
    }
    
    Bool_t GetZeroSharing() {
        return fZeroSharing;
    }

    // Setters
    void SetShareQualityMax(Float_t val) {
        fShareQualityMax = val;
    }

    void SetShareFractionMax(Float_t val) {
        fShareFractionMax = val;
    }

    void SetMinNoHits(Int_t val) {
        fMinNoHits = val;
    }
    
    void SetZeroSharing(Bool_t val) {
        fZeroSharing = val;
    }

    void CheckTwoTrackEffects();
    Float_t Quality(Int_t, Int_t);
    Float_t Sharing(Int_t, Int_t);

private:

    inline Int_t GetHitIndex(TClonesArray* fHits, Int_t hit) {
        return ((MpdKalmanHit*) (fHits->UncheckedAt(hit)))->GetIndex(0);
    };

    inline Int_t GetHitLayer(TClonesArray* fHits, Int_t hit) {
        return ((MpdKalmanHit*) (fHits->UncheckedAt(hit)))->GetLayer();
    };

    TFile* fFile;
    TChain* fChain;

    TClonesArray* fTracks;
    TClonesArray* fHits_1st;
    TClonesArray* fHits_2nd;

    Int_t fMinNoHits;

    UInt_t fNPairsPassed; // Number of pairs consideered that passed the cut 
    UInt_t fNPairsFailed; // Number of pairs consideered that failed the cut

    Float_t fShareQualityMax; // Maximum allowed pair quality
    Float_t fShareFractionMax; // Maximum allowed share fraction
    
    Bool_t fZeroSharing;


    ClassDef(MpdFemtoShareQualityPairCut, 1)
};

#endif