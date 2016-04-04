
#include "TH1F.h"
#include "TGraph.h"
#include "TClonesArray.h"
#include "TChain.h"
#include "TH2F.h"

using namespace std;
using namespace TMath;

void EmcDataAnalysis(Int_t opt = 1) {

    // opt:
    // 1 - bin length corresponds to energy 
    // 2 - bin length and color corresponds to energy 
    // 3 - bin color corresponds to energy 

    /* Load basic libraries */
    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(kTRUE, kTRUE); // all libs

    TChain *dstTree = new TChain("cbmsim");
    dstTree->Add("$VMCWORKDIR/macro/mpd/mpddst.root");

    // Activate branches
    TClonesArray *EmcHits;
    dstTree->SetBranchAddress("MpdEmcHit", &EmcHits);
    TClonesArray *TpcHits;
    dstTree->SetBranchAddress("TpcHit", &TpcHits);
    TClonesArray *TofHits;
    dstTree->SetBranchAddress("TOFHit", &TofHits);

    UInt_t events = dstTree->GetEntries();
    cout << " Number of events in DST file = " << events << endl;

    MpdEmcGeoPar* fGeoPar = new MpdEmcGeoPar();
    Float_t rMinEmc = fGeoPar->GetRmin() * 0.1;
    Float_t rMaxEmc = fGeoPar->GetRmax() * 0.1;
    Float_t deltaR = rMaxEmc - rMinEmc;

    Float_t rMinTpc = 0.0;
    Float_t rMaxTpc = 140.0;
    Float_t rMinTof = 0.0; //150.0;
    Float_t rMaxTof = 0.0; //160.2;


    const Int_t n2Dbins = 360;
    Float_t eneArr[n2Dbins]; // array of energies in each angle bin of EMC

    TGraph* tpcGr = new TGraph();
    TGraph* tofGr = new TGraph();

    events = 1;

    for (UInt_t iEv = 0; iEv < events; iEv++) {
        dstTree->GetEntry(iEv);

        UInt_t fNhits = EmcHits->GetEntriesFast();

        cout << " Number of EMC hits = " << fNhits << endl;

        for (UInt_t iHit = 0; iHit < fNhits; ++iHit) {
            MpdEmcHit* hit = (MpdEmcHit*) EmcHits->At(iHit);
            Int_t iCur = GetIdxByPhi(hit->GetPhiCenter() * DegToRad(), n2Dbins);
            eneArr[iCur] += hit->GetE();
        } // hits loop

        for (UInt_t iHitTpc = 0; iHitTpc < TpcHits->GetEntriesFast(); ++iHitTpc) {
            MpdTpcHit* hitTpc = (MpdTpcHit*) TpcHits->At(iHitTpc);
            tpcGr->SetPoint(iHitTpc, hitTpc->GetX(), hitTpc->GetY());
        } // hits loop

        for (UInt_t iHitTof = 0; iHitTof < TofHits->GetEntriesFast(); ++iHitTof) {
            MpdTofHit* hitTof = (MpdTofHit*) TofHits->At(iHitTof);
            tofGr->SetPoint(iHitTof, hitTof->GetX(), hitTof->GetY());
        } // hits loop

    } // event loop

    Float_t maxE = 0.0;
    for (Int_t i = 0; i < n2Dbins; ++i) { //search for maximum bin
        Float_t E = eneArr[i];
        if (E > maxE) maxE = E;
    }

    for (Int_t i = 0; i < n2Dbins; ++i) { //normilizing
        eneArr[i] = eneArr[i] / maxE;
    }

    TLine * line[n2Dbins];
    Float_t dPhi = TwoPi() / n2Dbins;
    Int_t lineWidth = 10;

    const Int_t Number = 4;
    const Int_t nb = 100;
    Int_t myPalette[nb];
    Double_t R[Number] = {0.00, 0.00, 1.00, 1.00};
    Double_t G[Number] = {0.00, 1.00, 0.65, 0.00};
    Double_t B[Number] = {1.00, 0.00, 0.00, 0.00};
    Double_t Length[Number] = {0.0, 0.33, 0.66, 1.0};

    Int_t FI = TColor::CreateGradientColorTable(Number, Length, R, G, B, nb);
    for (Int_t i = 0; i < nb; ++i) {
        myPalette[i] = FI + i;
    }

    if (opt == 1) {
        for (Int_t i = 0; i < n2Dbins; ++i) {
            Float_t phi = i * dPhi;
            Float_t x1 = rMinEmc * Cos(phi);
            Float_t y1 = rMinEmc * Sin(phi);
            Float_t rNew = rMinEmc + eneArr[i] * deltaR;
            Float_t x2 = rNew * Cos(phi);
            Float_t y2 = rNew * Sin(phi);
            line[i] = new TLine(x1, y1, x2, y2);
            line[i]->SetLineWidth(lineWidth);
            line[i]->SetLineColor(38);
        }
    } else if (opt == 2) {
        for (Int_t i = 0; i < n2Dbins; ++i) {
            Float_t phi = i * dPhi;
            Float_t x1 = rMinEmc * Cos(phi);
            Float_t y1 = rMinEmc * Sin(phi);
            Float_t rNew = rMinEmc + eneArr[i] * deltaR;
            Float_t x2 = rNew * Cos(phi);
            Float_t y2 = rNew * Sin(phi);
            Int_t colorId = Int_t(eneArr[i] * (nb - 1));
            line[i] = new TLine(x1, y1, x2, y2);
            line[i]->SetLineWidth(lineWidth);
            line[i]->SetLineColor(myPalette[colorId]);
        }
    } else if (opt == 3) {
        for (Int_t i = 0; i < n2Dbins; ++i) {
            Float_t phi = i * dPhi;
            Float_t x1 = rMinEmc * Cos(phi);
            Float_t y1 = rMinEmc * Sin(phi);
            Float_t x2 = rMaxEmc * Cos(phi);
            Float_t y2 = rMaxEmc * Sin(phi);
            Int_t colorId = Int_t(eneArr[i] * (nb - 1));
            line[i] = new TLine(x1, y1, x2, y2);
            line[i]->SetLineWidth(lineWidth);
            if (eneArr[i] < 0.05 * maxE)
                line[i]->SetLineColor(38);
            else
                line[i]->SetLineColor(myPalette[colorId]);
        }
    }


    Double_t bound = 230.0;
    TCanvas* c1 = new TCanvas("c", "c", 1000, 1000);
    tpcGr->SetMarkerStyle(20);
    tpcGr->SetMarkerColor(32);
    tpcGr->SetMarkerSize(0.4);
    tpcGr->GetXaxis()->SetLimits(-bound, bound);
    tpcGr->SetMaximum(bound);
    tpcGr->SetMinimum(-bound);
    tpcGr->GetXaxis()->SetTitle("X, cm");
    tpcGr->GetXaxis()->CenterTitle();
    tpcGr->GetYaxis()->SetTitle("Y, cm");
    tpcGr->GetYaxis()->CenterTitle();
    tpcGr->SetTitle("XY projection of hits in TPC, TOF and EMC");
    tpcGr->Draw("PA");

    tofGr->SetMarkerStyle(20);
    tofGr->SetMarkerColor(46);
    tofGr->SetMarkerSize(0.4);
    tofGr->Draw("SAME P");


    for (Int_t i = 0; i < n2Dbins - 1; ++i) {
        if (eneArr[i] < 0.05 * maxE) continue;
        line[i]->Draw("same");
    }
    
    DrawCircle(rMinEmc);
    DrawCircle(rMaxEmc);
    DrawCircle(rMinTpc);
    DrawCircle(rMaxTpc);
    DrawCircle(rMinTof);
    DrawCircle(rMaxTof);

}

Int_t GetIdxByPhi(Float_t phi, Int_t N) {
    //phi in radian
    return Int_t(phi / TwoPi() * N);
}

void DrawCircle(Float_t r) {
    TArc* circ = new TArc(0.0, 0.0, r);
    circ->SetFillStyle(0);
    circ->Draw("same");
}