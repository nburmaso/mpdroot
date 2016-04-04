/* Macro draws histograms of radiation length scan of the MPD TPC
 Output result values are "effective radiation lengths" which are calculated as radiation length (cm) divided by distance (cm).
 14/07/2012 A. Basalaev*/

#include <TVector3.h>
#include <TChain.h>
#include <TClonesArray.h>
#include <TH2F.h>
#include "TROOT.h"
#include "TMath.h"

using namespace TMath;

void RadLenAna(TString infile1 = "RadLenSim.root") {

    TH2F *hXY = new TH2F("hXY", "Material budget in the MPD", 3000, -320., 320., 3000, -320., 320.);
    TH2F *hZR = new TH2F("hZR", "Material budget in the MPD", 3000, -320., 320., 1500, 0., 320.);
    TH2F *hEtaDist = new TH2F("hEtaDist", "Material budget in the MPD", 3000, 0., 5., 1500, 0., 550.);

    /* Load basic libraries */
    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(kTRUE, kTRUE); // all libs


    TChain *tree = new TChain("cbmsim");
    tree->Add(infile1);

    // Activate branches
    TClonesArray *fRadLenPs;
    tree->SetBranchAddress("RadLen", &fRadLenPs);

    Int_t events = tree->GetEntries();
    std::cout << " Processing RadLenSimZR..." << std::endl;
    std::cout << " Number of events in file = " << events << std::endl;

    TVector3 PosIn, PosOut, DistVec;

    for (Int_t iEv = 0; iEv < events; iEv++) {
        tree->GetEntry(iEv);

        Int_t fNpoints = fRadLenPs->GetEntriesFast();

        for (Int_t pointIndex = 0; pointIndex < fNpoints; pointIndex++) {
            FairRadLenPoint* RadLenP = (FairRadLenPoint*) fRadLenPs->At(pointIndex);
            Float_t Len = RadLenP->GetRadLength();
            PosIn = RadLenP->GetPosition();
            PosOut = RadLenP->GetPositionOut();
            DistVec = PosIn - PosOut;
            Float_t Dist = DistVec.Mag();
            Float_t LenEff = Dist / Len;

            Float_t X = PosIn.X();
            Float_t Y = PosIn.Y();
            Float_t Z = PosIn.Z();
            Float_t r = Sqrt(X * X + Y * Y);
            Float_t Theta = PosIn.Theta();
            Float_t Eta = -Log(Tan(Theta / 2));

            hXY->Fill(X, Y, LenEff);
            hZR->Fill(Z, r, LenEff);
            hEtaDist->Fill(Eta, PosIn.Mag(), LenEff);

            if (pointIndex % 10000 == 0) cout << pointIndex * 100.0 / fNpoints << "% processed" << endl;
        }
        cout << iEv + 1 << " out of " << events << " events processed" << endl;
    }

        
    hXY->GetXaxis()->SetTitle("X, cm");
    hXY->GetXaxis()->CenterTitle();
    hXY->GetYaxis()->SetTitle("Y, cm");
    hXY->GetYaxis()->CenterTitle();
    hXY->GetZaxis()->SetTitle("Radiation length, X/X0");
//    hXY->SetMaximum(10.);

    hZR->GetXaxis()->SetTitle("Z, cm");
    hZR->GetXaxis()->CenterTitle();
    hZR->GetYaxis()->SetTitle("R, cm");
    hZR->GetYaxis()->CenterTitle();
    hZR->GetZaxis()->SetTitle("Radiation length, X/X0");
//    hZR->SetMaximum(10.);

    hEtaDist->GetXaxis()->SetTitle("#eta");
    hEtaDist->GetXaxis()->CenterTitle();
    hEtaDist->GetYaxis()->SetTitle("Distance from IP, cm");
    hEtaDist->GetYaxis()->CenterTitle();
    hEtaDist->GetZaxis()->SetTitle("Radiation length, X/X0");
//    hEtaDist->SetMaximum(10.);

    gStyle->SetOptStat(0);
    TCanvas *c1 = new TCanvas("c1", "c1", 1000, 1000);
    hXY->Draw("colz");
    TCanvas *c2 = new TCanvas("c2", "c2", 1000, 500);
    hZR->Draw("colz");
    TCanvas *c3 = new TCanvas("c3", "c3", 1000, 500);
    hEtaDist->Draw("colz");
}
