
#include "TH1F.h"
#include "TH2F.h"

void EmcDataAnalysis() {

    /* Load basic libraries */
    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(kTRUE, kTRUE); // all libs

    TChain *dstTree = new TChain("cbmsim");
    dstTree->Add("$VMCWORKDIR/macro/mpd/mpddst.root");

    // Activate branches
    TClonesArray *EmcHits;
    dstTree->SetBranchAddress("MpdEmcHit", &EmcHits);

    UInt_t events = dstTree->GetEntries();
    cout << " Number of events in DST file = " << events << endl;

    MpdEmcGeoPar* fGeoPar = new MpdEmcGeoPar();

    Int_t nSec = fGeoPar->GetNsec();
    Int_t nMod = fGeoPar->GetNmod();
    Int_t nRow = fGeoPar->GetNrows();

    TFile outFile("emc_histos.root", "RECREATE");
    UInt_t nBins = 500;
    TH1F* hEne = new TH1F("hEne", "hEne", nBins, 0.0, 0.5);
    TH1F* hRow = new TH1F("hRow", "hRow", 2 * nRow, -nRow, nRow);
    TH1F* hSec = new TH1F("hSec", "hSec", 2 * nSec, 0, 2 * nSec);
    TH1F* hMod = new TH1F("hMod", "hMod", nMod, 0, nMod);
    TH2F* hScan = new TH2F("hScan", "hScan", nRow * 2, -nRow, nRow, nSec * nMod, 0, nSec * nMod);
    TH2F* hScanCyl = new TH2F("hScanCyl", "Cylindrical coordinates", nSec * nMod, 0, nSec * nMod, nRow * 2, -nRow, nRow);

    for (UInt_t i = 0; i < events; i++) {
        dstTree->GetEntry(i);

        UInt_t fNhits = EmcHits->GetEntriesFast();

        cout << " Number of EMC hits = " << fNhits << endl;
        for (UInt_t iHit = 0; iHit < fNhits; ++iHit) {
            MpdEmcHit* hit = (MpdEmcHit*) EmcHits->At(iHit);
            Int_t sec = hit->GetSec();
            Int_t row = hit->GetRow();
            Int_t mod = hit->GetMod();
            Float_t E = hit->GetE();

            Int_t globModNumber;
            if (sec > nSec) {
                row *= -1;
                globModNumber = (sec - nSec) * 4 + mod;
            } else {
                globModNumber = sec * 4 + mod;
            }

            hScan->Fill(row, globModNumber, E);
            hScanCyl->Fill(globModNumber, row, E);
            hEne->Fill(E);
            hRow->Fill(row, E);
            hSec->Fill(sec, E);
            hMod->Fill(globModNumber, E);
        } // hits loop
    } // event loop
    
    hScanCyl->Draw("LEGO2Z CYL");
    
    hScan->Write();
    hScanCyl->Write();
    hRow->Write();
    hSec->Write();
    hMod->Write();
    hEne->Write();
}
