
#include "TH1F.h"
#include "TClonesArray.h"
#include "TChain.h"
#include "TH2F.h"

using namespace std;
using namespace TMath;

void EmcDataAnalysis() {

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

    Int_t nSec = fGeoPar->GetNsec();
    Int_t nMod = fGeoPar->GetNmod();
    Int_t nSupMod = fGeoPar->GetNsupMod();
    Int_t nRow = fGeoPar->GetNrows();
    
    Float_t rMin = fGeoPar->GetRmin() * 0.1;
    Float_t rMax = fGeoPar->GetRmax() * 0.1;
    Float_t l = rMax - rMin;
    
//    TFile outFile("emc_histos.root", "RECREATE");
    
    UInt_t nBins = 500;
    TH1F* hEne = new TH1F("hEne", "hEne", nBins, 0.0, 0.5);
    TH1F* hRow = new TH1F("hRow", "hRow", nRow, 0, nRow);
    TH1F* hSec = new TH1F("hSec", "hSec", 2 * nSec, 0, 2 * nSec);
    TH1F* hMod = new TH1F("hMod", "hMod", nMod, 0, nMod);
    TH1F* hSupMod = new TH1F("hSupMod", "hSupMod", nSupMod, 0, nSupMod);
    TH2F* hScan = new TH2F("hScan", "hScan", 100, -300, 300, 100, 0, 360);
    TH2F* hScanCyl = new TH2F("hScanCyl", "Cylindrical coordinates", 180, 0, 360, 300, -300, 300);
    
    Int_t n2Dbins = 300;
    
    TH2F* hXY_tpc = new TH2F("hXY_tpc", "hXY_tpc", 600, -(rMax + 10), rMax + 10, 600, -(rMax + 10), rMax + 10);
    TH2F* hXY_tof = new TH2F("hXY_tof", "hXY_tof", 600, -(rMax + 10), rMax + 10, 600, -(rMax + 10), rMax + 10);
    TH2F* hXY_emc = new TH2F("hXY_emc", "hXY_emc", n2Dbins, -(rMax + 10), rMax + 10, n2Dbins, -(rMax + 10), rMax + 10);
    
    

    events = 1;
    for (UInt_t iEv = 0; iEv < events; iEv++) {
        dstTree->GetEntry(iEv);

        UInt_t fNhits = EmcHits->GetEntriesFast();

        cout << " Number of EMC hits = " << fNhits << endl;
        Float_t maxE = 0.0;
        
        for (UInt_t iHit = 0; iHit < fNhits; ++iHit) {
            MpdEmcHit* hit = (MpdEmcHit*) EmcHits->At(iHit);
            Float_t E = hit->GetE();
            if (E > maxE) maxE = E;
        } // hits loop
        
        for (UInt_t iHit = 0; iHit < fNhits; ++iHit) {
            MpdEmcHit* hit = (MpdEmcHit*) EmcHits->At(iHit);
            Int_t sec = hit->GetSec();
            Int_t row = hit->GetRow();
            Int_t mod = hit->GetMod();
            Int_t supMod = hit->GetSupMod();
            Float_t E = hit->GetE();
            Float_t phi = hit->GetPhiCenter();
            Float_t z = hit->GetZcenter();
            
            Float_t rMaxHit = rMin + E / maxE * l;

            hScan->Fill(z, phi, E);
            hScanCyl->Fill(phi, z, E);
            hEne->Fill(E);
            hRow->Fill(row, E);
            hSec->Fill(sec, E);
            hMod->Fill(mod, E);
            hSupMod->Fill(supMod, E);
            
            Float_t emcXmin = rMin * Cos(phi * DegToRad());
            Float_t emcYmin = rMin * Sin(phi * DegToRad());
            Float_t emcXmax = rMaxHit * Cos(phi * DegToRad());
            Float_t emcYmax = rMaxHit * Sin(phi * DegToRad());
            
            Int_t n = 10;
            Float_t deltaX = (emcXmax - emcXmin) / n;
            Float_t deltaY = (emcYmax - emcYmin) / n;
            
            
            for (Int_t i = 0; i < 10; ++i) {
                hXY_emc->Fill(emcXmin + deltaX * i, emcYmin + deltaY * i);
            }
        } // hits loop
        
        for (UInt_t iHitTpc = 0; iHitTpc < TpcHits->GetEntriesFast(); ++iHitTpc) {
            MpdTpcHit* hitTpc = (MpdTpcHit*) TpcHits->At(iHitTpc);
            Int_t x = hitTpc->GetX();
            Int_t y = hitTpc->GetY();
            
            hXY_tpc->Fill(x, y, maxE * 0.01);
        } // hits loop
        
        for (UInt_t iHitTof = 0; iHitTof < TofHits->GetEntriesFast(); ++iHitTof) {
            MpdTofHit* hitTof = (MpdTofHit*) TofHits->At(iHitTof);
            Int_t x = hitTof->GetX();
            Int_t y = hitTof->GetY();
            
            hXY_tof->Fill(x, y, maxE * 0.005);
        } // hits loop
    } // event loop
    
    TCanvas* c1 = new TCanvas("c", "c", 1000, 1000);
    hXY_tpc->Draw("BOX");
    hXY_tof->Draw("SAME BOX");
    hXY_emc->Draw("SAME BOX");

    
//    hScanCyl->Draw("SAME LEGO2Z POL");
    
    TCanvas* c2 = new TCanvas("c1", "c1", 1000, 1000);
    hScan->Draw("CONT");
//    hScan->Write();
//    hScanCyl->Write();
//    hRow->Write();
//    hSec->Write();
//    hSupMod->Write();
//    hMod->Write();
//    hEne->Write();
}
