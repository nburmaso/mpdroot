#include <iostream>
#include <TChain.h>
#include <TParticle.h>
#include <TParticlePDG.h>
#include <TDatabasePDG.h>
#include <fstream>

using namespace std;

void femto_test() {

    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(1, 1); // load main libraries

    MpdFemtoHistos* histos = new MpdFemtoHistos();

    for (Int_t iFile = 0; iFile < 10; iFile++) {
        
        // /nica/user/b/basalaev/vHLLE_mpdroot - is a path on NICA-cluster to the test data sample (vHLLE+UrQMD, 11.5 GeV/nn, 1PT, MPD, geometry_stage1) 
        MpdFemto* femto = new MpdFemto(Form("/nica/user/b/basalaev/vHLLE_mpdroot/mpddst_%d.root", iFile), histos);

        femto->SetSourceSize(3.); // Source size in fm;
        femto->SetPdgCode(211); // pi+ are considered only;
        femto->SetEtaCuts(-1.0, 1.0); // Define a range over pseudorapidity: val1 < eta < val2;
        femto->SetPtCuts(0.15, 1.5); // Define a range over Pt: val1 < Pt < val2;
        femto->SetNumMixedEvents(10); // Number of events to be mixed
        femto->SetKtCuts(0.15, 1.5); // Define a range over Kt: val1 < Kt < val2;
        femto->SetQinv(0.20);

        femto->MakeCFs_1D();

        delete femto;
    }

    // A procedure to normalize filled histograms (nom, denom, CF, Cf_base) over all data samples processed
    histos->MakeNorm_1D(); 

    // Histos Draw & Save 
    MpdFemtoHistos* histosDraw[4] = {
        histos->GetNominator(),
        histos->GetDenominator(),
        histos->GetCF(),
        histos->GetCFBase()
    };

    TCanvas* c = new TCanvas("1", "1", 600, 600);
    c->Divide(2, 2);

    for (Int_t iPad = 1; iPad < 5; iPad++) {
        c->cd(iPad);
        histosDraw[iPad - 1]->Draw();
    }
    //
    c->SaveAs("testCF.png");
}
