#include <iostream>
#include <TChain.h>
#include <TParticle.h>
#include <TParticlePDG.h>
#include <TDatabasePDG.h>
#include <fstream>
#include <TH1.h>

using namespace std;

void femtoAna(TString inFileDST = "/nfs/test_femto/mpddst_", //tutaj wrzucam path do initial root
        TString inFileEve = "/nica/user/b/basalaev/mpdroot_vHLLE/macro/mpd/simReco/evetest_",
        TString outFile = "_test.root", Int_t nStartEvent = 0, Int_t nEvents = 1000) {
    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(1, 1); // load main libraries

    Float_t Qinv = 0.30;
    Int_t nKtBins = 6; //Define number of kT-bins to be used
    const Int_t dim = nKtBins + 1;
    Float_t KtRanges[dim] = {0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.5};

    MpdFemtoHistos* histos = new MpdFemtoHistos(Qinv, nKtBins, 3,outFile);
    histos->SetfKtBins(nKtBins);
    for (Int_t idx = 0; idx < dim; idx++)
        histos->SetfKtRange(idx, KtRanges[idx]);
   
    for (Int_t iFile = 0; iFile < 3; iFile++) {
        // /nica/user/b/basalaev/vHLLE_mpdroot - is a path on NICA-cluster to the test data sample (vHLLE+UrQMD, 11.5 GeV/n, 1PT, MPD, geometry_stage1)
        MpdFemto* femto = new MpdFemto(inFileDST + TString::Format("%d.root", iFile), histos);
        // MpdFemto* femto = new MpdFemto(inFileDST + TString::Format("%d.root", iFile), inFileEve + TString::Format("%d.root", iFile), histos);

        femto->SetStartEvent(nStartEvent);
        femto->SetEvNumToRead(nEvents);
        femto->SetSourceSize(3.); // Source size in fm;
        femto->SetPdgCode(211); // pi+ are considered;
        femto->SetEtaCuts(-1., 1.); // Define a range over pseudorapidity: val1 < eta < val2;
        // femto->SetPtCuts(0.15, 1.5); // Define a range over Pt: val1 < Pt < val2;
        femto->SetNumMixedEvents(10); // Number of events to be mixed
        femto->SetQinv(Qinv);
        femto->SetMagField(0.5); // Define magnitude of the mag.field
        femto->SetRadTpc(1.0); // Define average TPC rad. where split. && merg. effects are studied
        femto->SetMinNoHits(40);
        femto->SetZeroSharing(kTRUE); // If true, pairs with the number of shared hits equal to 0, are put in the histo _hSharing.
        femto->SetSplittingCut(kTRUE);
        femto->SetSharingCut(kTRUE);
        femto->SetQualityMax(-0.4);
        femto->SetSharingMax(0.);

        // A method to perform 1D-analysis
        //	femto->MakeCFs_1D();

        // A method to perform 3D-analysis
        femto->MakeCFs_3D();

        // A method to perform SH-analysis
        femto->MakeCFs_SH();

        // A method to perform DEtaDPhi-analysis
        // femto->DeltaEtaDeltaPhi();

        // A method to obtain Quality & Sharing cuts
        // femto->QualityAndSharing();

        // femto->EffSplitting();
        delete femto;
    }

    // Return parameters of the 1D-fit. [0] corresponds to R0, [1] -- to \lambda
    // Double_t* fitRes1D = histos->GetFitParams1D();
    // cout << "R0 = " << fitRes1D[0] << " Lambda = " << fitRes1D[1] << endl;

    // Return parameters of the 3D-fit. [0] corresponds to Rout, [1] -- to Rside, [2] -- to Rlong
    histos->GetFitParams3D();
   
    delete histos;
}
