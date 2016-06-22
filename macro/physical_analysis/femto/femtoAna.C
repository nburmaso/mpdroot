#include <iostream>
#include <TChain.h>
#include <TParticle.h>
#include <TParticlePDG.h>
#include <TDatabasePDG.h>
#include <fstream>
#include <TH1.h>

using namespace std;

void femtoAna(TString inFile = "/nica/user/b/basalaev/vHLLE_mpdroot/mpddst_", TString outFile = "outputFemto.root", Int_t nStartEvent = 3, Int_t nEvents = 2) {
    gStyle->SetOptFit(1111111);

    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(1, 1); // load main libraries

    Float_t Qinv = 0.20;
    MpdFemtoHistos* histos = new MpdFemtoHistos(Qinv, outFile);

    for (Int_t iFile = 0; iFile < 1; iFile++) {
        // /nica/user/b/basalaev/vHLLE_mpdroot - is a path on NICA-cluster to the test data sample (vHLLE+UrQMD, 11.5 GeV/nn, 1PT, MPD, geometry_stage1) 
        MpdFemto* femto = new MpdFemto(inFile + TString::Format("%d.root", iFile), histos);
       
        femto->SetStartEvent(nStartEvent);
        femto->SetEvNumToRead(nEvents);
        femto->SetSourceSize(3.); // Source size in fm;
        femto->SetPdgCode(211); // pi+ are considered;
        femto->SetEtaCuts(-1.0, 1.0); // Define a range over pseudorapidity: val1 < eta < val2;
        femto->SetPtCuts(0.15, 1.5); // Define a range over Pt: val1 < Pt < val2;
        femto->SetNumMixedEvents(10); // Number of events to be mixed
        femto->SetKtCuts(0.15, 1.5); // Define a range over Kt: val1 < Kt < val2;
        femto->SetQinv(Qinv);
        femto->SetMagField(0.5); // Define magnitude of the mag.field
        femto->SetRadTpc(1.0); // Define average TPC rad. where split. && merg. effects are studied
       // femto->SetQualityPairCut(kTRUE);
        // femto->SetMinNoHits(13);

        // A method to perform 3D-analysis
	//femto->MakeCFs_1D();

        // A method to perform 1D-analysis
	// femto->MakeCFs_3D(); 

        // A method to perform DEtaDPhi-analysis
        femto->DeltaEtaDeltaPhi();

        delete femto;
    }
   
    // Return parameters of the 1D-fit. [0] corresponds to R0, [1] -- to \lambda 
    /*
    Double_t* fitRes1D = histos->GetFitParams1D();
    cout << "R0 = " << fitRes1D[0] << " Lambda = " << fitRes1D[1] << endl;
        
    // Return parameters of the 3D-fit. [0] corresponds to Rout, [1] -- to Rside, [2] -- to Rlong
    Double_t* fitRes3D = histos->GetFitParams3D();
    cout << "Rout = " << fitRes3D[0] << " Rside = " << fitRes3D[1] << " Rlong = " << fitRes3D[2] << endl;  
    */
    
    delete histos;
}
