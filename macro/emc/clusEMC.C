// Macro for running reconstruction

#if !defined(__CINT__) || defined(__MAKECINT__)
// ROOT includes
#include "TString.h"
#include "TStopwatch.h"
#include "TSystem.h"
#include "TChain.h"

// Fair includes
#include "FairRunAna.h"
#include "FairRuntimeDb.h"
#include "FairParRootFileIo.h"
#include "FairTask.h"
#include "FairField.h"
#include "FairTrackParP.h"

#include "MpdEmcClusterCreation.h"

#include <iostream>
using namespace std;
#endif


void clusEMC(TString inFile = "$VMCWORKDIR/macro/mpd/mpddst.root", TString outFile = "emc_cl.root", Int_t nStartEvent = 0, 
	Int_t nEvents = 100000) {

    // ========================================================================
    // Verbosity level (0=quiet, 1=event level, 2=track level, 3=debug)

    Int_t iVerbose = 0;

    // ----  Load libraries   -------------------------------------------------
    gROOT->LoadMacro("mpdloadlibs.C");
    mpdloadlibs(kTRUE); // load full set of main libraries

    gROOT->LoadMacro("geometry_stage1.C");
    geometry_stage1(0x0, kFALSE);

    TStopwatch timer;
    timer.Start();

    FairRunAna* fRun;
    fRun = new FairRunAna();

    FairSource* fFileSource = new FairFileSource(inFile);
    fRun->SetSource(fFileSource);
    fRun->SetOutputFile(outFile);
    fRun->SetGenerateRunInfo(false);

    MpdEmcClusterCreation *EmcCluster = new MpdEmcClusterCreation();

// Algortim number : 1 - join hits to cluster by radius; 2 - by frame
    EmcCluster->SetAlgorithmNumber(2); // algorith number (1 or 2)
    EmcCluster->SetClusterFrame(4, 3);
    EmcCluster->SetEnergyThreshold(1.5); // MeV
//    EmcCluster->SetMaxClusterRadius(25.); // cm
    
    fRun->AddTask(EmcCluster);
    fRun->Init();
    fRun->Run(nStartEvent, nStartEvent + nEvents);

    timer.Stop();
    Double_t rtime = timer.RealTime();
    Double_t ctime = timer.CpuTime();
    cout << endl << endl;
    cout << "Macro finished successfully." << endl;      // marker of successful execution for CDASH
    cout << "Output file is " << outFile << endl;
    cout << "Real time " << rtime << " s, CPU time " << ctime << " s" << endl;
    cout << endl;
    // ------------------------------------------------------------------------

  gSystem->Exit(0);

}
