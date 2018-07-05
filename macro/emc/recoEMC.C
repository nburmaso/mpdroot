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

// MPD includes
#include "MpdGetNumEvents.h"
#include "MpdEmcHitCreation.h"

#include <iostream>
using namespace std;
#endif


void recoEMC(TString inFile = "$VMCWORKDIR/macro/mpd/evetest.root", TString outFile = "mpddst.root", Int_t nStartEvent = 0, Int_t nEvents = 100000, TString run_type = 
"local") {

    // ========================================================================
    // Verbosity level (0=quiet, 1=event level, 2=track level, 3=debug)

    Int_t iVerbose = 0;

    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(kTRUE);
    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_stage1.C");
    geometry_stage1(0x0, kFALSE);

    TStopwatch timer;
    timer.Start();

    FairRunAna* fRun;
    fRun = new FairRunAna();

    FairSource* fFileSource = new FairFileSource(inFile);
    fRun->SetSource(fFileSource);
    fRun->SetOutputFile(outFile);
    fRun->SetGenerateRunInfo(false);
    fRun->SetUseFairLinks(false);

    // ------------------------------------------------------------------------

    // Parameter file
    TString parFile = inFile;

    // -----  Parameter database   --------------------------------------------

    FairRuntimeDb* rtdb = fRun->GetRuntimeDb();

    FairParRootFileIo* parIo1 = new FairParRootFileIo();
    parIo1->open(parFile.Data());
    rtdb->setFirstInput(parIo1);
    rtdb->setOutput(parIo1);
    rtdb->saveOutput();

    FairTask *emcHP = new MpdEmcHitCreation();
    fRun->AddTask(emcHP);

    fRun->Init();

    cout<<"Field: "<<fRun->GetField()->GetBz(0., 0., 0.)<<endl;

    // if nEvents is equal 0 then all events of the given file starting with "nStartEvent" should be processed
    if (nEvents == 0)
        nEvents = MpdGetNumEvents::GetNumROOTEvents(inFile.Data()) - nStartEvent;

    // -----   Run   ______________--------------------------------------------
    fRun->Run(nStartEvent, nStartEvent + nEvents);
    // ------------------------------------------------------------------------

    // -----   Finish   -------------------------------------------------------
    timer.Stop();
    Double_t rtime = timer.RealTime();
    Double_t ctime = timer.CpuTime();
    cout << endl << endl;
    cout << "Macro finished successfully." << endl;      // marker of successful execution for CDASH
    cout << "Output file is " << outFile << endl;
    cout << "Parameter file is " << parFile << endl;
    cout << "Real time " << rtime << " s, CPU time " << ctime << " s" << endl;
    cout << endl;
    // ------------------------------------------------------------------------
gSystem->Exit(0);

}
