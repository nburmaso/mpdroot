
/*
 * mpddst.C
 *
 *  Created on: 23 lut 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#if !defined(__CINT__) && !defined(__CLING__)
#include "TString.h"
#include "TStopwatch.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"

#include "FairRunAna.h"
#include "FairRuntimeDb.h"
#include "FairParRootFileIo.h"
#include "FairTrajFilter.h"
#include "FairUrqmdGenerator.h"
#include "FairPrimaryGenerator.h"
#include "FairCave.h"
#include "FairPipe.h"
#include "FairMagnet.h"

#include "MpdSts.h"
#include "TpcDetector.h"
#include "MpdEtof.h"
#include "MpdFsa.h"
#include "MpdBbc.h"
#include "MpdCpc.h"
#include "MpdTof.h"
#include "MpdStrawendcap.h"
#include "MpdZdc.h"
#include "MpdFfd.h"
#include "MpdGetNumEvents.h"
#include "MpdDstCompressTask.h"
#include "FairLogger.h"
#include <iostream>
using namespace std;
#endif

#include "mpdloadlibs.C"
/**
 * macro for compressing root files creates by reco.C macro into file that contains only event header
 * and MpdEvent, other branches are optional. Such file can be later processed by FairRunAna chain.
 * @param inFIle input file
 * @param outFile output file
 * @param start_event start event
 * @param end_event end event
 */
void mpddst(TString inFIle="mpddst.root",TString outFile="mudst.root",Int_t start_event =0, Int_t end_event =-1){
    TStopwatch timer;
    timer.Start();
    gDebug = 0;

#if ROOT_VERSION_CODE < ROOT_VERSION(5,99,99)
    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
#endif
    mpdloadlibs(1, 1); // load main libraries

#if ROOT_VERSION_CODE < ROOT_VERSION(5,99,99)
    //gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_v2.C");
    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_stage1.C");
#endif

    FairRunAna *fRun = new FairRunAna();
    fRun->SetOutputFile(outFile);
    fRun->SetSource(new FairFileSource(inFIle));

    FairLogger::GetLogger()->SetLogVerbosityLevel("HIGH");
    FairLogger::GetLogger()->SetColoredLog(kTRUE);
    MpdDstCompressTask *write = new MpdDstCompressTask();
    write->RegisterTpcKalmans();
    write->RegisterMC();
    fRun->AddTask(write);
    fRun->Init();
    fRun->Run(start_event,end_event);

    timer.Stop();
    Double_t rtime = timer.RealTime(), ctime = timer.CpuTime();
    printf("RealTime=%f seconds, CpuTime=%f seconds\n", rtime, ctime);
    cout << "Macro finished successfully." << endl;     // marker of successful execution for CDASH
}


