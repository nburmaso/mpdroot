// Macro for running Fair  with Geant3  or Geant4 (M. Al-Turany , D. Bertini)
// Modified 22/06/2005 D.Bertini
// Modified 14/07/2012 A. Basalaev

#if !defined(__CINT__) || defined(__MAKECINT__)
#include "TString.h"
#include "TStopwatch.h"
#include "TROOT.h"
#include "TSystem.h"

#include "FairRunSim.h"
#include "FairRuntimeDb.h"
#include "FairParRootFileIo.h"
#include "FairTrajFilter.h"
#include "PndConstField.h"
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

#include <iostream>
#include <fstream>
using namespace std;
#endif


void RadLenSimEtaDist (Int_t nStartEvent = 0, Int_t nEvents = 1, TString outFile = "RadLenSimEtaDist.root", Bool_t flag_store_FairRadLenPoint=kTRUE)
{

  TStopwatch timer;
  timer.Start();
  gDebug=0;

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  mpdloadlibs(1,1);                 // load main libraries

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_v2_option.C");
  geometry_v2_option(0x0, kFALSE);     // load mpd detectors libraries

  FairRunSim *fRun = new FairRunSim();
  
  // set the MC version used
  // ------------------------

  //  fRun->SetName("TGeant4");
  fRun->SetName("TGeant3");
  
  fRun->SetOutputFile(outFile.Data()); //output file

  geometry_v2(fRun, kTRUE);    // load mpd standard geometries

  // Create and Set Event Generator
  //-------------------------------

  FairPrimaryGenerator* primGen = new FairPrimaryGenerator();
  fRun->SetGenerator(primGen); 

  
  // Box Generator
  
    FairBoxGenerator* boxGen = new  
    FairBoxGenerator(0, 20000); // 0 = geantino - required for RadLen Simulation
   //Do not change for RadLen Simulation 
  boxGen->SetPRange(.2,.2); // GeV/c
  boxGen->SetPhiRange(0, 0); 
  boxGen->SetEtaRange(0., 3.); 
  boxGen->SetXYZ(0.,0.,0.); 
  primGen->AddGenerator(boxGen); 


  fRun->SetStoreTraj(kFALSE);
  fRun->SetRadLenRegister(flag_store_FairRadLenPoint); // radiation length manager 

  fRun->Init(); 


  // Fill the Parameter containers for this run
  //-------------------------------------------

  FairRuntimeDb *rtdb=fRun->GetRuntimeDb();
  Bool_t kParameterMerged=kTRUE;

  rtdb->print();

  
  // Transport nEvents
  // -----------------

  fRun->Run(nEvents); 
  
 
  timer.Stop();
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime();
  printf("RealTime=%f seconds, CpuTime=%f seconds\n",rtime,ctime);

  cout << " Simulation passed" << endl;
  cout << " All ok " << endl;
  exit(0);
}  
