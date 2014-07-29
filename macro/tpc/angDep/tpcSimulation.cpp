// ROOT includes
#include <TROOT.h>
#include "TString.h"
#include "TStopwatch.h"
#include "TSystem.h"

#include "FairRunAna.h"
#include "FairRuntimeDb.h"
#include "FairParRootFileIo.h"
#include "FairTask.h"
#include "FairField.h"
#include "FairRunSim.h"

// MPD includes
#include "TpcClusterizerTask.h"
#include "TpcDriftTask.h"
#include "TpcHitFinderTask.h"
#include "MpdKalmanFilter.h"
#include "TpcLheHitsMaker.h"
#include "MpdTofHitProducer.h"
#include "MpdTofMatching.h"
#include "MpdFillDstTask.h"

#include <TpcDistributor.h>

#include <iostream>

#include "parameters.hpp"
void geometry_v1(FairRunSim *fRun, Bool_t build);

void tpcSimulation(const MCParameters& param)
{

  TString inFile = TString(param._name) + ".root";
  TString outFile = TString(param._name) + "Histogram.root";
  
  // ========================================================================
  // Verbosity level (0=quiet, 1=event level, 2=track level, 3=debug)
  Int_t iVerbose = 1;

  // Parameter file
  //TString parFile = "testparams.root";
  TString parFile = inFile;

  geometry_v1(0x0, kFALSE);
 
  // -----   Timer   --------------------------------------------------------
//  TStopwatch timer;
//  timer.Start();

  // -----   Digitization run   -------------------------------------------
  FairRunAna *fRun= new FairRunAna();
  fRun->SetInputFile(inFile);
  fRun->SetOutputFile(outFile);

  // -----  Parameter database   --------------------------------------------
  FairRuntimeDb* rtdb = fRun->GetRuntimeDb();
  FairParRootFileIo* parInput1 = new FairParRootFileIo();
  parInput1->open(parFile.Data());
  rtdb->setFirstInput(parInput1);

  fRun->LoadGeometry();


  // ------------------------------------------------------------------------  

  TpcDistributor* tpcChain = new TpcDistributor();
  fRun->AddTask(tpcChain);
    
  
  // -----   Intialise and run   --------------------------------------------
  fRun->Init();
  std::cout << "Field: " << fRun->GetField()->GetBz(0.,0.,0.) << std::endl;
  fRun->Run(0, param._nEvents);
  // ------------------------------------------------------------------------

  // -----   Finish   -------------------------------------------------------

  delete fRun;

//  timer.Stop();
//  Double_t rtime = timer.RealTime();
//  Double_t ctime = timer.CpuTime();
  std::cout << "Macro finished succesfully." << std::endl;
  std::cout << "Output file is "    << outFile << std::endl;
  std::cout << "Parameter file is " << parFile << std::endl;
//  std::cout << "Real time " << rtime << " s, CPU time " << ctime << " s" << std::endl;
  std::cout << std::endl;
  // ------------------------------------------------------------------------

 return;
}
