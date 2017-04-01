// Macro for running reconstruction

#if !defined(__CINT__) || defined(__MAKECINT__)
// ROOT includes
#include "TString.h"
#include "TStopwatch.h"
#include "TSystem.h"
#include "TROOT.h"

// Fair includes
#include "FairRunAna.h"
#include "FairRuntimeDb.h"
#include "FairParRootFileIo.h"
#include "FairTask.h"
#include "FairField.h"
#include "FairTrackParP.h"
#include "FairCave.h"
#include "FairPipe.h"
#include "FairMagnet.h"

// MPD includes
#include "MpdTpcHitProducer.h"
#include "MpdKalmanFilter.h"
#include "TpcLheHitsMaker.h"
#include "MpdVertexZfinder.h"
#include "MpdTpcKalmanFilter.h"
#include "MpdKfPrimaryVertexFinder.h"
#include "MpdTofHitProducer.h"
#include "MpdEtofHitProducer.h"
#include "MpdEctTrackFinderTpc.h"
#include "MpdEctTrackFinderTof.h"
#include "MpdTofMatching.h"
#include "MpdEtofMatching.h"
#include "MpdFillDstTask.h"
#include "MpdEmc.h"
#include "MpdCpc.h"
#include "MpdZdc.h"
#include "MpdStrawendcap.h"
#include "FairUrqmdGenerator.h"
#include "FairFileSource.h"
#include "MpdTpcDigitizerAZ.h"
#include "MpdTpcClusterFinderAZ.h"
#include "MpdEctTrackFinderCpc.h"
#include "MpdStrawendcapPoint.h"
#include "MpdStrawendcapGeoPar.h"
#include "MpdLAQGSMGenerator.h"
#include "MpdCpcGeoPar.h"
#include "MpdCpcPoint.h"
#include "MpdEmcPoint.h"

//#include "google/heap-profiler.h"
//#include "google/heap-checker.h"

#include <iostream>
using namespace std;
#endif

void runRecoCpc30(TString inFile = "mc.30.root")
{
    // for adding ROOT dictionary description of classes below
    MpdStrawendcapPoint* pMpdStrawendcapPoint = new MpdStrawendcapPoint();
    MpdCpcPoint* pMpdCpcPoint = new MpdCpcPoint();
    MpdEmcPoint* pMpdEmcPoint = new MpdEmcPoint();
    FairCave* pFairCave = new FairCave();
    FairPipe* pFairPipe = new FairPipe();
    FairMagnet* pFairMagnet = new FairMagnet();
    MpdStrawendcap* pMpdStrawendcap = new MpdStrawendcap();
    MpdCpc* pCpc = new MpdCpc();
    MpdEmc* pEmc = new MpdEmc();
    MpdLAQGSMGenerator* pMpdLAQGSMGenerator = new MpdLAQGSMGenerator();
    MpdStrawendcapGeoPar* pMpdStrawendcapGeoPar = new MpdStrawendcapGeoPar();
    MpdCpcGeoPar* pMpdCpcGeoPar = new MpdCpcGeoPar();

    delete pMpdStrawendcapPoint;
    delete pMpdCpcPoint;
    delete pMpdEmcPoint;
    delete pFairCave;
    delete pFairPipe;
    delete pFairMagnet;
    delete pMpdStrawendcap;
    delete pCpc;
    delete pEmc;
    delete pMpdLAQGSMGenerator;
    delete pMpdStrawendcapGeoPar;
    delete pMpdCpcGeoPar;


  // ========================================================================
  // Verbosity level (0=quiet, 1=event level, 2=track level, 3=debug)
  Int_t iVerbose = 1;

  // Input file (MC events)
  //TString inFile = "mc.root";
  //TString inFile = "mc_10.root";
  TString mcFile = TString(gSystem->Getenv("MCFILE"));
  if (mcFile != "") inFile = mcFile;

  // Parameter file
  //TString parFile = "testparams.root";
  TString parFile = inFile;

  // Output file
  //TString outFile = "test.raw.1251-1500.root";
  TString outFile = "tpc.reco.30.root";
  TString recoFile = TString(gSystem->Getenv("RECOFILE"));
  if (recoFile != "") outFile = recoFile;

  // ------------------------------------------------------------------------

  // ---  Now choose concrete engines for the different tasks   -------------
  // ------------------------------------------------------------------------

  // In general, the following parts need not be touched
  // ========================================================================

  // -----   Timer   --------------------------------------------------------
  TStopwatch timer;
  timer.Start();
  // ------------------------------------------------------------------------

  // -----   Digitization run   -------------------------------------------
  FairRunAna *fRun= new FairRunAna();

  FairSource* fFileSource = new FairFileSource(inFile);
  fRun->SetSource(fFileSource);
  fRun->SetOutputFile(outFile);
  fRun->SetGenerateRunInfo(false);
  fRun->SetUseFairLinks(true);

  // -----  Parameter database   --------------------------------------------
  FairRuntimeDb* rtdb = fRun->GetRuntimeDb();
  FairParRootFileIo* parInput1 = new FairParRootFileIo();
  parInput1->open(parFile.Data());
  //FairParAsciiFileIo* parInput2 = new FairParAsciiFileIo();
  //TString stsDigiFile = gSystem->Getenv("VMCWORKDIR");
  //stsDigiFile += "/parameters/sts/sts_digi_new_standard.par";
  //parInput2->open(stsDigiFile.Data(),"in");
  rtdb->setFirstInput(parInput1);
  //rtdb->setSecondInput(parInput2);
  // ------------------------------------------------------------------------
  
  MpdKalmanFilter *kalman = MpdKalmanFilter::Instance("KF");
  fRun->AddTask(kalman);
  //kalman->SetNumer(0);

  MpdTpcDigitizerAZ* tpcDigitizer = new MpdTpcDigitizerAZ();
  tpcDigitizer->SetPersistence(kFALSE);
  //fRun->AddTask(tpcDigitizer);

  /*
  MpdTpcClusterFinderTask *tpcClusterFinder = new MpdTpcClusterFinderTask();
  //  tpcClusterFinder->SetDebug(kFALSE);
  //  tpcClusterFinder->SetMakeQA(kTRUE);
  //  tpcClusterFinder->SetCalcResiduals(kFALSE);
  fRun->AddTask(tpcClusterFinder);
  */

  MpdTpcClusterFinderAZ *tpcClusAZ = new MpdTpcClusterFinderAZ();
  //MpdTpcClusterFinderMlem *tpcClusAZ = new MpdTpcClusterFinderMlem();
  tpcClusAZ->SetPersistence();
  //fRun->AddTask(tpcClusAZ);

  MpdTpcHitProducer* hitPr = new MpdTpcHitProducer();
  //hitPr->SetModular(1);
  //hitPr->SetModular(0);
  fRun->AddTask(hitPr);

  FairTask* vertZ = new MpdVertexZfinder();
  fRun->AddTask(vertZ);

  MpdTpcKalmanFilter* recoKF = new MpdTpcKalmanFilter("Kalman filter");
  //recoKF->UseTpcHit(kFALSE); // do not use hits from the hit producer
  fRun->AddTask(recoKF);
  
  MpdKfPrimaryVertexFinder* findVtx = new MpdKfPrimaryVertexFinder("Vertex finder");
  //findVtx->SetConstrFlag();
  fRun->AddTask(findVtx);

  // TOF hit producers
  MpdTofHitProducer* tofHit = new MpdTofHitProducer("TOF Hit producer", true, 1, true, "QA.MpdTofHitProducer.30.root");
  fRun->AddTask(tofHit);

  MpdEtofHitProducer* etofHitProd = new MpdEtofHitProducer("ETOF HitProducer", true, 1, true, "QA.MpdEtofHitProducer.30.root");
  fRun->AddTask(etofHitProd);

  // Endcap tracking
  //*
  FairTask* tpcECT = new MpdEctTrackFinderTpc();
  fRun->AddTask(tpcECT);
  //*/

  //MpdEctTrackFinderTof* tofECT = new MpdEctTrackFinderTof();
  //*
  MpdEctTrackFinderCpc* tofECT = new MpdEctTrackFinderCpc();
  tofECT->SetTpc(kTRUE);
  fRun->AddTask(tofECT);
  //*/

  // TOF matching
  MpdTofMatching* tofMatch = new MpdTofMatching("TOF matching", 1, true, "QA.MpdTofMatching.30.root");
  fRun->AddTask(tofMatch);

  // ETOF matching
  MpdEtofMatching* etofMatch = new MpdEtofMatching("ETOF matching", 1, true, "QA.MpdEtofMatching.30.root");
  fRun->AddTask(etofMatch);
  

  FairTask* fillDST = new  MpdFillDstTask("MpdDst task");
  fRun->AddTask(fillDST);

  // Number of events to process
  Int_t nEvents = 30; //55; //50; //250; //90;
  
  // -----   Intialise and run   --------------------------------------------
  fRun->Init();
  cout << "Field: " << fRun->GetField()->GetBz(0.,0.,0.) << endl;
  fRun->Run(0, nEvents);
  // ------------------------------------------------------------------------

  // -----   Finish   -------------------------------------------------------

  timer.Stop();
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime();
  cout << endl << endl;
  cout << "Macro finished succesfully." << endl;
  cout << "Output file is "    << outFile << endl;
  cout << "Parameter file is " << parFile << endl;
  cout << "Real time " << rtime << " s, CPU time " << ctime << " s" << endl;
  cout << endl;
  // ------------------------------------------------------------------------

}

int main(int argc, char** arg)
{
   runRecoCpc30();
}
