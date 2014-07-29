// Macro for running reconstruction

#if !defined(__CINT__) || defined(__MAKECINT__)
// ROOT includes
#include "TString.h"
#include "TStopwatch.h"
#include "TSystem.h"

// Fair includes
#include "FairRunAna.h"
#include "FairRuntimeDb.h"
#include "FairParRootFileIo.h"
#include "FairTask.h"
#include "FairField.h"
#include "FairTrackParP.h"

// MPD includes
#include "MpdTpcHitProducer.h"
#include "TpcHitFinderTask.h"
#include "MpdKalmanFilter.h"
#include "MpdTpcHitProducer.h"
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
#include "MpdFsa.h"
#include "MpdStrawendcap.h"
#include "FairUrqmdGenerator.h"
#include "FairCave.h"
#include "FairMagnet.h"

//#include "google/heap-profiler.h"
//#include "google/heap-checker.h"

#include <iostream>
using namespace std;
#endif

// inFile - input file with MC data, default: evetest.root
// nStartEvent - number (start with zero) of first event to process, default: 0
// nEvents - number of events to process, default: 1
// outFile - output file with reconstructed data, default: mpddst.root
// run_type - proof execution, default "local". e.g.:
//      "proof" - run on proof-lite with "CPU" count workers,
//      "proof:workers=3" - run on proof-lite with 3 workers
//      "proof:pod://proof.server:21001" - to run on the PROOF cluster created with PoD
void reco(TString inFile = "$VMCWORKDIR/macro/mpd/evetest.root", Int_t nStartEvent = 0, Int_t nEvents = 1, TString outFile = "mpddst.root", TString run_type="local")
{

  // ========================================================================
  // Verbosity level (0=quiet, 1=event level, 2=track level, 3=debug)
  Int_t iVerbose = 1;

  // Parameter file
  TString parFile = inFile;

  // -----   Timer   --------------------------------------------------------
  TStopwatch timer;
  timer.Start();
  // ------------------------------------------------------------------------

  // -----   Digitization run   -------------------------------------------
  int ind = run_type.Index(':');
  TString proof_name = "";
  if (ind >= 0){
      proof_name = run_type(ind+1,run_type.Length()-ind-1);
      run_type = run_type(0, ind);
  }

  FairRunAna *fRun= new FairRunAna(run_type, proof_name);
  fRun->SetInputFile(inFile);
  //fRun->AddFriend(inFile);
  fRun->SetOutputFile(outFile);
  fRun->SetProofParName("$VMCWORKDIR/gconfig/libMpdRoot.par");
  // ------------------------------------------------------------------------

  // -----  Parameter database   --------------------------------------------
  FairRuntimeDb* rtdb = fRun->GetRuntimeDb();
  FairParRootFileIo* parInput1 = new FairParRootFileIo();
  parInput1->open(parFile.Data());
  rtdb->setFirstInput(parInput1);

  // fRun->LoadGeometry();  // EL

  // ------------------------------------------------------------------------

  MpdEmc* pEmc = new MpdEmc();
  MpdCpc* pCpc = new MpdCpc();
  MpdZdc* pZdc = new MpdZdc();
  MpdFsa* pFsa = new MpdFsa();
  MpdStrawendcap* pStrawEndcap = new MpdStrawendcap();
  FairCave* pFairCave = new FairCave();
  FairMagnet* pFairMagnet = new FairMagnet();
  FairUrqmdGenerator* pFairUrqmdGenerator = new FairUrqmdGenerator();

  MpdKalmanFilter *kalman = MpdKalmanFilter::Instance("KF");
  fRun->AddTask(kalman);

  //FairTask* trackMS = new TpcLheHitsMaker("Hit producer");
  //fRun->AddTask(trackMS);

//  TpcDistributor* tpcDistributor = new TpcDistributor(10000, kTRUE, kTRUE);
//  fRun->AddTask(tpcDistributor);

//  MpdTpcClusterFinderTask *tpcClusterFinder = new MpdTpcClusterFinderTask();
//  fRun->AddTask( tpcClusterFinder );

  MpdTpcHitProducer* hitPr = new MpdTpcHitProducer();
  hitPr->SetModular(0);
  fRun->AddTask(hitPr);

  FairTask* vertZ = new MpdVertexZfinder();
  fRun->AddTask(vertZ);

  FairTask* recoKF = new MpdTpcKalmanFilter("Kalman filter");
  fRun->AddTask(recoKF);

  FairTask* findVtx = new MpdKfPrimaryVertexFinder("Vertex finder");
  fRun->AddTask(findVtx);

  // TOF hit producers
  MpdTofHitProducer* tofHit = new MpdTofHitProducer("Hit producer");
  fRun->AddTask(tofHit);

  MpdEtofHitProducer* etofHitProd = new MpdEtofHitProducer("ETOF HitProducer");
  etofHitProd->SetParamFlnm("etof.geo.par.xml");
  fRun->AddTask(etofHitProd);

  // Endcap tracking
  FairTask* tpcECT = new MpdEctTrackFinderTpc();
  fRun->AddTask(tpcECT);

  MpdEctTrackFinderTof* tofECT = new MpdEctTrackFinderTof();
  tofECT->SetTpc(kTRUE);
  fRun->AddTask(tofECT);

  // TOF matching
  MpdTofMatching* tofMatch = new MpdTofMatching("TOF matching");
  fRun->AddTask(tofMatch);

  // ETOF matching
  MpdEtofMatching* etofMatch = new MpdEtofMatching("ETOF matching");
  fRun->AddTask(etofMatch);

  MpdFillDstTask* fillDST = new MpdFillDstTask("MpdDst task");
  fRun->AddTask(fillDST);

  // -----   Intialise   ----------------------------------------------------
  fRun->Init();
  if (run_type != "proof") cout << "Field: " << fRun->GetField()->GetBz(0.,0.,0.) << endl;

  // -----   Run   ______________--------------------------------------------
  fRun->Run(nStartEvent, nStartEvent+nEvents);
  // ------------------------------------------------------------------------

  // -----   Finish   -------------------------------------------------------

  delete fRun;

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

  //HeapLeakChecker::NoGlobalLeaks();
  //HeapLeakChecker::CancelGlobalCheck();
}

int main(int argc, char** arg)
{
   reco();
}
