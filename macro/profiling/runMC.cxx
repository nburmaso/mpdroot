// Macro for running Fair  with Geant3  or Geant4 (M. Al-Turany , D. Bertini)
// Modified 22/06/2005 D.Bertini

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
#include "FairParticleGenerator.h"
#include "FairCave.h"
#include "FairPipe.h"
#include "FairMagnet.h"
#include "PndMultiFieldPar.h"

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
#include "MpdEmc.h"

#include <iostream>
using namespace std;
#endif

TString find_path_to_URQMD_files ()
{
  TString hostname = gSystem->HostName();
  TString path_to_URQMD_files;

  if ((hostname=="nc2")||(hostname=="nc3")||
      (hostname=="nc2.jinr.ru")||(hostname=="nc3.jinr.ru") ||
      (hostname=="nc8.jinr.ru")||(hostname=="nc9.jinr.ru")) {   //  nc2, nc3
    path_to_URQMD_files="/nica/mpd1/data4mpd/UrQMD/1.3/";
  }
  else {
    if ((hostname=="lxmpd-ui.jinr.ru")||(hostname=="lxmpd-ui"))    // linux farm
      path_to_URQMD_files = "/opt/exp_soft/mpd/urqmd/";
    else {
      if ( (hostname=="mpd")||(hostname=="mpd.jinr.ru")
           ||(hostname=="nc11.jinr.ru")||(hostname=="nc12.jinr.ru")||(hostname=="nc13.jinr.ru")||(hostname=="se63-36.jinr.ru")
       ||(hostname=="se63-37.jinr.ru")||(hostname=="se63-40.jinr.ru")||(hostname=="se51-99.jinr.ru") )
    path_to_URQMD_files = "/opt/data/";                        // mpd, nc11
      else{
    if (hostname == "seashore")
          path_to_URQMD_files = "/data/mpd/";
    else {
      if ((hostname=="kanske")||(hostname=="kanske.itep.ru"))     // Moscow
        path_to_URQMD_files ="/scratch2/kmikhail/data4mpd/UrQMD/1.3/";
      else
            path_to_URQMD_files = gSystem->Getenv("HOME") + TString("/");
    }
      }
    }
  }
  return  path_to_URQMD_files;
}

// inFile - input file with generator data, default: auau.09gev.mbias.98k.ftn14
// nStartEvent - for compatibility, any number
// nEvents - number of events to transport, default: 1
// outFile - output file with MC data, default: evetest.root
// flag_store_FairRadLenPoint
void runMC (TString inFile = "auau.09gev.mbias.98k.ftn14", Int_t nStartEvent = 0, Int_t nEvents = 2, TString outFile = "evetest.root", Bool_t flag_store_FairRadLenPoint=kTRUE)
{
  TString parFile = outFile;

  TStopwatch timer;
  timer.Start();
  gDebug=0;

#define URQMD

  FairRunSim *fRun = new FairRunSim();

  // set the MC version used
  // ------------------------

  //  fRun->SetName("TGeant4");
  fRun->SetName("TGeant3");
  // Choose the Geant Navigation System
  // fRun->SetGeoModel("G3Native");

  fRun->SetOutputFile(outFile.Data());

  // Set Material file Name
  fRun->SetMaterials("media.geo");

  // Create and add detectors
  //-------------------------

  FairModule *Cave= new FairCave("CAVE");
  Cave->SetGeometryFileName("cave.geo");
  fRun->AddModule(Cave);

  FairModule *Pipe= new FairPipe("PIPE");
  Pipe->SetGeometryFileName("pipe.geo");
  fRun->AddModule(Pipe);

  FairModule *Magnet= new FairMagnet("MAGNET");
  Magnet->SetGeometryFileName("magnet_v2.geo");
  fRun->AddModule(Magnet);

  //   Silicon Tracker Stations
  FairDetector *Sts= new MpdSts("STS", kTRUE);
  Sts->SetGeometryFileName("sts_v3.geo");
  fRun->AddModule(Sts);

//     FairDetector *Ffd = new MpdFfd("FFD",kTRUE );
//     Ffd->SetGeometryFileName("ffd.geo");
//     fRun->AddModule(Ffd);

  FairDetector *Tpc = new TpcDetector("TPC", kTRUE);
  //    Tpc->SetGeometryFileName("tpc_v2_el.geo");
  Tpc->SetGeometryFileName("tpc_v2.geo");
  fRun->AddModule(Tpc);

  FairDetector *Tof= new MpdTof("TOF", kTRUE );
  Tof->SetGeometryFileName("tof_v2.geo");
  fRun->AddModule(Tof);

  FairDetector *eTof= new MpdEtof("ETOF", kTRUE );
  eTof->SetGeometryFileName("etof_v3.geo");
  fRun->AddModule(eTof);

  FairDetector *Emc= new MpdEmc("ECAL", kTRUE);
  Emc->SetGeometryFileName("emc_v1.geo");
  fRun->AddModule(Emc);

  FairDetector *straw_ecStt= new MpdStrawendcap("ESTT", kTRUE);
  //    straw_ecStt->SetGeometryFileName("straw_60_layers.geo");
  //    FairDetector *straw_ecStt= new CbmStt ("ESTT", kTRUE);
  straw_ecStt->SetGeometryFileName("ect.geo");
  fRun->AddModule(straw_ecStt);

//     FairDetector *Bbc = new MpdBbc("BBC",kTRUE );
//     Bbc->SetGeometryFileName("bbc.geo");
//     fRun->AddModule(Bbc);

  FairDetector *Cpc = new MpdCpc("CPC",kTRUE );
  Cpc->SetGeometryFileName("cpc.geo");
  fRun->AddModule(Cpc);

  FairDetector *Zdc = new MpdZdc("ZDC",kTRUE );
  Zdc->SetGeometryFileName("zdc_modules84_layers60_16_4.geo");
  fRun->AddModule(Zdc);

  FairDetector *Fsa = new MpdFsa("FSA",kTRUE );
  Fsa->SetGeometryFileName("fsa.geo");
  fRun->AddModule(Fsa);

  // Create and Set Event Generator
  //-------------------------------

  FairPrimaryGenerator* primGen = new FairPrimaryGenerator();
  fRun->SetGenerator(primGen);

#ifdef URQMD
  // Urqmd  Generator

  TString hostname = gSystem->HostName();
  TString dataFile;

  // if ((hostname=="nc2")||(hostname=="nc3")||
  //     (hostname=="nc2.jinr.ru")||(hostname=="nc3.jinr.ru")) {   //  nc2, nc3

  //   //    dataFile = "/1/data4mpd/";
  //   dataFile = "/1/data4mpd/UrQMD/1.3/";
  //   dataFile += "auau.09gev.mbias.98k.ftn14";

  // }
  // else {
  //   if ((hostname=="lxmpd-ui.jinr.ru")||(hostname=="lxmpd-ui")) {   // linux farm
  //     dataFile = "/opt/exp_soft/mpd/urqmd/";
  //     dataFile += "auau.09gev.mbias.10k.f14";
  //   }
  //   else {
  //     if ((hostname=="mpd")||(hostname=="mpd.jinr.ru")||(hostname=="nc12.jinr.ru")||
  //         (hostname=="nc11.jinr.ru")||(hostname=="nc13.jinr.ru")||(hostname=="nc14.jinr.ru")
  //         ||(hostname=="se63-36.jinr.ru")||(hostname=="se63-37.jinr.ru"))
  // 	dataFile = "/opt/data/";                        // mpd, nc11
  //     else{
  //         if (hostname == "seashore")
  //         	dataFile = "/data/mpd/";
  //         else {
  // 		TString sHome(gSystem->Getenv("HOME"));
  // 		dataFile = sHome + "/data/";
  // 	}
  //     }

  //     //dataFile += "urqmd.auau.ecm9gev.mbias-10k.0001.ftn14";
  //     dataFile += "auau.09gev.mbias.98k.ftn14";
  //   }
  // }

  dataFile=find_path_to_URQMD_files();
  if ((hostname=="lxmpd-ui.jinr.ru")||(hostname=="lxmpd-ui"))
    dataFile += "auau.09gev.mbias.10k.f14";
  else
    dataFile += inFile;

  FairUrqmdGenerator* urqmdGen = new FairUrqmdGenerator(dataFile);

  primGen->AddGenerator(urqmdGen);

#else
#ifdef PART
  // ------- Particle Generator
  FairParticleGenerator* partGen =
    new FairParticleGenerator(211, 100, 1, 0, 3, kTRUE);
  //FairParticleGenerator* partGen =
  //  new FairParticleGenerator(211, 10, 1, 0, 3, 1, 0, 0);
  primGen->AddGenerator(partGen);

#else
#ifdef ION
  // -------  Ion Generator
  FairIonGenerator *fIongen= new FairIonGenerator(79, 197,79,1, 0.,0., 25, 0.,0.,-1.);
  primGen->AddGenerator(fIongen);

#else
#ifdef BOX

  // Box Generator
  FairBoxGenerator* boxGen = new
    FairBoxGenerator(13, 1); // 13 = muon; 1 = multipl.
  boxGen->SetPRange(0.25,2.5); // GeV/c //setPRange vs setPtRange
  boxGen->SetPhiRange(0, 360); // Azimuth angle range [degree]
  boxGen->SetThetaRange(0, 180); // Polar angle in lab system range [degree]
  boxGen->SetXYZ(0., 0., 0.); // mm o cm ??
  primGen->AddGenerator(boxGen);

#else
#ifdef HSD

  // HSD Generator
  MpdHSDGenerator *hsdGen = new MpdHSDGenerator();
  hsdGen->Input("/1/data4mpd/HSD/auau.09gev.mbias.5k.01.input"); // optional
  hsdGen->Baryons("/1/data4mpd/HSD/auau.09gev.mbias.5k.01.fort.300");
  hsdGen-> Mesons("/1/data4mpd/HSD/auau.09gev.mbias.5k.01.fort.301");
  primGen->AddGenerator(hsdGen);

#endif
#endif
#endif
#endif
#endif

  // Magnetic Field Map - for proper use in the analysis MultiField is necessary here
  // --------------------
  PndMultiField *fField= new PndMultiField();

  // Constant Field
  PndConstField *fMagField = new PndConstField();
  fMagField->SetField(0, 0 , 5. ); // values are in kG:  1T = 10kG
  // MinX=-75, MinY=-40,MinZ=-12 ,MaxX=75, MaxY=40 ,MaxZ=124 );  // values are in cm
  fMagField->SetFieldRegion(-205, 205, -205, 205, -261, 261); //cm

  fField->AddField(fMagField);
  //  fRun->SetField(fMagField);
  fRun->SetField(fField);

  fRun->SetStoreTraj(kTRUE);
  fRun->SetRadLenRegister(flag_store_FairRadLenPoint); // radiation length manager

  fRun->Init();


  // -Trajectories Visualization (TGeoManager Only )
  // -----------------------------------------------

  //fRun->SetStoreTraj(kFALSE);

  ;
 // Set cuts for storing the trajectories
  FairTrajFilter* trajFilter = FairTrajFilter::Instance();
  trajFilter->SetStepSizeCut(0.01); // 1 cm
  //   trajFilter->SetVertexCut(-2000., -2000., 4., 2000., 2000., 100.);
  trajFilter->SetMomentumCutP(.50); // p_lab > 500 MeV
  //  trajFilter->SetEnergyCut(.2, 3.02); // 0 < Etot < 1.04 GeV

  trajFilter->SetStorePrimaries(kTRUE);
  trajFilter->SetStoreSecondaries(kFALSE);

  //   trajFilter->SetStoreSecondaries(kTRUE);


  // Fill the Parameter containers for this run
  //-------------------------------------------

  FairRuntimeDb *rtdb=fRun->GetRuntimeDb();
  Bool_t kParameterMerged=kTRUE;
  FairParRootFileIo* output=new FairParRootFileIo(kParameterMerged);
  //AZ output->open(parFile.Data());
  output->open(gFile);
  rtdb->setOutput(output);

  PndMultiFieldPar* Par = (PndMultiFieldPar*) rtdb->getContainer("PndMultiFieldPar");
  if (fField)
    Par->SetParameters(fField);
  Par->setInputVersion(fRun->GetRunId(),1);
  Par->setChanged();

  rtdb->saveOutput();
  rtdb->print();


  // Transport nEvents
  // -----------------

  fRun->Run(nEvents);

  timer.Stop();
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime();
  printf("RealTime=%f seconds, CpuTime=%f seconds\n",rtime,ctime);

  cout << " Test passed" << endl;
  cout << " All ok " << endl;
  exit(0);
}

int main()
{
    runMC();
}
