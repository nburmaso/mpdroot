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
using namespace std;
#endif

runMC (const char *datadir="")
{

  TString the_datadir=datadir;

  if (the_datadir=="")
    the_datadir=".";

  TString outFile = the_datadir+ "/evetest.root";             
  //TString parFile = the_datadir+ "/testparams.root";
  TString parFile = outFile;
  

  TStopwatch timer;
  timer.Start();
  gDebug=0;

#define PART

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  mpdloadlibs(1,1);                 // load main libraries

//   gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_v2.C");
//   geometry_v2(0x0, kFALSE);     // load mpd detectors libraries
  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_v2_option.C");
  geometry_v2_option(0x0, kFALSE);     // load mpd detectors libraries

  FairRunSim *fRun = new FairRunSim();
  
  // set the MC version used
  // ------------------------

  //  fRun->SetName("TGeant4");
  fRun->SetName("TGeant3");
  // Choose the Geant Navigation System
  // fRun->SetGeoModel("G3Native");
  
  fRun->SetOutputFile(outFile.Data());

  
  //  geometry_v2_option (fRun, kTRUE,"TOF","tof_v2.geo",kFALSE);                      // check only tof geometry
  //  geometry_v2_option (fRun, kTRUE,"FFD","ffd.geo",kFALSE);                         // check only ffd geometry
  //  geometry_v2_option (fRun, kTRUE,"ZDC","zdc_modules84_layers60_16_4.geo",kFALSE); // check only zdc geometry

    geometry_v2(fRun, kTRUE);    // load mpd standard geometries

  // Create and Set Event Generator
  //-------------------------------

  FairPrimaryGenerator* primGen = new FairPrimaryGenerator();
  fRun->SetGenerator(primGen); 

#ifdef URQMD
  // Urqmd  Generator

  TString hostname = gSystem->HostName();
  TString dataFile;

  if ((hostname=="nc2")||(hostname=="nc3")||
      (hostname=="nc2.jinr.ru")||(hostname=="nc3.jinr.ru")) {   //  nc2, nc3

    //    dataFile = "/1/data4mpd/";                      
    dataFile = "/1/data4mpd/UrQMD/1.3/";                        
    dataFile += "auau.09gev.mbias.98k.ftn14";

  }
  else {
    if ((hostname=="lxmpd-ui.jinr.ru")||(hostname=="lxmpd-ui")) {   // linux farm
      dataFile = "/opt/exp_soft/mpd/urqmd/";
      dataFile += "auau.09gev.mbias.10k.f14";
    }
    else {
      if ((hostname=="mpd")||(hostname=="mpd.jinr.ru")||(hostname=="nc12.jinr.ru")||
	  (hostname=="nc11.jinr.ru")||(hostname=="nc13.jinr.ru")||(hostname=="nc14.jinr.ru"))
	dataFile = "/opt/data/";                        // mpd, nc11
      else{
          if (hostname == "seashore")
          	dataFile = "/data/mpd/";
          else {
		TString sHome(gSystem->Getenv("HOME"));
		dataFile = sHome + "/data/";
	}
      }

      //dataFile += "urqmd.auau.ecm9gev.mbias-10k.0001.ftn14";
      dataFile += "auau.09gev.mbias.98k.ftn14";
    }
  }


    FairUrqmdGenerator* urqmdGen = new FairUrqmdGenerator(dataFile);

  primGen->AddGenerator(urqmdGen);

#else
#ifdef PART
  // ------- Particle Generator
//  FairParticleGenerator* partGen =
//    new FairParticleGenerator(211, 10, 1, 0, 3, kTRUE);
  FairParticleGenerator* partGen =
    new FairParticleGenerator(211, 10, 0, 0, 3, 80, 60, 0);
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
#endif
#endif
#endif
#endif

  // Magnetic Field Map - for proper use in the analysis MultiField is necessary here
  // --------------------
    PndMultiField *fField= new PndMultiField();

  // Constant Field
  PndConstField *fMagField = new PndConstField();
//  fMagField->SetField(0, 0 , 5. ); // values are in kG:  1T = 10kG
  fMagField->SetField(0, 0 , 0); // values are in kG:  1T = 10kG
  // MinX=-75, MinY=-40,MinZ=-12 ,MaxX=75, MaxY=40 ,MaxZ=124 );  // values are in cm
  fMagField->SetFieldRegion(-205, 205, -205, 205, -261, 261); //cm

  fField->AddField(fMagField);  
  //  fRun->SetField(fMagField);
  fRun->SetField(fField);

  fRun->SetStoreTraj(kTRUE);

  fRun->Init();


  // -Trajectories Visualization (TGeoManager Only )
  // -----------------------------------------------

  //fRun->SetStoreTraj(kFALSE);

  ;
 // Set cuts for storing the trajectpries
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

  Int_t nEvents = 1;
  fRun->Run(nEvents);

  timer.Stop();
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime();
  printf("RealTime=%f seconds, CpuTime=%f seconds\n",rtime,ctime);

  cout << " Test passed" << endl;
  cout << " All ok " << endl;
  exit(0);

}  
