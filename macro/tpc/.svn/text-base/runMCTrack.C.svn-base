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

runMCTrack (const char *datadir="")
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

    geometry_v2(fRun, kTRUE);    // load mpd standard geometries

  // Create and Set Event Generator
  //-------------------------------

  FairPrimaryGenerator* primGen = new FairPrimaryGenerator();
  fRun->SetGenerator(primGen); 

  TVector3 shift(0, 0, 10);
  const Double_t R = 100; //cm
  const Double_t phi = TMath::Pi() / 4;
  const Double_t theta = TMath::Pi() /2; // + TMath::Pi() /4 ; // - TMath::Pi()/10;
  TVector3 startPoint(0, 0, 0);
//   startPoint.SetTheta(theta);
//   startPoint.SetPhi(phi);
   startPoint += shift;
  std::cout << "Start point for phi = " << phi << " theta = " << theta << " is";
  startPoint.Print();
  TVector3 p(1, 0, 0);
  p.SetTheta(theta);
  p.SetPhi(phi);
  p *= -1;
  std::cout << " momentum is ";
  p.Print();

  // ------- Particle Generator
  FairParticleGenerator* partGen =
    new FairParticleGenerator(13, 1, p.X(), p.Y(), p.Z(), startPoint.X(), startPoint.Y(), startPoint.Z());
  primGen->AddGenerator(partGen);


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

  Int_t nEvents = 10;
  fRun->Run(nEvents);

  timer.Stop();
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime();
  printf("RealTime=%f seconds, CpuTime=%f seconds\n",rtime,ctime);

  cout << " Test passed" << endl;
  cout << " All ok " << endl;
  exit(0);

}  
