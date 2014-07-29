// Original macro for running Fair  with Geant3  or Geant4 (M. Al-Turany , D. Bertini)
// adopted for mpdroot

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

runMC (const char *output_dir="",const char *output_file="",
       const char *input_dir="" ,const char *input_file="", 
       Int_t nEvents=1, Int_t nSkip=0 )
{

  TString the_output_dir=output_dir;
  TString the_output_file=output_file;
  TString the_input_file=input_file;

  if (the_output_dir=="")
    the_output_dir=".";

  if (the_output_file=="")
    the_output_file="evetest.root";

  //  TString outFile = the_datadir+ "/evetest.root";             
  TString outFile = the_output_dir+ "/" + the_output_file;             
  //TString parFile = the_datadir+ "/testparams.root";
  TString parFile = outFile;
  

  TStopwatch timer;
  timer.Start();
  gDebug=0;

#define URQMD
  //#define LAQGSM
//#define PART
//#define ION

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  mpdloadlibs(1,1);                 // load main libraries

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_v2_minimal.C");  // new macro for mini-set

  FairRunSim *fRun = new FairRunSim();
  
  // set the MC version used
  // ------------------------

#ifdef LAQGSM
  fRun->SetName("TGeant4");
#else
  fRun->SetName("TGeant3");
#endif

  // Choose the Geant Navigation System
  // fRun->SetGeoModel("G3Native");
  
  fRun->SetOutputFile(outFile.Data());

  geometry_v2_minimal(fRun);    // load minimal set of detector geometries
                                //  (cave+pipe+magnet+tpc+tof)

  // Create and Set Event Generator
  //-------------------------------

  FairPrimaryGenerator* primGen = new FairPrimaryGenerator();
  fRun->SetGenerator(primGen);

  TString hostname = gSystem->HostName();
  TString dataFile;


#ifdef URQMD
  // Urqmd  Generator

  dataFile=input_dir;

  if (dataFile=="")
    dataFile=find_path_to_URQMD_files();
  else
    dataFile += "/";

  if (the_input_file=="") {
    if ((hostname=="lxmpd-ui.jinr.ru")||(hostname=="lxmpd-ui"))  
      dataFile += "auau.09gev.mbias.10k.f14";
    else
      dataFile += "auau.09gev.mbias.98k.ftn14";
  }
  else
    dataFile += the_input_file;


  FairUrqmdGenerator* urqmdGen = new FairUrqmdGenerator(dataFile);

  primGen->AddGenerator(urqmdGen);
  urqmdGen->SkipEvents(nSkip);

#else
#ifdef PART
  // ------- Particle Generator
  FairParticleGenerator* partGen =
    new FairParticleGenerator(211, 10, 1, 0, 3, kTRUE);
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
#ifdef LAQGSM

  dataFile=input_dir;

  if (dataFile=="") {
    dataFile=find_path_to_URQMD_files();
    dataFile += "/../../QGSM/";           //  nc2,nc3,nc8,nc9
  }
  else
    dataFile += "/";

  if (the_input_file=="") 
    dataFile += "AuAuss5m.r12";
  else
    dataFile += the_input_file;

  MpdLAQGSMGenerator* guGen= new MpdLAQGSMGenerator(dataFile.Data(),1,0);
  primGen->AddGenerator(guGen);
  guGen->SkipEvents(nSkip);

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
  //  fMagField->SetFieldRegion(-205, 205, -205, 205, -261, 261); //cm
#define magnet_v4
#ifdef  magnet_v4
  fMagField->SetFieldRegion(-230, 230, -230, 230, -375, 375); //cm   // magnet_v4 or later
#else
  fMagField->SetFieldRegion(-205, 205, -205, 205, -261, 261); //cm   // old magnet
#endif

  fField->AddField(fMagField);  
  //  fRun->SetField(fMagField);
  fRun->SetField(fField);

  //  fRun->SetStoreTraj(kTRUE);
fRun->SetStoreTraj(kFALSE);

#ifdef LAQGSM
// fRun->SetUserDecay(1); // if true gconfig/UserDecay.C macro will be called
#endif

  fRun->Init();


  // -Trajectories Visualization (TGeoManager Only )
  // -----------------------------------------------

  //fRun->SetStoreTraj(kFALSE);

  ;
  // Set cuts for storing the trajectpries
  // FairTrajFilter* trajFilter = FairTrajFilter::Instance();
  // trajFilter->SetStepSizeCut(0.01); // 1 cm
  // //   trajFilter->SetVertexCut(-2000., -2000., 4., 2000., 2000., 100.);
  // trajFilter->SetMomentumCutP(.50); // p_lab > 500 MeV
  // //  trajFilter->SetEnergyCut(.2, 3.02); // 0 < Etot < 1.04 GeV

  // trajFilter->SetStorePrimaries(kTRUE);
  // trajFilter->SetStoreSecondaries(kFALSE);

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

  //  Int_t nEvents =  1; // 1000;
  fRun->Run(nEvents);

#define SAVEPDG
#ifdef LAQGSM
#ifdef SAVEPDG
  TString Pdg_table_name=gSystem->BaseName(dataFile.Data());
  Pdg_table_name += ".g";
  Pdg_table_name += (fRun->GetName())[6];
  Pdg_table_name += ".pdg_table.dat";
  (TDatabasePDG::Instance())->WritePDGTable(Pdg_table_name.Data());
#endif
#endif

  timer.Stop();
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime();
  printf("RealTime=%f seconds, CpuTime=%f seconds\n",rtime,ctime);

  cout << " Test passed" << endl;
  cout << " All ok " << endl;
  exit(0);

}  
