#include "TString.h"
#include "TStopwatch.h"
#include "TROOT.h"
#include "TSystem.h"

#include "FairRunSim.h"
#include "FairRuntimeDb.h"
#include "FairParRootFileIo.h"
#include "FairTrajFilter.h"
#include "PndConstField.h"
#include "FairParticleGenerator.h"
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

#include <PndMultiField.h>
#include <PndMultiFieldPar.h>

#include <iostream>

#include "parameters.hpp"
void geometry_v1 (FairRunSim *fRun, Bool_t build);

void runMCTrack (const MCParameters& param)
{

  TString the_datadir=".";

  TString outFile = the_datadir+ "/" + param._name + ".root";        
  //TString parFile = the_datadir+ "/testparams.root";
  TString parFile = outFile;
  

  gDebug=0;

  FairRunSim *fRun = new FairRunSim();
  
  // set the MC version used
  // ------------------------

  //  fRun->SetName("TGeant4");
  fRun->SetName("TGeant3");
  // Choose the Geant Navigation System
  // fRun->SetGeoModel("G3Native");
  
  fRun->SetOutputFile(outFile.Data());

  std::cout << "Let's load geometry: " << std::endl;
    geometry_v1(fRun, kTRUE);    // load mpd standard geometries
  std::cout << "Geometry loaded " << std::endl;

  // Create and Set Event Generator
  //-------------------------------

  FairPrimaryGenerator* primGen = new FairPrimaryGenerator();
  fRun->SetGenerator(primGen); 

  const Double_t R = 100; //cm
  TVector3 startPoint(0, 0, 0);
//   startPoint.SetTheta(theta);
//   startPoint.SetPhi(param->_phi);
   startPoint += param._shift;
//  std::cout << "Start point for phi = " << phi << " theta = " << theta << " is";
  startPoint.Print();
  TVector3 p(1, 0, 0);
  p.SetTheta(param._theta);
  p.SetPhi(param._phi);
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

  fRun->Run(param._nEvents);


  cout << " All ok " << endl;

}  
