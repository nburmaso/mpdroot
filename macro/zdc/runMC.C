
// Macro for running Fair  with Geant3  or Geant4 (M. Al-Turany , D. Bertini)
// Modified 22/06/2005 D.Bertini
// Modified  ...           roleg      (adopted for mpdroot)
// Modified 27-Mar-2008    litvin     (field definition fixed to try to store the field parameters)
// Modified 26-Dec-2008    litvin     (adopted to current mpdroot changes)
// Modified 30-Jan-2009    litvin     (new hostname mpd.jinr.ru)
// Modified 17-Nov-2011    litvin     (use macro find_path_to_URQMD_files)

runMC (const char *datadir="")
{

  Int_t nEvents = 1;

  TString the_datadir=datadir;

  if (the_datadir=="")
    the_datadir=".";

  TString outFile = the_datadir+ "/zdctest.root";             
  TString parFile = outFile ; 

  TStopwatch timer;
  timer.Start();
  gDebug=0;

#define URQMD

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  mpdloadlibs(1,1);                 // load all libraries

  //  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_v1_option.C");
  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_v2.C");

  FairRunSim *fRun = new FairRunSim();
  
  // set the MC version used
  // ------------------------

  //  fRun->SetName("TGeant4");
  fRun->SetName("TGeant3");

  fRun->SetOutputFile(outFile.Data());

  
  // Load mpd detectors libraries with changed ZDC geometry file:
  // ------------------------

  //  geometry_v1_option (fRun, kTRUE, "ZDC","zdc_modules84_layers60_16_4.geo");  
    geometry_v2(fRun, kTRUE);    // load mpd standard geometries


  // Create and Set Event Generator
  //-------------------------------

  FairPrimaryGenerator* primGen = new FairPrimaryGenerator();
  fRun->SetGenerator(primGen);

#ifdef URQMD
  // Urqmd  Generator

  TString hostname = gSystem->HostName();
  TString dataFile;

  dataFile=find_path_to_URQMD_files();
    if ((hostname=="lxmpd-ui.jinr.ru")||(hostname=="lxmpd-ui"))  
    dataFile += "auau.09gev.mbias.10k.f14";
  else
    dataFile += "auau.09gev.mbias.98k.ftn14";

  FairUrqmdGenerator* urqmdGen = new FairUrqmdGenerator(dataFile);
  primGen->AddGenerator(urqmdGen);

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
    FairBoxGenerator(2212, 10); // 2212 =proton ; 10 = multipl.
  
  //    FairBoxGenerator* boxGen = new
  //      FairBoxGenerator(13, 100); // 13 = muon; 1 = multipl.
  boxGen->SetPRange(0.25,2.5); // GeV/c //setPRange vs setPtRange
  boxGen->SetPhiRange(0, 360); // Azimuth angle range [degree]
  boxGen->SetThetaRange(0, 180); // Polar angle in lab system range [degree]
  boxGen->SetXYZ(0., 0., 0.); // mm o cm ??
  primGen->AddGenerator(boxGen);
#endif
#endif
#endif
#endif

  // Field Map Definition
  // --------------------
  //   // 1- Reading the new field map in the old format

  //   // Constant Field
  //   PndConstField *fMagField=new PndConstField();
  //   fMagField->SetField(0, 0 , 5. ); // values are in kG:  1T = 10kG
  //   // MinX=-75, MinY=-40,MinZ=-12 ,MaxX=75, MaxY=40 ,MaxZ=124 );  // values are in cm
  //   fMagField->SetFieldRegion(-205, 205, -205, 205, -261, 261); //cm
  
  //   // 2- Reading the new field map in the new format

  //   //  FairField *fMagField= new FairFieldMapSym3("FieldActive");
  //   // Active Shielding
  
  //   fRun->SetField(fMagField);

  PndMultiField *fField= new PndMultiField();
  PndConstField *fMagField=new PndConstField();
  fMagField->SetField(0, 0 , 5. ); // values are in kG:  1T = 10kG
  //  fMagField->SetFieldRegion(-205, 205, -205, 205, -270, 270); //cm
#define magnet_v4
#ifdef  magnet_v4
  fMagField->SetFieldRegion(-230, 230, -230, 230, -375, 375); //cm   // magnet_v4 or later
#else
  fMagField->SetFieldRegion(-205, 205, -205, 205, -261, 261); //cm   // old magnet
#endif
  fField->AddField(fMagField);
  
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
  //  output->open(parFile.Data());
  output->open(gFile);
  rtdb->setOutput(output);
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
  
