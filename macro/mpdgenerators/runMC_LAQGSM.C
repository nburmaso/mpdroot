// Macro for running Fair with Geant3  or Geant4 (M. Al-Turany , D. Bertini)
// Modified 22/06/2005 D.Bertini
// Modified  ...           roleg      (adopted for mpdroot)
// Modified 27-Mar-2008    litvin     (field definition fixed to try to store the field parameters)
// Modified 26-Dec-2008    litvin     (adopted to current mpdroot changes)
// Modified 30-Jan-2009    litvin     (new hostname mpd.jinr.ru)
// Modified 26-Feb-2009    litvin     (updated to use new MpdLAQGSMGenerator class)

runMC_LAQGSM (const char *output_dir="",const char *output_file="",
	      const char *input_dir="" ,const char *input_file="", 
	      Int_t nEvents=1, Int_t nSkip=0 )
{


//(const char *datadir="", Int_t nEvents = 1)
//   TString the_datadir=datadir;
//   if (the_datadir=="")
//     the_datadir=".";
//   TString outFile = the_datadir+ "/zdctest4big.root";             
//   TString parFile = the_datadir+ "/zdcparams4big.root";

  TString the_output_dir=output_dir;
  TString the_output_file=output_file;
  TString the_input_file=input_file;

  if (the_output_dir=="")
    the_output_dir=".";

  if (the_output_file=="")
    the_output_file="zdctest_1ev.root";

  TString outFile = the_output_dir+ "/" + the_output_file;             
  TString parFile = outFile;

  TStopwatch timer;
  timer.Start();
  gDebug=0;

  //#define URQMD
#define LAQGSM

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  mpdloadlibs(1,1);                 // load all libraries

  //  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_v1_option.C");
  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_v2_minimal.C");
  //  geometry_v1_option (0x0, kFALSE);   // load mpd detectors libraries
  //    geometry_v2(fRun, kTRUE);    // load mpd standard geometries

  FairRunSim *fRun = new FairRunSim();
  
  // set the MC version used
  // ------------------------

     fRun->SetName("TGeant4");
     //  fRun->SetName("TGeant3");

  fRun->SetOutputFile(outFile.Data());

  
  // Load mpd detectors libraries with changed ZDC geometry file and switch OFF other detectors:
  // ------------------------

  //  geometry_v1_option (fRun, kTRUE, "ZDC","zdc_modules84_layers60_16_4.geo",kFALSE);  
    geometry_v2_minimal(fRun);    // load mpd standard geometries


  // Create and Set Event Generator
  //-------------------------------

  FairPrimaryGenerator* primGen = new FairPrimaryGenerator();
  fRun->SetGenerator(primGen);

  TString hostname = gSystem->HostName();
  TString dataFile;

#ifdef LAQGSM

  if (the_input_file=="") {
    dataFile=find_path_to_URQMD_files();
    dataFile += "/../../QGSM/";           //  nc2,nc3,nc8,nc9

    dataFile += "/mer/AuAu40c_mer.r12";
  }
  else
    dataFile += the_input_file;

  MpdLAQGSMGenerator* guGen= new MpdLAQGSMGenerator(dataFile.Data());
  primGen->AddGenerator(guGen);
  guGen->SkipEvents(nSkip);

#else
#ifdef URQMD

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
    FairBoxGenerator(13, 10); // 13 = muon; 1 = multipl.
  
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
#endif

  // Field Map Definition
  // --------------------

  PndMultiField *fField= new PndMultiField();
  PndConstField *fMagField=new PndConstField();
  fMagField->SetField(0, 0 , 5. ); // values are in kG:  1T = 10kG
  //  fMagField->SetFieldRegion(-205, 205, -205, 205, -270, 270); //cm
#define magnet_v4
#ifdef  magnet_v4
  fMagField->SetFieldRegion(-230, 230, -230, 230, -375, 375); //cm   // magnet_v4 or later
#else
  fMagField->SetFieldRegion(-205, 205, -205, 205, -270, 270); //cm   // old magnet
#endif
  fField->AddField(fMagField);
  
  fRun->SetField(fField);
  fRun->SetStoreTraj(kTRUE);

#ifdef LAQGSM
 fRun->SetUserDecay(1); // if true gconfig/UserDecay.C macro will be called
#endif

  fRun->Init();


  // -Trajectories Visualization (TGeoManager Only )
  // -----------------------------------------------

  fRun->SetStoreTraj(kFALSE);

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
  output->open(gFile);
  rtdb->setOutput(output);
  rtdb->saveOutput();
  rtdb->print();

  // Transport nEvents
  // -----------------

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
  
