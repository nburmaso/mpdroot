//
// May 2009
//
void recoMC_ndet()
{

  // ========================================================================
  // Verbosity level (0=quiet, 1=event level, 2=track level, 3=debug)
  Int_t iVerbose = 3;

  // Input file (MC events)
  TString inFile = "evetest.root";

  // Number of events to process
  Int_t nEvents = 10; // 1999;


  // Parameter file
  TString parFile = "testparams.root";

  // Output file
  TString outFile = "test.raw.root";

  
  
  // ----  Load libraries   -------------------------------------------------
  gROOT->LoadMacro("$VMCWORKDIR/gconfig/basiclibs.C");
  basiclibs();
  gSystem->Load("libGeoBase");
  gSystem->Load("libParBase");
  gSystem->Load("libBase");
  gSystem->Load("libMCStack");
  //... gSystem->Load("libField");
  gSystem->Load("libPassive");
  gSystem->Load("libGen");

//    gSystem->Load("libtpc");
//    gSystem->Load("libmvd");
//     gSystem->Load("libEtof");
//    gSystem->Load("libTof");
//    //    gSystem->Load("libStrawendcap");
//     gSystem->Load("libBbc");
//     gSystem->Load("libStt");
//     gSystem->Load("libEmc");
//    gSystem->Load("libZdc");
//   //  gSystem->Load("libGen");

  //---- ndet ----
  gSystem->Load("libNDet");

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_v2.C");
  geometry_v2(0x0, kFALSE);

  //???
  //geometry_v2(fRun, kTRUE);    // load mpd standard geometries


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
  fRun->SetInputFile(inFile);
  fRun->SetOutputFile(outFile);
  // ------------------------------------------------------------------------



   // Fill the Parameter containers for this run
  //-------------------------------------------

  FairRuntimeDb *rtdb=fRun->GetRuntimeDb();
  Bool_t kParameterMerged=kTRUE;
  FairParRootFileIo* output=new FairParRootFileIo(kParameterMerged);
  output->open(parFile.Data());
  rtdb->setOutput(output);
  rtdb->saveOutput();
  rtdb->print();

 // -----  Parameter database   --------------------------------------------
  //...  FairRuntimeDb* rtdb = fRun->GetRuntimeDb();
  //... FairParRootFileIo* parInput1 = new FairParRootFileIo();
  //... parInput1->open(parFile.Data());
  //... //FairParAsciiFileIo* parInput2 = new FairParAsciiFileIo();
  //... //TString stsDigiFile = gSystem->Getenv("VMCWORKDIR");
  //... //stsDigiFile += "/parameters/sts/sts_digi_new_standard.par";
  //... //parInput2->open(stsDigiFile.Data(),"in");
  //... rtdb->setFirstInput(parInput1);
  //rtdb->setSecondInput(parInput2);
  //... fRun->LoadGeometry();
  // ------------------------------------------------------------------------

#if 0
  // -----    Digi Sequence  --------------------------------------------
  /*
  TpcClusterizerTask* tpcClusterizer = new TpcClusterizerTask();
  fRun->AddTask(tpcClusterizer);

  TpcDriftTask* tpcDrifter = new TpcDriftTask();
  fRun->AddTask(tpcDrifter);

  TpcGemTask* tpcGem = new TpcGemTask();
  fRun->AddTask(tpcGem);

  TpcPadResponseTask* tpcPadResponse = new TpcPadResponseTask();
  tpcPadResponse->SetMinSigAmp(100);
  fRun->AddTask(tpcPadResponse);

  TpcAdcTask* tpcAdc = new TpcAdcTask();
  fRun->AddTask(tpcAdc);

  TpcPSATask* tpcPSA = new TpcPSATask();
  tpcPSA->SetPersistence();
  fRun->AddTask(tpcPSA);
  */
//digitization done


/*
  IdealPatternRecTask *IdealPatternRec= new IdealPatternRecTask();
  IdealPatternRec->SetPersistence();
  fRun->AddTask(IdealPatternRec);


  GenfitTask *Genfit= new GenfitTask();
  Genfit->SetPersistence();
  fRun->AddTask(Genfit);

    TrackVisTask *trackVis= new TrackVisTask();
    fRun->AddTask(trackVis);
*/
    
//  FitStatTask *fitStat= new FitStatTask();
//  fRun->AddTask(fitStat);
#endif

  //-------- NDet begin ------

  MpdNDetAnalysis *simplendet = new MpdNDetAnalysis("first NDET Test","----KM---");
  fRun->AddTask(simplendet);

  //-------- NDet done -------

  // -----   Intialise and run   --------------------------------------------
  fRun->Init();
  cout << "-I- recoMC_ndet.C:  run init completed." << endl;
  fRun->Run(0,nEvents);
  // ------------------------------------------------------------------------



  // -----   Finish   -------------------------------------------------------
   cout << "-----   Finish   ---------------------" << endl;
  fRun->Run(-1,0);
 
  //delete fitStat;



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
