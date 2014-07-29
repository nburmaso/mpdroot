tpc (const char *datadir="")
{
  
  // ========================================================================
  // Verbosity level (0=quiet, 1=event level, 2=track level, 3=debug)
  Int_t iVerbose = 1;
  
  // Input file (MC events)
  TString inFile = "evetest.root";
  
  // Number of events to process
  Int_t nEvents = 1;
  
  
  // Parameter file
  TString parFile = "testparams.root";
  
  // Output file
  TString outFile = "evetest_hit.root";
  

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  mpdloadlibs();                 // load main libraries

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_v2.C");
  geometry_v2(0x0, kFALSE);     // load mpd detectors libraries

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
  //fRun->LoadGeometry();  // EL
  // ------------------------------------------------------------------------
  
  // -----    Digi Sequence  --------------------------------------------
  TpcClusterizerTask* tpcClusterizer = new TpcClusterizerTask();
  fRun->AddTask(tpcClusterizer);

    
  TpcDriftTask* tpcDrifter = new TpcDriftTask();
  fRun->AddTask(tpcDrifter);
  
    
  //TpcGemTask* tpcGem = new TpcGemTask();
  //fRun->AddTask(tpcGem);
  
  TpcHitFinderTask* tphHitFinderTask = new TpcHitFinderTask();
  fRun->AddTask(tphHitFinderTask);
  
/*  TpcPadResponseTask* tpcPadResponse = new TpcPadResponseTask();
  tpcPadResponse->SetMinSigAmp(100);
  fRun->AddTask(tpcPadResponse);
  
  
  TpcAdcTask* tpcAdc = new TpcAdcTask();
  fRun->AddTask(tpcAdc);
  
  TpcPSATask* tpcPSA = new TpcPSATask();
  //tpcPSA->SetPersistence();
  fRun->AddTask(tpcPSA);
  //digitization done

      
  TpcClusterFinderTask* tpcClusterFinder = new TpcClusterFinderTask();
  tpcClusterFinder->SetPersistence();
  fRun->AddTask(tpcClusterFinder);
*/  
  
  // -----   Intialise and run   --------------------------------------------
  fRun->Init();
  fRun->Run(0,nEvents);
  // ------------------------------------------------------------------------
  
  
  
  // -----   Finish   -------------------------------------------------------
  
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
  
  exit(0);
  
}
