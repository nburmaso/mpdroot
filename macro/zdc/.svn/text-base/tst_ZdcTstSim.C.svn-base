void tst_ZdcTstSim (const char *datadir="",const char *inputfile="" )
{
  TStopwatch timer;
  timer.Start();
  gDebug=0;

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  mpdloadlibs();                      // load main libraries

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_v2_option.C");
  geometry_v2_option (0x0, kFALSE);   // load mpd detectors libraries

  Int_t iVerbose = 2;
  Int_t nEvents = 1;


  TString the_datadir=datadir;

  if (the_datadir=="")
    the_datadir=".";

  TString the_inputfile=inputfile;
  if (the_inputfile=="")
    the_inputfile="zdctest.root";

  TString inFile =  the_datadir+ "/"+the_inputfile;
  TString parFile = inFile;
  TString outFile = the_datadir+ "/tst_"+the_inputfile;


  FairRunAna *fRun = new FairRunAna();
  fRun->SetInputFile(inFile);
  //fRun->AddFile(inFile2);
  fRun->SetOutputFile(outFile);


  FairRuntimeDb *rtdb = fRun->GetRuntimeDb();
  FairParRootFileIo *io1 = new FairParRootFileIo();
  //  io1->open(parFile.Data());
  io1->open(gFile);
  rtdb->setFirstInput(io1);
  // fRun->LoadGeometry();  // EL


  FairTask *tsim= new MpdZdcTstSim("MpdZdcTstSim","MpdZdcTstSim",iVerbose);
  fRun->AddTask(tsim);

  fRun->Init();
  fRun->Run(0,nEvents);
 	
  delete tsim;
 	
  timer.Stop();
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime();
  printf("RealTime=%f seconds, CpuTime=%f seconds\n",rtime,ctime);
}
