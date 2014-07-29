void tst_ZdcScheme ()
{
  TStopwatch timer;
  timer.Start();
  gDebug=0;


  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  mpdloadlibs();                 // load main libraries

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/geometry_v2_option.C");
  geometry_v2_option (0x0, kFALSE);   // load mpd detectors libraries

  Int_t iVerbose = 2;
  Int_t nEvents = 1;

  TString inFile = "zdctest.root";             
  TString parFile = "zdcparams.root";
  TString outFile = "tmp.root";


  FairRunAna *fRun = new FairRunAna();
  fRun->SetInputFile(inFile);
  //fRun->AddFile(inFile2);
  fRun->SetOutputFile(outFile);


  FairRuntimeDb *rtdb = fRun->GetRuntimeDb();
  FairParRootFileIo *io1 = new FairParRootFileIo();
  io1->open(parFile.Data());
  rtdb->setFirstInput(io1);

  // fRun->LoadGeometry();  // EL

   fRun->Init();

   rtdb->activateParIo(rtdb->getFirstInput());
  MpdZdcGeoPar *p=( MpdZdcGeoPar*) gROOT->FindObject("MpdZdcGeoPar");
  p->print();
  fDigiScheme  = MpdZdcDigiScheme::Instance();
  fDigiScheme->Init(p,0,kTRUE,iVerbose);

  fDigiScheme->Print();

  fDigiScheme->PrintVolume (29,59,1,1);	
 	
  timer.Stop();
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime();
  printf("RealTime=%f seconds, CpuTime=%f seconds\n",rtime,ctime);
}
