// sim_file - file with simulation data, dst_file - file with DST data, out_file - output file
void eventdisplay (char* sim_file = 0, char* dst_file = 0, char* out_file = 0)
{
  TStopwatch timer;
  timer.Start();
  gDebug = 0;


  // load main and detectors libraries
  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  // load main and detectors libraries
  mpdloadlibs(1,1);

  // load TEve libraries
  gSystem->Load("libEve");
  gSystem->Load("libEventDisplay");


  // Define simulation file
  TString simFile = "$VMCWORKDIR/macro/mpd/evetest.root";
  if (sim_file)
    simFile = sim_file;

  // define parameter file
  TString parFile = simFile;

  // define reconstructed file
  TString dstFile = "$VMCWORKDIR/macro/mpd/mpddst.root";
  if (dst_file)
    dstFile = dst_file;

  // define output file
  TString outFile = "tmp.root";
  if (out_file)
    outFile = out_file;

  // Create FairRunAna
  FairRunAna *fRun = new FairRunAna();

  // set input file
  if (CheckFileExist(simFile))
    fRun->SetInputFile(simFile);
  else
    cout<<endl<<"ERROR: Input file wasn't found!"<<endl;

  // add friend file with reconstruction data
  if (CheckFileExist(dstFile))
    fRun->AddFriend(dstFile);
  else
    cout<<endl<<"Warning: File with reconstructed data wasn't found!"<<endl;

  // set parameter file
  if (CheckFileExist(parFile))
  {
    FairRuntimeDb *rtdb = fRun->GetRuntimeDb();
    FairParRootFileIo *parIo1 = new FairParRootFileIo();
    parIo1->open(parFile.Data());
    rtdb->setFirstInput(parIo1);
    rtdb->setOutput(parIo1);
    rtdb->saveOutput();
  }
  else
    cout<<endl<<"ERROR: Parameter file wasn't found!"<<endl;

  // set output file
  fRun->SetOutputFile(outFile);


  // Create event manager
  FairEventManager *fMan= new FairEventManager();

  // draw MC points
  FairMCPointDraw *TpcPoint = new FairMCPointDraw("TpcPoint", kOrange, kDot);
  fMan->AddTask(TpcPoint);
  FairMCPointDraw *TofPoint = new FairMCPointDraw("TOFPoint", kOrange, kDot);
  fMan->AddTask(TofPoint);
  FairMCPointDraw *ZdcPoint = new FairMCPointDraw("ZdcPoint", kOrange, kFullSquare);
  fMan->AddTask(ZdcPoint);
  FairMCPointDraw *EmcPoint = new FairMCPointDraw("MpdEmcPoint", kOrange, kDot);
  fMan->AddTask(EmcPoint);

  // draw MC geometry tracks
  FairMCTracks* GeoTrack = new FairMCTracks("GeoTracks");
  fMan->AddTask(GeoTrack);

  // draw MC tracks
  //FairMCStack* MCTrack = new FairMCStack("MCTrack");
  //fMan->AddTask(MCTrack);

  // DST hits
  FairHitPointSetDraw *MpdTpcHit = new FairHitPointSetDraw("MpdTpcHit", kOrange, kDot);
  fMan->AddTask(MpdTpcHit);

  // DST hits (box view)
  //FairHitDraw *MpdTpcHit = new FairHitDraw("MpdTpcHit", 1);
  //fMan->AddTask(MpdTpcHit);

  // DST tracks
  MpdTrackDraw *MpdGlobalTrack = new MpdTrackDraw("GlobalTracks");
  fMan->AddTask(MpdGlobalTrack);

  if (gGeoManager)
      gGeoManager->SetVisLevel(3);

  //fMan->background_color = 17;

  //FairEventManager::Init(Int_t visopt = 1, Int_t vislvl = 3, Int_t maxvisnds = 10000);
  fMan->Init();
}
