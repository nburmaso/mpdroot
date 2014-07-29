// disp_type: 0 - DST points, 1 - DST tracks, 2 - DST points (box view); 10 - MC points, 11 - MC tracks, 12 - MC geometry tracks
void eventdisplay (int disp_type = 0, char *infile=0, char* parfile=0, char *outfile=0)
{
  TStopwatch timer;
  timer.Start();
  gDebug = 0;

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  // load main and detectors libraries
  mpdloadlibs(1,1);

  gSystem->Load("libEve");
  gSystem->Load("libEventDisplay");

  // define input file
  TString inFile;
  if (disp_type > 9)
    inFile = "$VMCWORKDIR/macro/mpd/evetest.root";
  else
    inFile = "$VMCWORKDIR/macro/mpd/mpddst.root";
  if (infile)
    inFile = infile;

  // define parameter file
  TString parFile = inFile;
  if (parfile)
    parFile = parfile;

  // define output file
  TString outFile = "tmp.root";
  if (outfile)
    outFile = outfile;

  FairRunAna *fRun = new FairRunAna();

  // set input file
  if (CheckFileExist(inFile))
    fRun->SetInputFile(inFile);
  else
    cout<<endl<<"Error: Input file wasn't found!"<<endl;

  // set parameter file
  if (CheckFileExist(parFile))
  {
    FairRuntimeDb *rtdb = fRun->GetRuntimeDb();
    FairParRootFileIo *io1 = new FairParRootFileIo();
    io1->open(parFile.Data());
    rtdb->setFirstInput(io1);
  }
  else
    cout<<endl<<"Error: Parameter file wasn't found!"<<endl;

  // set output file
  fRun->SetOutputFile(outFile);

  // create event manager
  FairEventManager *fMan= new FairEventManager();

  switch (disp_type)
  {
    // DST points
    case 0:{
      FairHitPointSetDraw *MpdTpcHit = new FairHitPointSetDraw("MpdTpcHit", kOrange, kDot);
      fMan->AddTask(MpdTpcHit);

      break;
    }
    // DST tracks
    case 1:{
      MpdTrackDraw *MpdGlobalTrack = new MpdTrackDraw("GlobalTracks");
      fMan->AddTask(MpdGlobalTrack);

      break;
    }
    // DST points (box view)
    case 2:{
      FairHitDraw *MpdTpcHit = new FairHitDraw("MpdTpcHit", 1);
      fMan->AddTask(MpdTpcHit);

      break;
    }
    // MC points
    case 10:{
      FairMCPointDraw *TpcPoint = new FairMCPointDraw("TpcPoint", kOrange, kDot);
      fMan->AddTask(TpcPoint);
      FairMCPointDraw *TofPoint = new FairMCPointDraw("TOFPoint", kOrange, kDot);
      fMan->AddTask(TofPoint);
      FairMCPointDraw *ZdcPoint = new FairMCPointDraw("ZdcPoint", kOrange, kFullSquare);
      fMan->AddTask(ZdcPoint);
      FairMCPointDraw *EmcPoint = new FairMCPointDraw("MpdEmcPoint", kOrange, kDot);
      fMan->AddTask(EmcPoint);

      break;
    }
    // MC tracks
    case 11:{
      FairMCStack* MCTrack = new FairMCStack("MCTrack");
      fMan->AddTask(MCTrack);

      break;
    }
    // MC geometry tracks
    case 12:{
      FairMCTracks* GeoTrack = new FairMCTracks("GeoTracks");
      fMan->AddTask(GeoTrack);

      break;
    }
  }

  if (gGeoManager)
    gGeoManager->SetVisLevel(3);

  fMan->Init();
}
