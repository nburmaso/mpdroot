// EVENT DISPLAY macro for MPD simulated data
//
// sim_file - path to the file with MC data and detector geometry
// reco_file - file with reconstructed data for simulated events
// is_online: false - use Offline Mode (manual switching of events), default; true - use Online Mode (continious view events)
void eventdisplay(char* sim_file = "$VMCWORKDIR/macro/mpd/evetest.root", char* reco_file = "$VMCWORKDIR/macro/mpd/mpddst.root", bool is_online = false)
{
    gDebug = 0;

    // load main and detectors libraries
    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(1,1);

    // load TEve libraries
    gSystem->Load("libEve");
    gSystem->Load("libEventDisplay");

    // CREATE FairRunAna
    FairRunAna* fRun = new FairRunAna();

    // check file existence with MC data and detector geometry
    if (!CheckFileExist(sim_file))
    {
        cout<<endl<<"ERROR: Simulation file with detector geometry wasn't found!"<<endl;
        return;
    }

    // set source of events to display and addtiional parameters
    FairSource* fFileSource = new FairFileSource(sim_file);

    // set parameter file with MC data and detector geometry
    FairRuntimeDb* rtdb = fRun->GetRuntimeDb();
    FairParRootFileIo* parIo1 = new FairParRootFileIo();
    parIo1->open(sim_file);
    rtdb->setFirstInput(parIo1);
    rtdb->setOutput(parIo1);
    rtdb->saveOutput();

    // add file with reconstructed data as a friend
    if (CheckFileExist(reco_file))
        ((FairFileSource*)fFileSource)->AddFriend(reco_file);
    else
        cout<<endl<<"Warning: File with reconstructed data wasn't found!"<<endl;

    fRun->SetSource(fFileSource);

    // Create Event Manager
    FairEventManager* fMan = new FairEventManager();
    fMan->isOnline = is_online;

    // set output file
    fRun->SetOutputFile("ed_out.root");

    // set FairRunAna drawing tasks
    Style_t pointMarker = kFullDotSmall;
    Color_t mcPointColor = kOrange, recoPointColor = kBlack;

    // draw MC points
    FairMCPointDraw *TpcPoint = new FairMCPointDraw("TpcPoint", mcPointColor, kDot);
    fMan->AddTask(TpcPoint);
    FairMCPointDraw *TofPoint = new FairMCPointDraw("TOFPoint", mcPointColor, kDot);
    fMan->AddTask(TofPoint);
    FairMCPointDraw *ZdcPoint = new FairMCPointDraw("ZdcPoint", mcPointColor, kFullSquare);
    fMan->AddTask(ZdcPoint);
    FairMCPointDraw *EmcPoint = new FairMCPointDraw("EmcPoint", mcPointColor, kDot);
    fMan->AddTask(EmcPoint);

    // draw MC geometry tracks
    FairMCTracks* GeoTrack = new FairMCTracks("GeoTracks");
    fMan->AddTask(GeoTrack);

    // draw MC tracks
    //FairMCStack* MCTrack = new FairMCStack("MCTrack");
    //fMan->AddTask(MCTrack);

    // DST hits
    FairHitPointSetDraw *MpdTpcHit = new FairHitPointSetDraw("MpdTpcHit", mcPointColor, kDot);
    fMan->AddTask(MpdTpcHit);

    // DST hits (box view)
    //FairHitDraw *MpdTpcHit = new FairHitDraw("MpdTpcHit", 1);
    //fMan->AddTask(MpdTpcHit);

    // DST tracks
    MpdGlobalTrackDraw *MpdGlobalTrack = new MpdGlobalTrackDraw("GlobalTracks");
    fMan->AddTask(MpdGlobalTrack);

    // draw EMC towers : MpdEmcTowerDraw(TaskName, emcEnergyThreshold in GeV, 2)
    MpdEmcTowerDraw *MpdEmcTower= new MpdEmcTowerDraw("MpdEmcTower", 0.0075, 2);
    fMan->AddTask(MpdEmcTower);

    // draw ZDC towers : MpdZdcTowerDraw(TaskName, zdcEnergyThreshold in GeV, kFALSE, 2)
    MpdZdcTowerDraw *MpdZdcTower= new MpdZdcTowerDraw("MpdZdcTower", 0.0001, kFALSE, 2);
    fMan->AddTask(MpdZdcTower);

    // save EventDisplay Screenshot
    //FairWebScreenshots* WebScreenshots = new FairWebScreenshots("WebScreenshots", "/var/www/html/events"); // for WEB-page
    //FairWebScreenshots* WebScreenshots = new FairWebScreenshots("WebScreenshots","screenshots"); // folder to save the screenshots
    //WebScreenshots->SetFormatFiles(0); // 0 -.png, 1 -.jpg, 2 -.jpg and .png
    //WebScreenshots->SetMultiFiles(0); //0 - the same file (event.png), 1 - multiple files (event_nnn.png)
    //WebScreenshots->SetPort(8016); // 8016 by default
    //fMan->AddTask(WebScreenshots);


    //if (gGeoManager)
    //    gGeoManager->SetVisLevel(3);

    // FairEventManager::Init(Int_t visopt = 1, Int_t vislvl = 3, Int_t maxvisnds = 10000)
    fMan->Init();

    // -----   Finish   -------------------------------------------------------
    cout << endl << endl;
    cout << "Event Display was initialized successfully." << endl;
    cout << endl;
    // ------------------------------------------------------------------------
}
