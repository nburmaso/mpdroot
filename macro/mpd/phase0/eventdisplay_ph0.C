R__ADD_INCLUDE_PATH($VMCWORKDIR)
#include "macro/mpd/mpdloadlibs.C"
void SetTasks(MpdEventManager* fMan);

// EVENT DISPLAY macro for MPD simulated data
//
// sim_file - path to the file with MC data and detector geometry
// reco_file - file with reconstructed data for simulated events
// is_online: false - use Offline Mode (manual switching of events), default; true - use Online Mode (continious viewing of events)
void eventdisplay_ph0(const char* sim_file = "evetest.root", const char* reco_file = "mpddst.root", bool is_online = false)
{
    gDebug = 0;

    // CREATE FairRunAna
    FairRunAna* fRun = new FairRunAna();

    // check file existence with MC data and detector geometry
    TString strSimFile(sim_file);
    if (!CheckFileExist(strSimFile))
    {
        cout<<endl<<"ERROR: Simulation file with detector geometry wasn't found!"<<endl;
        return;
    }

    // set source of events to display and addtiional parameters
    FairSource* fFileSource = new FairFileSource(strSimFile);

    // set parameter file with MC data and detector geometry
    FairRuntimeDb* rtdb = fRun->GetRuntimeDb();
    FairParRootFileIo* parIo1 = new FairParRootFileIo();
    parIo1->open(strSimFile);
    rtdb->setFirstInput(parIo1);
    rtdb->setOutput(parIo1);
    rtdb->saveOutput();

    // add file with reconstructed data as a friend
    TString strDstFile(reco_file);
    if (CheckFileExist(strDstFile))
        ((FairFileSource*)fFileSource)->AddFriend(strDstFile);
    else
        cout<<endl<<"Warning: File with reconstructed data wasn't found!"<<endl;

    fRun->SetSource(fFileSource);

    // Create Event Manager
    MpdEventManager* fMan = new MpdEventManager();
    fMan->isOnline = is_online;

    // set output file
    fRun->SetOutputFile("ed_out.root");

    // set tasks to draw
    SetTasks(fMan);

    //if (gGeoManager)
    //    gGeoManager->SetVisLevel(3);

    // MpdEventManager::Init(Int_t visopt = 1, Int_t vislvl = 3, Int_t maxvisnds = 10000)
    fMan->Init();

    // -----   Finish   -------------------------------------------------------
    cout<<endl<<endl<<"Event Display was initialized successfully."<<endl<<endl;
    // ------------------------------------------------------------------------
}

// set FairRunAna drawing tasks depending from data source and on/offline mode
void SetTasks(MpdEventManager* fMan)
{
    Style_t pointMarker = kFullDotSmall;
    Color_t mcPointColor = kOrange, recoPointColor = kBlack;

    // draw MC points
    MpdMCPointDraw* McordPoint = new MpdMCPointDraw("McordPoint", kCyan, kFullSquare);
    fMan->AddTask(McordPoint);
    MpdMCPointDraw* MBBPoint = new MpdMCPointDraw("MbbPoint", kBlue, kFullCircle);
    fMan->AddTask(MBBPoint);

    // draw MC geometry tracks
    MpdMCTracks* GeoTrack = new MpdMCTracks("GeoTracks");
    fMan->AddTask(GeoTrack);

    // draw MC tracks
    //MpdMCStack* MCTrack = new MpdMCStack("MCTrack");
    //fMan->AddTask(MCTrack);

}
