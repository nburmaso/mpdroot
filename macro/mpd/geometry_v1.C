//void geometry_v1_demezhan (FairRunSim *fRun, Bool_t build){
void geometry_v1(FairRunSim *fRun)
  // load libs and build detector geometry

/*  if (!build) {
    gSystem->Load("libtpc");
    gSystem->Load("libTof");
    gSystem->Load("libEtof");
    gSystem->Load("libZdc");
    gSystem->Load("libStrawendcap");
    gSystem->Load("libSts");
	gSystem->Load("libEmc");
    //    gSystem->Load("libBbc");
    gSystem->Load("libCpc");
    //    gSystem->Load("libFsa");
    //    gSystem->Load("libFfd");
  }
  else
  */
	{
    // Set Material file Name
    fRun->SetMaterials("media.geo");

    // Create and add detectors
    //-------------------------

    FairModule *Cave= new FairCave("CAVE");
    Cave->SetGeometryFileName("cave.geo");
    fRun->AddModule(Cave);

    FairModule *Pipe= new FairPipe("PIPE");
    Pipe->SetGeometryFileName("pipe.geo");
    //Pipe->SetGeometryFileName("pipe_v2.root");
    fRun->AddModule(Pipe);

    FairModule *Magnet= new FairMagnet("MAGNET");
    Magnet->SetGeometryFileName("magnet_v5.root");
    fRun->AddModule(Magnet);

    //   Silicon Tracker Stations
//    FairDetector *Sts= new MpdSts("STS", kTRUE);
//    Sts->SetGeometryFileName("sts.geo");
//    fRun->AddModule(Sts);

    FairDetector *Tpc = new TpcDetector("TPC", kTRUE);
    //    Tpc->SetGeometryFileName("tpc_v2_el.geo");
    Tpc->SetGeometryFileName("tpc_v8.root");
    fRun->AddModule(Tpc);

    FairDetector *Tof= new MpdTof("TOF", kTRUE );
    Tof->SetGeometryFileName("tof_v8.root");
    fRun->AddModule(Tof);

    FairDetector *eTof= new MpdEtof("ETOF", kTRUE );
    eTof->SetGeometryFileName("etof_v2.geo");
    fRun->AddModule(eTof);

//    FairDetector *straw_ecStt= new MpdStrawendcap("ESTT", kTRUE);
//    straw_ecStt->SetGeometryFileName("straw_60_layers.geo");
//    fRun->AddModule(straw_ecStt);

    FairDetector *Zdc = new MpdZdc("ZDC",kTRUE );
    Zdc->SetGeometryFileName("zdc_oldnames_7sect_v1_no_overlaps_w_pipe_magnet.root");
   fRun->AddModule(Zdc);

    FairDetector *Emc= new MpdEmc("EMC", kTRUE );
    Emc->SetGeometryFileName("emc_v3.root");
    fRun->AddModule(Emc);

    FairDetector *Bbc = new BmdDetector("BMD",kTRUE );
    Bbc->SetGeometryFileName("bbc_hexagon_v3.root");
    //Bbc->SetGeometryFileName("simple1_v1.root");
    fRun->AddModule(Bbc);

//    FairDetector *Cpc = new MpdCpc("CPC",kTRUE );
//    Cpc->SetGeometryFileName("simple1_v2.root");
//    fRun->AddModule(Cpc);

    //    FairDetector *Fsa = new MpdFsa("FSA",kTRUE );
    //    Fsa->SetGeometryFileName("fsa.geo");
//    fRun->AddModule(Fsa);

//    FairDetector *Ffd = new MpdFfd("FFD",kTRUE );
//    Ffd->SetGeometryFileName("ffd.geo");
//    fRun->AddModule(Ffd);
  }

//}
