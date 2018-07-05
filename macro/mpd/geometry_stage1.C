void geometry_stage1(FairRunSim *fRun, Bool_t build)
{
  // load libs and build detector geometry
  if (!build){
    gSystem->Load("libtpc");
    gSystem->Load("libTof");
    gSystem->Load("libEmc");
    gSystem->Load("libZdc");
    gSystem->Load("libFfd");
  }
  else
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
    fRun->AddModule(Pipe);
  
    FairModule *Magnet= new FairMagnet("MAGNET");
    Magnet->SetGeometryFileName("magnet_v4_0.geo");
    fRun->AddModule(Magnet);

    //FairDetector *Ffd = new MpdFfd("FFD",kTRUE );
    //Ffd->SetGeometryFileName("ffd.geo");
    //fRun->AddModule(Ffd);

    FairDetector *Tpc = new TpcDetector("TPC", kTRUE);
    Tpc->SetGeometryFileName("tpc_v7.root");
    fRun->AddModule(Tpc);
  
    FairDetector *Tof= new MpdTof("TOF", kTRUE );
    Tof->SetGeometryFileName("tof_v7.root");
    fRun->AddModule(Tof);

    FairDetector *Emc= new MpdEmc("EMC", kTRUE );
    Emc->SetGeometryFileName("emc_v2.root");
    fRun->AddModule(Emc);
	
    FairDetector *Zdc = new MpdZdc("ZDC",kTRUE );
    Zdc->SetGeometryFileName("zdc_oldnames_7sect_v1.root");
    fRun->AddModule(Zdc);
  }//else
}
