void geometry_phase0(FairRunSim *fRun, Int_t bebe_ver, Int_t mcord_ver)
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
  
    FairModule *Mbebe= new MbbDetector("MBB",kTRUE);
    switch(bebe_ver){
    case 0:{
        Mbebe->SetGeometryFileName("mbb_v0_strips60cm.root");
    }break;
    default:{
        Mbebe->SetGeometryFileName("mbb_v1_rings60cm.root");
    }break;
    }
    
    fRun->AddModule(Mbebe);
    FairModule *Mcord = new MpdMcord("MCORD",kTRUE);
    switch(mcord_ver){
        case 0:{
            Mcord->SetGeometryFileName("mcord_bebe1.geo");
        }break;
        case 1:{
            Mcord->SetGeometryFileName("mcord_bebe2.geo");
        }break;
        case 2:{
            Mcord->SetGeometryFileName("mcord_bebe3.geo");
        }break;
        default:{
            Mcord->SetGeometryFileName("mcord_v2.geo");
        }break;
    }
    fRun->AddModule(Mcord);


    //FairDetector *Ffd = new MpdFfd("FFD",kTRUE );
    //Ffd->SetGeometryFileName("ffd.geo");
    //fRun->AddModule(Ffd);
}
