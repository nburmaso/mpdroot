#include <FairRunSim.h>
#include <FairCave.h>
#include <FairPipe.h>
#include <FairMagnet.h>
#include <FairModule.h>
#include <MpdSts.h>
#include <MpdFfd.h>
#include <TpcDetector.h>
#include <MpdTof.h>
#include <MpdEtof.h>
#include <MpdEmc.h>
#include <MpdStrawendcap.h>
#include <MpdCpc.h>
#include <MpdZdc.h>
#include <MpdFsa.h>


void geometry_v1 (FairRunSim *fRun, Bool_t build) {
  // load libs and build detector geometry

  if (!build) {
  }
  else {
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
    Magnet->SetGeometryFileName("magnet_v2.geo");
    fRun->AddModule(Magnet);

    //   Silicon Tracker Stations
    FairDetector *Sts= new MpdSts("STS", kTRUE);
    Sts->SetGeometryFileName("sts_v3.geo");
    fRun->AddModule(Sts);

//     FairDetector *Ffd = new MpdFfd("FFD",kTRUE );
//     Ffd->SetGeometryFileName("ffd.geo");
//     fRun->AddModule(Ffd);

    FairDetector *Tpc = new TpcDetector("TPC", kTRUE);
    //    Tpc->SetGeometryFileName("tpc_v2_el.geo");
    Tpc->SetGeometryFileName("tpc_v2.geo");
    fRun->AddModule(Tpc);
  
    FairDetector *Tof= new MpdTof("TOF", kTRUE );
    Tof->SetGeometryFileName("tof_v2.geo");
    fRun->AddModule(Tof);
	
    FairDetector *eTof= new MpdEtof("ETOF", kTRUE );
    eTof->SetGeometryFileName("etof_v2.geo");
    fRun->AddModule(eTof);

    FairDetector *Emc= new MpdEmc("ECAL", kTRUE);
    Emc->SetGeometryFileName("emc.geo");
    fRun->AddModule(Emc);

    FairDetector *straw_ecStt= new MpdStrawendcap("ESTT", kTRUE);
    //    straw_ecStt->SetGeometryFileName("straw_60_layers.geo");
    //    FairDetector *straw_ecStt= new CbmStt ("ESTT", kTRUE);
    straw_ecStt->SetGeometryFileName("ect.geo");
    fRun->AddModule(straw_ecStt);

//     FairDetector *Bbc = new MpdBbc("BBC",kTRUE );
//     Bbc->SetGeometryFileName("bbc.geo");
//     fRun->AddModule(Bbc);

    FairDetector *Cpc = new MpdCpc("CPC",kTRUE );
    Cpc->SetGeometryFileName("cpc.geo");
    fRun->AddModule(Cpc);
	
    FairDetector *Zdc = new MpdZdc("ZDC",kTRUE );
    Zdc->SetGeometryFileName("zdc_modules84_layers60_16_4.geo");
    fRun->AddModule(Zdc);

    FairDetector *Fsa = new MpdFsa("FSA",kTRUE );
    Fsa->SetGeometryFileName("fsa.geo");
    fRun->AddModule(Fsa);

  }

}
