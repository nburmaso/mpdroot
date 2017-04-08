//---------------------------
//M.Golubeva (INR RAS), February 2016
//---------------------------

#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"
#include "TFile.h"

//#include "ZDC_geom_par.h"

//Detector's position
const Double_t WALL_Xpos = 0.0;
const Double_t WALL_Ypos = 0.0;
const Double_t WALL_Zpos = 319.5;//cm to WALL surface
const Double_t WALL_center_Zpos = 320.;//3.2 m (319.5+0.5);//cm

//Detector's construct parameters ===================

//Global dimensions for WALL in cm
Double_t Wall_inner_radius = 5.; //10./2.
Double_t Wall_outer_radius = 70.; //140./2.
Double_t Wall_Z_size = 0.5; //1./2.

//media ============================================
TGeoMedium *pMedVacuum = 0;
TGeoMedium *pMedAir = 0;
TGeoMedium *pMedScint = 0;

class FairGeoMedia;
class FairGeoBuilder;

void DefineRequiredMedia(FairGeoMedia* geoMedia, FairGeoBuilder* geoBuild) {
    //vacuum medium
    FairGeoMedium* mVacuum = geoMedia->getMedium("vacuum");
    if ( ! mVacuum ) Fatal("Main", "FairMedium vacuum not found");
    geoBuild->createMedium(mVacuum);
    pMedVacuum = gGeoManager->GetMedium("vacuum");
    if ( ! pMedVacuum ) Fatal("Main", "Medium vacuum not found");

    //air medium
    FairGeoMedium* mAir = geoMedia->getMedium("air");
    if ( ! mAir ) Fatal("Main", "FairMedium air not found");
    geoBuild->createMedium(mAir);
    pMedAir = gGeoManager->GetMedium("air");
    if ( ! pMedAir ) Fatal("Main", "Medium air not found");

    // medium FscScint (Scint.)
    FairGeoMedium* mPolystyrene = geoMedia->getMedium("FscScint");
    if ( ! mPolystyrene ) Fatal("Main", "FairMedium FscScint not found");
    geoBuild->createMedium(mPolystyrene);
    pMedScint = gGeoManager->GetMedium("FscScint");
    if ( ! pMedScint ) Fatal("Main", "Medium FscScint not found");
}//DefineRequiredMedia


void create_rootgeom_wall_oldnames_v1() {

    // Load necessary libraries
    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(); // load libraries

    // ----  set working directory  --------------------------------------------
    TString gPath = gSystem->Getenv("VMCWORKDIR");

    // -------   Geometry file name (output)   ---------------------------------
    const TString geoDetectorName = "wall_oldnames";
    const TString geoDetectorVersion = "v1";
    TString geoFileName = gPath + "/geometry/" + geoDetectorName + "_"+ geoDetectorVersion + ".root";
    // -------------------------------------------------------------------------

    // ----  global geometry parameters  ---------------------------------------
    FairGeoLoader*    geoLoad = new FairGeoLoader("TGeo","FairGeoLoader");
    FairGeoInterface* geoFace = geoLoad->getGeoInterface();

    // -------   Load media from media file   ----------------------------------
    TString medFile = gPath + "/geometry/media.geo";
    geoFace->setMediaFile(medFile);
    geoFace->readMedia();
    FairGeoMedia*   geoMedia = geoFace->getMedia();
    FairGeoBuilder* geoBuild = geoLoad->getGeoBuilder();
    //geoMedia->list();
    //geoMedia->print();
    DefineRequiredMedia(geoMedia, geoBuild);
    // -------------------------------------------------------------------------

    // --------------   Create geometry and global top volume  ------------------------
    gGeoManager = (TGeoManager*)gROOT->FindObject("FAIRGeom");
    gGeoManager->SetName(geoDetectorName + "_geom");
    TGeoVolume* top = new TGeoVolumeAssembly("TOP");
    top->SetMedium(pMedAir);
    gGeoManager->SetTopVolume(top);
    //gGeoMan->SetTopVisible(1);
    // --------------------------------------------------------------------------

    // Define WALL Geometry
    TString wall_common_name = "WALL_common";
    TGeoVolume *WALL_common = new TGeoVolumeAssembly(wall_common_name);
    WALL_common->SetMedium(pMedAir);
    //WALL_common->SetMedium(pMedVacuum);

    TString wall_name = "zdc01";
    TGeoTube *WALLS = new TGeoTube(wall_name, Wall_inner_radius, Wall_outer_radius, Wall_Z_size);
    TGeoVolume *WALLV = new TGeoVolume(wall_name, WALLS);
    WALLV->SetMedium(pMedAir);

    TString wallSensitive_name = "zdc01s";
    TGeoTube *wallSensitiveVolumeS = new TGeoTube(wallSensitive_name, Wall_inner_radius, Wall_outer_radius, Wall_Z_size);
    TGeoVolume *wallSensitiveVolumeV = new TGeoVolume(wallSensitive_name, wallSensitiveVolumeS);
    wallSensitiveVolumeV->SetMedium(pMedScint);

    //insert wallSensitiveVolumeV into WALLV 
    TGeoTranslation *wall_position = new TGeoTranslation(0.,0.,0.);
    WALLV->AddNode(wallSensitiveVolumeV,1,wall_position);

    //Adding the wall mother volume to the global TOP Volume
    TGeoTranslation *wallWALL = new TGeoTranslation("wallWALL",WALL_Xpos, WALL_Ypos, WALL_center_Zpos);
    WALL_common->AddNode(WALLV, 1, wallWALL);//WALL right

    TGeoRotation *rotWALL = new TGeoRotation();
    rotWALL->RotateY(180.);
    TGeoCombiTrans *wallWALL2 = new TGeoCombiTrans(WALL_Xpos, WALL_Ypos, -WALL_center_Zpos, rotWALL);
    WALL_common->AddNode(WALLV, 2, wallWALL2);//WALL left

    top->AddNode(WALL_common, 0);

    top->SetVisContainers(kTRUE);

    // ---------------   Finish   ----------------------------------------------
    gGeoManager->CloseGeometry();
    gGeoManager->CheckOverlaps(0.001);
    gGeoManager->PrintOverlaps();

    gGeoManager->Test();

    TFile* geoFile = new TFile(geoFileName, "RECREATE");
    top->Write();
    geoFile->Close();

    //TString gdmlname = gPath + "/geometry/tpc_v7.gdml";
    //gGeoManager->Export(gdmlname);

    top->Draw("ogl");
    TGLViewer *v = (TGLViewer*)gPad->GetViewer3D();
    v->SetStyle(TGLRnrCtx::kOutline);
    //top->Draw("");

}//create_rootgeom_zdc_v1


