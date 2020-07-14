// Geometry for pipe: aluminium + beryllium
// Demezhan Myktybekov
// July 2020
// myktybekov@jinr.ru


//---------------------------

#include <vector>
//---------------------------
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"
#include "TFile.h"

#include "../mpdloadlibs.C"

//Pipe's position
const Double_t PIPE_Xpos = 0.0;
const Double_t PIPE_Ypos = 0.0;
const Double_t PIPE_Zpos = 0.0;


//media
TGeoMedium *pMedAir = 0;
TGeoMedium *pMedAluminium = 0;
TGeoMedium *pMedBeryllium = 0;
TGeoMedium *pMedVacuum = 0;
TGeoMedium *pMedSteel = 0;

class FairGeoMedia;
class FairGeoBuilder;

void DefineRequiredMedia(FairGeoMedia* geoMedia, FairGeoBuilder* geoBuild);
/*
void CreateBerylliumTube(TGeoVolume* mother_volume);
void CreateAluminiumInnerSections(TGeoVolume* mother_volume);
void CreateAluminiumAdaptersSmall(TGeoVolume* mother_volume, Double_t rmin, Double_t rmax, Bool_t is_outer_wall);
void CreateAluminiumMiddleSections(TGeoVolume* mother_volume);
void CreateAluminiumInnerFlanges(TGeoVolume* mother_volume);
void CreateAluminiumOuterFlangesSmall(TGeoVolume* mother_volume, Double_t width, Double_t rmin, Double_t rmax);
void CreateAluminiumOuterSectionsSmall(TGeoVolume *mother_volume, Double_t vol_width);
void CreateAluminiumAdaptersBig(TGeoVolume *mother_volume, Double_t width, Double_t rmin, Double_t rmax);
void CreateAluminiumOuterSectionsBig(TGeoVolume *mother_volume, Double_t vol_width);
void CreateAluminiumOuterFlangesBig(TGeoVolume *mother_volume, Double_t vol_width);
*/
void pipe_v2()
{   
    // Load necessary libraries
    //gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    //mpdloadlibs(); // load libraries

    // ----  set working directory  --------------------------------------------
    TString gPath = gSystem->Getenv("VMCWORKDIR");

    // -------   Module file name (output)   ---------------------------------
    const TString geoModuleName = "pipe";
    const TString geoModuleVersion = "v2";
    TString geoFileName = gPath + "/geometry/" + geoModuleName + "_"+ geoModuleVersion + ".root";
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

    DefineRequiredMedia(geoMedia, geoBuild);
    // -------------------------------------------------------------------------

    // --------------   Create geometry and global top volume  ------------------------
    gGeoManager = (TGeoManager*)gROOT->FindObject("FAIRGeom");
    gGeoManager->SetName(geoModuleName + "_geom");
    TGeoVolume* top = new TGeoVolumeAssembly("TOP");
    top->SetMedium(pMedVacuum);
    gGeoManager->SetTopVolume(top);
    //gGeoMan->SetTopVisible(1);
    // --------------------------------------------------------------------------
    TGeoTube* pipe_mother_vol_tube = new TGeoTube("pipe_mother_vol_tube", 0., 20., 897./2);
    TGeoVolume* pipe_mother_vol = new TGeoVolume("pipe_mother_vol", pipe_mother_vol_tube);
    pipe_mother_vol->SetMedium(pMedVacuum);
    pipe_mother_vol->SetTransparency(95);

    TGeoTranslation* pipe_mother_vol_pos = new TGeoTranslation("pipe_mother_vol_pos", PIPE_Xpos, PIPE_Ypos, PIPE_Zpos);

    top->AddNode(pipe_mother_vol, 0, pipe_mother_vol_pos);
    ///////////////////////////////////////////////////////////////////////////////////
    //DONE!!!
    // Define PIPE Geometry
    TGeoTube* pipe_beryllium_inner = new TGeoTube("pipe_beryllium_inner", 3.1, 3.225, 169.4/2);
    TGeoVolume* pipe_beryllium = new TGeoVolume("pipe_beryllium", pipe_beryllium_inner);
    pipe_beryllium->SetLineColor(kRed);
    pipe_beryllium->SetMedium(pMedBeryllium);
    pipe_beryllium->SetTransparency(0);

    TGeoTranslation* pipe_beryllium_pos = new TGeoTranslation("pipe_beryllium_pos", PIPE_Xpos, PIPE_Ypos, PIPE_Zpos);
    pipe_mother_vol->AddNode(pipe_beryllium, 0, pipe_beryllium_pos);


    
    TGeoTranslation *aluminium_inner_tube_location_right = new TGeoTranslation("aluminium_inner_tube_location_right", 0., 0., 86.675);
    aluminium_inner_tube_location_right->RegisterYourself();
    TGeoTranslation *aluminium_inner_adapter_location_right = new TGeoTranslation("aluminium_inner_adapter_location_right", 0., 0., 92.225);
    aluminium_inner_adapter_location_right->RegisterYourself();
    TGeoTranslation *aluminium_middle_tube_location_right = new TGeoTranslation("aluminium_middle_tube_location_right", 0., 0., 113.75);
    aluminium_middle_tube_location_right->RegisterYourself();
    TGeoTranslation *aluminium_inner_flange_location_right = new TGeoTranslation("aluminium_inner_flange_location_right", 0., 0., 132.85);
    aluminium_inner_flange_location_right->RegisterYourself();

    TGeoTranslation *aluminium_inner_tube_location_left = new TGeoTranslation("aluminium_inner_tube_location_left", 0., 0., -86.675);
    aluminium_inner_tube_location_left->RegisterYourself();
    TGeoTranslation *aluminium_inner_adapter_location_left = new TGeoTranslation("aluminium_inner_adapter_location_left", 0., 0., -92.225);
    aluminium_inner_adapter_location_left->RegisterYourself();
    TGeoTranslation *aluminium_middle_tube_location_left = new TGeoTranslation("aluminium_middle_tube_location_left", 0., 0., -113.75);
    aluminium_middle_tube_location_left->RegisterYourself();
    TGeoTranslation *aluminium_inner_flange_location_left = new TGeoTranslation("aluminium_inner_flange_location_left", 0., 0., -132.85);
    aluminium_inner_flange_location_left->RegisterYourself();

    TGeoTube *aluminium_inner_tube = new TGeoTube("aluminium_inner_tube", 3.1, 3.225, 3.95/2);
    TGeoCone *aluminium_inner_adapter_right = new TGeoCone("aluminium_inner_adapter_right", 7.15/2, 3.1, 3.225, 4., 4.15);
    TGeoCone *aluminium_inner_adapter_left = new TGeoCone("aluminium_inner_adapter_left", 7.15/2, 4., 4.15, 3.1, 3.225);
    TGeoTube *aluminium_middle_tube = new TGeoTube("aluminium_middle_tube", 4., 4.15, 35.9/2);
    TGeoTube *aluminium_inner_flange = new TGeoTube("aluminium_inner_flange", 4., 7.5, 2.3/2);

    TGeoCompositeShape *aluminium_composite_inner_right = new TGeoCompositeShape("aluminium_composite_inner_right", "(aluminium_inner_tube:aluminium_inner_tube_location_right + aluminium_inner_adapter_right:aluminium_inner_adapter_location_right + aluminium_middle_tube:aluminium_middle_tube_location_right + aluminium_inner_flange:aluminium_inner_flange_location_right)");
    TGeoVolume *volume_aluminium_composite_inner_right = new TGeoVolume ("volume_aluminium_composite_inner_right", aluminium_composite_inner_right, pMedAluminium);
    volume_aluminium_composite_inner_right->SetLineColor(kYellow);
    pipe_mother_vol->AddNode(volume_aluminium_composite_inner_right, 1, pipe_beryllium_pos);

    TGeoCompositeShape *aluminium_composite_inner_left = new TGeoCompositeShape("aluminium_composite_inner_left", "(aluminium_inner_tube:aluminium_inner_tube_location_left + aluminium_inner_adapter_left:aluminium_inner_adapter_location_left + aluminium_middle_tube:aluminium_middle_tube_location_left + aluminium_inner_flange:aluminium_inner_flange_location_left)");
    TGeoVolume *volume_aluminium_composite_inner_left = new TGeoVolume ("volume_aluminium_composite_inner_left", aluminium_composite_inner_left, pMedAluminium);
    volume_aluminium_composite_inner_left->SetLineColor(kYellow);
    pipe_mother_vol->AddNode(volume_aluminium_composite_inner_left, 2, pipe_beryllium_pos);
    


    TGeoTranslation *aluminium_outer_flange_small_location_right = new TGeoTranslation("aluminium_outer_flange_small_location_right", 0., 0., 135.15);
    aluminium_outer_flange_small_location_right->RegisterYourself();
    TGeoTranslation *aluminium_outer_tube_small_location_right = new TGeoTranslation("aluminium_outer_tube_small_location_right", 0., 0., 191.3);
    aluminium_outer_tube_small_location_right->RegisterYourself();
    TGeoTranslation *aluminium_middle_adapter_location_right = new TGeoTranslation("aluminium_middle_adapter_location_right", 0., 0., 282.2);
    aluminium_middle_adapter_location_right->RegisterYourself();
    TGeoTranslation *aluminium_outer_tube_big_location_right = new TGeoTranslation("aluminium_outer_tube_big_location_right", 0., 0., 373.5);
    aluminium_outer_tube_big_location_right->RegisterYourself();
    TGeoTranslation *aluminium_outer_flange_big_location_right = new TGeoTranslation("aluminium_outer_flange_big_location_right", 0., 0., 430.15);
    aluminium_outer_flange_big_location_right->RegisterYourself();

    TGeoTranslation *aluminium_outer_flange_small_location_left = new TGeoTranslation("aluminium_outer_flange_small_location_left", 0., 0., -135.15);
    aluminium_outer_flange_small_location_left->RegisterYourself();
    TGeoTranslation *aluminium_outer_tube_small_location_left = new TGeoTranslation("aluminium_outer_tube_small_location_left", 0., 0., -191.3);
    aluminium_outer_tube_small_location_left->RegisterYourself();
    TGeoTranslation *aluminium_middle_adapter_location_left = new TGeoTranslation("aluminium_middle_adapter_location_left", 0., 0., -282.2);
    aluminium_middle_adapter_location_left->RegisterYourself();
    TGeoTranslation *aluminium_outer_tube_big_location_left = new TGeoTranslation("aluminium_outer_tube_big_location_left", 0., 0., -373.5);
    aluminium_outer_tube_big_location_left->RegisterYourself();
    TGeoTranslation *aluminium_outer_flange_big_location_left = new TGeoTranslation("aluminium_outer_flange_big_location_left", 0., 0., -430.15);
    aluminium_outer_flange_big_location_left->RegisterYourself();

    TGeoTube *aluminium_outer_flange_small = new TGeoTube("aluminium_outer_flange_small", 4., 7.5, 2.3/2);
    TGeoTube *aluminium_outer_tube_small = new TGeoTube("aluminium_outer_tube_small", 4., 4.15, 55.);
    TGeoCone *aluminium_middle_adapter_right = new TGeoCone("aluminium_middle_adapter_right", 35.9, 4., 4.15, 6.35, 6.5);
    TGeoCone *aluminium_middle_adapter_left = new TGeoCone("aluminium_middle_adapter_left", 35.9, 6.35, 6.5, 4., 4.15);
    TGeoTube *aluminium_outer_tube_big = new TGeoTube("aluminium_outer_tube_big", 6.35, 6.5, 55.);
    TGeoTube *aluminium_outer_flange_big = new TGeoTube("aluminium_outer_flange_big", 6.35, 10., 2.3/2);

    TGeoCompositeShape *aluminium_composite_outer_right = new TGeoCompositeShape("aluminium_composite_outer_right", "(aluminium_outer_flange_small:aluminium_outer_flange_small_location_right + aluminium_outer_tube_small:aluminium_outer_tube_small_location_right + aluminium_middle_adapter_right:aluminium_middle_adapter_location_right + aluminium_outer_tube_big:aluminium_outer_tube_big_location_right + aluminium_outer_flange_big:aluminium_outer_flange_big_location_right)");
    TGeoVolume *volume_aluminium_composite_outer_right = new TGeoVolume ("volume_aluminium_composite_outer_right", aluminium_composite_outer_right, pMedAluminium);
    volume_aluminium_composite_outer_right->SetLineColor(kYellow);
    pipe_mother_vol->AddNode(volume_aluminium_composite_outer_right, 3, pipe_beryllium_pos);

    TGeoCompositeShape *aluminium_composite_outer_left = new TGeoCompositeShape("aluminium_composite_outer_left", "(aluminium_outer_flange_small:aluminium_outer_flange_small_location_left + aluminium_outer_tube_small:aluminium_outer_tube_small_location_left + aluminium_middle_adapter_left:aluminium_middle_adapter_location_left + aluminium_outer_tube_big:aluminium_outer_tube_big_location_left + aluminium_outer_flange_big:aluminium_outer_flange_big_location_left)");
    TGeoVolume *volume_aluminium_composite_outer_left = new TGeoVolume ("volume_aluminium_composite_outer_left", aluminium_composite_outer_left, pMedAluminium);
    volume_aluminium_composite_outer_left->SetLineColor(kYellow);
    pipe_mother_vol->AddNode(volume_aluminium_composite_outer_left, 4, pipe_beryllium_pos);

    top->SetVisContainers(kTRUE);


    // ---------------   Finish   ----------------------------------------------
    gGeoManager->CloseGeometry();
    gGeoManager->CheckOverlaps(0.001);
    gGeoManager->PrintOverlaps();

    gGeoManager->Test();

    gGeoManager->SetMaxVisNodes(100000);

    TFile* geoFile = new TFile(geoFileName, "RECREATE");
    top->Write();
    geoFile->Close();

    //if (wrGeoWithMaterials)
    //{
    //    TString geoFile_wMat = gPath + "/geometry/" + geoDetectorName + "_"+ geoDetectorVersion + "_with_materials.root";
    //    gGeoManager->Export(geoFile_wMat);
        //TString geoFile_gdml = gPath + "/geometry/" + geoDetectorName + "_"+ geoDetectorVersion + ".gdml";
        //gGeoManager->Export(geoFile_gdml);
    //}

    top->Draw("ogl");
    TGLViewer *v = (TGLViewer*)gPad->GetViewer3D();
    v->SetStyle(TGLRnrCtx::kOutline);
}
void DefineRequiredMedia(FairGeoMedia* geoMedia, FairGeoBuilder* geoBuild) 
{
    //air medium
    FairGeoMedium* mAir = geoMedia->getMedium("air");
    if ( ! mAir ) Fatal("Main", "FairMedium air not found");
    geoBuild->createMedium(mAir);
    pMedAir = gGeoManager->GetMedium("air");
    if ( ! pMedAir ) Fatal("Main", "Medium air not found");

    //aluminium medium
    FairGeoMedium* mAluminium = geoMedia->getMedium("aluminium");
    if ( ! mAluminium  ) Fatal("Main", "FairMedium aluminium not found");
    geoBuild->createMedium(mAluminium);
    pMedAluminium  = gGeoManager->GetMedium("aluminium");
    if ( ! pMedAluminium  ) Fatal("Main", "Medium aluminium not found");

    //beryllium medium
    FairGeoMedium* mBeryllium = geoMedia->getMedium("beryllium");
    if ( ! mBeryllium  ) Fatal("Main", "FairMedium beryllium not found");
    geoBuild->createMedium(mBeryllium);
    pMedBeryllium = gGeoManager->GetMedium("beryllium");
    if ( ! pMedBeryllium  ) Fatal("Main", "Medium beryllium not found");

    //vacuum medium
    FairGeoMedium* mVacuum = geoMedia->getMedium("vacuum");
    if ( ! mVacuum  ) Fatal("Main", "FairMedium vacuum not found");
    geoBuild->createMedium(mVacuum);
    pMedVacuum = gGeoManager->GetMedium("vacuum");
    if ( ! pMedVacuum  ) Fatal("Main", "Medium Vacuum not found");

    //steel medium
    FairGeoMedium* mSteel = geoMedia->getMedium("steel");
    if ( ! mSteel  ) Fatal("Main", "FairMedium steel not found");
    geoBuild->createMedium(mSteel);
    pMedSteel = gGeoManager->GetMedium("steel");
    if ( ! pMedSteel  ) Fatal("Main", "Medium steel not found");
}
