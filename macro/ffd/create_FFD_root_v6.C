#include <iomanip>
#include <iostream>
#include "TGeoManager.h"

#include "TGeoTube.h"
#include "TGeoPara.h"
#include "TGeoCone.h"
#include "TGeoTrd2.h"
#include "TGeoCompositeShape.h"

R__ADD_INCLUDE_PATH($VMCWORKDIR)
#include "macro/mpd/mpdloadlibs.C"

TGeoManager* gGeoMan = NULL;


const Double_t x_size_cube = 5.6;
const Double_t y_size_cube = 5.6;
const Double_t z_size_cube = 1.5;

const Double_t x_size_fpanel = 6.4;
const Double_t y_size_fpanel = 6.4;
const Double_t z_size_fpanel = 0.5;

const Double_t x_size_Pb = 5.8;
const Double_t y_size_Pb = 5.8;
const Double_t z_size_Pb = 1;
	
const Double_t x_size_Rub = 5.8;
const Double_t y_size_Rub = 5.8;
const Double_t z_size_Rub = 0.3;	

const Double_t x_size_pmt_b_f = 5.9;
const Double_t y_size_pmt_b_f = 5.9;
const Double_t z_size_pmt_b_f = 0.3;

const Double_t x_size_pmt_up_d = 5.9;
const Double_t y_size_pmt_up_d = 0.3;
const Double_t z_size_pmt_up_d = 2.4;

const Double_t x_size_pmt_l_r = 0.3;
const Double_t y_size_pmt_l_r = 5.9;
const Double_t z_size_pmt_l_r = 2.4;

const Double_t x_size_electr = 5.5;
const Double_t y_size_electr = 5.5;
const Double_t z_size_electr = 0.3;

const Double_t x_size_lr_panel = 0.2;
const Double_t y_size_lr_panel = 6.4;
const Double_t z_size_lr_panel = 11.;

const Double_t x_size_ud_panel = 6.4;
const Double_t y_size_ud_panel = 0.2;
const Double_t z_size_ud_panel = 11.;

const Double_t mod_pos_X_1 = 7.9;
const Double_t mod_pos_X_2 = 14.5;
const Double_t mod_pos_X_3 = 6.6;
const Double_t mod_pos_X_4 = 13.2;

const Double_t mod_pos_Y_1 = 7.9;
const Double_t mod_pos_Y_2 = 14.5;
const Double_t mod_pos_Y_3 = 6.6;
const Double_t mod_pos_Y_4 = 13.2;

const Double_t mod_pos_Z = 140.7;

const Double_t r_max_Shield_tube = 39.6;
const Double_t r_min_Shield_tube = 9.6;
const Double_t z_size_Shield_tube = 0.25;

const Double_t r_max_Shield_2_tube = 36.8;
const Double_t r_min_Shield_2_tube = 9.6;
const Double_t z_size_Shield_2_tube = 0.7;

const Double_t r_max_Shield_Ph_tube = 15.6;
const Double_t r_min_Shield_Ph_tube = 9.6;
const Double_t z_size_Shield_Ph_tube = 0.4;

const Double_t x_size_plane = 7.6;
const Double_t y_size_plane = 1.4;
const Double_t z_size_plane = 49.05;


void create_FFD_root_v6() {
        
    // -------   Load media from media file   -----------------------------------
    FairGeoLoader*    geoLoad = new FairGeoLoader("TGeo","FairGeoLoader");
    FairGeoInterface* geoFace = geoLoad->getGeoInterface();
    TString geoPath = gSystem->Getenv("VMCWORKDIR");
    TString medFile = geoPath + "/geometry/media.geo";
    geoFace->setMediaFile(medFile);
    geoFace->readMedia();
    gGeoMan = gGeoManager;
    // --------------------------------------------------------------------------

    // -------   Geometry file name (output)   ----------------------------------
    const TString geoDetectorName = "FFD";
    const TString geoDetectorVersion = "v6";
       TString geoFileName = geoPath + "/geometry/" + geoDetectorName + "_"+ geoDetectorVersion + ".root";
    // --------------------------------------------------------------------------  

    // -----------------   Get and create the required media    -----------------
    FairGeoMedia*   geoMedia = geoFace->getMedia();
    FairGeoBuilder* geoBuild = geoLoad->getGeoBuilder();

    FairGeoMedium* mAir = geoMedia->getMedium("air");
    if ( ! mAir ) Fatal("Main", "FairMedium air not found");
    geoBuild->createMedium(mAir);
    TGeoMedium* pMedAir = gGeoMan->GetMedium("air");
    if ( ! pMedAir ) Fatal("Main", "Medium air not found");
	
	FairGeoMedium* mFusedSil = geoMedia->getMedium("FusedSil");
    if ( ! mFusedSil ) Fatal("Main", "FairMedium FusedSil not found");
    geoBuild->createMedium(mFusedSil);
    TGeoMedium* pMedFusedSil = gGeoMan->GetMedium("FusedSil");
    if ( ! pMedFusedSil ) Fatal("Main", "Medium FusedSil not found");
	
	FairGeoMedium* maluminium = geoMedia->getMedium("aluminium");
    if ( ! maluminium ) Fatal("Main", "FairMedium aluminium not found");
    geoBuild->createMedium(maluminium);
    TGeoMedium* pMedaluminium = gGeoMan->GetMedium("aluminium");
    if ( ! pMedaluminium ) Fatal("Main", "Medium aluminium not found");
    
    FairGeoMedium* mlead = geoMedia->getMedium("lead");
    if ( ! mlead ) Fatal("Main", "FairMedium lead not found");
    geoBuild->createMedium(mlead);
    TGeoMedium* pMedlead = gGeoMan->GetMedium("lead");
    if ( ! pMedlead) Fatal("Main", "Medium lead not found");
	
	FairGeoMedium* mpolystyrene = geoMedia->getMedium("polystyrene");
    if ( ! mpolystyrene ) Fatal("Main", "FairMedium polystyrene not found");
    geoBuild->createMedium(mpolystyrene);
    TGeoMedium* pMedpolystyrene = gGeoMan->GetMedium("polystyrene");
    if ( ! pMedpolystyrene) Fatal("Main", "Medium polystyrene not found");
	
	FairGeoMedium* mG10 = geoMedia->getMedium("G10");
    if ( ! mG10 ) Fatal("Main", "FairMedium G10 not found");
    geoBuild->createMedium(mG10);
    TGeoMedium* pMedG10 = gGeoMan->GetMedium("G10");
    if ( ! pMedG10) Fatal("Main", "Medium G10 not found");
	
	FairGeoMedium* mPMTglass = geoMedia->getMedium("PMTglass");
    if ( ! mPMTglass ) Fatal("Main", "FairMedium PMTglass not found");
    geoBuild->createMedium(mPMTglass);
    TGeoMedium* pMedPMTglass = gGeoMan->GetMedium("PMTglass");
    if ( ! pMedPMTglass) Fatal("Main", "Medium PMTglass not found");
	
	FairGeoMedium* mrohacellhf71 = geoMedia->getMedium("rohacellhf71");
    if ( ! mrohacellhf71 ) Fatal("Main", "FairMedium rohacellhf71 not found");
    geoBuild->createMedium(mrohacellhf71);
    TGeoMedium* pMedrohacellhf71 = gGeoMan->GetMedium("rohacellhf71");
    if ( ! pMedrohacellhf71) Fatal("Main", "Medium rohacellhf71 not found");
	
    // --------------------------------------------------------------------------

	// --------------   Create geometry and top volume  -------------------------
    gGeoMan = (TGeoManager*)gROOT->FindObject("FAIRGeom");
    gGeoMan->SetName(geoDetectorName + "_geom");
    TGeoVolume* top = new TGeoVolumeAssembly("TOP");
    top->SetMedium(pMedAir);
    gGeoMan->SetTopVolume(top);
    //gGeoMan->SetTopVisible(1);
    // --------------------------------------------------------------------------

	TGeoCombiTrans *FFD_Position = new TGeoCombiTrans("FFD_Position", 0, 0, 0, new TGeoRotation("rot",0,0,45));	
	TGeoCombiTrans *Plane_Position = new TGeoCombiTrans("Plane_Position", 0, 0, 0, new TGeoRotation("rot11",0,0,-45));	

	TGeoTranslation *Qu_pos = new TGeoTranslation("Qu_pos", 0, 0, 2.55);
	TGeoTranslation *Pb_pos = new TGeoTranslation("Pb_pos", 0, 0, 1.);
	TGeoTranslation *Rub_pos = new TGeoTranslation("Pb_pos", 0, 0, 1.65);
												   
	TGeoCombiTrans *Fr_panel_pos = new TGeoCombiTrans("Fr_panel_pos", 0, 0, 0.25, new TGeoRotation("rot01",0,0,0));
	Fr_panel_pos->RegisterYourself();									   
	TGeoCombiTrans *Back_panel_pos = new TGeoCombiTrans("Back_panel_pos", 0, 0, 10.75, new TGeoRotation("rot02",0,0,0));											   
	Back_panel_pos->RegisterYourself();	
	TGeoCombiTrans *R_panel_pos = new TGeoCombiTrans("R_panel_pos", 3.15, 0, 5.5, new TGeoRotation("rot03",0,0,0));
	R_panel_pos->RegisterYourself();											   
	TGeoCombiTrans *L_panel_pos = new TGeoCombiTrans("L_panel_pos", -3.15, 0, 5.5, new TGeoRotation("rot04",0,0,0));											   
	L_panel_pos->RegisterYourself();
	TGeoCombiTrans *Up_panel_pos = new TGeoCombiTrans("Up_panel_pos", 0, 3.15, 5.5, new TGeoRotation("rot05",0,0,0));
	Up_panel_pos->RegisterYourself();											   
	TGeoCombiTrans *Down_panel_pos = new TGeoCombiTrans("Down_panel_pos", 0, -3.15, 5.5, new TGeoRotation("rot06",0,0,0));											   
	Down_panel_pos->RegisterYourself();
												   
	TGeoCombiTrans *Pmt_front_pos = new TGeoCombiTrans("Pmt_front_pos", 0, 0, 3.45, new TGeoRotation("rot1",0,0,0));
	Pmt_front_pos->RegisterYourself();
	TGeoCombiTrans *Pmt_back_pos = new TGeoCombiTrans("Pmt_back_pos", 0, 0, 5.65, new TGeoRotation("rot2",0,0,0));
	Pmt_back_pos->RegisterYourself();
	TGeoCombiTrans *Pmt_sideUp_pos = new TGeoCombiTrans("Pmt_sideUp_pos", 0, 2.8, 4.5, new TGeoRotation("rot3",0,0,0));
	Pmt_sideUp_pos->RegisterYourself();
	TGeoCombiTrans *Pmt_sideDown_pos = new TGeoCombiTrans("Pmt_sideDown_pos", 0, -2.8, 4.5, new TGeoRotation("rot4",0,0,0));
	Pmt_sideDown_pos->RegisterYourself();
	TGeoCombiTrans *Pmt_sideLeft_pos = new TGeoCombiTrans("Pmt_sideLeft_pos", -2.8, 0, 4.5, new TGeoRotation("rot5",0,0,0));
	Pmt_sideLeft_pos->RegisterYourself();
	TGeoCombiTrans *Pmt_sideRight_pos = new TGeoCombiTrans("Pmt_sideRight_pos", 2.8, 0, 4.5, new TGeoRotation("rot6",0,0,0));
	Pmt_sideRight_pos->RegisterYourself();

	TGeoTranslation *Pmt_pos = new TGeoTranslation("Pmt_pos", 0, 0, 0);
	TGeoTranslation *Panel_pos = new TGeoTranslation("Panel_pos", 0, 0, 0);
	
	TGeoTranslation *Electr_pos = new TGeoTranslation("Electr_pos", 0, 0, 6);
	
	TGeoTranslation *Shield_pos_1 = new TGeoTranslation("Shield_pos_1", 0, 0, 132.145);
	TGeoTranslation *Shield_pos_2 = new TGeoTranslation("Shield_pos_2", 0, 0, -132.125);
		
	TGeoTranslation *Shield_2_pos_1 = new TGeoTranslation("Shield_2_pos_1", 0, 0, 140.35);
	TGeoTranslation *Shield_2_pos_2 = new TGeoTranslation("Shield_2_pos_2", 0, 0, -140.35);
	
	TGeoTranslation *Shield_3_pos_1 = new TGeoTranslation("Shield_3_pos_1", 0, 0, 181.65);
	TGeoTranslation *Shield_3_pos_2 = new TGeoTranslation("Shield_3_pos_2", 0, 0, -181.65);
	
	TGeoTranslation *Shield_Ph_pos_1 = new TGeoTranslation("Shield_Ph_pos_1", 0, 0, 218.4);
	TGeoTranslation *Shield_Ph_pos_2 = new TGeoTranslation("Shield_Ph_pos_2", 0, 0, -218.4);
	
	TGeoCombiTrans *plane_up_pos = new TGeoCombiTrans("plane_up_pos", 0, 19.65, 157.0, new TGeoRotation("rot7",0,0,0));
	TGeoCombiTrans *plane_down_pos = new TGeoCombiTrans("plane_down_pos", 0, -19.65, 157.0, new TGeoRotation("rot8",0,0,0));
	TGeoCombiTrans *plane_left_pos = new TGeoCombiTrans("plane_left_pos", -19.65, 0, 157.0, new TGeoRotation("rot9",0,0,90));
	TGeoCombiTrans *plane_right_pos = new TGeoCombiTrans("plane_right_pos", 19.65, 0, 157.0, new TGeoRotation("rot10",0,0,90));
	
	TGeoCombiTrans *plane_up_pos_inv = new TGeoCombiTrans("plane_up_pos_inv", 0, 19.65, -157.0, new TGeoRotation("rot11",0,0,0));
	TGeoCombiTrans *plane_down_pos_inv = new TGeoCombiTrans("plane_down_pos_inv", 0, -19.65, -157.0, new TGeoRotation("rot12",0,0,0));
	TGeoCombiTrans *plane_left_pos_inv = new TGeoCombiTrans("plane_left_pos_inv", -19.65, 0, -157.0, new TGeoRotation("rot13",0,0,90));
	TGeoCombiTrans *plane_right_pos_inv = new TGeoCombiTrans("plane_right_pos_inv", 19.65, 0, -157.0, new TGeoRotation("rot14",0,0,90));
	
	//*****************************************************************************************************************
	
	TGeoTranslation *FFD_east_pos1 = new TGeoTranslation("FFD_east_pos1", mod_pos_X_1, 0, mod_pos_Z);
	TGeoTranslation *FFD_east_pos2 = new TGeoTranslation("FFD_east_pos2", mod_pos_X_2, 0, mod_pos_Z);
	TGeoTranslation *FFD_east_pos3 = new TGeoTranslation("FFD_east_pos3", -mod_pos_X_1, 0, mod_pos_Z);
	TGeoTranslation *FFD_east_pos4 = new TGeoTranslation("FFD_east_pos4", -mod_pos_X_2, 0, mod_pos_Z);
	
	TGeoTranslation *FFD_east_pos5 = new TGeoTranslation("FFD_east_pos5", 0, mod_pos_Y_1, mod_pos_Z);
	TGeoTranslation *FFD_east_pos6 = new TGeoTranslation("FFD_east_pos6", 0, mod_pos_Y_2, mod_pos_Z);
	TGeoTranslation *FFD_east_pos7 = new TGeoTranslation("FFD_east_pos7", 0, -mod_pos_Y_1, mod_pos_Z);
	TGeoTranslation *FFD_east_pos8 = new TGeoTranslation("FFD_east_pos8", 0, -mod_pos_Y_2, mod_pos_Z);
	
	TGeoTranslation *FFD_east_pos9 = new TGeoTranslation("FFD_east_pos9", mod_pos_X_3, mod_pos_Y_3, mod_pos_Z);
	TGeoTranslation *FFD_east_pos10 = new TGeoTranslation("FFD_east_pos10", -mod_pos_X_3, mod_pos_Y_3, mod_pos_Z);
	TGeoTranslation *FFD_east_pos11 = new TGeoTranslation("FFD_east_pos11", mod_pos_X_3, -mod_pos_Y_3, mod_pos_Z);
	TGeoTranslation *FFD_east_pos12 = new TGeoTranslation("FFD_east_pos12", -mod_pos_X_3, -mod_pos_Y_3, mod_pos_Z);
	
	TGeoTranslation *FFD_east_pos13 = new TGeoTranslation("FFD_east_pos13", mod_pos_X_3, mod_pos_Y_4, mod_pos_Z);
	TGeoTranslation *FFD_east_pos14 = new TGeoTranslation("FFD_east_pos14", -mod_pos_X_3, mod_pos_Y_4, mod_pos_Z);
	TGeoTranslation *FFD_east_pos15 = new TGeoTranslation("FFD_east_pos15", mod_pos_X_3, -mod_pos_Y_4, mod_pos_Z);
	TGeoTranslation *FFD_east_pos16 = new TGeoTranslation("FFD_east_pos16", -mod_pos_X_3, -mod_pos_Y_4, mod_pos_Z);
	
	TGeoTranslation *FFD_east_pos17 = new TGeoTranslation("FFD_east_pos17", mod_pos_X_4, mod_pos_Y_3, mod_pos_Z);
	TGeoTranslation *FFD_east_pos18 = new TGeoTranslation("FFD_east_pos18", -mod_pos_X_4, mod_pos_Y_3, mod_pos_Z);
	TGeoTranslation *FFD_east_pos19 = new TGeoTranslation("FFD_east_pos19", mod_pos_X_4, -mod_pos_Y_3, mod_pos_Z);
	TGeoTranslation *FFD_east_pos20 = new TGeoTranslation("FFD_east_pos20", -mod_pos_X_4, -mod_pos_Y_3, mod_pos_Z);
			
//*****************************************************************************************************************	
		
	TGeoCombiTrans *FFD_west_pos1 = new TGeoCombiTrans("FFD_west_pos1", mod_pos_X_1, 0, -mod_pos_Z, new TGeoRotation("rot1",0,180,0));
	TGeoCombiTrans *FFD_west_pos2 = new TGeoCombiTrans("FFD_west_pos2", mod_pos_X_2, 0, -mod_pos_Z, new TGeoRotation("rot2",0,180,0));
	TGeoCombiTrans *FFD_west_pos3 = new TGeoCombiTrans("FFD_west_pos3", -mod_pos_X_1, 0, -mod_pos_Z, new TGeoRotation("rot3",0,180,0));
	TGeoCombiTrans *FFD_west_pos4 = new TGeoCombiTrans("FFD_west_pos4", -mod_pos_X_2, 0, -mod_pos_Z, new TGeoRotation("rot4",0,180,0));
	
	TGeoCombiTrans *FFD_west_pos5 = new TGeoCombiTrans("FFD_west_pos5", 0, mod_pos_Y_1, -mod_pos_Z, new TGeoRotation("rot5",0,180,0));
	TGeoCombiTrans *FFD_west_pos6 = new TGeoCombiTrans("FFD_west_pos6", 0, mod_pos_Y_2, -mod_pos_Z, new TGeoRotation("rot6",0,180,0));
	TGeoCombiTrans *FFD_west_pos7 = new TGeoCombiTrans("FFD_west_pos7", 0, -mod_pos_Y_1, -mod_pos_Z, new TGeoRotation("rot7",0,180,0));
	TGeoCombiTrans *FFD_west_pos8 = new TGeoCombiTrans("FFD_west_pos8", 0, -mod_pos_Y_2, -mod_pos_Z, new TGeoRotation("rot8",0,180,0));
	
	TGeoCombiTrans *FFD_west_pos9 = new TGeoCombiTrans("FFD_west_pos9", mod_pos_X_3, mod_pos_Y_3, -mod_pos_Z, new TGeoRotation("rot9",0,180,0));
	TGeoCombiTrans *FFD_west_pos10 = new TGeoCombiTrans("FFD_west_pos10", -mod_pos_X_3, mod_pos_Y_3, -mod_pos_Z, new TGeoRotation("rot10",0,180,0));
	TGeoCombiTrans *FFD_west_pos11 = new TGeoCombiTrans("FFD_west_pos11", mod_pos_X_3, -mod_pos_Y_3, -mod_pos_Z, new TGeoRotation("rot11",0,180,0));
	TGeoCombiTrans *FFD_west_pos12 = new TGeoCombiTrans("FFD_west_pos12", -mod_pos_X_3, -mod_pos_Y_3, -mod_pos_Z, new TGeoRotation("rot12",0,180,0));
	
	TGeoCombiTrans *FFD_west_pos13 = new TGeoCombiTrans("FFD_west_pos13", mod_pos_X_3, mod_pos_Y_4, -mod_pos_Z, new TGeoRotation("rot13",0,180,0));
	TGeoCombiTrans *FFD_west_pos14 = new TGeoCombiTrans("FFD_west_pos14", -mod_pos_X_3, mod_pos_Y_4, -mod_pos_Z, new TGeoRotation("rot14",0,180,0));
	TGeoCombiTrans *FFD_west_pos15 = new TGeoCombiTrans("FFD_west_pos15", mod_pos_X_3, -mod_pos_Y_4, -mod_pos_Z, new TGeoRotation("rot15",0,180,0));
	TGeoCombiTrans *FFD_west_pos16 = new TGeoCombiTrans("FFD_west_pos16", -mod_pos_X_3, -mod_pos_Y_4, -mod_pos_Z, new TGeoRotation("rot16",0,180,0));
	
	TGeoCombiTrans *FFD_west_pos17 = new TGeoCombiTrans("FFD_west_pos17", mod_pos_X_4, mod_pos_Y_3, -mod_pos_Z, new TGeoRotation("rot17",0,180,0));
	TGeoCombiTrans *FFD_west_pos18 = new TGeoCombiTrans("FFD_west_pos18", -mod_pos_X_4, mod_pos_Y_3, -mod_pos_Z, new TGeoRotation("rot18",0,180,0));
	TGeoCombiTrans *FFD_west_pos19 = new TGeoCombiTrans("FFD_west_pos19", mod_pos_X_4, -mod_pos_Y_3, -mod_pos_Z, new TGeoRotation("rot19",0,180,0));
	TGeoCombiTrans *FFD_west_pos20 = new TGeoCombiTrans("FFD_west_pos20", -mod_pos_X_4, -mod_pos_Y_3, -mod_pos_Z, new TGeoRotation("rot20",0,180,0));
			
//*****************************************************************************************************************		


	TGeoVolume* FFD_system_top = new TGeoVolumeAssembly("FFD_system_top");
	TGeoVolume* mod_unite = new TGeoVolumeAssembly("mod_unite");
	TGeoVolume* plane_unite = new TGeoVolumeAssembly("plane_unite");

	TGeoBBox *Quart_cube = new TGeoBBox("Quart_cube", x_size_cube/2., y_size_cube/2., z_size_cube/2.);	
	TGeoBBox *Front_panel = new TGeoBBox("Front_panel", x_size_fpanel/2., y_size_fpanel/2., z_size_fpanel/2.);	
	TGeoBBox *L_r_panel = new TGeoBBox("L_r_panel", x_size_lr_panel/2., y_size_lr_panel/2., z_size_lr_panel/2.);	
	TGeoBBox *Up_d_panel = new TGeoBBox("Up_d_panel", x_size_ud_panel/2., y_size_ud_panel/2., z_size_ud_panel/2.);	
	
	TGeoBBox *Pb_plate = new TGeoBBox("Pb_plate", x_size_Pb/2., y_size_Pb/2., z_size_Pb/2.);	
	TGeoBBox *Rub_plate = new TGeoBBox("Rub_plate", x_size_Rub/2., y_size_Rub/2., z_size_Rub/2.);	
	
	TGeoBBox *Pmt_front_back = new TGeoBBox("Pmt_front_back", x_size_pmt_b_f/2., y_size_pmt_b_f/2., z_size_pmt_b_f/2.);	
	TGeoBBox *Pmt_up_down = new TGeoBBox("Pmt_up_down", x_size_pmt_up_d/2., y_size_pmt_up_d/2., z_size_pmt_up_d/2.);	
	TGeoBBox *Pmt_left_right = new TGeoBBox("Pmt_left_right", x_size_pmt_l_r/2., y_size_pmt_l_r/2., z_size_pmt_l_r/2.);	
	
	TGeoBBox *Electr_box = new TGeoBBox("Electr_box", x_size_electr/2., y_size_electr/2., z_size_electr/2.);	
	
	TGeoBBox *plane_box = new TGeoBBox("plane_box", x_size_plane/2., y_size_plane/2., z_size_plane/2.);	
	
	TGeoTube *Shield_1 = new TGeoTube("Shield_1", r_min_Shield_tube/2., r_max_Shield_tube/2., z_size_Shield_tube/2.);
	TGeoTube *Shield_2 = new TGeoTube("Shield_2", r_min_Shield_2_tube/2., r_max_Shield_2_tube/2., z_size_Shield_2_tube/2.);
	
	TGeoTube *Shield_Ph = new TGeoTube("Shield_Ph", r_min_Shield_Ph_tube/2., r_max_Shield_Ph_tube/2., z_size_Shield_Ph_tube/2.);

	TGeoCompositeShape *PMT_box = new TGeoCompositeShape("PMT_box", "Pmt_front_back:Pmt_front_pos + Pmt_front_back:Pmt_back_pos + Pmt_up_down:Pmt_sideUp_pos + Pmt_up_down:Pmt_sideDown_pos + Pmt_left_right:Pmt_sideLeft_pos + Pmt_left_right:Pmt_sideRight_pos");
	TGeoCompositeShape *Al_box = new TGeoCompositeShape("Al_box", "Front_panel:Fr_panel_pos + Front_panel:Back_panel_pos + L_r_panel:L_panel_pos + L_r_panel:R_panel_pos + Up_d_panel:Up_panel_pos + Up_d_panel:Down_panel_pos");

	
	TGeoVolume *CubeActiveVolumeV1 = new TGeoVolume("CubeActiveVolumeV1", Quart_cube);
    CubeActiveVolumeV1->SetMedium(pMedFusedSil);
    CubeActiveVolumeV1->SetLineColor(kCyan);
	
	TGeoVolume *Fpanel_Al = new TGeoVolume("Fpanel_Al", Al_box);
    Fpanel_Al->SetMedium(pMedaluminium);
    Fpanel_Al->SetLineColor(kBlue-10);
	
	TGeoVolume *Shield_Al = new TGeoVolume("Shield_Al", Shield_1);
    Shield_Al->SetMedium(pMedaluminium);
    Shield_Al->SetLineColor(kGray);
	
	TGeoVolume *Shield_2_Al = new TGeoVolume("Shield_2_Al", Shield_2);
    Shield_2_Al->SetMedium(pMedaluminium);
    Shield_2_Al->SetLineColor(kGray);
	
	TGeoVolume *Shield_Ph_Al = new TGeoVolume("Shield_Ph_Al", Shield_Ph);
    Shield_Ph_Al->SetMedium(pMedaluminium);
    Shield_Ph_Al->SetLineColor(kGray);
	
	TGeoVolume *Plate_Pb = new TGeoVolume("Plate_Pb", Pb_plate);
    Plate_Pb->SetMedium(pMedlead);
    Plate_Pb->SetLineColor(kBlue-5);
	
	TGeoVolume *Plate_Poly = new TGeoVolume("Plate_Poly", Rub_plate);
    Plate_Poly->SetMedium(pMedpolystyrene);
    Plate_Poly->SetLineColor(kOrange+2);
	
	TGeoVolume *Glass_box = new TGeoVolume("Glass_box", PMT_box);
    Glass_box->SetMedium(pMedPMTglass);
    Glass_box->SetLineColor(kYellow-9);
	
	TGeoVolume *G10_box = new TGeoVolume("G10_box", Electr_box);
    G10_box->SetMedium(pMedG10);
    G10_box->SetLineColor(kMagenta-9);
	
	TGeoVolume *roh_box = new TGeoVolume("roh_box", plane_box);
    roh_box->SetMedium(pMedrohacellhf71);
    roh_box->SetLineColor(kRed-10);
  
	
	mod_unite->AddNode(CubeActiveVolumeV1, 1, Qu_pos);
	mod_unite->AddNode(Fpanel_Al, 2, Panel_pos);
	mod_unite->AddNode(Plate_Pb, 3, Pb_pos);
	mod_unite->AddNode(Plate_Poly, 4, Rub_pos);
	mod_unite->AddNode(Glass_box, 5, Pmt_pos);
	mod_unite->AddNode(G10_box, 6, Electr_pos);
	
	FFD_system_top->AddNode(mod_unite, 7, FFD_east_pos1);	
	FFD_system_top->AddNode(mod_unite, 8, FFD_east_pos2);
	FFD_system_top->AddNode(mod_unite, 9, FFD_east_pos3);
	FFD_system_top->AddNode(mod_unite, 10, FFD_east_pos4);
	
	FFD_system_top->AddNode(mod_unite, 11, FFD_east_pos5);
	FFD_system_top->AddNode(mod_unite, 12, FFD_east_pos6);
	FFD_system_top->AddNode(mod_unite, 13, FFD_east_pos7);
	FFD_system_top->AddNode(mod_unite, 14, FFD_east_pos8);
	
	FFD_system_top->AddNode(mod_unite, 15, FFD_east_pos9);
	FFD_system_top->AddNode(mod_unite, 16, FFD_east_pos10);
	FFD_system_top->AddNode(mod_unite, 17, FFD_east_pos11);
	FFD_system_top->AddNode(mod_unite, 18, FFD_east_pos12);
	
	FFD_system_top->AddNode(mod_unite, 19, FFD_east_pos13);
	FFD_system_top->AddNode(mod_unite, 20, FFD_east_pos14);
	FFD_system_top->AddNode(mod_unite, 21, FFD_east_pos15);
	FFD_system_top->AddNode(mod_unite, 22, FFD_east_pos16);
	
	FFD_system_top->AddNode(mod_unite, 23, FFD_east_pos17);
	FFD_system_top->AddNode(mod_unite, 24, FFD_east_pos18);
	FFD_system_top->AddNode(mod_unite, 25, FFD_east_pos19);
	FFD_system_top->AddNode(mod_unite, 26, FFD_east_pos20);
	
	FFD_system_top->AddNode(mod_unite, 27, FFD_west_pos1);
	FFD_system_top->AddNode(mod_unite, 28, FFD_west_pos2);
	FFD_system_top->AddNode(mod_unite, 29, FFD_west_pos3);
	FFD_system_top->AddNode(mod_unite, 30, FFD_west_pos4);
	
	FFD_system_top->AddNode(mod_unite, 31, FFD_west_pos5);
	FFD_system_top->AddNode(mod_unite, 32, FFD_west_pos6);
	FFD_system_top->AddNode(mod_unite, 33, FFD_west_pos7);
	FFD_system_top->AddNode(mod_unite, 34, FFD_west_pos8);
	
	FFD_system_top->AddNode(mod_unite, 35, FFD_west_pos9);
	FFD_system_top->AddNode(mod_unite, 36, FFD_west_pos10);
	FFD_system_top->AddNode(mod_unite, 37, FFD_west_pos11);
	FFD_system_top->AddNode(mod_unite, 38, FFD_west_pos12);
	
	FFD_system_top->AddNode(mod_unite, 39, FFD_west_pos13);
	FFD_system_top->AddNode(mod_unite, 40, FFD_west_pos14);
	FFD_system_top->AddNode(mod_unite, 41, FFD_west_pos15);
	FFD_system_top->AddNode(mod_unite, 42, FFD_west_pos16);
	
	FFD_system_top->AddNode(mod_unite, 43, FFD_west_pos17);
	FFD_system_top->AddNode(mod_unite, 44, FFD_west_pos18);
	FFD_system_top->AddNode(mod_unite, 45, FFD_west_pos19);
	FFD_system_top->AddNode(mod_unite, 46, FFD_west_pos20);
	
	FFD_system_top->AddNode(Shield_Al, 47, Shield_pos_1);
	FFD_system_top->AddNode(Shield_Al, 48, Shield_pos_2);
	
	FFD_system_top->AddNode(Shield_2_Al, 49, Shield_2_pos_1);
	FFD_system_top->AddNode(Shield_2_Al, 50, Shield_2_pos_2);

	FFD_system_top->AddNode(Shield_Al, 51, Shield_3_pos_1);
	FFD_system_top->AddNode(Shield_Al, 52, Shield_3_pos_2);
	
	FFD_system_top->AddNode(Shield_Ph_Al, 51, Shield_Ph_pos_1);
	FFD_system_top->AddNode(Shield_Ph_Al, 52, Shield_Ph_pos_2);
	
	
	
	plane_unite->AddNode(roh_box, 53, plane_up_pos);
	plane_unite->AddNode(roh_box, 54, plane_down_pos);
	plane_unite->AddNode(roh_box, 55, plane_left_pos);
	plane_unite->AddNode(roh_box, 55, plane_right_pos);
	
	plane_unite->AddNode(roh_box, 54, plane_up_pos_inv);
	plane_unite->AddNode(roh_box, 55, plane_down_pos_inv);
	plane_unite->AddNode(roh_box, 56, plane_left_pos_inv);
	plane_unite->AddNode(roh_box, 57, plane_right_pos_inv);
	
	FFD_system_top->AddNode(plane_unite, 58, Plane_Position);
	
	top->AddNode(FFD_system_top, 1, FFD_Position);
	//top->AddNode(plane_unite, 1, Plane_Position);
    
    top->SetVisContainers(kTRUE);
    
    // ---------------   Finish   -----------------------------------------------
    gGeoMan->CloseGeometry();
    gGeoMan->CheckOverlaps(0.0001);
    gGeoMan->PrintOverlaps();
    gGeoMan->Test();
    
    TFile* geoFile = new TFile(geoFileName, "RECREATE");
    top->Write();
    geoFile->Close();
    top->Draw("ogl");
    //top->Draw("");
}
