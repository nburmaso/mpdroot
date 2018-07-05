#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"
#include "TFile.h"
#include "TMath.h"

R__ADD_INCLUDE_PATH($VMCWORKDIR)
#include "macro/mpd/mpdloadlibs.C"

Double_t initDist = 1735.0; // Distance from interacting point, mm

const Double_t innerRadius = 150.0; // Layer inner radius, mm
const Double_t outerRadius = 500.0; // Layer outer radius, mm

Double_t layerThickness = 70.0;

//media
TGeoMedium *pMedAir = 0;
TGeoMedium *pMedArCO28020 = 0;
TGeoMedium *pMedRohacellhf71 = 0;
TGeoMedium *pMedG10 = 0;
TGeoMedium *pMedCopper = 0;


void DefineRequiredMedia(FairGeoMedia* geoMedia, FairGeoBuilder* geoBuild) {
    //air medium
    FairGeoMedium* mAir = geoMedia->getMedium("air");
    if ( ! mAir ) Fatal("Main", "FairMedium air not found");
    geoBuild->createMedium(mAir);
    pMedAir = gGeoManager->GetMedium("air");
    if ( ! pMedAir ) Fatal("Main", "Medium air not found");

    //aluminium medium
    FairGeoMedium* mArCO28020 = geoMedia->getMedium("arco28020");
    if ( ! mArCO28020  ) Fatal("Main", "FairMedium Argon CO2 (80/20) not found");
    geoBuild->createMedium(mArCO28020);
    pMedArCO28020  = gGeoManager->GetMedium("arco28020");
    if ( ! pMedArCO28020  ) Fatal("Main", "Medium Argon CO2 (80/20) not found");

    //rohacellhf71 medium
    FairGeoMedium* mRohacellhf71 = geoMedia->getMedium("rohacellhf71");
    if ( ! mRohacellhf71 ) Fatal("Main", "FairMedium rohacellhf71 not found");
    geoBuild->createMedium(mRohacellhf71);
    pMedRohacellhf71 = gGeoManager->GetMedium("rohacellhf71");
    if ( ! pMedRohacellhf71  ) Fatal("Main", "Medium rohacellhf71 not found");

    //G10 medium
    FairGeoMedium* mG10 = geoMedia->getMedium("G10");
    if ( ! mG10 ) Fatal("Main", "FairMedium G10 not found");
    geoBuild->createMedium(mG10);
    pMedG10 = gGeoManager->GetMedium("G10");
    if ( ! pMedG10 ) Fatal("Main", "Medium G10 not found");

    //copper medium
    FairGeoMedium* mCopper = geoMedia->getMedium("copper");
    if ( ! mCopper ) Fatal("Main", "FairMedium copper not found");
    geoBuild->createMedium(mCopper);
    pMedCopper = gGeoManager->GetMedium("copper");
    if ( ! pMedCopper ) Fatal("Main", "Medium copper not found");
}


void create_rootgeom_cpc2(Bool_t wrGeoWithMaterials = false, Bool_t fatOuterG10Layers = true) {

    // Load necessary libraries
    //gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(); // load libraries

    // ----  set working directory  --------------------------------------------
    TString gPath = gSystem->Getenv("VMCWORKDIR");

    // -------   Geometry file name (output)   ---------------------------------
    const TString geoDetectorName = "cpc";
	TString geoFileName = gPath + "/geometry/" + geoDetectorName + ".root";
    //const TString geoDetectorVersion = "v2";
    //TString geoFileName = gPath + "/geometry/" + geoDetectorName + "_"+ geoDetectorVersion + ".root";
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
    gGeoManager->SetName(geoDetectorName + "_geom");
    TGeoVolume* top = new TGeoVolumeAssembly("TOP");
    top->SetMedium(pMedAir);
    gGeoManager->SetTopVolume(top);
    //gGeoMan->SetTopVisible(1);
    // --------------------------------------------------------------------------

    // Define CPC Geometry
    
    TString cpc_cave_name = "cpc01l";
    TGeoTube *cpcCaveS = new TGeoTube(cpc_cave_name, innerRadius, outerRadius, layerThickness / 2.0);
    TGeoVolume *cpcCaveV = new TGeoVolume(cpc_cave_name, cpcCaveS);
    cpcCaveV->SetMedium(pMedAir);
    cpcCaveV->SetVisibility(kFALSE);
    cpcCaveV->SetTransparency(95);
    
    //     panel definition
    TString cpc_layer_name = "cpc01sl";
    TGeoTube *cpcPanelS = new TGeoTube(cpc_layer_name, innerRadius, outerRadius, 10.0 / 2.0);
    TGeoVolume *cpcPanelV = new TGeoVolume(cpc_layer_name, cpcPanelS);
    cpcPanelV->SetMedium(pMedAir);
    cpcPanelV->SetLineColor(kGreen);
    cpcPanelV->SetTransparency(95);
    
    for (Int_t i = 0; i < 2; i++)
    {
        TGeoTranslation * cpc_panel_position = new TGeoTranslation("cpc_panel_position", 0.0, 0.0, -10.0 + 20.0 * i);
        cpcCaveV->AddNode(cpcPanelV, i, cpc_panel_position);
    }
	
    //     arco centr-volume
    cpc_layer_name = "cpc01al";
    TGeoTube *cpcACVolS = new TGeoTube(cpc_layer_name, innerRadius, outerRadius, 10.0 / 2.0);
    TGeoVolume *cpcACVolV = new TGeoVolume(cpc_layer_name, cpcACVolS);
    cpcACVolV->SetMedium(pMedArCO28020);
    cpcACVolV->SetLineColor(kRed);
    cpcACVolV->SetTransparency(75);
    
    for (Int_t i = -1; i < 2; i++)
    {
        TGeoTranslation * cpc_acv_position = new TGeoTranslation("cpc_arco_centr-volume_position", 0.0, 0.0, 20.0 * i);
        cpcCaveV->AddNode(cpcACVolV, i + 1, cpc_acv_position);
    }
    
    //    base definition
    cpc_layer_name = "cpc01osn";
    TGeoTube *cpcBaseS = new TGeoTube(cpc_layer_name, innerRadius, outerRadius, 9.19 / 2.0);
    TGeoVolume *cpcBaseV = new TGeoVolume(cpc_layer_name, cpcBaseS);
    cpcBaseV->SetMedium(pMedG10);
    cpcBaseV->SetLineColor(kBlue);
    cpcBaseV->SetTransparency(65);
    
    TGeoTranslation * cpc_base_position = new TGeoTranslation("cpc_base_position", 0.0, 0.0, 0.0);
    cpcPanelV->AddNode(cpcBaseV, 0, cpc_base_position);
    
    //    layer from rohacell definition
    cpc_layer_name = "cpc01osnrh";
    TGeoTube *cpcRHLayS = new TGeoTube(cpc_layer_name, innerRadius, outerRadius, (9.99 / 2.0 - 9.19 / 2.0) / 2.0);
    TGeoVolume *cpcRHLayV = new TGeoVolume(cpc_layer_name, cpcRHLayS);
    cpcRHLayV->SetMedium(pMedRohacellhf71);
    cpcRHLayV->SetLineColor(kMagenta);
    cpcRHLayV->SetTransparency(65);
    
    TGeoTranslation * cpc_rhlay_position_right = new TGeoTranslation("cpc_layer_from_rohacell_right_position", 0.0, 0.0, ((9.99 + 9.19) / 2.0) / 2.0);
    cpcPanelV->AddNode(cpcRHLayV, 0, cpc_rhlay_position_right);
    TGeoTranslation * cpc_rhlay_position_left = new TGeoTranslation("cpc_layer_from_rohacell_left_position", 0.0, 0.0, -((9.99 + 9.19) / 2.0) / 2.0);
    cpcPanelV->AddNode(cpcRHLayV, 1, cpc_rhlay_position_left);
    
    //     volume for R pads
    cpc_layer_name = "cpc01vrl";
    TGeoTube *cpcRPadsS = new TGeoTube(cpc_layer_name, innerRadius, outerRadius, (10.0 / 2.0 - 9.99 / 2.0) / 2.0);
    TGeoVolume *cpcRPadsV = new TGeoVolume(cpc_layer_name, cpcRPadsS);
    cpcRPadsV->SetMedium(pMedAir);
    cpcRPadsV->SetLineColor(kYellow);
    
    TGeoTranslation * cpc_rpads_position = new TGeoTranslation("cpc_r_pads_position", 0.0, 0.0, -((10.0 + 9.99) / 2.0) / 2.0);
    cpcPanelV->AddNode(cpcRPadsV, 0, cpc_rpads_position);
    
    //      volume for Fi
    cpc_layer_name = "cpc01vfl";
    TGeoTube *cpcFiPadsS = new TGeoTube(cpc_layer_name, innerRadius, outerRadius, (10.0 / 2.0 - 9.99 / 2.0) / 2.0);
    TGeoVolume *cpcFiPadsV = new TGeoVolume(cpc_layer_name, cpcFiPadsS);
    cpcFiPadsV->SetMedium(pMedAir);
    cpcFiPadsV->SetLineColor(kOrange);
    
    TGeoTranslation * cpc_fipads_position = new TGeoTranslation("cpc_fi_pads_position", 0.0, 0.0, ((10.0 + 9.99) / 2.0) / 2.0);
    cpcPanelV->AddNode(cpcFiPadsV, 0, cpc_fipads_position);
    
    //    sector in Fi
    cpc_layer_name = "cpc01sectu";
    Double_t padRealOuterRadius = outerRadius * TMath::Cos(1.125 / 2.0 * TMath::DegToRad());
    TGeoPgon *cpcFiSectorS = new TGeoPgon(cpc_layer_name, 0.0, 1.125, 1, 2);
    cpcFiSectorS->DefineSection(0, -0.005 / 2.0, innerRadius, padRealOuterRadius);
    cpcFiSectorS->DefineSection(1, 0.005 / 2.0, innerRadius, padRealOuterRadius);
    TGeoVolume *cpcFiSectorV = new TGeoVolume(cpc_layer_name, cpcFiSectorS);
    cpcFiSectorV->SetMedium(pMedAir);
    cpcFiSectorV->SetLineColor(kTeal);

    Double_t angle = 0.0;
    for(Int_t i = 0; i < 320; i++)
    {
        angle += 1.125;
        TGeoRotation * cpc_fi_sector_rot = new TGeoRotation("cpc_fi_sector_rot", 0.0, 0.0, angle);
        cpcFiPadsV->AddNode(cpcFiSectorV, i, cpc_fi_sector_rot);
    }

    //    padFi1
    Double_t padMovX = 0.2/TMath::Sin(1.125*TMath::DegToRad());
    Double_t innerRadiusPad = innerRadius - padMovX;
    Double_t outerRadiusPad = padRealOuterRadius - padMovX;

    cpc_layer_name = "cpc01padR1";
    TGeoPgon *cpcFiPad1S = new TGeoPgon(cpc_layer_name, 0.0, 1.125, 1, 2);
    cpcFiPad1S->DefineSection(0, -0.005 / 2.0, innerRadius, 200.0 - padMovX);
    cpcFiPad1S->DefineSection(1, 0.005 / 2.0, innerRadius, 200.0 - padMovX);
    TGeoVolume *cpcFiPad1V = new TGeoVolume(cpc_layer_name, cpcFiPad1S);
    cpcFiPad1V->SetMedium(pMedCopper);
    cpcFiPad1V->SetLineColor(kViolet);
    cpcFiPad1V->SetTransparency(45);

    TGeoTranslation * cpc_fipad1_position = new TGeoTranslation("cpc_fi_pad1_position", padMovX, 0.0, 0.0);
    cpcFiSectorV->AddNode(cpcFiPad1V, 0, cpc_fipad1_position);

    //    padFi2
    cpc_layer_name = "cpc01padR2";
    TGeoPgon *cpcFiPad2S = new TGeoPgon(cpc_layer_name, 0.0, 1.125, 1, 2);
    cpcFiPad2S->DefineSection(0, -0.005 / 2.0, 205.0 - padMovX, 300.0 - padMovX);
    cpcFiPad2S->DefineSection(1, 0.005 / 2.0, 205.0 - padMovX, 300.0 - padMovX);
    TGeoVolume *cpcFiPad2V = new TGeoVolume(cpc_layer_name, cpcFiPad2S);
    cpcFiPad2V->SetMedium(pMedCopper);
    cpcFiPad2V->SetLineColor(kViolet);

    TGeoTranslation * cpc_fipad2_position = new TGeoTranslation("cpc_fi_pad2_position", padMovX, 0.0, 0.0);
    cpcFiSectorV->AddNode(cpcFiPad2V, 0, cpc_fipad2_position);

    //    padFi3
    cpc_layer_name = "cpc01padR3";
    TGeoPgon *cpcFiPad3S = new TGeoPgon(cpc_layer_name, 0.0, 1.125, 1, 2);
    cpcFiPad3S->DefineSection(0, -0.005 / 2.0, 305.0 - padMovX, outerRadiusPad);
    cpcFiPad3S->DefineSection(1, 0.005 / 2.0, 305.0 - padMovX, outerRadiusPad);
    TGeoVolume *cpcFiPad3V = new TGeoVolume(cpc_layer_name, cpcFiPad3S);
    cpcFiPad3V->SetMedium(pMedCopper);
    cpcFiPad3V->SetLineColor(kViolet);

    TGeoTranslation * cpc_fipad3_position = new TGeoTranslation("cpc_fi_pad3_position", padMovX, 0.0, 0.0);
    cpcFiSectorV->AddNode(cpcFiPad3V, 0, cpc_fipad3_position);

    //    sector in R
    cpc_layer_name = "cpc01sectd";
    Double_t phidRealOuterRadius = outerRadius * TMath::Cos((10.0 / 4.0) / 2.0 * TMath::DegToRad());
    TGeoPgon *cpcRSectorS = new TGeoPgon(cpc_layer_name, 0.0, 10.0, 4, 2);
    cpcRSectorS->DefineSection(0, -0.005 / 2.0, innerRadius, phidRealOuterRadius);
    cpcRSectorS->DefineSection(1, 0.005 / 2.0, innerRadius, phidRealOuterRadius);
    TGeoVolume *cpcRSectorV = new TGeoVolume(cpc_layer_name, cpcRSectorS);
    cpcRSectorV->SetMedium(pMedAir);
    cpcRSectorV->SetLineColor(kTeal);

    angle = 0.0;
    for(Int_t i = 0; i < 36; i++)
    {
        angle += 10.0;
        TGeoRotation * cpc_r_sector_rot = new TGeoRotation("cpc_r_sector_rot", 0.0, 0.0, angle);
        cpcRPadsV->AddNode(cpcRSectorV, i, cpc_r_sector_rot);
    }

    TString name;
    Double_t phiMovX = 0.2 / TMath::Sin(10.0 * TMath::DegToRad());
    Double_t innerRadiusPhi = innerRadius - phiMovX;
    Double_t outerRadiusPhi = phidRealOuterRadius - phiMovX;
    Double_t step = (outerRadiusPhi - innerRadiusPhi) / 31.0;

    for (Int_t i = 0; i < 31; i++)
    {
        name = "cpc01padPhi";
        name += TString::Itoa(i, 10);

        TGeoPgon *cpcPhiSectorS = new TGeoPgon(name, 0.0, 10.0, 4, 2);
        cpcPhiSectorS->DefineSection(0, -0.005 / 2.0, innerRadiusPhi + step * i + 0.2, innerRadiusPhi + step * (i+1));
        cpcPhiSectorS->DefineSection(1, 0.005 / 2.0, innerRadiusPhi + step * i + 0.2, innerRadiusPhi + step * (i+1));
        TGeoVolume *cpcPhiSectorV = new TGeoVolume(name, cpcPhiSectorS);
        cpcPhiSectorV->SetMedium(pMedCopper);
        cpcPhiSectorV->SetLineColor(kTeal);

        TGeoTranslation * cpc_phipad_position = new TGeoTranslation("cpc_phi_pad" + TString::Itoa(i, 10) + "_position", phiMovX, 0.0, 0.0);
        cpcRSectorV->AddNode(cpcPhiSectorV, 0, cpc_phipad_position);	
    }
    
    //     panel left definition
    cpc_layer_name = "cpc01sl2";
    TGeoTube *cpcPanellS = new TGeoTube(cpc_layer_name, innerRadius, outerRadius, 10.0 / 2.0);
    TGeoVolume *cpcPanellV = new TGeoVolume(cpc_layer_name, cpcPanellS);
    cpcPanellV->SetMedium(pMedAir);
    cpcPanellV->SetLineColor(kGreen);
    cpcPanellV->SetTransparency(95);
    TGeoTranslation * cpc_panell_position = new TGeoTranslation("cpc_panell_position", 0.0, 0.0, -30.0);
    cpcCaveV->AddNode(cpcPanellV, 0, cpc_panell_position);
      
    //    layer from rohacell left definition
    //  copy from center cayers instead of create new ones
    cpcPanellV->AddNode(cpcRHLayV, 0, cpc_rhlay_position_right);
    
    //      volume for Fi left
    //  copy from center cayers instead of create new ones
    cpcPanellV->AddNode(cpcFiPadsV, 0, cpc_fipads_position);
    
    //     panel right definition
    cpc_layer_name = "cpc01sl3";
    TGeoTube * cpcPanelrS = new TGeoTube(cpc_layer_name, innerRadius, outerRadius, 10.0 / 2.0);
    TGeoVolume * cpcPanelrV = new TGeoVolume(cpc_layer_name, cpcPanelrS);
    cpcPanelrV->SetMedium(pMedAir);
    cpcPanelrV->SetLineColor(kGreen);
    cpcPanelrV->SetTransparency(95);
    TGeoTranslation * cpc_panelr_position = new TGeoTranslation("cpc_panelr_position", 0.0, 0.0, 30.0);
    cpcCaveV->AddNode(cpcPanelrV, 0, cpc_panelr_position);
    
    //    layer from rohacell right
    //  copy from center cayers instead of create new ones
    cpcPanelrV->AddNode(cpcRHLayV, 0, cpc_rhlay_position_left);
    
    //     volume for R pads right
    //  copy from center cayers instead of create new ones
    cpcPanelrV->AddNode(cpcRPadsV, 0, cpc_rpads_position);
    
    if (fatOuterG10Layers)
    {
        //    base left definition
        cpc_layer_name = "cpc01osn2";
        TGeoTube *cpcBasefatS = new TGeoTube(cpc_layer_name, innerRadius, outerRadius, ((10.0 + 9.19) / 2.0) / 2.0);
        TGeoVolume *cpcBasefatV = new TGeoVolume(cpc_layer_name, cpcBasefatS);
        cpcBasefatV->SetMedium(pMedG10);
        cpcBasefatV->SetLineColor(kBlue);
        cpcBasefatV->SetTransparency(65);

        TGeoTranslation * cpc_basel_position = new TGeoTranslation("cpc_basel_position", 0.0, 0.0, -((10.0 - 9.19) / 2.0) / 2.0);
        cpcPanellV->AddNode(cpcBasefatV, 0, cpc_basel_position);
        
        //    base right definition    
        TGeoTranslation * cpc_baser_position = new TGeoTranslation("cpc_baser_position", 0.0, 0.0, ((10.0 - 9.19) / 2.0) / 2.0);
        cpcPanelrV->AddNode(cpcBasefatV, 0, cpc_baser_position);
    }
    else
    {
        //  copy from center cayers instead of create new ones
        //    base left definition
        cpcPanellV->AddNode(cpcBaseV, 0, cpc_base_position);
        //    base right definition
        cpcPanelrV->AddNode(cpcBaseV, 0, cpc_base_position);
    }
    
    
    //CPC position
    TGeoTranslation * cpc_position0 = new TGeoTranslation("cpc_position0", 0.0, 0.0, initDist);
    
    TGeoRotation * cpc_rot1 = new TGeoRotation("cpc_rot1", 0.0, 180.0, 0.0);
    TGeoCombiTrans * cpc_position1 = new TGeoCombiTrans("cpc_position1", 0.0, 0.0, -initDist, cpc_rot1);
    
    TGeoRotation * cpc_rot2 = new TGeoRotation("cpc_rot2", 0.0, 180.0, 0.0);
    TGeoCombiTrans * cpc_position2 = new TGeoCombiTrans("cpc_position2", 0.0, 0.0, -initDist - 320.0, cpc_rot2);
    
    TGeoTranslation * cpc_position3 = new TGeoTranslation("cpc_position3", 0.0, 0.0, initDist + 320.0);
    
	top->AddNode(cpcCaveV, 0, cpc_position0);
    top->AddNode(cpcCaveV, 1, cpc_position1);
    top->AddNode(cpcCaveV, 2, cpc_position2);
    top->AddNode(cpcCaveV, 3, cpc_position3);

    top->SetVisContainers(kTRUE);


    // ---------------   Finish   ----------------------------------------------
    gGeoManager->CloseGeometry();
    gGeoManager->CheckOverlaps(0.001);
    gGeoManager->PrintOverlaps();

    gGeoManager->Test();

    TFile* geoFile = new TFile(geoFileName, "RECREATE");
    top->Write();
    geoFile->Close();

	if (wrGeoWithMaterials)
	{
		TString geoFile_wMat = gPath + "/geometry/" + geoDetectorName + "_with_materials.root";
		//TString geoFile_wMat = gPath + "/geometry/" + geoDetectorName + "_"+ geoDetectorVersion + "_with_materials.root";
		gGeoManager->Export(geoFile_wMat);
	}

    top->Draw("ogl");
    TGLViewer *v = (TGLViewer*)gPad->GetViewer3D();
    v->SetStyle(TGLRnrCtx::kOutline);
    //top->Draw("");
}