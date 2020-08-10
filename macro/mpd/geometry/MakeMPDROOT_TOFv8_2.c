//**************************************************************************************
// MPDROOT TOF version 8
// ??????????????????
// 02.07.2019
//**************************************************************************************

R__ADD_INCLUDE_PATH($VMCWORKDIR)
#include "macro/mpd/mpdloadlibs.C"

#include "TString.h"
#include "TStopwatch.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"

#include <iostream>
using namespace std;


void RecursiveVisible(TGeoVolume* vol)
{
   vol->SetVisibility();
   vol->VisibleDaughters();
   Int_t n_daughters = vol->GetNdaughters();
   for (Int_t i=0; i<n_daughters; i++) {
      RecursiveVisible(vol->GetNode(i)->GetVolume());
   }
}

void RecursiveVisible()
{
  RecursiveVisible(gGeoManager->GetTopVolume());
}


//---------------------------------------------------------------------------------------
void MakeMPDROOT_TOFv8_2()
{
    gSystem->Load("libGeom");

    mpdloadlibs(); // load libraries

    // -------   Load media from media file   --------------------------------
    FairGeoLoader*    geoLoad = new FairGeoLoader("TGeo","FairGeoLoader");
    FairGeoInterface* geoFace = geoLoad->getGeoInterface();
    TString geoPath = gSystem->Getenv("VMCWORKDIR");
    TString medFile = geoPath + "/geometry/media.geo";
    geoFace->setMediaFile(medFile);
    geoFace->readMedia();

    // -------   Geometry file name (output)   -------------------------------
    const TString geoDetectorName = "tof1";
    const TString geoDetectorVersion = "v8";
    const TString geoFileName = geoPath + "/geometry/tof_v8_2.root";

    // -----------------   Get and create the required media    --------------
    FairGeoMedia*   geoMedia = geoFace->getMedia();
    FairGeoBuilder* geoBuild = geoLoad->getGeoBuilder();

    // Air
    FairGeoMedium* matAir = geoMedia->getMedium("air");	if(! matAir) Fatal("Main", "FairMedium air not found");
    geoBuild->createMedium(matAir);
    TGeoMedium* medAir = gGeoManager->GetMedium("air");		if(! medAir) Fatal("Main", "Medium air not found");

    // Polypropylene
	FairGeoMedium* matPprop = geoMedia->getMedium("polypropylene");	if(! matPprop) Fatal("Main", "FairMedium polypropylene not found");
	geoBuild->createMedium(matPprop);
	TGeoMedium* medPprop = gGeoManager->GetMedium("polypropylene");	if(! medPprop) Fatal("Main", "Medium polypropylene not found");

	// aluminium
	FairGeoMedium* matAl = geoMedia->getMedium("aluminium");	if(! matAl) Fatal("Main", "FairMedium aluminium not found");
	geoBuild->createMedium(matAl);
	TGeoMedium* medAl = gGeoManager->GetMedium("aluminium");	if(! medAl) Fatal("Main", "Medium aluminium not found");

	// copper
	FairGeoMedium* matCu = geoMedia->getMedium("copper");	if(! matCu) Fatal("Main", "FairMedium copper not found");
	geoBuild->createMedium(matCu);
	TGeoMedium* medCu = gGeoManager->GetMedium("copper");	if(! medCu) Fatal("Main", "Medium copper not found");

	// RPCglass
	FairGeoMedium* matGlass = geoMedia->getMedium("RPCglass");	if(! matGlass) Fatal("Main", "FairMedium RPCglass not found");
	geoBuild->createMedium(matGlass);
	TGeoMedium* medGlass = gGeoManager->GetMedium("RPCglass");	if(! medGlass) Fatal("Main", "Medium RPCglass not found");

	// RPCgas
	FairGeoMedium* matGas = geoMedia->getMedium("RPCgas");	if(! matGas) Fatal("Main", "FairMedium RPCgas not found");
	geoBuild->createMedium(matGas);
	TGeoMedium* medGas = gGeoManager->GetMedium("RPCgas");	if(! medGas) Fatal("Main", "Medium RPCgas not found");

	// G10
	FairGeoMedium* matG10 = geoMedia->getMedium("G10");	if(! matG10) Fatal("Main", "FairMedium G10 not found");
	geoBuild->createMedium(matG10);
	TGeoMedium* medG10 = gGeoManager->GetMedium("G10");		if(! medG10) Fatal("Main", "Medium G10 not found");

    // -----------------------------------------   Create geometry and top volume  -----------------------------------------------
    gGeoManager = (TGeoManager*)gROOT->FindObject("FAIRGeom");
    gGeoManager->SetName(geoDetectorName + "_geom");
    TGeoVolume* topTof = new TGeoVolumeAssembly("TOP"); // TOP VOLUME  LSP123

    gGeoManager->SetTopVolume(topTof); // LSP123
    gGeoManager->SetTopVisible();
 //   gGeoManager->SetVisLevel(7);

    
    
       TGeoVolume* top = new TGeoVolumeAssembly("tof1"); // TOP tof volume LSP123
    
    // ------------------------------------------------------ Create sector -------------
    const size_t Nsectors =  14;
    const double tofRmin = 147.5, tofRmax = 165.5, tofZ = 292.*2.;

    const double trap_dY =  (tofRmax - tofRmin)/2.;
    const double trap_dZ =  tofZ/2.;

    const double sectorAngle_2 = 25.5 /2. * TMath::DegToRad(); // [rad]
    const double trap_dXmax = tofRmin * TMath::Tan(sectorAngle_2);
    const double trap_dXmin = tofRmax * TMath::Tan(sectorAngle_2);

    // TGeoVolume *vSector = gGeoManager->MakeTrap("tof1Sector", medAir, trap_dZ, 0, 0, trap_dY, trap_dXmax, trap_dXmin, 0, trap_dY, trap_dXmax, trap_dXmin, 0);
    // vSector->SetLineWidth(2);
    // vSector->SetInvisible();

    /*
        замена коробки с воздухом на VolumeAssembly
    */

    TGeoVolumeAssembly *vSector = new TGeoVolumeAssembly("tof1Sector");

    // ----------------------------------------------------------- ASSEMBLE sector ----------------------------------------------
    const float sectorBoxX = 66.2, sectorBoxY = 16.3, sectorBoxZ = 291.8; // [cm]  SIZE OF ALL PART
    const double SecBoxZshift =  -291.2/2.-0.3;
    const double SecBox2Zshift =  281.2/2.+0.3;

    //--------------Dlinnij Student--------------
    ///////////////////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////////////////////////
    //------------------ Cable Holder ----------------------------------------------------------------------------
   
    // Body
    const float CableHolderBodyX = 65.4, CableHolderBodyY = 4.5, CableHolderBodyZ = 0.6;
    TGeoBBox *sCableHolderBody = new TGeoBBox("sCableHolderBody", CableHolderBodyX/2, CableHolderBodyY/2,CableHolderBodyZ/2 );

    // Cut Boxes
    const float TrimBox1X = 6.5, TrimBox1Y = 1.4, TrimBox1Z = 0.6;
    TGeoBBox *sTrimBox1 = new TGeoBBox("sTrimBox1", TrimBox1X/2, TrimBox1Y/2,TrimBox1Z );
    TGeoTranslation *tTrimBox1_1 = new TGeoTranslation("tTrimBox1_1",12.35, -0.24 , 0.);
    TGeoTranslation *tTrimBox1_2 = new TGeoTranslation("tTrimBox1_2", -12.35, -0.24, 0.);
    tTrimBox1_1->RegisterYourself();
    tTrimBox1_2->RegisterYourself();
    TGeoCompositeShape *sAllTrimBoxes1 = new TGeoCompositeShape("sAllTrimBoxes1",
    "sTrimBox1:tTrimBox1_1+sTrimBox1:tTrimBox1_2");

    const float TrimBox2X = 6.5, TrimBox2Y = 2.89, TrimBox2Z = 0.6;
    TGeoBBox *sTrimBox2 = new TGeoBBox("sTrimBox2", TrimBox2X/2, TrimBox2Y/2,TrimBox2Z );
    TGeoTranslation *tTrimBox2_1 = new TGeoTranslation("tTrimBox2_1", 12.35, 0.505, 0.);
    TGeoTranslation *tTrimBox2_2 = new TGeoTranslation("tTrimBox2_2", -12.35, 0.505, 0.);
    tTrimBox2_1->RegisterYourself();
    tTrimBox2_2->RegisterYourself();
    TGeoCompositeShape *sAllTrimBoxes2 = new TGeoCompositeShape("sAllTrimBoxes2",
    "sTrimBox2:tTrimBox2_1+sTrimBox2:tTrimBox2_2");

    // Cut Holes
    const float TrimTubeMin = 0, TrimTubeMax = 1.8, TrimTubeZ = CableHolderBodyZ*2;
    TGeoTube *sTrimTube = new TGeoTube("sTrimTube", TrimTubeMin, TrimTubeMax, TrimTubeZ);

    const float BetweenHoles = 0.7;
    TGeoTranslation *tTrimTube1 = new TGeoTranslation("tTrimTube1", TrimTubeMax+BetweenHoles/2, 0., 0. );
    TGeoTranslation *tTrimTube2 = new TGeoTranslation("tTrimTube2", -(TrimTubeMax+BetweenHoles/2), 0., 0. );
    TGeoTranslation *tTrimTube3 = new TGeoTranslation("tTrimTube3", TrimTubeMax*3 + BetweenHoles*1.5, 0. ,0. );
    TGeoTranslation *tTrimTube4 = new TGeoTranslation("tTrimTube4", -(TrimTubeMax*3 + BetweenHoles*1.5), 0. ,0. );
    tTrimTube1->RegisterYourself();
    tTrimTube2->RegisterYourself();
    tTrimTube3->RegisterYourself();
    tTrimTube4->RegisterYourself();

    TGeoCompositeShape *sFourHoles = new TGeoCompositeShape("sFourHoles",
    "sTrimTube:tTrimTube1+sTrimTube:tTrimTube2+sTrimTube:tTrimTube3+sTrimTube:tTrimTube4");

    TGeoTranslation *tFourHoles1 = new TGeoTranslation("tFourHoles1", 0, 0., 0. );
    TGeoTranslation *tFourHoles2 = new TGeoTranslation("tFourHoles2", 24.1, 0., 0. );
    TGeoTranslation *tFourHoles3 = new TGeoTranslation("tFourHoles3", -24.1, 0., 0. );
    tFourHoles1->RegisterYourself();
    tFourHoles2->RegisterYourself();
    tFourHoles3->RegisterYourself();

    TGeoCompositeShape *sRawOfHoles = new TGeoCompositeShape("sRawOfHoles",
    "sFourHoles:tFourHoles1+sFourHoles:tFourHoles2+sFourHoles:tFourHoles3");

    // Assemble Cabe Holders
    TGeoCompositeShape *sCableHolder1 = new TGeoCompositeShape("sCableHolder1",
    "sCableHolderBody-(sAllTrimBoxes1+sRawOfHoles)");
    TGeoCompositeShape *sCableHolder2 = new TGeoCompositeShape("sCableHolder2",
    "sCableHolderBody-(sAllTrimBoxes2+sRawOfHoles)");

    TGeoVolume *vCableHolder1 = new TGeoVolume("vCableHolder1", sCableHolder1, medAl);
    TGeoVolume *vCableHolder2 = new TGeoVolume("vCableHolder2", sCableHolder2, medAl);
    vCableHolder1->SetLineColor(kBlue);
    vCableHolder1->SetLineColor(kBlue);

    // Adding Cable Holders

    for(int i = 0; i < 9; i++){
        if(i < 5){
        vSector->AddNode(vCableHolder1, i+1, new TGeoTranslation(0, 1.85 + 0.5 - 0.035 + 0.07/2 + 4.5/2, 28.6 + 28 * i + 0.7/2) );
        }else{
        vSector->AddNode(vCableHolder2, i+1, new TGeoTranslation(0, 1.85 + 0.5 - 0.035 + 0.07/2 + 4.5/2, 28.6 + 28 * i + 0.7/2) );
        }
    }

    for(int i = 0; i < 9; i++){
        if(i < 5){
        vSector->AddNode(vCableHolder1, i+10, new TGeoTranslation(0, 1.85 + 0.5 - 0.035 + 0.07/2 + 4.5/2, -29.6 - 29.0 * i - 0.7/2) );
        }else{
        vSector->AddNode(vCableHolder2, i+10, new TGeoTranslation(0, 1.85 + 0.5 - 0.035 + 0.07/2 + 4.5/2, -29.6 - 29.0 * i - 0.7/2) );
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////

    // ---------------- MIDDLE PLATE ------------------
    const float sectorPlateX = 69.8, sectorPlateY = 0.07, sectorPlateZ = 291.2;
    TGeoBBox *sSectorPlate = new TGeoBBox(sectorPlateX/2., sectorPlateY/2., sectorPlateZ/2.); sSectorPlate->SetName("sSectorPlate");
    TGeoVolume *vSectorPlate = new TGeoVolume("vSectorPlate", sSectorPlate, medAl);
    vSectorPlate->SetLineColor(kBlack);

    vSector->AddNode(vSectorPlate, 1, new TGeoTranslation("tSecPlate",  0., 1.85+0.5-0.035, 0. +SecBoxZshift));
    vSector->AddNode(vSectorPlate, 2, new TGeoTranslation("tSecPlate",  0., 1.85-0.5+0.035, 0. +SecBoxZshift));

    // ---------------- MIDDLE PLATE 2 ------------------
    const float sectorPlateZ2 = 281.2;
    TGeoBBox *sSectorPlate2 = new TGeoBBox(sectorPlateX/2., sectorPlateY/2., sectorPlateZ2/2.); sSectorPlate2->SetName("sSectorPlate2");
    TGeoVolume *vSectorPlate2 = new TGeoVolume("vSectorPlate2", sSectorPlate2, medAl);          vSectorPlate2->SetLineColor(kBlack);

    vSector->AddNode(vSectorPlate2, 1, new TGeoTranslation("tSecPlate",  0., 1.85+0.5-0.035, 0. +SecBox2Zshift));
    vSector->AddNode(vSectorPlate2, 2, new TGeoTranslation("tSecPlate",  0., 1.85-0.5+0.035, 0. +SecBox2Zshift));

    // --------------- UPPER PLATE ------------------
    const float sectorPlateUpX = 65.9, sectorPlateUpY = .06, sectorPlateUpZ = 287.3;
    TGeoBBox *sSectorPlateUp = new TGeoBBox(sectorPlateUpX/2., sectorPlateUpY/2., sectorPlateUpZ/2.); sSectorPlateUp->SetName("sSectorPlateUp");
    TGeoVolume *vSectorPlateUp = new TGeoVolume("vSectorPlateUp", sSectorPlateUp, medAl);
    vSectorPlateUp->SetLineColor(kBlack);
    vSector->AddNode(vSectorPlateUp, 1, new TGeoTranslation("tSecPlateUp",  0., 7.6+0.25-0.03, 0.+ SecBoxZshift));
    vSector->AddNode(vSectorPlateUp, 2, new TGeoTranslation("tSecPlateUp",  0., 7.6-0.25+0.03, 0.+ SecBoxZshift));

    // --------------- UPPER PLATE 2 ------------------
    const float sectorPlateUpZ2 = 277.3;
    TGeoBBox *sSectorPlateUp2 = new TGeoBBox(sectorPlateUpX/2., sectorPlateUpY/2., sectorPlateUpZ2/2.); sSectorPlateUp2->SetName("sSectorPlateUp2");
    TGeoVolume *vSectorPlateUp2 = new TGeoVolume("vSectorPlateUp2", sSectorPlateUp2, medAl);            vSectorPlateUp2->SetLineColor(kBlack);

    vSector->AddNode(vSectorPlateUp2, 1, new TGeoTranslation("tSecPlateUp2",  0., 7.6+0.25-0.03, 0. +SecBox2Zshift));
    vSector->AddNode(vSectorPlateUp2, 2, new TGeoTranslation("tSecPlateUp2",  0., 7.6-0.25+0.03, 0. +SecBox2Zshift));

    // --------------- BOTTOM PLATE -----------------
    const float sectorPlateBotX = 65.9, sectorPlateBotY = .06, sectorPlateBotZ = 287.3 - 6.606;
    TGeoBBox *sSectorPlateBot = new TGeoBBox(sectorPlateBotX/2., sectorPlateBotY/2., sectorPlateBotZ/2.); sSectorPlateBot->SetName("sSectorPlateBot");
    TGeoVolume *vSectorPlateBot = new TGeoVolume("vSectorPlateBot", sSectorPlateBot, medAl);
    vSectorPlateBot->SetLineColor(kBlack);
    vSector->AddNode(vSectorPlateBot, 1, new TGeoTranslation("tSecPlateBot",  0., -7.6+0.25-0.03, -6.606/2 +SecBoxZshift));
    vSector->AddNode(vSectorPlateBot, 2, new TGeoTranslation("tSecPlateBot",  0., -7.6-0.25+0.03, -6.606/2 +SecBoxZshift));

    // --------------- BOTTOM PLATE 2 -----------------
    const float sectorPlateBotZ2 = 286.747;
    TGeoBBox *sSectorPlateBot2 = new TGeoBBox(sectorPlateBotX/2., sectorPlateBotY/2., sectorPlateBotZ2/2.); sSectorPlateBot2->SetName("sSectorPlateBot2");
    TGeoVolume *vSectorPlateBot2 = new TGeoVolume("vSectorPlateBot2", sSectorPlateBot2, medAl);             vSectorPlateBot2->SetLineColor(kBlack);

    vSector->AddNode(vSectorPlateBot2, 1, new TGeoTranslation("tSecPlateBot2",  0., -7.6+0.25-0.03, -8.7/2-0.4 +SecBox2Zshift));
    vSector->AddNode(vSectorPlateBot2, 2, new TGeoTranslation("tSecPlateBot2",  0., -7.6-0.25+0.03, -8.7/2-0.4 +SecBox2Zshift));

    // ---------------- LEFT (RIGHT) PLATES -----------------
    const float delta1 = 10, delta2 = 3.35;
    const float sectorPlateLeftMidX = 0.3, sectorPlateLeftMidY = 1., sectorPlateLeftMidZ = 281.2+delta1;
    const float sectorPlateLeftTier1X = 2.4, sectorPlateLeftTier1Y = 1.2, sectorPlateLeftTier1Z = 281.2+0.3*2+delta1;
    const float sectorPlateLeftTier2X = 0.3, sectorPlateLeftTier2Y = 4.3, sectorPlateLeftTier2Z = 281.2-1.9*2+delta1;
    const float sectorPlateLeftTier2BotY = 8., sectorPlateLeftTier2BotZ = 281.2-1.9*2+delta2;
    const float sectorPlateLeftTier3X = 1.8, sectorPlateLeftTier3Y = 0.3, sectorPlateLeftTier3Z = 281.2-3.4*2+delta1;
    const float sectorPlateLeftTier3BotZ = 281.2-3.4*2+delta2;
    const float sectorPlateBackZ = -5.006;
    TGeoBBox *ssectorPlateLeftMid = new TGeoBBox(sectorPlateLeftMidX/2., sectorPlateLeftMidY/2., sectorPlateLeftMidZ/2.);   ssectorPlateLeftMid->SetName("sectorPlateLeftMid");
    TGeoBBox *ssectorPlateLeftTier1 = new TGeoBBox(sectorPlateLeftTier1X/2., sectorPlateLeftTier1Y/2., sectorPlateLeftTier1Z/2.); ssectorPlateLeftTier1->SetName("sectorPlateLeftTier1");
    TGeoBBox *ssectorPlateLeftTier2 = new TGeoBBox(sectorPlateLeftTier2X/2., sectorPlateLeftTier2Y/2., sectorPlateLeftTier2Z/2.);   ssectorPlateLeftTier2->SetName("sectorPlateLeftTier2");
    TGeoBBox *ssectorPlateLeftTier2Bot = new TGeoBBox(sectorPlateLeftTier2X/2., sectorPlateLeftTier2BotY/2., sectorPlateLeftTier2BotZ/2.);  ssectorPlateLeftTier2Bot->SetName("sectorPlateLeftTier2Bot");
    TGeoBBox *ssectorPlateLeftTier3 = new TGeoBBox(sectorPlateLeftTier3X/2., sectorPlateLeftTier3Y/2., sectorPlateLeftTier3Z/2.);ssectorPlateLeftTier3->SetName("sectorPlateLeftTier3");
    TGeoBBox *ssectorPlateLeftTier3Bot = new TGeoBBox(sectorPlateLeftTier3X/2., sectorPlateLeftTier3Y/2., sectorPlateLeftTier3BotZ/2.); ssectorPlateLeftTier3Bot->SetName("sectorPlateLeftTier3Bot");

    TGeoVolume *vsectorPlateLeftMid = new TGeoVolume("vsectorPlateLeftMid", ssectorPlateLeftMid, medAl);                vsectorPlateLeftMid->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateLeftTier1 = new TGeoVolume("vsectorPlateLeftTier1", ssectorPlateLeftTier1, medAl);          vsectorPlateLeftTier1->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateLeftTier2 = new TGeoVolume("vsectorPlateLeftTier2", ssectorPlateLeftTier2, medAl);          vsectorPlateLeftTier2->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateLeftTier2Bot = new TGeoVolume("vsectorPlateLeftTier2Bot", ssectorPlateLeftTier2Bot, medAl); vsectorPlateLeftTier2Bot->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateLeftTier3 = new TGeoVolume("vsectorPlateLeftTier3", ssectorPlateLeftTier3, medAl);          vsectorPlateLeftTier3->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateLeftTier3Bot = new TGeoVolume("vsectorPlateLeftTier3Bot", ssectorPlateLeftTier3Bot, medAl); vsectorPlateLeftTier3Bot->SetLineColor(kGreen);

    // assemble
    vSector->AddNode(vsectorPlateLeftMid, 1, new TGeoTranslation("tsectorPlateLeftMid",  33.1+1.95, 1.85, sectorPlateBackZ+delta1/2.+SecBoxZshift));//Z CHECK !
    vSector->AddNode(vsectorPlateLeftTier1, 1, new TGeoTranslation("tsectorPlateLeftTier1", 32.05+1.95, 2.95, sectorPlateBackZ+delta1/2.+SecBoxZshift));
    vSector->AddNode(vsectorPlateLeftTier1, 2, new TGeoTranslation("tsectorPlateLeftTier1", 32.05+1.95, 0.75, sectorPlateBackZ+delta1/2.+SecBoxZshift));
    vSector->AddNode(vsectorPlateLeftTier2, 1, new TGeoTranslation("tsectorPlateLeftTier2", 31.+1.95+0.2, 5.7, sectorPlateBackZ+delta1/2.+SecBoxZshift));
    vSector->AddNode(vsectorPlateLeftTier2Bot, 1, new TGeoTranslation("tsectorPlateLeftTier2Bot", 31.+1.95+0.2, -3.85, sectorPlateBackZ+delta2/2.+SecBoxZshift));
    vSector->AddNode(vsectorPlateLeftTier3, 1, new TGeoTranslation("tsectorPlateLeftTier3", 30.1+1.95+0.4, 8., sectorPlateBackZ+delta1/2.+SecBoxZshift));
    vSector->AddNode(vsectorPlateLeftTier3Bot, 1, new TGeoTranslation("tsectorPlateLeftTier3Bot", 30.1+1.95+0.4, -8., sectorPlateBackZ+delta2/2.+SecBoxZshift));
    //reverse (x->-x) for right side
    vSector->AddNode(vsectorPlateLeftMid, 2, new TGeoTranslation("tsectorPlateRightMid", -33.1-1.95, 1.85, sectorPlateBackZ+delta1/2. +SecBoxZshift));
    vSector->AddNode(vsectorPlateLeftTier1, 3, new TGeoTranslation("tsectorPlateRightTier1", -32.05-1.95, 2.95, sectorPlateBackZ+delta1/2. +SecBoxZshift));
    vSector->AddNode(vsectorPlateLeftTier1, 4, new TGeoTranslation("tsectorPlateRightTier1", -32.05-1.95, 0.75, sectorPlateBackZ+delta1/2. +SecBoxZshift));
    vSector->AddNode(vsectorPlateLeftTier2, 2, new TGeoTranslation("tsectorPlateRightTier2", -31.-1.95-0.2, 5.7, sectorPlateBackZ+delta1/2. +SecBoxZshift));
    vSector->AddNode(vsectorPlateLeftTier2Bot, 2, new TGeoTranslation("tsectorPlateRightTier2Bot", -31.-1.95-0.2, -3.85, sectorPlateBackZ+delta2/2. +SecBoxZshift));
    vSector->AddNode(vsectorPlateLeftTier3, 2, new TGeoTranslation("tsectorPlateRightTier3", -30.1-1.95-0.4, 8., sectorPlateBackZ+delta1/2. +SecBoxZshift));
    vSector->AddNode(vsectorPlateLeftTier3Bot, 2, new TGeoTranslation("tsectorPlateRightTier3Bot", -30.1-1.95-0.4, -8., sectorPlateBackZ+delta2/2. +SecBoxZshift));

    // ----------------- BACK PLATE -------------------
    TGeoBBox *ssectorPlateBackMid = new TGeoBBox(65.9/2.+2.25, sectorPlateLeftMidY/2., sectorPlateLeftMidX/2.);     ssectorPlateBackMid->SetName("sectorPlateBackMid");
    TGeoBBox *ssectorPlateBackTier1 = new TGeoBBox(65.9/2.-0.2, sectorPlateLeftTier1Y/2., sectorPlateLeftTier1X/2.);    ssectorPlateBackTier1->SetName("sectorPlateBackTier1");
    TGeoBBox *ssectorPlateBackTier2 = new TGeoBBox(65.9/2.+0.35, sectorPlateLeftTier2Y/2., sectorPlateLeftTier2X/2.);   ssectorPlateBackTier2->SetName("sectorPlateBackTier2");
    TGeoBBox *ssectorPlateBackTier2Bot = new TGeoBBox(65.9/2.+0.35, sectorPlateLeftTier2BotY/2., sectorPlateLeftTier2X/2.); ssectorPlateBackTier2Bot->SetName("sectorPlateBackTier2Bot");
    TGeoBBox *ssectorPlateBackTier3 = new TGeoBBox(65.9/2.+0.4, sectorPlateLeftTier3Y/2., sectorPlateLeftTier3X/2.);    ssectorPlateBackTier3->SetName("sectorPlateBackTier3");

    TGeoVolume *vsectorPlateBackMid = new TGeoVolume("vsectorPlateBackMid", ssectorPlateBackMid, medAl);                vsectorPlateBackMid->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateBackTier1 = new TGeoVolume("vsectorPlateBackTier1", ssectorPlateBackTier1, medAl);          vsectorPlateBackTier1->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateBackTier2 = new TGeoVolume("vsectorPlateBackTier2", ssectorPlateBackTier2, medAl);          vsectorPlateBackTier2->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateBackTier2Bot = new TGeoVolume("vsectorPlateBackTier2Bot", ssectorPlateBackTier2Bot, medAl); vsectorPlateBackTier2Bot->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateBackTier3 = new TGeoVolume("vsectorPlateBackTier3", ssectorPlateBackTier3, medAl);          vsectorPlateBackTier3->SetLineColor(kGreen);

    vSector->AddNode(vsectorPlateBackMid, 1, new TGeoTranslation("tsectorPlateBackMid", 0., 1.85, -33.1-1.95-221.4/2. +SecBoxZshift));//X CHECK !
    vSector->AddNode(vsectorPlateBackTier1, 1, new TGeoTranslation("tsectorPlateBackTier1", 0., 2.95, -32.05-1.95-221.4/2. +SecBoxZshift));
    vSector->AddNode(vsectorPlateBackTier1, 2, new TGeoTranslation("tsectorPlateBackTier1", 0., 0.75, -32.05-1.95-221.4/2. +SecBoxZshift));
    vSector->AddNode(vsectorPlateBackTier2, 1, new TGeoTranslation("tsectorPlateBackTier2", 0., 5.7, -31.-1.95-0.2-221.4/2. +SecBoxZshift));
    vSector->AddNode(vsectorPlateBackTier2Bot, 1, new TGeoTranslation("tsectorPlateBackTier2Bot", 0., -3.85, -31.-1.95-0.2-221.4/2. +SecBoxZshift));
    vSector->AddNode(vsectorPlateBackTier3, 1, new TGeoTranslation("tsectorPlateBackTier3", 0., 8., -30.1-1.95-0.4-221.4/2. +SecBoxZshift));
    vSector->AddNode(vsectorPlateBackTier3, 2, new TGeoTranslation("tsectorPlateBackTier3", 0., -8., -30.1-1.95-0.4-221.4/2. +SecBoxZshift));

    // ---------------- LEFT (RIGHT) PLATES 2 -----------------
    const float delta3 = 5.022*2-0.623;
    const float sectorPlateLeftMidX2 = 0.3, sectorPlateLeftMidY2 = 1., sectorPlateLeftMidZ2 = 281.2;
    const float sectorPlateLeftTier1X2 = 2.4, sectorPlateLeftTier1Y2 = 1.2, sectorPlateLeftTier1Z2 = 281.2+0.3*2;
    const float sectorPlateLeftTier2X2 = 0.3, sectorPlateLeftTier2Y2 = 4.3, sectorPlateLeftTier2Z2 = 281.2-1.9*2;
    const float sectorPlateLeftTier2BotY2 = 8.;
    const float sectorPlateLeftTier3X2 = 1.8, sectorPlateLeftTier3Y2 = 0.3, sectorPlateLeftTier3Z2 = 281.2-3.4*2;
    const float sectorPlateLeftTier3BotZ2 = 281.2-3.4*2+delta3;

    TGeoBBox *ssectorPlateLeftMid2 = new TGeoBBox(sectorPlateLeftMidX2/2., sectorPlateLeftMidY2/2., sectorPlateLeftMidZ2/2.);           ssectorPlateLeftMid2->SetName("sectorPlateLeftMid2");
    TGeoBBox *ssectorPlateLeftTier12 = new TGeoBBox(sectorPlateLeftTier1X2/2., sectorPlateLeftTier1Y2/2., sectorPlateLeftTier1Z2/2.);   ssectorPlateLeftTier12->SetName("sectorPlateLeftTier12");
    TGeoBBox *ssectorPlateLeftTier22 = new TGeoBBox(sectorPlateLeftTier2X2/2., sectorPlateLeftTier2Y2/2., sectorPlateLeftTier2Z2/2.);   ssectorPlateLeftTier22->SetName("sectorPlateLeftTier22");
    TGeoBBox *ssectorPlateLeftTier2Bot2 = new TGeoBBox(sectorPlateLeftTier2X2/2., sectorPlateLeftTier2BotY2/2., sectorPlateLeftTier2Z2/2.);ssectorPlateLeftTier2Bot2->SetName("sectorPlateLeftTier2Bot2");
    TGeoBBox *ssectorPlateLeftTier32 = new TGeoBBox(sectorPlateLeftTier3X2/2., sectorPlateLeftTier3Y2/2., sectorPlateLeftTier3Z2/2.);   ssectorPlateLeftTier32->SetName("sectorPlateLeftTier32");
    TGeoBBox *ssectorPlateLeftTier3Bot2 = new TGeoBBox(sectorPlateLeftTier3X2/2., sectorPlateLeftTier3Y2/2., sectorPlateLeftTier3BotZ2/2.);ssectorPlateLeftTier3Bot2->SetName("sectorPlateLeftTier3Bot2");

    TGeoVolume *vsectorPlateLeftMid2 = new TGeoVolume("vsectorPlateLeftMid2", ssectorPlateLeftMid2, medAl);                 vsectorPlateLeftMid2->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateLeftTier12 = new TGeoVolume("vsectorPlateLeftTier12", ssectorPlateLeftTier12, medAl);           vsectorPlateLeftTier12->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateLeftTier22 = new TGeoVolume("vsectorPlateLeftTier22", ssectorPlateLeftTier22, medAl);           vsectorPlateLeftTier22->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateLeftTier2Bot2 = new TGeoVolume("vsectorPlateLeftTier2Bot2", ssectorPlateLeftTier2Bot2, medAl);  vsectorPlateLeftTier2Bot2->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateLeftTier32 = new TGeoVolume("vsectorPlateLeftTier32", ssectorPlateLeftTier32, medAl);           vsectorPlateLeftTier32->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateLeftTier3Bot2 = new TGeoVolume("vsectorPlateLeftTier3Bot2", ssectorPlateLeftTier3Bot2, medAl);  vsectorPlateLeftTier3Bot2->SetLineColor(kGreen);

    vSector->AddNode(vsectorPlateLeftMid2, 1, new TGeoTranslation("tsectorPlateLeftMid2",  33.1+1.95, 1.85, 0. +SecBox2Zshift));//Z CHECK !
    vSector->AddNode(vsectorPlateLeftTier12, 1, new TGeoTranslation("tsectorPlateLeftTier12", 32.05+1.95, 2.95, 0. +SecBox2Zshift));
    vSector->AddNode(vsectorPlateLeftTier12, 2, new TGeoTranslation("tsectorPlateLeftTier12", 32.05+1.95, 0.75, 0. +SecBox2Zshift));
    vSector->AddNode(vsectorPlateLeftTier22, 1, new TGeoTranslation("tsectorPlateLeftTier22", 31.+1.95+0.2, 5.7, 0. +SecBox2Zshift));
    vSector->AddNode(vsectorPlateLeftTier2Bot2, 1, new TGeoTranslation("tsectorPlateLeftTier2Bot2", 31.+1.95+0.2, -3.85, 0. +SecBox2Zshift));
    vSector->AddNode(vsectorPlateLeftTier32, 1, new TGeoTranslation("tsectorPlateLeftTier32", 30.1+1.95+0.4, 8., 0. +SecBox2Zshift));
    vSector->AddNode(vsectorPlateLeftTier3Bot2, 2, new TGeoTranslation("tsectorPlateLeftTier32", 30.1+1.95+0.4, -8., -delta3/2.));
    //reverse (x->-x) for right side
    vSector->AddNode(vsectorPlateLeftMid2, 2, new TGeoTranslation("tsectorPlateRightMid2", -33.1-1.95, 1.85, 0. +SecBox2Zshift));
    vSector->AddNode(vsectorPlateLeftTier12, 3, new TGeoTranslation("tsectorPlateRightTier12", -32.05-1.95, 2.95, 0. +SecBox2Zshift));
    vSector->AddNode(vsectorPlateLeftTier12, 4, new TGeoTranslation("tsectorPlateRightTier12", -32.05-1.95, 0.75, 0. +SecBox2Zshift));
    vSector->AddNode(vsectorPlateLeftTier22, 2, new TGeoTranslation("tsectorPlateRightTier22", -31.-1.95-0.2, 5.7, 0. +SecBox2Zshift));
    vSector->AddNode(vsectorPlateLeftTier2Bot2, 2, new TGeoTranslation("tsectorPlateRightTier2Bot2", -31.-1.95-0.2, -3.85, 0. +SecBox2Zshift));
    vSector->AddNode(vsectorPlateLeftTier32, 3, new TGeoTranslation("tsectorPlateRightTier32", -30.1-1.95-0.4, 8., 0. +SecBox2Zshift));
    vSector->AddNode(vsectorPlateLeftTier3Bot2, 4, new TGeoTranslation("tsectorPlateRightTier32", -30.1-1.95-0.4, -8., -delta3/2. +SecBox2Zshift));

    // ----------------- BACK PLATE 2 -------------------
    TGeoBBox *ssectorPlateBackMid2 = new TGeoBBox(65.9/2.+2.25, sectorPlateLeftMidY2/2., sectorPlateLeftMidX2/2.);      ssectorPlateBackMid2->SetName("sectorPlateBackMid2");
    TGeoBBox *ssectorPlateBackTier12 = new TGeoBBox(65.9/2.-0.2, sectorPlateLeftTier1Y2/2., sectorPlateLeftTier1X2/2.); ssectorPlateBackTier12->SetName("sectorPlateBackTier12");
    TGeoBBox *ssectorPlateBackTier22 = new TGeoBBox(65.9/2.+0.35, sectorPlateLeftTier2Y2/2., sectorPlateLeftTier2X2/2.);ssectorPlateBackTier22->SetName("sectorPlateBackTier22");
    TGeoBBox *ssectorPlateBackTier2Bot2 = new TGeoBBox(65.9/2.+0.35, sectorPlateLeftTier2BotY2/2., sectorPlateLeftTier2X2/2.);ssectorPlateBackTier2Bot2->SetName("sectorPlateBackTier2Bot2");
    TGeoBBox *ssectorPlateBackTier32 = new TGeoBBox(65.9/2.+0.4, sectorPlateLeftTier3Y2/2., sectorPlateLeftTier3X2/2.); ssectorPlateBackTier32->SetName("sectorPlateBackTier32");

    TGeoVolume *vsectorPlateBackMid2 = new TGeoVolume("vsectorPlateBackMid2", ssectorPlateBackMid2, medAl);         vsectorPlateBackMid2->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateBackTier12 = new TGeoVolume("vsectorPlateBackTier12", ssectorPlateBackTier12, medAl);   vsectorPlateBackTier12->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateBackTier22 = new TGeoVolume("vsectorPlateBackTier22", ssectorPlateBackTier22, medAl);   vsectorPlateBackTier22->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateBackTier2Bot2 = new TGeoVolume("vsectorPlateBackTier2Bot2", ssectorPlateBackTier2Bot2, medAl);vsectorPlateBackTier2Bot2->SetLineColor(kGreen);
    TGeoVolume *vsectorPlateBackTier32 = new TGeoVolume("vsectorPlateBackTier32", ssectorPlateBackTier32, medAl);   vsectorPlateBackTier32->SetLineColor(kGreen);

    vSector->AddNode(vsectorPlateBackMid2, 1, new TGeoTranslation("tsectorPlateBackMid2", 0., 1.85, +33.1+107.65 +SecBox2Zshift));//X CHECK !
    vSector->AddNode(vsectorPlateBackTier12, 1, new TGeoTranslation("tsectorPlateBackTier12", 0., 2.95, 32.05+107.65 +SecBox2Zshift));
    vSector->AddNode(vsectorPlateBackTier12, 2, new TGeoTranslation("tsectorPlateBackTier12", 0., 0.75, 32.05+107.65 +SecBox2Zshift));
    vSector->AddNode(vsectorPlateBackTier22, 1, new TGeoTranslation("tsectorPlateBackTier22", 0., 5.7, 31.+0.2+107.65 +SecBox2Zshift));
    vSector->AddNode(vsectorPlateBackTier2Bot2, 1, new TGeoTranslation("tsectorPlateBackTier2Bot2", 0., -3.85, +31.+0.2+107.65 +SecBox2Zshift));
    vSector->AddNode(vsectorPlateBackTier32, 1, new TGeoTranslation("tsectorPlateBackTier32", 0., 8., 30.1+0.4+107.65 +SecBox2Zshift));
    vSector->AddNode(vsectorPlateBackTier32, 2, new TGeoTranslation("tsectorPlateBackTier32", 0., -8., 30.1+0.4+107.65 +SecBox2Zshift));

    // -------------------- BIG CENTRAL MESS -------------------------
    TGeoBBox *ssectorPlate_1 = new TGeoBBox(65.9/2.+2.25, sectorPlateLeftMidY2/2., sectorPlateLeftMidX2);   ssectorPlate_1->SetName("sectorPlate_1");
    TGeoVolume *vsectorPlate_1 = new TGeoVolume("vsectorPlate_1", ssectorPlate_1, medAl);
    TGeoBBox *ssectorPlate_2 = new TGeoBBox(65.9/2.+0.35, 4.5/2., sectorPlateLeftTier2X/2.);    ssectorPlate_2->SetName("sectorPlate_2");
    TGeoVolume *vsectorPlate_2 = new TGeoVolume("vsectorPlate_2", ssectorPlate_2, medAl);
    TGeoBBox *ssectorPlate_3 = new TGeoBBox(65.9/2.+0.35, 4.144/2., sectorPlateLeftTier2X/2.);  ssectorPlate_3->SetName("sectorPlate_3");
    TGeoVolume *vsectorPlate_3 = new TGeoVolume("vsectorPlate_3", ssectorPlate_3, medAl);
    TGeoBBox *ssectorPlate_4 = new TGeoBBox(65.9/2.+0.35, 2.847/2., sectorPlateLeftTier2X/2.);  ssectorPlate_4->SetName("sectorPlate_4");
    TGeoVolume *vsectorPlate_4 = new TGeoVolume("vsectorPlate_4", ssectorPlate_4, medAl);
    TGeoBBox *ssectorPlate_5 = new TGeoBBox(65.9/2.+0.35, 3.014/2., sectorPlateLeftTier2X/2.);  ssectorPlate_5->SetName("sectorPlate_5");
    TGeoVolume *vsectorPlate_5 = new TGeoVolume("vsectorPlate_5", ssectorPlate_5, medAl);

    vSector->AddNode(vsectorPlate_1, 1, new TGeoTranslation("tsectorPlate_1", 0., 1.85, 0.));
    vSector->AddNode(vsectorPlate_2, 2, new TGeoTranslation("tsectorPlate_2", 0., -5.765+0.15+0.015, -8.856+0.15));
    vSector->AddNode(vsectorPlate_3, 3, new TGeoTranslation("tsectorPlate_3", 0., -5.783+0.15-0.093, -7.222-0.15-0.002)); //overlap
    vSector->AddNode(vsectorPlate_4, 4, new TGeoTranslation("tsectorPlate_4", 0., 0.15-3.434/2.+0.2185, -2.05));
    vSector->AddNode(vsectorPlate_5, 5, new TGeoTranslation("tsectorPlate_5", 0., 0.15-3.627/2.+0.3065, 2.05));

        vsectorPlate_1->SetLineColor(kRed);


    vSector->AddNode(vsectorPlateBackTier1, 3, new TGeoTranslation("tsectorPlateBackTier1", 0., 2.95, -1.2));
    vSector->AddNode(vsectorPlateBackTier1, 4, new TGeoTranslation("tsectorPlateBackTier1", 0., 0.75, -1.2));
    vSector->AddNode(vsectorPlateBackTier1, 5, new TGeoTranslation("tsectorPlateBackTier1", 0., 2.95, 1.2));
    vSector->AddNode(vsectorPlateBackTier1, 6, new TGeoTranslation("tsectorPlateBackTier1", 0., 0.75, 1.2));
    vSector->AddNode(vsectorPlateBackTier2, 7, new TGeoTranslation("tsectorPlateBackTier2", 0., 5.7, 2.05));
    vSector->AddNode(vsectorPlateBackTier2, 8, new TGeoTranslation("tsectorPlateBackTier2", 0., 5.7, -2.05));
    vSector->AddNode(vsectorPlateBackTier32, 9, new TGeoTranslation("tsectorPlateBackTier32", 0., -8., -6.622));
    vSector->AddNode(vsectorPlateBackTier32, 10, new TGeoTranslation("tsectorPlateBackTier32", 0., -8., -9.456));
    vSector->AddNode(vsectorPlateBackTier32, 11, new TGeoTranslation("tsectorPlateBackTier32", 0., 8., -2.8));
    vSector->AddNode(vsectorPlateBackTier32, 12, new TGeoTranslation("tsectorPlateBackTier32", 0., 8., 2.8));

    TGeoRotation *geoDetRot3 = new TGeoRotation; geoDetRot3->RotateX(84); // [degree]
    TGeoCombiTrans *cMidWierdPlateTrans = new TGeoCombiTrans("cMidWierdPlateTrans", 0., -3.382+0.285,  -5.378, geoDetRot3);
    TGeoBBox *sMidWierdPlate = new TGeoBBox(65.9/2.+0.35, 6.35/2., 0.1/2.);                             sMidWierdPlate->SetName("sMidWierdPlate");
    TGeoCombiTrans *cMidWierdPlateTrans2 = new TGeoCombiTrans("cMidWierdPlateTrans2", 0., -3.4985+0.2,  -2.611-0.05, geoDetRot3);
    TGeoBBox *sMidWierdPlate2 = new TGeoBBox(65.9/2.+0.35, 8.65/2.+0.25, 0.1/2.);                       sMidWierdPlate2->SetName("sMidWierdPlate2");

    TGeoVolume *vMidWierdPlate = new TGeoVolume("vMidWierdPlate", sMidWierdPlate, medAl);       vMidWierdPlate->SetLineColor(kGreen);
    TGeoVolume *vMidWierdPlate2 = new TGeoVolume("vMidWierdPlate2", sMidWierdPlate2, medAl);    vMidWierdPlate2->SetLineColor(kGreen);

    vSector->AddNode(vMidWierdPlate, 13, cMidWierdPlateTrans);
    vSector->AddNode(vMidWierdPlate2, 14, cMidWierdPlateTrans2);

    TGeoBBox *ssectorPlate_15 = new TGeoBBox(sectorPlateLeftTier2X/2., 3.98/2., 9.412/2.);  ssectorPlate_15->SetName("sectorPlate_15");
    TGeoVolume *vsectorPlate_15 = new TGeoVolume("vsectorPlate_15", ssectorPlate_15, medAl);    vsectorPlate_15->SetLineColor(kGreen);

    vSector->AddNode(vsectorPlate_15, 15, new TGeoTranslation("tsectorPlate_15", 31.+1.95+0.2, -5.785-0.075, -2.606+0.1));
    vSector->AddNode(vsectorPlate_15, 16, new TGeoTranslation("tsectorPlate_15", -31.-1.95-0.2, -5.785-0.075, -2.606+0.1));

    TGeoBBox *ssectorPlate_17 = new TGeoBBox(sectorPlateLeftTier2X/2., 2.834/2., 6.656/2.); ssectorPlate_17->SetName("sectorPlate_17");
    TGeoVolume *vsectorPlate_17 = new TGeoVolume("vsectorPlate_17", ssectorPlate_17, medAl);    vsectorPlate_17->SetLineColor(kGreen);

    vSector->AddNode(vsectorPlate_17, 17, new TGeoTranslation("tsectorPlate_17", 31.+1.95+0.2, -1.567+0.3, -5.778+0.25));
    vSector->AddNode(vsectorPlate_17, 18, new TGeoTranslation("tsectorPlate_17", -31.-1.95-0.2, -1.567+0.3, -5.778+0.25));

    //------------- Create detector box ----------------
    TGeoVolume* vDetBox = new TGeoVolumeAssembly("tof1DetectorBox");

    // ----------------------------------------------------------- ASSEMBLE detector ----------------------------------------------
    //--------------------- SOLIDS (HONEYCOMB) -----------------------
    const float solPlateX = 64.5, solPlateY = 0.06, solPlateZ = 30.0;// [cm]
    TGeoBBox *sSolPlate = new TGeoBBox(solPlateX/2., solPlateY/2., solPlateZ/2.); sSolPlate->SetName("SolidPlate");
    TGeoVolume *vSolPlate = new TGeoVolume("tof1SolPlateV", sSolPlate, medG10);
    vSolPlate->SetLineColor(kBlack);

    // Add solids
    vDetBox->AddNode(vSolPlate, 1, new TGeoTranslation("tdetPlate",  0.,  1.3, 0.));
    vDetBox->AddNode(vSolPlate, 2, new TGeoTranslation("tdetPlate",  0., -1.3, 0.));
    vDetBox->AddNode(vSolPlate, 3, new TGeoTranslation("tdetPlate",  0.,  0.713 + 0.005, 0.)); //was overlap
    vDetBox->AddNode(vSolPlate, 4, new TGeoTranslation("tdetPlate",  0., -0.713 - 0.005, 0.));//was overlap

    //--------------------- GLASS (1&2) -----------------------
    const float detGlassX = 64.5, detGlassY = 0.196, detGlassZ = 30.0;// [cm]
    TGeoBBox *sDetGlass = new TGeoBBox(detGlassX/2., detGlassY/2., detGlassZ/2.);   sDetGlass->SetName("DetectorGlass");
    TGeoVolume *vDetGlass = new TGeoVolume("tof1DetGlassV", sDetGlass, medGlass);   vDetGlass->SetLineColor(kBlue);

    // Add Glass
    vDetBox->AddNode(vDetGlass, 1, new TGeoTranslation("tdetGlass",  0.,  0.442 + 0.048, 0.));
    vDetBox->AddNode(vDetGlass, 2, new TGeoTranslation("tdetGlass",  0.,  0.    + 0.048, 0.));
    vDetBox->AddNode(vDetGlass, 3, new TGeoTranslation("tdetGlass",  0., -0.442 + 0.048, 0.));

    //----------------------- G10 THIN (1&2) -------------------------
    const float detG10ThinX = 65, detG10ThinY = 0.1, detG10ThinZ = 33.0; // [cm]
    TGeoBBox *sDetG10Thin = new TGeoBBox(detG10ThinX/2., detG10ThinY/2., detG10ThinZ/2.);   sDetG10Thin->SetName("DetectorG10Thin");
    TGeoVolume *vDetG10Thin = new TGeoVolume("tof1DetG10ThinV", sDetG10Thin, medG10);       vDetG10Thin->SetLineColor(kGreen);

    // Add Glass thin plates
    vDetBox->AddNode(vDetG10Thin, 1, new TGeoTranslation("tdetPlate",  0.,  0.638, 0.));
    vDetBox->AddNode(vDetG10Thin, 2, new TGeoTranslation("tdetPlate",  0., -0.638, 0.));

    //----------------------- G10 THICK (1&2) -------------------------
    const float detG10ThickX = 65, detG10ThickY = 0.07325, detG10ThickZ = 33.0; // [cm]
    TGeoBBox *sDetG10Thick = new TGeoBBox(detG10ThickX/2., detG10ThickY/2., detG10ThickZ/2.);   sDetG10Thick->SetName("DetectorG10Thick");
    TGeoVolume *vDetG10Thick = new TGeoVolume("tof1DetG10ThickV", sDetG10Thick, medG10);        vDetG10Thick->SetLineColor(kGreen);

    // Add Glass thin plates
    vDetBox->AddNode(vDetG10Thick, 1, new TGeoTranslation("tdetPlate",  0.,  0.221 + 0.038375, 0.));
    vDetBox->AddNode(vDetG10Thick, 3, new TGeoTranslation("tdetPlate",  0., -0.221 + 0.038375, 0.));
    vDetBox->AddNode(vDetG10Thick, 2, new TGeoTranslation("tdetPlate",  0.,  0.221 - 0.038375, 0.));
    vDetBox->AddNode(vDetG10Thick, 4, new TGeoTranslation("tdetPlate",  0., -0.221 - 0.038375, 0.));

    // --------------------- Create detector big mount ------------------
    const float bmountX = 0.4,  bmountY = 1.7, bmountZ1 = 4., bmountZ2 = 2.;
    TGeoTrd1 *sBmountTrd = new TGeoTrd1(bmountZ1/2., bmountZ2/2., bmountX/2., bmountY/2.);  sBmountTrd->SetName("sBmountTrd");
    TGeoVolume *vBMountTrd = new TGeoVolume("vBMountTrd", sBmountTrd, medAl);               vBMountTrd->SetLineColor(kBlack);
    TGeoVolume *vBMountTrdUD = new TGeoVolume("vBMountTrdUD", sBmountTrd, medAl);           vBMountTrdUD->SetLineColor(kBlack);

    // --------------------- Create detector big Middle mount ------------------
    const float bmountXM = 0.4,  bmountYM = 2.5, bmountZ1M = 4., bmountZ2M = 2.;
    TGeoTrd1 *sBmountTrdM = new TGeoTrd1(bmountZ1M/2., bmountZ2M/2., bmountXM/2., bmountYM/2.); sBmountTrdM->SetName("sBmountTrdM");
    TGeoVolume *vBMountTrdM = new TGeoVolume("vBMountTrdM", sBmountTrdM, medAl);                vBMountTrdM->SetLineColor(kBlack);
    TGeoVolume *vBMountTrdUDM = new TGeoVolume("vBMountTrdUDM", sBmountTrdM, medAl);            vBMountTrdUDM->SetLineColor(kBlack);

    // --------------------- Create detector small mount ------------------
    const float smountX = 0.6, smountY = 1.6, smountZ1 = 4., smountZ2 = 1.35;
    TGeoTrd1 *sSmountTrd = new TGeoTrd1(smountZ1/2., smountZ2/2., smountX/2., smountY/2.);  sSmountTrd->SetName("sSmountTrd");
    TGeoVolume *vSMountTrd = new TGeoVolume("vSMount", sSmountTrd, medAl);                  vSMountTrd->SetLineColor(kBlack);

    // --------------------- Create detector small Middle mount ------------------
    const float smountXM = 0.6, smountYM = 5., smountZ1M = 4., smountZ2M = 1.35;
    TGeoTrd1 *sSmountTrdM = new TGeoTrd1(smountZ1M/2., smountZ2M/2., smountXM/2., smountYM/2.); sSmountTrdM->SetName("sSmountTrdM");
    TGeoVolume *vSMountTrdM = new TGeoVolume("vSMountM", sSmountTrdM, medAl);                   vSMountTrdM->SetLineColor(kBlack);

    //---------------- STRIPS ----------------------
    // --------------------- Create strip ------------------
    const float stripGasX = 65, stripGasY = 0.096, stripGasZ = 1.25;// [cm]
    TGeoBBox *sStripGas = new TGeoBBox(stripGasX/2., stripGasY/2., stripGasZ/2.);       sStripGas->SetName("StripGas");
    TGeoVolume *vStripGas = new TGeoVolume("tof1StripActiveGasV", sStripGas, medGas);   vStripGas->SetLineColor(kYellow);

    // Add strips
    const int NStrips = 24;	//	24 strips group
    const double StripStep = 1.25; // [cm]
    for(int i = 1; i <= NStrips; i++) // LSP123
    {
        vDetBox->AddNode(vStripGas, 0*NStrips + i, new TGeoTranslation("",  0.,  0.442 - 0.098, -detGlassZ/2. + StripStep/2. + i * StripStep));// [ 1,24]
        vDetBox->AddNode(vStripGas, 1*NStrips + i, new TGeoTranslation("",  0.,  0.    - 0.098, -detGlassZ/2. + StripStep/2. + i * StripStep));// [25,48] - middle layer
        vDetBox->AddNode(vStripGas, 2*NStrips + i, new TGeoTranslation("",  0., -0.442 - 0.098, -detGlassZ/2. + StripStep/2. + i * StripStep));// [49,72]
// LSP123    
    }

    //------------------------ CU STRIPS IN G10 THICK ---------------------
    const float stripCuX = 65, stripCuY = 0.0035, stripCuZ = 1.;// [cm]
    TGeoBBox *sStripCu = new TGeoBBox(stripCuX/2., stripCuY/2., stripCuZ/2.);   sStripCu->SetName("StripCu");
    TGeoVolume *vStripCu = new TGeoVolume("tof1StripCuV", sStripCu, medCu);     vStripCu->SetLineColor(kRed);

    // Add Cu strips
    const double StripStepCu = 1.25; // [cm]
    for(int i=0; i < NStrips; i++)
    {
        vDetBox->AddNode(vStripCu, 3*i+1, new TGeoTranslation("",  0.,  0.221, -detGlassZ/2. + StripStepCu/2. + i * StripStep));
        vDetBox->AddNode(vStripCu, 3*i+2, new TGeoTranslation("",  0., -0.221, -detGlassZ/2. + StripStepCu/2. + i * StripStep));
    }

    // ----------------------------------------------------------- ASSEMBLE sector ----------------------------------------------
    // ---------------------------------- Install mounts
    int mountID = 1;

    TGeoRotation *geoMountRotUD = new TGeoRotation; geoMountRotUD->RotateZ(90); geoMountRotUD->RotateX(90);
    TGeoRotation *geoMountRot2 = new TGeoRotation;   	geoMountRot2->RotateZ(90); 	geoMountRot2->RotateX(-90);// [degree]

    TGeoCombiTrans *cT = new TGeoCombiTrans("",   19.6, 2.153+0.027,  -9., geoMountRot2); cT->RegisterYourself(); // left big mount
    TGeoCombiTrans *cT1 = new TGeoCombiTrans("", -19.6, 2.153+0.027,  -9., geoMountRot2); cT1->RegisterYourself(); // right big mount
    TGeoCombiTrans *cT2 = new TGeoCombiTrans("",     0, 2.103+0.027,   6., geoMountRot2); cT2->RegisterYourself(); // small pmount

    TGeoCombiTrans *cT_2 = new TGeoCombiTrans("",   19.6, 2.153+0.027,   9., geoMountRot2); cT_2->RegisterYourself(); // left big mount
    TGeoCombiTrans *cT1_2 = new TGeoCombiTrans("", -19.6, 2.153+0.027,   9., geoMountRot2); cT1_2->RegisterYourself(); // right big mount
    TGeoCombiTrans *cT2_2 = new TGeoCombiTrans("",     0, 2.103+0.027,  -6., geoMountRot2); cT2_2->RegisterYourself(); // small pmount

    TGeoCombiTrans *cT_M = new TGeoCombiTrans("",   19.6, 1.325+0.027+bmountYM/2.,  -6., geoMountRot2); cT_M->RegisterYourself(); // left big mount
    TGeoCombiTrans *cT1_M = new TGeoCombiTrans("", -19.6, 1.325+0.027+bmountYM/2.,  -6., geoMountRot2); cT1_M->RegisterYourself(); // right big mount
    TGeoCombiTrans *cT2_M = new TGeoCombiTrans("",     0, 1.325+0.027+smountYM/2.,   6., geoMountRot2); cT2_M->RegisterYourself(); // small pmount

    double Zstep = 28.15, Z02 = -126.6185-8.7/2-0.4, Z0 = -126.6185;
    for(int i = 0; i < 10 ; i++)
    {
        TGeoCombiTrans *cT3 = new TGeoCombiTrans("",  19.6, 0.5,  Z0 + i * Zstep-9.3813 +SecBoxZshift, geoMountRotUD); cT3->RegisterYourself(); // left big UD mount
        TGeoCombiTrans *cT4 = new TGeoCombiTrans("", -19.6, 0.5,  Z0 + i * Zstep-9.3813 +SecBoxZshift, geoMountRotUD); cT4->RegisterYourself(); // right big UD mount

        vSector->AddNode(vBMountTrdUD, mountID++, cT3);
        vSector->AddNode(vBMountTrdUD, mountID++, cT4);
    }

    TGeoCombiTrans *cTM = new TGeoCombiTrans("",   19.6, 0.5-(2.5-1.7)/2.,  Z02-5 +SecBox2Zshift, geoMountRotUD); cTM->RegisterYourself(); // left big UD mount
    TGeoCombiTrans *cTM2 = new TGeoCombiTrans("", -19.6, 0.5-(2.5-1.7)/2.,  Z02-5 +SecBox2Zshift, geoMountRotUD); cTM2->RegisterYourself(); // right big UD mount

    vSector->AddNode(vBMountTrdUDM, mountID++, cTM);
    vSector->AddNode(vBMountTrdUDM, mountID++, cTM2);

    for(int i = 1; i < 10 ; i++)
    {
        TGeoCombiTrans *cT3 = new TGeoCombiTrans("", 19.6, 0.5,  Z02 + i * Zstep+9.3813 +SecBox2Zshift, geoMountRotUD); cT3->RegisterYourself(); // left big UD mount
        TGeoCombiTrans *cT4 = new TGeoCombiTrans("", -19.6, 0.5,  Z02 + i * Zstep+9.3813 +SecBox2Zshift, geoMountRotUD); cT4->RegisterYourself(); // right big UD mount

        vSector->AddNode(vBMountTrdUD, mountID++, cT3);
        vSector->AddNode(vBMountTrdUD, mountID++, cT4);
    }

    // ---------------------------------- Install detectors & mounts
    int detectorID = 1;
    TGeoRotation *geoDetRot1 = new TGeoRotation; geoDetRot1->RotateX(-6); // [degree]
    for(int i = 0; i < 10 ; i++)
    {
        TGeoCombiTrans *cTdet = new TGeoCombiTrans("", 0., -2.8585,  Z0 + i * Zstep + SecBoxZshift, geoDetRot1); cTdet->RegisterYourself();
        vSector->AddNode(vDetBox, detectorID++, cTdet);

        TGeoCombiTrans *cTmount = new TGeoCombiTrans( (*cTdet)*(*cT));  cTmount->RegisterYourself();
        TGeoCombiTrans *cTmount1 = new TGeoCombiTrans( (*cTdet)*(*cT1));  cTmount1->RegisterYourself();
        TGeoCombiTrans *cTmountTrd = new TGeoCombiTrans( (*cTdet)*(*cT2));  cTmountTrd->RegisterYourself();

        vSector->AddNode(vBMountTrd, mountID++, cTmount);
        vSector->AddNode(vBMountTrd, mountID++, cTmount1);
        vSector->AddNode(vSMountTrd, mountID++, cTmountTrd);
    }

    // add central detector
    TGeoTranslation *cTdet = new TGeoTranslation("tSecBox",  0., -5.388, Z02+1.1 +SecBox2Zshift);
    vSector->AddNode(vDetBox, detectorID++, cTdet); cTdet->RegisterYourself();
    {
        TGeoCombiTrans *cTmount_M = new TGeoCombiTrans( (*cTdet)*(*cT_M));  cTmount_M->RegisterYourself();
        TGeoCombiTrans *cTmount1_M = new TGeoCombiTrans( (*cTdet)*(*cT1_M));  cTmount1_M->RegisterYourself();
        TGeoCombiTrans *cTmountTrd_M = new TGeoCombiTrans( (*cTdet)*(*cT2_M));  cTmountTrd_M->RegisterYourself();

        vSector->AddNode(vBMountTrdM, mountID++, cTmount_M);
        vSector->AddNode(vBMountTrdM, mountID++, cTmount1_M);
        vSector->AddNode(vSMountTrdM, mountID++, cTmountTrd_M);
    }

    TGeoRotation *geoDetRot2 = new TGeoRotation; geoDetRot2->RotateX(6.); // [degree]
    for(int i = 1; i < 10 ; i++)
    {
        TGeoCombiTrans *cTdet = new TGeoCombiTrans("", 0., -2.8585,  Z02 + i * Zstep +SecBox2Zshift, geoDetRot2); cTdet->RegisterYourself();
        vSector->AddNode(vDetBox, detectorID++, cTdet);

        TGeoCombiTrans *cTmount_2 = new TGeoCombiTrans( (*cTdet)*(*cT_2));  cTmount_2->RegisterYourself();
        TGeoCombiTrans *cTmount1_2 = new TGeoCombiTrans( (*cTdet)*(*cT1_2));  cTmount1_2->RegisterYourself();
        TGeoCombiTrans *cTmountTrd_2 = new TGeoCombiTrans( (*cTdet)*(*cT2_2));  cTmountTrd_2->RegisterYourself();

        vSector->AddNode(vBMountTrd, mountID++, cTmount_2);
        vSector->AddNode(vBMountTrd, mountID++, cTmount1_2);
        vSector->AddNode(vSMountTrd, mountID++, cTmountTrd_2);
    }

    // -------------------------------------- Install sectors -----------------------------------------------------------
    const float sectorR = 156.3;// [cm]  //NEW RADIUS NEEDED
    const float rotationAngle = 25.5; //  [degree]
    
    const double SectorAngles[] = {-76.5, -51., -25.5, 0., 25.5, 51., 76.5, 103.5, 129., 154.5, 180., 205.5, 231., 256.5}; //degrees

    int sectorID = 1;
    TVector3 pos0(0., sectorR, 0.); // [cm]

    cout<<"\n Install sector:";
    for(int i = 0; i < Nsectors; i++)
    {
        float angleDeg = SectorAngles[i]; // [degree]
        float angleRad = angleDeg * TMath::DegToRad(); // [rads]
        cout<<"\n #"<<i<<", angle = "<<angleRad<<" rad, "<<angleDeg<<" degree."<<flush;

        TRotation rot; //rot.RotateX(TMath::Pi());
        rot.RotateZ(angleRad);
        TVector3 posR1 = rot * pos0;

        TGeoRotation *geoRot = new TGeoRotation; geoRot->RotateZ(angleDeg);
        TGeoCombiTrans *cR1 = new TGeoCombiTrans("", posR1.X(),  posR1.Y(), 0., geoRot); cR1->RegisterYourself();

        top->AddNode(vSector, sectorID++, cR1);
    }




// ------------------------ Шкварк i Dlinnij ------------------------- 
//////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////
    /*
        -Everything is made from Aluminium.
        -Volume that contains Korpus has BBox shape while Korpus has "stair" shape.
         May cause overlapipng issues.
        -OGL badly displays the Korpus but raytracing is fine.
    */

    //Volume that contains Korpus + Os + Koleso
    TGeoVolume *vKorpOsKoleso = gGeoManager->MakeBox("vKorpOsKoleso", medAir, 1.5+1.25, 2.35, 5.75);
    vKorpOsKoleso->SetLineWidth(2);
    vKorpOsKoleso->SetInvisible();

//------------------ Korpus ----------------------------------------------------------------------------
    // MainKorpus
    const float MainKorpusX = 1.5, MainKorpusY = 2.35, MainKorpusZ = 5.75;
    TGeoBBox *sMainKorpus = new TGeoBBox(MainKorpusX/2, MainKorpusY/2, MainKorpusZ/2); sMainKorpus->SetName("sMainKorpus");
    TGeoTranslation *tMainKorpus = new TGeoTranslation("tMainKorpus", 0., 0., 0. );
    tMainKorpus -> RegisterYourself();

    // BottomKorpus
    const float MainKorpusX2 = 1.25, MainKorpusY2 = 0.6, MainKorpusZ2 = 5.75;
    TGeoBBox *sMainKorpus2 = new TGeoBBox(MainKorpusX2/2, MainKorpusY2/2, MainKorpusZ2/2); sMainKorpus2->SetName("sMainKorpus2");
    TGeoTranslation *tMainKorpus2 = new TGeoTranslation("tMainKorpus2", -MainKorpusX/2-MainKorpusX2/2, -MainKorpusY/2+MainKorpusY2/2, 0. );
    tMainKorpus2 -> RegisterYourself();
    
    // CutBBox
    const float MainBBoxX3 = 1, MainBBoxY3 = 2.35, MainBBoxZ3 = 1.7;
    TGeoBBox *sMainBBox3 = new TGeoBBox(MainBBoxX3/2, MainBBoxY3/2, MainBBoxZ3/2); sMainBBox3->SetName("sMainBBox3");
    TGeoTranslation *tMainBBox3 = new TGeoTranslation("tMainBBox3", 0.05 , 0., -1.225 );
    tMainBBox3 -> RegisterYourself();  

    //  CutBBox
    const float MainBBoxX4 = 1.5, MainBBoxY4 = 1, MainBBoxZ4 = 1.7;
    TGeoBBox *sMainBBox4 = new TGeoBBox(MainBBoxX4/2, MainBBoxY4/2, MainBBoxZ4/2); sMainBBox4->SetName("sMainBBox4");
    TGeoTranslation *tMainBBox4 = new TGeoTranslation("tMainBBox4", 0., 0.275, 1.225 );
    tMainBBox4 -> RegisterYourself();

    //  CutBBox
    const float MainBBoxX5 = 0.15, MainBBoxY5 = 0.03, MainBBoxZ5 = 5.75;
    TGeoBBox *sMainBBox5 = new TGeoBBox(MainBBoxX5/2, MainBBoxY5/2, MainBBoxZ5/2); sMainBBox5->SetName("sMainBBox5");
    TGeoTranslation *tMainBBox5 = new TGeoTranslation("tMainBBox5", (MainKorpusX/2-MainBBoxX5/2)-0.15, -MainKorpusY/2+MainBBoxY5/2, 0. );
    tMainBBox5 -> RegisterYourself();

    //  CutBBox
    const float MainBBoxX6 = 0.15, MainBBoxY6 = 0.03, MainBBoxZ6 = 5.75;
    TGeoBBox *sMainBBox6 = new TGeoBBox(MainBBoxX6/2, MainBBoxY6/2, MainBBoxZ6/2); sMainBBox6->SetName("sMainBBox6");
    TGeoTranslation *tMainBBox6 = new TGeoTranslation("tMainBBox6", (MainKorpusX/2-MainBBoxX6/2)-0.5, -MainKorpusY/2+MainBBoxY6/2, 0. );
    tMainBBox6 -> RegisterYourself();

    //  CutBBox
    const float MainBBoxX7 = 0.15, MainBBoxY7 = 0.03, MainBBoxZ7 = 5.75;
    TGeoBBox *sMainBBox7 = new TGeoBBox(MainBBoxX7/2, MainBBoxY7/2, MainBBoxZ7/2); sMainBBox7->SetName("sMainBBox7");
    TGeoTranslation *tMainBBox7 = new TGeoTranslation("tMainBBox7", (MainKorpusX/2-MainBBoxX7/2)-0.85, -MainKorpusY/2+MainBBoxY7/2, 0. );
    tMainBBox7 -> RegisterYourself();

    // CutHole
    const float TopTube1Z = 2.35, TopTube1Rmin = 0, TopTube1Rmax = 0.3;
    TGeoTube *sTopTube1 = new TGeoTube(TopTube1Rmin,TopTube1Rmax, TopTube1Z/2); sTopTube1->SetName("sTopTube1");
    TGeoRotation *rTopTube1 = new TGeoRotation; rTopTube1->RotateX(90);
    TGeoCombiTrans *tTopTube1 = new TGeoCombiTrans("tTopTube1", 0., 0., 1.225, rTopTube1);
    tTopTube1 -> RegisterYourself();

    // CutHole
    const float TopTube2Z = 0.2, TopTube2Rmin = 0., TopTube2Rmax = 0.45;
    TGeoTube *sTopTube2 = new TGeoTube(TopTube2Rmin,TopTube2Rmax, TopTube2Z/2); sTopTube2->SetName("sTopTube2");
    TGeoRotation *rTopTube2 = new TGeoRotation; rTopTube2->RotateX(90);
    TGeoCombiTrans *tTopTube2 = new TGeoCombiTrans("tTopTube2", 0., (MainKorpusY-TopTube2Z)/2, 1.225, rTopTube2);
    tTopTube2 -> RegisterYourself();

    // CutHole
    const float TopTube3Z = 1.5, TopTube3Rmin = 0, TopTube3Rmax = 0.3;
    TGeoTube *sTopTube3 = new TGeoTube(TopTube3Rmin,TopTube3Rmax, TopTube3Z/2); sTopTube3->SetName("sTopTube3");
    TGeoRotation *rTopTube3 = new TGeoRotation; rTopTube3->RotateY(90);
    TGeoCombiTrans *tTopTube3 = new TGeoCombiTrans("tTopTube3", 0., 0.3, -1.225, rTopTube3);
    tTopTube3 -> RegisterYourself();

    // CutHole
    const float TopTube4Z = 0.2, TopTube4Rmin = 0., TopTube4Rmax = 0.45;
    TGeoTube *sTopTube4 = new TGeoTube(TopTube4Rmin,TopTube4Rmax, TopTube4Z/2); sTopTube4->SetName("sTopTube4");
    TGeoRotation *rTopTube4 = new TGeoRotation; rTopTube4->RotateY(90);
    TGeoCombiTrans *tTopTube4 = new TGeoCombiTrans("tTopTube4", -(MainKorpusX-TopTube4Z)/2, 0.3, -1.225, rTopTube4);
    tTopTube4 -> RegisterYourself();

    // CutHole
    const float TopTube5Z = 0.6, TopTube5Rmin = 0., TopTube5Rmax = 0.25;
    TGeoTube *sTopTube5 = new TGeoTube(TopTube5Rmin,TopTube5Rmax, TopTube5Z/2); sTopTube5->SetName("sTopTube5");
    TGeoRotation *rTopTube5 = new TGeoRotation; rTopTube5->RotateX(90);
    TGeoCombiTrans *tTopTube5 = new TGeoCombiTrans("tTopTube5", -1.6, -(MainKorpusY-TopTube5Z)/2, 0., rTopTube5);
    tTopTube5 -> RegisterYourself();

    TGeoCombiTrans *tTopTube6 = new TGeoCombiTrans("tTopTube6", -1.6, -(MainKorpusY-TopTube5Z)/2, 2.48, rTopTube5);
    tTopTube6 -> RegisterYourself();

    TGeoCombiTrans *tTopTube7 = new TGeoCombiTrans("tTopTube7", -1.6, -(MainKorpusY-TopTube5Z)/2, -2.48, rTopTube5);
    tTopTube7 -> RegisterYourself();

    // CutHole
    const float TopTube8Z = 1, TopTube8Rmin = 0, TopTube8Rmax = 0.25;
    TGeoTube *sTopTube8 = new TGeoTube(TopTube8Rmin,TopTube8Rmax, TopTube8Z/2); sTopTube8->SetName("sTopTube8");
    TGeoRotation *rTopTube8 = new TGeoRotation; rTopTube8->RotateX(90);
    TGeoCombiTrans *tTopTube8 = new TGeoCombiTrans("tTopTube8", 0.15, -(MainKorpusY-TopTube8Z)/2, 2.48, rTopTube8);
    tTopTube8 -> RegisterYourself();

    TGeoCombiTrans *tTopTube9 = new TGeoCombiTrans("tTopTube9", 0.15, -(MainKorpusY-TopTube8Z)/2, -2.48, rTopTube8);
    tTopTube9 -> RegisterYourself();

    // Assemble  
    TGeoCompositeShape *sKorp = new TGeoCompositeShape("sKorp", "(sMainKorpus:tMainKorpus)+(sMainKorpus2:tMainKorpus2)-(sMainBBox3:tMainBBox3)-(sMainBBox4:tMainBBox4)-(sMainBBox5:tMainBBox5)-(sMainBBox6:tMainBBox6)-(sMainBBox7:tMainBBox7)");
    TGeoTranslation *tKorp = new TGeoTranslation("tKorp", 0., 0., 0.); tKorp -> RegisterYourself();
    TGeoCompositeShape *sKorp2 = new TGeoCompositeShape("sKorp2", "(sKorp:tKorp)-(sTopTube1:tTopTube1)-(sTopTube2:tTopTube2)-(sTopTube3:tTopTube3)-(sTopTube4:tTopTube4)-(sTopTube5:tTopTube5)-(sTopTube5:tTopTube6)-(sTopTube5:tTopTube7)-(sTopTube8:tTopTube8)-(sTopTube8:tTopTube9)");
    TGeoVolume *vKorp2 = new TGeoVolume("vKorp2", sKorp2, medAl);
    vKorpOsKoleso->AddNode(vKorp2, 1);

    //------------------ OS ----------------------------------------------------------------------------
    // MainTube for Small One
    const float MainTubeMin = 0., MainTubeMax = 0.3; float MainTubeZ = 1.24;
    TGeoTube *sMainTube1 = new TGeoTube("sMainTube1", MainTubeMin, MainTubeMax, MainTubeZ/2);

    //TopTube (bonnet)
    const float TopTubeMin = 0., TopTubeMax = 0.45, TopTubeZ = 0.1;
    TGeoTube *sTopTube = new TGeoTube("sTopTube", TopTubeMin, TopTubeMax, TopTubeZ/2);
    TGeoTranslation *tTopTube = new TGeoTranslation("tTopTube", 0., 0., (MainTubeZ+TopTubeZ)/2 );
    tTopTube->RegisterYourself();

    // TopCone (bonnet)
    const float TopConeZ = 0.1, TopConeRmin_1 = 0, TopConeRmax_1 = 0.45, TopConeRmin_2 = 0, TopConeRmax_2 = 0.35;
    TGeoCone *sTopCone = new TGeoCone("sTopCone", TopConeZ/2, TopConeRmin_1,TopConeRmax_1, TopConeRmin_2, TopConeRmax_2 );
    TGeoTranslation *tTopCone = new TGeoTranslation("tTopCone", 0., 0., (MainTubeZ+TopTubeZ*2+TopConeZ)/2 );
    tTopCone->RegisterYourself();

    // BottomCone (Tip)
    const float BottomConeZ = 0.06, BottomConeRmin_1 = 0, BottomConeRmax_1 = 0.27, BottomConeRmin_2 = 0, BottomConeRmax_2 = 0.3;
    TGeoCone *sBottomCone = new TGeoCone("sBottomCone", BottomConeZ/2, BottomConeRmin_1,BottomConeRmax_1, BottomConeRmin_2, BottomConeRmax_2 );
    TGeoTranslation *tBottomCone = new TGeoTranslation("tBottomCone", 0., 0., -(MainTubeZ+BottomConeZ)/2 );
    tBottomCone->RegisterYourself();

    // Combine Small OS
    TGeoCompositeShape *sOS1 = new TGeoCompositeShape("sOS1",
    "sMainTube1+(sTopTube:tTopTube)+(sTopCone:tTopCone)+(sBottomCone:tBottomCone)");
    TGeoVolume *vOS1 = new TGeoVolume("vOS1", sOS1, medAl);
    vOS1->SetLineColor(kRed);

    // Combine Big OS
    MainTubeZ = 2.04;
    TGeoTube *sMainTube_2 = new TGeoTube("sMainTube_2", MainTubeMin, MainTubeMax, MainTubeZ/2);

    TGeoTranslation *tTopTube_2 = new TGeoTranslation("tTopTube_2", 0., 0., (MainTubeZ+TopTubeZ)/2 );
    TGeoTranslation *tTopCone_2 = new TGeoTranslation("tTopCone_2", 0., 0., (MainTubeZ+TopTubeZ*2+TopConeZ)/2 );
    TGeoTranslation *tBottomCone_2 = new TGeoTranslation("tBottomCone_2", 0., 0., -(MainTubeZ+BottomConeZ)/2 );
    tTopTube_2->RegisterYourself();
    tTopCone_2->RegisterYourself();
    tBottomCone_2->RegisterYourself();
    
    TGeoCompositeShape *sOS2 = new TGeoCompositeShape("sOS2",
    "sMainTube_2+(sTopTube:tTopTube_2)+(sTopCone:tTopCone_2)+(sBottomCone:tBottomCone_2)");
    TGeoVolume *vOS2 = new TGeoVolume("vOS2", sOS2, medAl);
    vOS2->SetLineColor(kRed);
    
    // Adding OS to Korpus
    TGeoRotation *rOS1 = new TGeoRotation("rOs1", 180, 90, 90);
    TGeoCombiTrans *cOS1 = new TGeoCombiTrans("cOS1", 0., -0.06+0.015, 1.225,rOS1);
    TGeoRotation *rOS2 = new TGeoRotation("rOs2", 90, -90, 0);
    TGeoCombiTrans *cOS2 = new TGeoCombiTrans("cOS2" ,0.06+0.01, 0.3, -1.225, rOS2);

    vKorpOsKoleso->AddNode(vOS2, 1, cOS1);
    vKorpOsKoleso->AddNode(vOS1, 1, cOS2);

    //---------------------- Koleso ----------------------------------------------------------------------
    // MainKolesoTube
    const float MainKolesoTubeMin = 0.3, MainKolesoTubeMax = 0.45, MainKolesoTubeZ = 1;
    TGeoBBox *sMainKolesoTube = new TGeoTube("sMainKolesoTube", MainKolesoTubeMin, MainKolesoTubeMax, MainKolesoTubeZ/2);

    // TopKolesoTube 
    const float TopKolesoTubeMin = 0.45, TopKolesoTubeMax = 0.8, TopKolesoTubeZ = 0.7;
    TGeoBBox *sTopKolesoTube = new TGeoTube("sTopKolesoTube", TopKolesoTubeMin, TopKolesoTubeMax, TopKolesoTubeZ/2);
    TGeoTranslation *tTopKolesoTube = new TGeoTranslation("tTopKolesoTube", 0., 0., 0. );
    tTopKolesoTube->RegisterYourself();

    // TopKolesoCone
    const float TopKolesoConeZ = 0.05, TopKolesoConeRmin_1 = 0.45, TopKolesoConeRmax_1 = 0.75, TopKolesoConeRmin_2 = 0.45, TopKolesoConeRmax_2 = 0.45;
    TGeoCone *sTopKolesoCone = new TGeoCone("sTopKolesoCone", TopKolesoConeZ/2, TopKolesoConeRmin_1,TopKolesoConeRmax_1, TopKolesoConeRmin_2, TopKolesoConeRmax_2 );
    TGeoTranslation *tTopKolesoCone = new TGeoTranslation("tTopKolesoCone", 0., 0., (MainKolesoTubeZ-TopKolesoConeZ)/2 );
    tTopKolesoCone->RegisterYourself();

    // TopKolesoCone2
    const float TopKolesoCone2Z = 0.05, TopKolesoCone2Rmin_1 = 0.45, TopKolesoCone2Rmax_1 = 0.45, TopKolesoCone2Rmin_2 = 0.45, TopKolesoCone2Rmax_2 = 0.75;
    TGeoCone *sTopKolesoCone2 = new TGeoCone("sTopKolesoCone2", TopKolesoCone2Z/2, TopKolesoCone2Rmin_1,TopKolesoCone2Rmax_1, TopKolesoCone2Rmin_2, TopKolesoCone2Rmax_2 );
    TGeoTranslation *tTopKolesoCone2 = new TGeoTranslation("tTopKolesoCone2", 0., 0., -(MainKolesoTubeZ-TopKolesoCone2Z)/2 );
    tTopKolesoCone2->RegisterYourself();

    //  TopKolesoCone01
    const float TopKolesoConeO1Z = 0.05, TopKolesoConeO1Rmin_1 = 0.45, TopKolesoConeO1Rmax_1 = 0.8, TopKolesoConeO1Rmin_2 = 0.45, TopKolesoConeO1Rmax_2 = 0.7933;
    TGeoCone *sTopKolesoConeO1 = new TGeoCone("sTopKolesoConeO1", TopKolesoConeO1Z/2, TopKolesoConeO1Rmin_1,TopKolesoConeO1Rmax_1, TopKolesoConeO1Rmin_2, TopKolesoConeO1Rmax_2 );
    TGeoTranslation *tTopKolesoConeO1 = new TGeoTranslation("tTopKolesoConeO1", 0., 0., (0.7+0.05)/2 );
    tTopKolesoConeO1->RegisterYourself();

    // TopKolesoCone02
    const float TopKolesoConeO2Z = 0.0366, TopKolesoConeO2Rmin_1 = 0.45, TopKolesoConeO2Rmax_1 = 0.7933, TopKolesoConeO2Rmin_2 = 0.45, TopKolesoConeO2Rmax_2 = 0.775;
    TGeoCone *sTopKolesoConeO2 = new TGeoCone("sTopKolesoConeO2", TopKolesoConeO2Z/2, TopKolesoConeO2Rmin_1,TopKolesoConeO2Rmax_1, TopKolesoConeO2Rmin_2, TopKolesoConeO2Rmax_2 );
    TGeoTranslation *tTopKolesoConeO2 = new TGeoTranslation("tTopKolesoConeO2", 0., 0., (0.7+0.0366)/2+0.05 );
    tTopKolesoConeO2->RegisterYourself();

    // TopKolesoCone03
    const float TopKolesoConeO3Z = 0.0134, TopKolesoConeO3Rmin_1 = 0.45, TopKolesoConeO3Rmax_1 = 0.775, TopKolesoConeO3Rmin_2 = 0.45, TopKolesoConeO3Rmax_2 = 0.75;
    TGeoCone *sTopKolesoConeO3 = new TGeoCone("sTopKolesoConeO3", TopKolesoConeO3Z/2, TopKolesoConeO3Rmin_1,TopKolesoConeO3Rmax_1, TopKolesoConeO3Rmin_2, TopKolesoConeO3Rmax_2 );
    TGeoTranslation *tTopKolesoConeO3 = new TGeoTranslation("tTopKolesoConeO3", 0., 0., (0.7+0.0134)/2+0.05+0.0366);
    tTopKolesoConeO3->RegisterYourself();

    // TopKolesoCone012
    const float TopKolesoConeO12Z = 0.05, TopKolesoConeO12Rmin_1 = 0.45, TopKolesoConeO12Rmax_1 = 0.7933, TopKolesoConeO12Rmin_2 = 0.45, TopKolesoConeO12Rmax_2 = 0.8;
    TGeoCone *sTopKolesoConeO12 = new TGeoCone("sTopKolesoConeO12", TopKolesoConeO12Z/2, TopKolesoConeO12Rmin_1,TopKolesoConeO12Rmax_1, TopKolesoConeO12Rmin_2, TopKolesoConeO12Rmax_2 );
    TGeoTranslation *tTopKolesoConeO12 = new TGeoTranslation("tTopKolesoConeO12", 0., 0., -(0.7+0.05)/2 );
    tTopKolesoConeO12->RegisterYourself();

    //  TopKolesoCone022
    const float TopKolesoConeO22Z = 0.0366, TopKolesoConeO22Rmin_1 = 0.45, TopKolesoConeO22Rmax_1 = 0.775, TopKolesoConeO22Rmin_2 = 0.45, TopKolesoConeO22Rmax_2 = 0.7933;
    TGeoCone *sTopKolesoConeO22 = new TGeoCone("sTopKolesoConeO22", TopKolesoConeO22Z/2, TopKolesoConeO22Rmin_1,TopKolesoConeO22Rmax_1, TopKolesoConeO22Rmin_2, TopKolesoConeO22Rmax_2 );
    TGeoTranslation *tTopKolesoConeO22 = new TGeoTranslation("tTopKolesoConeO22", 0., 0., -(0.7+0.0366)/2-0.05 );
    tTopKolesoConeO22->RegisterYourself();

    // TopKolesoCone032
    const float TopKolesoConeO32Z = 0.0134, TopKolesoConeO32Rmin_1 = 0.45, TopKolesoConeO32Rmax_1 = 0.775, TopKolesoConeO32Rmin_2 = 0.45, TopKolesoConeO32Rmax_2 = 0.775;
    TGeoCone *sTopKolesoConeO32 = new TGeoCone("sTopKolesoConeO32", TopKolesoConeO32Z/2, TopKolesoConeO32Rmin_1,TopKolesoConeO32Rmax_1, TopKolesoConeO32Rmin_2, TopKolesoConeO32Rmax_2 );
    TGeoTranslation *tTopKolesoConeO32 = new TGeoTranslation("tTopKolesoConeO32", 0., 0., -(0.7+0.0134)/2-0.05-0.0366);
    tTopKolesoConeO32->RegisterYourself();

    // Combine Koleso 
    TGeoCompositeShape *sKoleso = new TGeoCompositeShape("sKoleso", 
    "sMainKolesoTube+sTopKolesoTube:tTopKolesoTube+sTopKolesoCone:tTopKolesoCone+sTopKolesoCone2:tTopKolesoCone2+sTopKolesoConeO1:tTopKolesoConeO1+sTopKolesoConeO2:tTopKolesoConeO2+sTopKolesoConeO3:tTopKolesoConeO3+sTopKolesoConeO12:tTopKolesoConeO12+sTopKolesoConeO22:tTopKolesoConeO22+sTopKolesoConeO32:tTopKolesoConeO32");
    TGeoVolume *vKoleso = new TGeoVolume("vKoleso", sKoleso, medAl);
    vKoleso->SetLineColor(kYellow);

    // Adding Koleso to Korpus
    TGeoRotation *rKoleso1 = new TGeoRotation("rKoleso1", 180, 90, 90);
    TGeoCombiTrans *cKoleso1 = new TGeoCombiTrans("cKoleso1", 0., 0.275, 1.225, rKoleso1);
    TGeoRotation *rKoleso2 = new TGeoRotation("rKoleso2", 90, -90, 0);
    TGeoCombiTrans *cKoleso2 = new TGeoCombiTrans("cKoleso2", 0.05 , 0.3, -1.225, rKoleso2);

    vKorpOsKoleso->AddNode(vKoleso, 1, cKoleso1);
    vKorpOsKoleso->AddNode(vKoleso, 2, cKoleso2);

    // Adding Korpus to vSector
    TGeoRotation *geoMainKorpus = new TGeoRotation; geoMainKorpus->RotateZ(90);
    TGeoRotation *geoMainKorpus2 = new TGeoRotation; geoMainKorpus2->RotateZ(90); geoMainKorpus2->RotateY(180);
    TGeoCombiTrans *cMainKorpusTrans = new TGeoCombiTrans("cMainKorpusTrans", 33.3+2.35/2, 1.85+1.25+0.75+2.55+0.5, -3.21 - 5.75/2 + 0.3, geoMainKorpus2);
    TGeoCombiTrans *cMainKorpusTrans2 = new TGeoCombiTrans("cMainKorpusTrans2", -33.3-2.35/2, 1.85+1.25+0.75+2.55+0.5, 3.21 + 5.75/2 + 0.3, geoMainKorpus);
    TGeoCombiTrans *cMainKorpusTrans3 = new TGeoCombiTrans("cMainKorpusTrans3", 33.3+2.35/2, 1.85+1.25+0.75+2.55+0.5, 3.21 + 5.75/2 + 0.3, geoMainKorpus2);
    TGeoCombiTrans *cMainKorpusTrans4 = new TGeoCombiTrans("cMainKorpusTrans4", -33.3-2.35/2, 1.85+1.25+0.75+2.55+0.5, -3.21 - 5.75/2 + 0.3, geoMainKorpus);
    TGeoCombiTrans *cMainKorpusTrans5 = new TGeoCombiTrans("cMainKorpusTrans5", 33.3+2.35/2, 1.85+1.25+0.75+2.55+0.5, -33.17 - 5.75/2 + 0.3, geoMainKorpus2);
    TGeoCombiTrans *cMainKorpusTrans6 = new TGeoCombiTrans("cMainKorpusTrans6", -33.3-2.35/2, 1.85+1.25+0.75+2.55+0.5, 33.17 + 5.75/2 + 0.3, geoMainKorpus);
    TGeoCombiTrans *cMainKorpusTrans7 = new TGeoCombiTrans("cMainKorpusTrans7", 33.3+2.35/2, 1.85+1.25+0.75+2.55+0.5, 33.17 + 5.75/2 + 0.3, geoMainKorpus2);
    TGeoCombiTrans *cMainKorpusTrans8 = new TGeoCombiTrans("cMainKorpusTrans8", -33.3-2.35/2, 1.85+1.25+0.75+2.55+0.5, -33.17 - 5.75/2 + 0.3, geoMainKorpus);
    TGeoCombiTrans *cMainKorpusTrans9 = new TGeoCombiTrans("cMainKorpusTrans9", 33.3+2.35/2, 1.85+1.25+0.75+2.55+0.5, -93.09 - 5.75/2 + 0.3, geoMainKorpus2);
    TGeoCombiTrans *cMainKorpusTrans10 = new TGeoCombiTrans("cMainKorpusTrans10", -33.3-2.35/2, 1.85+1.25+0.75+2.55+0.5, 93.09 + 5.75/2 + 0.3, geoMainKorpus);
    TGeoCombiTrans *cMainKorpusTrans11 = new TGeoCombiTrans("cMainKorpusTrans11", 33.3+2.35/2, 1.85+1.25+0.75+2.55+0.5, 93.09 + 5.75/2 + 0.3, geoMainKorpus2);
    TGeoCombiTrans *cMainKorpusTrans12 = new TGeoCombiTrans("cMainKorpusTrans12", -33.3-2.35/2, 1.85+1.25+0.75+2.55+0.5, -93.09 - 5.75/2 + 0.3, geoMainKorpus);
    TGeoCombiTrans *cMainKorpusTrans13 = new TGeoCombiTrans("cMainKorpusTrans13", 33.3+2.35/2, 1.85+1.25+0.75+2.55+0.5, -182.96 - 5.75/2 + 0.3, geoMainKorpus2);
    TGeoCombiTrans *cMainKorpusTrans14 = new TGeoCombiTrans("cMainKorpusTrans14", -33.3-2.35/2, 1.85+1.25+0.75+2.55+0.5, 182.96 + 5.75/2 + 0.3, geoMainKorpus);
    TGeoCombiTrans *cMainKorpusTrans15 = new TGeoCombiTrans("cMainKorpusTrans15", 33.3+2.35/2, 1.85+1.25+0.75+2.55+0.5, 182.96 + 5.75/2 + 0.3, geoMainKorpus2);
    TGeoCombiTrans *cMainKorpusTrans16 = new TGeoCombiTrans("cMainKorpusTrans16", -33.3-2.35/2, 1.85+1.25+0.75+2.55+0.5, -182.96 - 5.75/2 + 0.3, geoMainKorpus);
    TGeoCombiTrans *cMainKorpusTrans17 = new TGeoCombiTrans("cMainKorpusTrans17", 33.3+2.35/2, 1.85+1.25+0.75+2.55+0.5, -282.84 - 5.75/2 + 0.3, geoMainKorpus2);
    TGeoCombiTrans *cMainKorpusTrans18 = new TGeoCombiTrans("cMainKorpusTrans18", -33.3-2.35/2, 1.85+1.25+0.75+2.55+0.5, 272.84 + 5.75/2 + 0.3, geoMainKorpus);
    TGeoCombiTrans *cMainKorpusTrans19 = new TGeoCombiTrans("cMainKorpusTrans19", 33.3+2.35/2, 1.85+1.25+0.75+2.55+0.5, 272.84 + 5.75/2 + 0.3, geoMainKorpus2);
    TGeoCombiTrans *cMainKorpusTrans20 = new TGeoCombiTrans("cMainKorpusTrans20", -33.3-2.35/2, 1.85+1.25+0.75+2.55+0.5, -282.84 - 5.75/2 + 0.3, geoMainKorpus);

    /*vSector->AddNode(vKorpOsKoleso, 1, cMainKorpusTrans);
    vSector->AddNode(vKorpOsKoleso, 2, cMainKorpusTrans2);
    vSector->AddNode(vKorpOsKoleso, 3, cMainKorpusTrans3);
    vSector->AddNode(vKorpOsKoleso, 4, cMainKorpusTrans4);
    vSector->AddNode(vKorpOsKoleso, 5, cMainKorpusTrans5);
    vSector->AddNode(vKorpOsKoleso, 6, cMainKorpusTrans6);
    vSector->AddNode(vKorpOsKoleso, 7, cMainKorpusTrans7);
    vSector->AddNode(vKorpOsKoleso, 8, cMainKorpusTrans8);
    vSector->AddNode(vKorpOsKoleso, 9, cMainKorpusTrans9);
    vSector->AddNode(vKorpOsKoleso, 10, cMainKorpusTrans10);
    vSector->AddNode(vKorpOsKoleso, 11, cMainKorpusTrans11);
    vSector->AddNode(vKorpOsKoleso, 12, cMainKorpusTrans12);
    vSector->AddNode(vKorpOsKoleso, 13, cMainKorpusTrans13);
    vSector->AddNode(vKorpOsKoleso, 14, cMainKorpusTrans14);
    vSector->AddNode(vKorpOsKoleso, 15, cMainKorpusTrans15);
    vSector->AddNode(vKorpOsKoleso, 16, cMainKorpusTrans16);
    vSector->AddNode(vKorpOsKoleso, 17, cMainKorpusTrans17);
    vSector->AddNode(vKorpOsKoleso, 18, cMainKorpusTrans18);
    vSector->AddNode(vKorpOsKoleso, 19, cMainKorpusTrans19);
    vSector->AddNode(vKorpOsKoleso, 20, cMainKorpusTrans20);*/


//------------------ Power Frame ----------------------------------------------------------------------------

    // Outer Tube
    const float OuterTubeMin = 227.8, OuterTubeMax = 229.3, OuterTubeZ = 766.0 ;
    TGeoTube *sOuterTube = new TGeoTube("sOuterTube", OuterTubeMin, OuterTubeMax, OuterTubeZ/2);
    TGeoVolume *vOuterTube = new TGeoVolume("vOuterTube", sOuterTube, medAl);
    vOuterTube->SetLineColor(kBlue);
 ////   top->AddNode(vOuterTube, 1);  LSP123 <<<<<<<<<<<<<<<<<<

    // Inner Tube
    const float InnerTubeMin = 168.0, InnerTubeMax = 170.25, InnerTubeZ = 624.4 ;
    TGeoTube *sInnerTube = new TGeoTube("sInnerTube", InnerTubeMin, InnerTubeMax, InnerTubeZ/2);
    TGeoVolume *vInnerTube = new TGeoVolume("vInnerTube", sInnerTube, medAl);
    vInnerTube->SetLineColor(kBlue);
////    top->AddNode(vInnerTube, 1); LSP123 <<<<<<<<<<<<<<<<<<

    // BigRails
    TGeoBBox *sBigRailBase = new TGeoBBox("sBigRailBase", 0.4/2, 10.2/2, 600./2 );

    TGeoBBox *sBigRailGuide = new TGeoBBox("sBigRailGuide", 1.7/2, 1.6, 601./2 );
    TGeoRotation *rBigRailGuide1 = new TGeoRotation("rBigRailGuide1", 0, 0, -13.5);
    TGeoRotation *rBigRailGuide2 = new TGeoRotation("rBigRailGuide2", 0, 0, 13.5);
    TGeoCombiTrans *cBigRailGuide1 = new TGeoCombiTrans("cBigRailGuide1", 1.89, 10.2/2 - 0.36, 0, rBigRailGuide1);
    TGeoCombiTrans *cBigRailGuide2 = new TGeoCombiTrans("cBigRailGuide2", 1.89, -10.2/2 + 0.36, 0, rBigRailGuide2);
    cBigRailGuide1->RegisterYourself();
    cBigRailGuide2->RegisterYourself();

    TGeoTrd1 *sBigRailTrd1 = new TGeoTrd1("sBigRailTrd1", 10.2/2, 8.37/2, 600./2, 3.8/2 );
    TGeoRotation *rBigRailTrd1 = new TGeoRotation("rBigRailTrd1", 90, 90, 0);
    TGeoCombiTrans *cBigRailTrd1 = new TGeoCombiTrans("cBigRailTrd1", 0.4/2 + 3.8/2, 0, 0, rBigRailTrd1);
    cBigRailTrd1->RegisterYourself();
    
    TGeoTrd1 *sBigRailTrd2 = new TGeoTrd1("sBigRailTrd2", 5.73/2, 4.2/2, 600./2, 3.186/2 );
    TGeoRotation *rBigRailTrd2 = new TGeoRotation("rBigRailTrd2", 90, 90, 0);
    TGeoCombiTrans *cBigRailTrd2 = new TGeoCombiTrans("cBigRailTrd2", 0.4/2 + 3.8 + 3.186/2 , 0, 0, rBigRailTrd2);
    cBigRailTrd2->RegisterYourself();

    TGeoBBox *sBigRailWaist = new TGeoBBox("sBigRailWaist", 2.311/2, 4.2/2, 600./2 );
    TGeoTranslation *tBigRailWaist = new TGeoTranslation("tBigRailWaist", 0.4/2 + 3.8 + 3.186 + 2.311/2, 0, 0);
    tBigRailWaist->RegisterYourself();

    TGeoTrd1 *sBigRailTrd3 = new TGeoTrd1("sBigRailTrd3", 6.872/2, 3.13/2, 600./2, 7.803/2 );
    TGeoRotation *rBigRailTrd3 = new TGeoRotation("rBigRailTrd3", 90, 90, 0);
    TGeoCombiTrans *cBigRailTrd3 = new TGeoCombiTrans("cBigRailTrd3", 0.4/2 + 3.8 + 3.186 + 2.311 + 7.803/2 , 0, 0, rBigRailTrd3);
    cBigRailTrd3->RegisterYourself();

    TGeoBBox *sBigRailCollar = new TGeoBBox("sBigRailCollar", 1./2, 5./2, 600./2 );
    TGeoTranslation *tBigRailCollar = new TGeoTranslation("tBigRailCollar", 0.4/2 + 3.8 + 3.186 + 2.311 + 7.803 + 1./2, 0, 0);
    tBigRailCollar->RegisterYourself();
    
    TGeoBBox *sBigRailNeck = new TGeoBBox("sBigRailNeck", 4.023/2, 4.2/2, 600./2 );
    TGeoTranslation *tBigRailNeck = new TGeoTranslation("tBigRailNeck", 0.4/2 + 3.8 + 3.186 + 2.311 + 7.803 + 1. + 4.023/2, 0, 0);
    tBigRailNeck->RegisterYourself();
    
    TGeoBBox *sBigRailHead = new TGeoBBox("sBigRailHead", 3.4/2, 8./2, 600./2 );
    TGeoTranslation *tBigRailHead = new TGeoTranslation("tBigRailHead", 0.4/2 + 3.8 + 3.186 + 2.311 + 7.803 + 1. + 4.023 + 3.4/2, 0, 0);
    tBigRailHead->RegisterYourself();

    TGeoBBox *sBigRailForelock = new TGeoBBox("sBigRailForelock", 0.3, 4.2/2, 601./2 );
    TGeoTranslation *tBigRailForelock = new TGeoTranslation("tBigRailForelock", 0.4/2 + 3.8 + 3.186 + 2.311 + 7.803 + 1. + 4.023 + 3.4, 0, 0);
    tBigRailForelock->RegisterYourself();

    TGeoTranslation *tInnerTubeCut = new TGeoTranslation("tInnerTubeCut", InnerTubeMin, 0, 0);
    tInnerTubeCut->RegisterYourself();

    TGeoCompositeShape *sBigRail = new TGeoCompositeShape("sBigRail",
    "(sBigRailBase+sBigRailTrd1:cBigRailTrd1+sBigRailTrd2:cBigRailTrd2+sBigRailWaist:tBigRailWaist+sBigRailTrd3:cBigRailTrd3+sBigRailCollar:tBigRailCollar+sBigRailNeck:tBigRailNeck+sBigRailHead:tBigRailHead)-(sBigRailForelock:tBigRailForelock+sBigRailGuide:cBigRailGuide1+sBigRailGuide:cBigRailGuide2+sInnerTube:tInnerTubeCut)");
    TGeoVolume *vBigRail = new TGeoVolume("vBigRail", sBigRail, medAl);
    vBigRail->SetLineColor(kGreen);
    TGeoRotation *rBigRail = new TGeoRotation("rBigRail", 0, 0, 180);

    top->AddNode(vBigRail, 1, new TGeoCombiTrans( InnerTubeMin, 0, 0, rBigRail) ); // LSP123 <<<<<<<<<<<<<<<<<<
    top->AddNode(vBigRail, 2, new TGeoTranslation( -(InnerTubeMin), 0, 0) );// LSP123 <<<<<<<<<<<<<<<<<<

    // Small Rails
    const float SmallRailY = 3.59;
    TGeoTrd1 *sSmallRailBody = new TGeoTrd1("sSmallRailBody", 4.1/2, 5.9/2, 600/2, 3.59/2);
    TGeoVolume *vSmallRailBody = new TGeoVolume("vSmallRailBody", sSmallRailBody, medAl);
    TGeoRotation *rSmallrailBody = new TGeoRotation("rSmallrailBody", 180, 90, 0);
    rSmallrailBody->RegisterYourself();

    TGeoBBox *sSmallRailGuide = new TGeoBBox("sSmallRailGuide", 3/2, 1.9/2, 602/2);
    TGeoRotation *rSmallRailGuide1 = new TGeoRotation("rSmallRailGuide1", 0, 0, -12.75);
    TGeoRotation *rSmallRailGuide2 = new TGeoRotation("rSmallRailGuide2", 0, 0, 12.75);
    TGeoCombiTrans *cSmallRailGuide1 = new TGeoCombiTrans("cSmallRailGuide1", 5/2, 0, 0, rSmallRailGuide1);
    TGeoCombiTrans *cSmallRailGuide2 = new TGeoCombiTrans("cSmallRailGuide2", -5/2, 0, 0, rSmallRailGuide2);
    cSmallRailGuide1->RegisterYourself();
    cSmallRailGuide2->RegisterYourself();

    TGeoTranslation *tInnerTubeCut2 = new TGeoTranslation("tInnerTubeCut2", 0, SmallRailY/2 - InnerTubeMin, 0);
    tInnerTubeCut2->RegisterYourself();

    TGeoCompositeShape *sSmallRaill = new TGeoCompositeShape("sSmallRaill",
    "sSmallRailBody:rSmallrailBody-(sSmallRailGuide:cSmallRailGuide1+sSmallRailGuide:cSmallRailGuide2+sInnerTube:tInnerTubeCut2)");
    TGeoVolume *vSmallRail = new TGeoVolume("vSmallRail", sSmallRaill, medAl);
    vSmallRail->SetLineColor(kGreen);
    
    TVector3 RailPos(0., InnerTubeMin - SmallRailY/2, 0.);
    const double SmallRailAngles[] = {-63.75, -38.25, -12.75, 12.75, 38.25, 63.75, -116.25, -141.75, -167.25, -192.75, -218.25, -243.75}; //degrees
    for(int i = 0; i < 12; i++){
        float RailAngleDeg = SmallRailAngles[i];
        float RailAngleRad = RailAngleDeg * TMath::DegToRad();

        TRotation RailRot;
        RailRot.RotateZ(RailAngleRad);
        TVector3 RailPosR1 = RailRot * RailPos;

        TGeoRotation *RailGeoRot = new TGeoRotation; RailGeoRot->RotateZ(RailAngleDeg);
        TGeoCombiTrans *RailCombi = new TGeoCombiTrans("RailCombi", RailPosR1.X(),  RailPosR1.Y(), 0., RailGeoRot); RailCombi->RegisterYourself();

        top->AddNode(vSmallRail, i+1, RailCombi); //LSP123
    }

    //------------------ Clock Face --------------------
    const float TrapezX_1 = OuterTubeZ+60, TrapezX_2 =InnerTubeZ, TrapezY = 0.195, TrapezZ = OuterTubeMin - InnerTubeMax;
    TGeoTrd1 *sTrapez = new TGeoTrd1("sTrapez", TrapezX_1/2, TrapezX_2/2, TrapezY/2, TrapezZ/2);
    TGeoRotation rTrapez1; rTrapez1.SetAngles(0,90,90);
    TGeoTranslation tTrapez1(0,0,0);
    TGeoCombiTrans cTrapez1(tTrapez1,rTrapez1);
    TGeoVolume *vTrapez = new TGeoVolume("vTrapez", sTrapez, medAl);
    vTrapez->SetLineColor(kRed);

    TVector3 TrapezPos(0., (InnerTubeMax + OuterTubeMin)/2, 0.);
    for(int i = 0; i < 25; i++){
/*        float TrapezAngleDeg = 14.4*i;
        float TrapezAngleRad = TrapezAngleDeg * TMath::DegToRad();

        TRotation TrapezRot;
        TrapezRot.RotateZ(TrapezAngleRad);
        TVector3 TrapezPosR1 = TrapezRot * TrapezPos;

        TGeoRotation rTrapez2; rTrapez2.SetAngles(0 , 0, TrapezAngleDeg);
        TGeoTranslation tTrapez2(TrapezPosR1.X(), TrapezPosR1.Y(), 0.);
        TGeoCombiTrans cTrapez2(tTrapez2,rTrapez2);
        TGeoHMatrix cTrapez = cTrapez2 * cTrapez1;

        TGeoCombiTrans *TrapezCombi = new TGeoCombiTrans(cTrapez);
        TrapezCombi->RegisterYourself();

        top->AddNode(vTrapez, i+1, TrapezCombi);
*/ //LSP123
    }

// ---------------- Tooth ------------------
    const float ToothRmin = 227.8, ToothRmax = 229.3, ToothZ = 40, Toothf1 = 90-4.734, Toothf2 = 94.734;
    TGeoTubeSeg *sTooth = new TGeoTubeSeg("sTooth", ToothRmin, ToothRmax, ToothZ/2, Toothf1, Toothf2);
    TGeoTranslation *tTooth = new TGeoTranslation("tTooth", 0., 0., 0. );
    tTooth -> RegisterYourself();

    const float ToothOZ = 1.5, ToothORmin = 0, ToothORmax = 1.218;
    TGeoTube *sToothO = new TGeoTube(ToothORmin,ToothORmax, ToothOZ); sToothO->SetName("sToothO");
    TGeoRotation *rToothO = new TGeoRotation; rToothO->RotateX(90);
    TGeoCombiTrans *tToothO = new TGeoCombiTrans("tToothO", 6.52, 228.55, -10, rToothO);
    tToothO -> RegisterYourself();

    TGeoCombiTrans *tToothO1 = new TGeoCombiTrans("tToothO1", -6.52, 228.55, -10, rToothO);
    tToothO1 -> RegisterYourself();

    TGeoCombiTrans *tToothO2 = new TGeoCombiTrans("tToothO2", 6.52, 228.55, -4.5, rToothO);
    tToothO2 -> RegisterYourself();

    TGeoCombiTrans *tToothO3 = new TGeoCombiTrans("tToothO3", -6.52, 228.55, -4.5, rToothO);
    tToothO3 -> RegisterYourself();

    TGeoCombiTrans *tToothO4 = new TGeoCombiTrans("tToothO4", 12.57, 228.55, -10, rToothO);
    tToothO4 -> RegisterYourself();

    TGeoCombiTrans *tToothO5 = new TGeoCombiTrans("tToothO5", -12.57, 228.55, -10, rToothO);
    tToothO5 -> RegisterYourself();

    TGeoCombiTrans *tToothO6 = new TGeoCombiTrans("tToothO6", 12.57, 228.55, -4.5, rToothO);
    tToothO6 -> RegisterYourself();

    TGeoCombiTrans *tToothO7 = new TGeoCombiTrans("tToothO7", -12.57, 228.55, -4.5, rToothO);
    tToothO7 -> RegisterYourself();

    TGeoCompositeShape *sTooth0 = new TGeoCompositeShape("sTooth0", "(sTooth:tTooth)-(sToothO:tToothO)-(sToothO:tToothO1)-(sToothO:tToothO2)-(sToothO:tToothO3)-(sToothO:tToothO4)-(sToothO:tToothO5)-(sToothO:tToothO6)-(sToothO:tToothO7)");
    TGeoVolume *vTooth = new TGeoVolume("vTooth", sTooth0, medAl);
    vTooth -> SetLineColor(kBlue);

    TVector3 ToothPos(0., 0., 0.);
    for(int i = 0; i < 50; i++){
 /*   LSP123    
        float ToothAngleDeg = 14.4*i;
        float ToothAngleRad = ToothAngleDeg * TMath::DegToRad();

        TRotation ToothRot;
        ToothRot.RotateZ(ToothAngleRad);
        TVector3 ToothPosR1 = ToothRot * ToothPos;

        TGeoRotation *ToothGeoRot = new TGeoRotation; ToothGeoRot->RotateZ(ToothAngleDeg);
        TGeoRotation *ToothGeoRot2 = new TGeoRotation; ToothGeoRot2->RotateZ(ToothAngleDeg); ToothGeoRot2->RotateY(180);
        
        if(i>24){
        TGeoCombiTrans *ToothCombi = new TGeoCombiTrans("ToothCombi", ToothPosR1.X(),  ToothPosR1.Y(), -(OuterTubeZ+ToothZ)/2, ToothGeoRot); ToothCombi->RegisterYourself();
        top->AddNode(vTooth, i+1, ToothCombi);
        }else{
        TGeoCombiTrans *ToothCombi1 = new TGeoCombiTrans("ToothCombi1", ToothPosR1.X(),  ToothPosR1.Y(), (OuterTubeZ+ToothZ)/2, ToothGeoRot2); ToothCombi1->RegisterYourself();
        top->AddNode(vTooth, i+1, ToothCombi1);
        }
*/        // top->AddNode(vTooth, i+1, ToothCombi);
    }







//////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////

    topTof->AddNode(top, 1, new TGeoTranslation("tw",  0.,  0., 0.)); // LSP123

    cerr<<endl<<endl;
    // ---------------   Finish   --------------------------------------------
    gGeoManager->CloseGeometry();

    // ------------------------------------------------- Check overlaps
    gGeoManager->CheckOverlaps(0.001);
    gGeoManager->PrintOverlaps();
    gGeoManager->Test();

    // --------------------------------------------------- Save
    TFile* geoFile = new TFile(geoFileName, "RECREATE");
    gGeoManager->GetTopVolume()->Write();
    geoFile->Close();

    // --------------------------------------------------- Vizualization
/*  TEveManager::Create();

  RecursiveVisible();

    TGeoNode* node = gGeoManager->GetTopNode();
    TEveGeoTopNode* en = new TEveGeoTopNode(gGeoManager, node);
    en->SetVisLevel(9);
  //  en->GetNode()->GetVolume()->SetVisibility(kFALSE);

    gEve->AddGlobalElement(en);
    gEve->Redraw3D(kTRUE);
  */

    top->SetVisContainers(kTRUE);
    gGeoManager->SetVisOption(0);
    gGeoManager->SetVisLevel(7);
    top->Draw("ogl");
    // top->Raytrace();

}
