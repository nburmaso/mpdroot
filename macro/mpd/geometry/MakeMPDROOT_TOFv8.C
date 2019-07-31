//**************************************************************************************
// MPDROOT TOF version 8
// sector 	[1,...,14]
// detector	[1,...,20]
// gap	 	[1,...,3]
// strip 	[1,...,24]
// N strips = 14 * 20 * 24 = 6720
// N sensitive volumes = 6720 * 3 gap = 20160 sv
// strip area = stripGasX(65.) * stripGasZ(1.25)  = 81.25 // [cm^2]
// total strips area = 6720 * 81.25 = 5.46E+5 cm^2 = 54.6 m^2
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
void MakeMPDROOT_TOFv8()
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
    const TString geoFileName = geoPath + "/geometry/" + geoDetectorName + "_"+ geoDetectorVersion + "_amakempdroot_tof_part2.root";

    // -----------------   Get and create the required media    --------------
    FairGeoMedia*   geoMedia = geoFace->getMedia();
    FairGeoBuilder* geoBuild = geoLoad->getGeoBuilder();

    // Air
    FairGeoMedium* matAir = geoMedia->getMedium("air");		if(! matAir) Fatal("Main", "FairMedium air not found");
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
    FairGeoMedium* matG10 = geoMedia->getMedium("G10");		if(! matG10) Fatal("Main", "FairMedium G10 not found");
    geoBuild->createMedium(matG10);
    TGeoMedium* medG10 = gGeoManager->GetMedium("G10");		if(! medG10) Fatal("Main", "Medium G10 not found");

    // -----------------------------------------   Create geometry and top volume  -----------------------------------------------
    gGeoManager = (TGeoManager*)gROOT->FindObject("FAIRGeom");
    gGeoManager->SetName(geoDetectorName + "_geom");
    TGeoVolume* top = new TGeoVolumeAssembly("TOP"); // TOP VOLUME

    gGeoManager->SetTopVolume(top);
    gGeoManager->SetTopVisible();
 //   gGeoManager->SetVisLevel(7);

    
    TGeoVolume* topTof = new TGeoVolumeAssembly("tof1"); // TOP tof volume
    // ------------------------------------------------------ Create sector -------------
    const size_t Nsectors =  14;
    const double tofRmin = 147.5, tofRmax = 165.5, tofZ = 292.*2.;
  
    const double trap_dY =  (tofRmax - tofRmin)/2.;
    const double trap_dZ =  tofZ/2.;  
  
    const double sectorAngle_2 = 25.5 /2. * TMath::DegToRad(); // [rad]
    const double trap_dXmax = tofRmin * TMath::Tan(sectorAngle_2);
    const double trap_dXmin = tofRmax * TMath::Tan(sectorAngle_2);   
   
    TGeoVolume *vSector = gGeoManager->MakeTrap("tof1Sector", medAir, trap_dZ, 0, 0, trap_dY, trap_dXmax, trap_dXmin, 0, trap_dY, trap_dXmax, trap_dXmin, 0);
    vSector->SetLineWidth(2);
 //   vSector->SetLineColor(kBlue);
    vSector->SetInvisible();
    // ----------------------------------------------------------- ASSEMBLE sector ----------------------------------------------
    const float sectorBoxX = 66.2, sectorBoxY = 16.3, sectorBoxZ = 291.8; // [cm]  SIZE OF ALL PART
    const double SecBoxZshift =  -291.2/2.-0.3;
    const double SecBox2Zshift =  281.2/2.+0.3;

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

    //------------- Create detector boxe ----------------
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
    TGeoVolume *vDetGlass = new TGeoVolume("tof1DetGlassV", sDetGlass, medGlass);   vDetGlass->SetLineColor(TColor::GetColor("#00ff00"));

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
    for(int i=1; i <= NStrips; i++)
    {
        vDetBox->AddNode(vStripGas, 0*NStrips + i, new TGeoTranslation("",  0.,  0.442 - 0.098, -detGlassZ/2. + StripStep/2. + i * StripStep)); // [ 1,24]
        vDetBox->AddNode(vStripGas, 1*NStrips + i, new TGeoTranslation("",  0.,  0.    - 0.098, -detGlassZ/2. + StripStep/2. + i * StripStep)); // [25,48] - middle layer
        vDetBox->AddNode(vStripGas, 2*NStrips + i, new TGeoTranslation("",  0., -0.442 - 0.098, -detGlassZ/2. + StripStep/2. + i * StripStep)); // [49,72]
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
    const float sectorR = 148.5 +  16./2.;// [cm]  //NEW RADIUS NEEDED
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

        topTof->AddNode(vSector, sectorID++, cR1);
    }
    
    
    
       	top->AddNode(topTof, 1, new TGeoTranslation("tw",  0.,  0., 0.)); 
        
    cerr<<endl<<endl;
    // ---------------   Finish   --------------------------------------------
    gGeoManager->CloseGeometry();

    // ------------------------------------------------- Check overlaps
    gGeoManager->CheckOverlaps(0.001);
    gGeoManager->PrintOverlaps();
    gGeoManager->Test();
    
    // --------------------------------------------------- Save 
    TFile* geoFile = new TFile(geoFileName, "RECREATE");
    top->Write();
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
/*  
    top->SetVisContainers(kTRUE);
    gGeoManager->SetVisOption(0);
    gGeoManager->SetVisLevel(7);
    gGeoManager->SetMaxVisNodes(100000);
    top->Draw("ogl");
*/
}
