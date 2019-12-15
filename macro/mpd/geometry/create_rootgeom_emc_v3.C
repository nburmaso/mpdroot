// ********************************************************************
// *                                                                  *
// *  Proje\ct:   mpd.jinr.ru			                      *
// *  Created:   20-September-2019				      *
// *  Author: M. Martemianov / MPD collaboratiobn		      *
// *								      *
// ********************************************************************

#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"
#include "TFile.h"

#include "../mpdloadlibs.C"


// Media

TGeoMedium *pMedAir = 0;
TGeoMedium *pMedAluminium = 0;
TGeoMedium *pMedFscScint = 0;
TGeoMedium *pMedLead = 0;
TGeoMedium *pMedKapton = 0;
TGeoMedium *pMedGlue = 0;
TGeoMedium *pMedCarbComp = 0;
TGeoMedium *pMedFbGlass = 0;
TGeoMedium *pMedTiPaint = 0;

// Detector coordinates and position
const Double_t EMC_Xpos = 0.0;
const Double_t EMC_Ypos = 0.0;
const Double_t EMC_Zpos = 0.0;

// ECal Total sizes 
Double_t EMC_Box_Length = 41.55; // Tower height
Double_t EMC_Length; // max length of ECAL

// Power frame of EMC
Double_t EMC_Frame_InnerRadius = 168.0, EMC_Frame_OuterRadius = 229.3;
Double_t EMC_Frame_Wall1 = 2.5, EMC_Frame_Wall2 = 1.0, EMC_Frame_Wall3 = 2.0; 
Double_t EMC_Frame_gap1 = 0.2; 

// Sector parameters 
const Int_t EMC_NSectors = 25; // Number of sectors
const Int_t EMC_NSector_div1 = 6, EMC_NSector_div2 = 8; 
Double_t EMC_Sector_wall1 = 0.5, EMC_Sector_wall2 = 0.2;
Double_t EMC_Sector_InnerFrame = 170.7, EMC_Sector_InnerRadius = 171.5; 
Double_t EMC_Sector_OuterRadius = 227.0;
Double_t EMC_Tower_InnerRadius = 171.56;
Double_t EMC_Box_gap = 0.02;

const Int_t EMC_Sector_NHoles1 = 4, EMC_Sector_NHoles2 = 8;
Double_t EMC_SectorHoleLen1[EMC_Sector_NHoles2] = {24.35, 24.21, 24.98, 26.23, 28.08, 30.74, 34.55, 39.13}; 
Double_t EMC_SectorHoleLen2[EMC_Sector_NHoles2] = {24.29, 24.14, 24.91, 26.16, 27.99, 30.64, 34.42, 38.96}; 
Double_t EMC_Sector_Wall3 = 1.5; // half size

Double_t EMC_Sector_angle0 = 0.; 
Double_t EMC_Sector_angle = TMath::TwoPi()/EMC_NSectors; 
Double_t EMC_Module_angle = EMC_Sector_angle/EMC_NSector_div1;

// Tower parameters

TList* listECALModules = new TList();
TList* listECALTowers = new TList();

Int_t EMC_Box_Layers = 210; 
Double_t EMC_Box_Sc = 0.15; // Scintillator size
Double_t EMC_Box_Pb = 0.03; // Pb size
Double_t EMC_Box_Col = 0.005; // Pb couting color size 
Double_t EMC_Box_Cell = EMC_Box_Sc + EMC_Box_Pb + 2.*EMC_Box_Col;// Total size of cell
Double_t EMC_Box_Plastic1 = 0.7, EMC_Box_Plastic2 = 0.8; // Fixing plates sizes
Double_t EMC_Box_Angle1 = 0.5*0.9*TMath::DegToRad();
Double_t EMC_Box_Angle2 = 0.5*1.2*TMath::DegToRad();
Double_t EMC_Box_Bottom_X = 3.32, EMC_Box_Top_X = 3.97;
Double_t EMC_Box_Bottom_Y = 3.31, EMC_Box_Top_Y = 4.00;
Double_t EMC_Box_Z = EMC_Box_Length;

Double_t EMC_Wall_Add = 4.0;
Double_t EMC_Module_side = EMC_Box_Length/cos(EMC_Box_Angle1);

const Int_t EMC_Index_Box = 64;
const Int_t numBoxSize = 100;
Double_t* EMC_Box_A = new Double_t[numBoxSize];
Double_t* EMC_Box_B = new Double_t[numBoxSize];
Double_t* EMC_Box_C = new Double_t[numBoxSize];
Double_t EMC_Box_angle;

class FairGeoMedia;
class FairGeoBuilder;
void DefineMediaParameters(FairGeoMedia* , FairGeoBuilder*);
void ParseNode(TXMLEngine*, XMLNodePointer_t);

void CreateECALFrame(); // Power frame settings
void CreateECALSectors(); // Sector geometry settings
void CreateECALModShape(); // Shape for module
void CreateECALModule(); // Module (2x8 towers) 

void CreateECALTower(Int_t); // tower as one trapezoid
void CreateECALCompTower(Int_t, Double_t); // tower as two trapezoids
void CreateECALCompTower(Int_t, Double_t, Double_t); // tower as three trepezoids
void CreateECALStructure(TGeoVolume*, TList*);
void CreateECALTowerEdges(TList*, Double_t, Double_t, Double_t*, Double_t*);

Double_t* CalculateZPos(); // central position of each tower in module
Double_t* CalculateZPosWall(); // transverse wall position in sector
Double_t* EMC_zPosition = new Double_t[numBoxSize];
Double_t* EMC_zWallPosition = new Double_t[numBoxSize];
	
// Settings to check geometry quality

Bool_t fDebug = kTRUE;
Double_t fPrecision = 0.00001;

TGeoVolume *emcChH;
TGeoVolume *emcSector; 
TGeoVolume *emcCrate; 

//
// Root function to create EMC/MPD geometry for root - file
//

void create_rootgeom_emc_v3() {

// Load MPD libraries

/////   mpdloadlibs(1, 1);

// Set working directory

  TString gPath = gSystem->Getenv("VMCWORKDIR");
  const TString geoDetectorName = "emcChamber";
  const TString geoDetectorVersion = "v3";
  TString geoFileName = gPath + "/geometry/emc_"+ geoDetectorVersion + ".root";

// Global geometry parameters

  FairGeoLoader*    geoLoad = new FairGeoLoader("TGeo","FairGeoLoader");
  FairGeoInterface* geoFace = geoLoad->getGeoInterface();
  
// Load media from media file

  TString medFile = gPath + "/geometry/media.geo";
  geoFace->setMediaFile(medFile);
  geoFace->readMedia();
  FairGeoMedia*   geoMedia = geoFace->getMedia();
  FairGeoBuilder* geoBuild = geoLoad->getGeoBuilder();
  DefineMediaParameters(geoMedia, geoBuild);

// Create geometry and global top volume

  gGeoManager = (TGeoManager*)gROOT->FindObject("FAIRGeom");
  gGeoManager->SetName(geoDetectorName + "_geom");
  TGeoVolume* top = new TGeoVolumeAssembly("TOP");
  top->SetMedium(pMedAir);
  gGeoManager->SetTopVolume(top);

// Additional parameters and updates


  EMC_Box_Top_X =   EMC_Box_Bottom_X + 2.*EMC_Box_Length*tan(EMC_Box_Angle1);
  EMC_zPosition = CalculateZPos(); // tower Z - position in sector
  EMC_zWallPosition = CalculateZPosWall(); // wall Z - position in sector
  Double_t rangle = 2*(EMC_NSector_div2*8)*EMC_Box_Angle1;
  Double_t fEMC_Pos = EMC_zWallPosition[EMC_NSector_div2] - 
		0.5*((EMC_Module_side+EMC_Wall_Add)*sin(rangle)-EMC_Sector_wall2/cos(rangle));
  EMC_Length = fEMC_Pos + (EMC_Module_side+EMC_Wall_Add)*sin(rangle);

////  printf("MpdRoot : calculated EMC half length  %f \n", EMC_Length);  
  
// ECal barrel

  TGeoTube *EMC_ChamberShape = new TGeoTube(EMC_Frame_InnerRadius, EMC_Frame_OuterRadius, EMC_Length);
  TGeoVolume *EMC_ChamberVolume = new TGeoVolume(geoDetectorName,EMC_ChamberShape,pMedAir);
  EMC_ChamberVolume->SetVisibility(0);

// ECal right / left side

  TGeoTube *emcChHTube = new TGeoTube(EMC_Frame_InnerRadius, EMC_Frame_OuterRadius, EMC_Length/2.);
  emcChH = new TGeoVolume("emcChH",emcChHTube,pMedAir);
  emcChH->SetLineColor(TColor::GetColor("#008000")); 
  emcChH->SetLineColor(kGreen); 
  
// emcChH->SetVisibility(kFALSE);

  TGeoCombiTrans* rotBarrelPlus = new TGeoCombiTrans();
  rotBarrelPlus->SetTranslation(EMC_Xpos, EMC_Ypos, EMC_Length/2.);
  TGeoCombiTrans* rotBarrelMinus = new TGeoCombiTrans();
  rotBarrelMinus->ReflectZ(true);
  rotBarrelMinus->SetTranslation(EMC_Xpos, EMC_Ypos,-EMC_Length/2.);

  EMC_ChamberVolume->AddNode(emcChH,0,rotBarrelPlus);
  EMC_ChamberVolume->AddNode(emcChH,1,rotBarrelMinus);

// Create ECAL power frame 

  Double_t rotation_angle = 90.;
  while  ( rotation_angle > 0 ) rotation_angle -= EMC_Sector_angle*TMath::RadToDeg();
  rotation_angle += EMC_Sector_angle*TMath::RadToDeg();
  EMC_Sector_angle0 = rotation_angle;
  
  CreateECALFrame();

// ECAL sectors (emcSector)

  CreateECALSectors();
  
//  printf("MpdRoot : ECal sector weight : %.2f kg\n", emcSector->Weight(0.01,"a")); 

// Create ECAL module shape (with glue content)

  CreateECALModShape();

// Create ECAL module

  CreateECALModule();

  top->AddNode(EMC_ChamberVolume,0,new TGeoTranslation( 0.0, 0.0, 0.0));
  top->SetVisContainers(kTRUE);

  printf("MpdRoot : ECal total weight : %.2f kg\n", EMC_ChamberVolume->Weight(0.01,"a")); 

  gGeoManager->CloseGeometry();
  gGeoManager->CheckOverlaps(fPrecision);
  gGeoManager->PrintOverlaps();
  gGeoManager->Test();

  gGeoManager->SetTopVisible(kTRUE);

// Draw different parts of ECal 

// Power Frame

  TCanvas *c1 = new TCanvas("c1","ECal power frame",10,10,585,585);
  c1->cd();
   
  gGeoManager->SetVisLevel(3);
  gGeoManager->SetNsegments(20);
  emcSector->SetVisibility(kFALSE);
  top->Draw();
  TView3D *view1 = (TView3D*)gPad->GetView();
  view1->ShowAxis();
  c1->Update();

// Write to root - file 

  TFile* geomFile = new TFile(geoFileName, "RECREATE");
  top->Write();
  geomFile->Close();

}

// Define media parameters

void DefineMediaParameters(FairGeoMedia* geoMedia, FairGeoBuilder* geoBuild) {

// Air

    FairGeoMedium* mAir = geoMedia->getMedium("air");
    if ( ! mAir ) Fatal("Main", "FairGeoMedium air not found");
    geoBuild->createMedium(mAir);
    pMedAir = gGeoManager->GetMedium("air");
    pMedAir->GetMaterial()->Print();
    if ( ! pMedAir ) Fatal("Main", "TGeoMedium air not found");

// Aluminium

    FairGeoMedium* mAluminium = geoMedia->getMedium("aluminium");
    if ( ! mAluminium  ) Fatal("Main", "FairGeoMedium aluminium not found");
    geoBuild->createMedium(mAluminium);
    pMedAluminium  = gGeoManager->GetMedium("aluminium");
    pMedAluminium->GetMaterial()->Print();
    if ( ! pMedAluminium  ) Fatal("Main", "TGeoMedium aluminium not found");

// Scintillator
   
    FairGeoMedium* mFscScint = geoMedia->getMedium("FscScint");
    if ( ! mFscScint ) Fatal("Main", "FairGeoMedium FscScint not found");
    geoBuild->createMedium(mFscScint);
    pMedFscScint = gGeoManager->GetMedium("FscScint");
    pMedFscScint->GetMaterial()->Print();
    if ( ! pMedFscScint ) Fatal("Main", "TGeoMedium FscScint not found");

// Lead    

    FairGeoMedium* mLead = geoMedia->getMedium("lead");
    if ( ! mLead ) Fatal("Main", "FairGeoMedium Lead not found");
    geoBuild->createMedium(mLead);
    pMedLead = gGeoManager->GetMedium("lead");
    pMedLead->GetMaterial()->Print();
    if ( ! pMedLead ) Fatal("Main", "TGeoMedium Lead not found");

// Plastic 

    FairGeoMedium* mKapton = geoMedia->getMedium("kapton");
    if ( ! mKapton ) Fatal("Main", "FairGeoMedium Kapton not found");
    geoBuild->createMedium(mKapton);
    pMedKapton = gGeoManager->GetMedium("kapton");
    pMedKapton->GetMaterial()->Print();
    if ( ! pMedKapton ) Fatal("Main", "TGeoMedium Kapton not found");  

// Fiberglass material

    FairGeoMedium* mFbGlass = geoMedia->getMedium("fiberglass");
    if ( ! mFbGlass ) Fatal("Main", "FairGeoMedium mFbGlass not found");
    geoBuild->createMedium(mFbGlass);
    pMedFbGlass = gGeoManager->GetMedium("fiberglass");
    pMedFbGlass->GetMaterial()->Print();
    if ( ! pMedFbGlass ) Fatal("Main", "TGeoMedium fiberglass not found");  

// ECal glue with Ti support

    FairGeoMedium* mGlue = geoMedia->getMedium("glueTi");
    if ( ! mGlue ) Fatal("Main", "FairGeoMedium mGlue not found");
    geoBuild->createMedium(mGlue);
    pMedGlue = gGeoManager->GetMedium("glueTi");
    pMedGlue->GetMaterial()->Print();
    if ( ! pMedGlue ) Fatal("Main", "TGeoMedium glueTi not found");  

// Ti02 reflecting color (EJ510)

    FairGeoMedium* mTiPaint = geoMedia->getMedium("TiO2Col");
    if ( ! mTiPaint ) Fatal("Main", "FairGeoMedium mTiPaint not found");
    geoBuild->createMedium(mTiPaint);
    pMedTiPaint = gGeoManager->GetMedium("TiO2Col");
    pMedTiPaint->GetMaterial()->Print();
    if ( ! pMedTiPaint ) Fatal("Main", "TGeoMedium TiO2Col not found");  

// Carbon composite (Graphite epoxy suprt)

    FairGeoMedium* mCarbComp = geoMedia->getMedium("carboncomp");
    if ( ! mCarbComp ) Fatal("Main", "FairGeoMedium mCarbComp not found");
    geoBuild->createMedium(mCarbComp);
    pMedCarbComp = gGeoManager->GetMedium("carboncomp");
    pMedCarbComp->GetMaterial()->Print();
    if ( ! pMedCarbComp ) Fatal("Main", "TGeoMedium carboncomp not found");  

}

// Central tower Z - position

Double_t* CalculateZPos() {

  Double_t gap = EMC_Box_gap, rotation_angle = 0.;
  Double_t* zPos = new Double_t[numBoxSize];
  Double_t* zPositions = new Double_t[numBoxSize];
  zPositions[0] = EMC_Sector_wall1 + EMC_Box_gap;

  for (int ip = 0; ip < EMC_Index_Box; ip++) {
   gap = EMC_Box_gap; rotation_angle = (2*ip + 1)*EMC_Box_Angle1;
   if ( (ip == 1*EMC_Index_Box/EMC_NSector_div2) || (ip == 2*EMC_Index_Box/EMC_NSector_div2) || 
	(ip == 3*EMC_Index_Box/EMC_NSector_div2) || (ip == 4*EMC_Index_Box/EMC_NSector_div2) ||
        (ip == 5*EMC_Index_Box/EMC_NSector_div2) || (ip == 6*EMC_Index_Box/EMC_NSector_div2) || 
	(ip == 7*EMC_Index_Box/EMC_NSector_div2) ) gap = 4.*EMC_Box_gap + EMC_Sector_wall2;
    zPositions[ip+1] = zPositions[ip] + gap/cos(rotation_angle-EMC_Box_Angle1) +
            EMC_Box_Bottom_X*(cos(rotation_angle)+sin(rotation_angle)*tan(rotation_angle-EMC_Box_Angle1));
    zPos[ip] = zPositions[ip+1] - 0.5*(EMC_Box_Bottom_X*cos(rotation_angle) -
                        EMC_Box_Length*sin(rotation_angle));
   }
  return zPos;
}

// Central Z - position of walls

Double_t* CalculateZPosWall() {

   Int_t icount = 0;
   Double_t dAddWall, dAddGap, fWallLength, rotation_angle;
   Double_t* zPos = new Double_t[numBoxSize];
   fWallLength = EMC_Box_Length + (EMC_Tower_InnerRadius - EMC_Sector_InnerRadius);
   zPos[0] = 0.5*EMC_Sector_wall1;
   
   for (int ip = 0; ip < EMC_Index_Box + 1; ip++) {
   rotation_angle = (2*ip-1)*EMC_Box_Angle1;	
   if ( (ip == 1*EMC_Index_Box/EMC_NSector_div2) || (ip == 2*EMC_Index_Box/EMC_NSector_div2) || 
	(ip == 3*EMC_Index_Box/EMC_NSector_div2) || (ip == 4*EMC_Index_Box/EMC_NSector_div2) ||
        (ip == 5*EMC_Index_Box/EMC_NSector_div2) || (ip == 6*EMC_Index_Box/EMC_NSector_div2) || 
	(ip == 7*EMC_Index_Box/EMC_NSector_div2) || (ip == EMC_Index_Box) ) {
	 dAddGap = 2.*EMC_Box_gap/cos(rotation_angle+EMC_Box_Angle1); icount++;
	 dAddWall = fWallLength + EMC_Box_Bottom_X*tan(rotation_angle+EMC_Box_Angle1);
	if (ip == EMC_Index_Box) dAddWall = EMC_Module_side+EMC_Wall_Add;
       zPos[icount] = EMC_zPosition[ip-1] + dAddGap + 0.5*(dAddWall*sin(rotation_angle+EMC_Box_Angle1) +
		      EMC_Sector_wall2/cos(rotation_angle+EMC_Box_Angle1) - EMC_Box_Length*sin(rotation_angle) +
		      EMC_Box_Bottom_X*cos(rotation_angle)) - (fWallLength - EMC_Box_Length)*
		       tan(rotation_angle+EMC_Box_Angle1);	
    }
   }
  return zPos;
}

// Create ECAL power frame 

void CreateECALFrame() {

// Parallel edges 

 Double_t fFrameInner = EMC_Frame_InnerRadius + EMC_Frame_Wall1;
 Double_t fFrameOuter = EMC_Frame_OuterRadius - EMC_Frame_Wall3; 
 Double_t fEdgeAngle = TMath::ASin(0.5*EMC_Frame_Wall2/fFrameOuter); 
 Double_t fEdgeLength = 0.5*EMC_Frame_Wall2/tan(fEdgeAngle) - fFrameInner;
 Double_t fEdgeOrigin[3] = {fFrameInner + 0.5*fEdgeLength, 0., 0.0};
 
 TGeoBBox* frameEdgeShape1 = new TGeoBBox(0.5*fEdgeLength, 0.0, 0.5*EMC_Length, fEdgeOrigin);
 TGeoVolume *frameEdge1 = new TGeoVolume("frameEdge1",frameEdgeShape1,pMedCarbComp);
 frameEdge1->SetLineColor(12);

 Double_t rotation_angle =  EMC_Sector_angle0; 
 for (int iEdge = 0; iEdge < EMC_NSectors; iEdge++) {
  TGeoCombiTrans *edge_trans1 = new TGeoCombiTrans();
  edge_trans1->SetTranslation(0.,0.,0.); edge_trans1->RotateZ(rotation_angle);
  emcChH->AddNode(frameEdge1,iEdge,edge_trans1); rotation_angle += EMC_Sector_angle*TMath::RadToDeg();
 }

// Upper and lower power parts

  TGeoPcon* frameLatticeShape1 = new TGeoPcon(0.,360.,2);
  frameLatticeShape1->DefineSection(0, 0.5*EMC_Length, EMC_Frame_InnerRadius, fFrameInner);
  frameLatticeShape1->DefineSection(1,-0.5*EMC_Length, EMC_Frame_InnerRadius, fFrameInner);
  TGeoVolume *frameLattice1 = new TGeoVolume("frameLattice1",frameLatticeShape1,pMedCarbComp);
  emcChH->AddNode(frameLattice1,0,new TGeoTranslation(0.,0.,0.));

  TGeoPcon* frameLatticeShape2 = new TGeoPcon(0.,360.,2);
  frameLatticeShape2->DefineSection(0, 0.5*EMC_Length, fFrameOuter, EMC_Frame_OuterRadius);
  frameLatticeShape2->DefineSection(1,-0.5*EMC_Length, fFrameOuter, EMC_Frame_OuterRadius);
  TGeoVolume *frameLattice2 = new TGeoVolume("frameLattice2",frameLatticeShape2,pMedCarbComp);
  emcChH->AddNode(frameLattice2,0,new TGeoTranslation(0.,0.,0.));
  frameLattice1->SetLineColor(12); frameLattice2->SetLineColor(12);

}


// Create ECAL sectors and walls 

void CreateECALSectors() {

  Double_t xPos, yPos, zPos, rangle, fEMC_Pos, fEMC_Z0, fEMC_Z1, fEMC_Z2, rotAngle;
  Double_t fHoleSector, fSectorAngle, fSectorRad, fSectorRadius, fGapWallAngle, fCrateScale;
  Double_t fInnerRadius, fOuterRadius, fFrameWidth;

  rangle = 2*(EMC_NSector_div2*8)*EMC_Box_Angle1;
  fEMC_Pos = EMC_zWallPosition[EMC_NSector_div2] -
             0.5*((EMC_Module_side+EMC_Wall_Add)*sin(rangle)-EMC_Sector_wall2/cos(rangle));
  fEMC_Z0 = -EMC_Length/2., fEMC_Z1 = -EMC_Length/2.+fEMC_Pos, fEMC_Z2 =  EMC_Length/2.;
  rotAngle = 0.5*EMC_Sector_angle;

  fFrameWidth = EMC_Sector_InnerRadius - EMC_Sector_InnerFrame;
  fHoleSector  = 0.5*EMC_Frame_Wall2 + EMC_Frame_gap1;
  fGapWallAngle = TMath::ASin(fHoleSector/EMC_Sector_InnerFrame);
  fSectorAngle  = 2.*(0.5*EMC_Sector_angle - fGapWallAngle);
  xPos = fHoleSector/tan(rotAngle); yPos = fHoleSector;
  fSectorRad = EMC_Sector_InnerFrame - xPos;
  fInnerRadius = EMC_Sector_InnerRadius + (EMC_Module_side+EMC_Wall_Add)*cos(rangle);

  TGeoRotation *coneRot = new TGeoRotation("coneRot");
  coneRot->RotateZ(fGapWallAngle*TMath::RadToDeg());
  TGeoTranslation *coneTrans = new TGeoTranslation("coneTrans", xPos, yPos, 0.);
  coneRot->RegisterYourself(); coneTrans->RegisterYourself();

// Create sector (composite shape)

  TGeoPcon *emcSectorAdditional = new TGeoPcon(0.,fSectorAngle*TMath::RadToDeg(),2);
  emcSectorAdditional->SetName("emcSectorAdditional");
  emcSectorAdditional->DefineSection(0, fEMC_Z0, EMC_Sector_InnerFrame, EMC_Sector_OuterRadius);
  emcSectorAdditional->DefineSection(1, fEMC_Z1, EMC_Sector_InnerFrame, EMC_Sector_OuterRadius);

  fInnerRadius = fSectorRad + (EMC_Module_side+EMC_Wall_Add)*cos(rangle);
  fOuterRadius = fSectorRad + EMC_Sector_OuterRadius - EMC_Sector_InnerFrame;
  TGeoPcon *emcSectorPcone = new TGeoPcon(0.,EMC_Sector_angle*TMath::RadToDeg(),3);
  emcSectorPcone->SetName("emcSectorPcone");
  emcSectorPcone->DefineSection(0, fEMC_Z0, fSectorRad, fOuterRadius);
  emcSectorPcone->DefineSection(1, fEMC_Z1, fSectorRad, fOuterRadius);
  emcSectorPcone->DefineSection(2, fEMC_Z2, fInnerRadius, fOuterRadius);

  TGeoCompositeShape *emcSectorShape = new TGeoCompositeShape("emcSectorShape",
        "emcSectorAdditional:coneRot+emcSectorPcone:coneTrans");
  emcSector = new TGeoVolume("emcSector", emcSectorShape, pMedAir);
  emcSector->SetVisibility(kTRUE); emcSector->SetLineColor(4);
  emcSector->SetVisContainers(kTRUE);

  Double_t rotation_angle = EMC_Sector_angle0;

  for (Int_t iSec = 0; iSec < EMC_NSectors; iSec++) {
   TGeoCombiTrans *combi_sector = new TGeoCombiTrans();
   combi_sector->RotateZ(rotation_angle); rotation_angle += 0.5*EMC_Sector_angle*TMath::RadToDeg();
   emcChH->AddNode(emcSector,iSec,combi_sector);
   rotation_angle += 0.5*EMC_Sector_angle*TMath::RadToDeg();
  }

// Lower wall

  Double_t fHoleWallAngle = EMC_Sector_Wall3/EMC_Sector_InnerFrame;
  Double_t fHoleAngle = fSectorAngle/EMC_NSector_div1 - 2.*fHoleWallAngle;

  TGeoPcon* emcSectorWallShape1 = new TGeoPcon(0.,fSectorAngle*TMath::RadToDeg(),2);
  emcSectorWallShape1->DefineSection(0, fEMC_Z0, EMC_Sector_InnerFrame, EMC_Sector_InnerRadius);
  emcSectorWallShape1->DefineSection(1, fEMC_Z1, EMC_Sector_InnerFrame, EMC_Sector_InnerRadius);
  TGeoVolume* emcSectorWall1 = new TGeoVolume("emcSectorWall1", emcSectorWallShape1, pMedFbGlass);
  emcSectorWall1->SetLineColor(8);
  emcSector->AddNode(emcSectorWall1,0,coneRot);

  Int_t iHole = 0;
  Double_t rAngle1 = 0.0, rAngle2 = 0.0;
  TString nameSectorHole;
  TGeoPcon *emcSectorHoleShape;
  Double_t fHoleLength1, fHoleLength2, fHoleEdgPos1 = 0.0, fHoleEdgPos2 = 0.0;
  rotation_angle = 0.0; zPos = - EMC_Length/2.;

  for (Int_t iHole2 = 0; iHole2 < EMC_NSector_div2; iHole2++) {
   rAngle2 = 2.*(iHole2 + 1)*EMC_NSector_div2*EMC_Box_Angle1;
   fHoleLength1 = 0.5*EMC_SectorHoleLen1[iHole2];
   fHoleEdgPos1 = fHoleLength1 - fFrameWidth*tan(rAngle2);
   if (rAngle1 > 0) {
     fHoleEdgPos2 = fHoleLength1 + fFrameWidth*tan(rAngle1);
     fHoleLength2 = (fHoleEdgPos1 + fHoleEdgPos2)/2.;
   }

   zPos += EMC_Sector_Wall3 + fHoleLength1;
   if (iHole2 == 0) {

   emcSectorHoleShape = new TGeoPcon(0., fHoleAngle*TMath::RadToDeg(),3);
     emcSectorHoleShape->DefineSection(0, -fHoleLength1, EMC_Sector_InnerFrame, EMC_Sector_InnerRadius);
     emcSectorHoleShape->DefineSection(1,  fHoleEdgPos1, EMC_Sector_InnerFrame, EMC_Sector_InnerRadius);
     emcSectorHoleShape->DefineSection(2,  fHoleLength1, EMC_Sector_InnerRadius, EMC_Sector_InnerRadius);
   }
   else {
     emcSectorHoleShape = new TGeoPcon(0., fHoleAngle*TMath::RadToDeg(),4);
     emcSectorHoleShape->DefineSection(0,-fHoleEdgPos2, EMC_Sector_InnerFrame, EMC_Sector_InnerFrame);
     emcSectorHoleShape->DefineSection(1,-fHoleLength1, EMC_Sector_InnerFrame, EMC_Sector_InnerRadius);
     emcSectorHoleShape->DefineSection(2, fHoleEdgPos1, EMC_Sector_InnerFrame, EMC_Sector_InnerRadius);
     emcSectorHoleShape->DefineSection(3, fHoleLength1, EMC_Sector_InnerRadius, EMC_Sector_InnerRadius);
   }

   nameSectorHole.Form("emcSectorHole%d",iHole2);
   TGeoVolume* emcSectorHole = new TGeoVolume(nameSectorHole, emcSectorHoleShape, pMedAir);
   emcSectorHole->SetLineColor(0);

   for (Int_t iHole1 = 0; iHole1 < EMC_NSector_div1; iHole1++) {
    rotation_angle += fHoleWallAngle;
    TGeoCombiTrans *combi_hole = new TGeoCombiTrans();
    combi_hole->RotateZ(rotation_angle*TMath::RadToDeg());
    combi_hole->SetTranslation(0., 0., zPos);
    emcSectorWall1->AddNode(emcSectorHole,iHole,combi_hole); iHole++;
    rotation_angle += fHoleAngle+fHoleWallAngle;
   }

   zPos += EMC_Sector_Wall3 + fHoleLength1;
   rAngle1 = rAngle2; rotation_angle = 0.0;
  }

// Parrallel walls in sector

  Double_t fWallInnerRadius, fWallOuterRadius, fEdgeRadius, fEdgeAngle, fEdgeLength;
  xPos = xPos + 0.5*EMC_Sector_wall2/tan(rotAngle);
  yPos = yPos + 0.5*EMC_Sector_wall2;
  fWallInnerRadius = EMC_Sector_InnerRadius - xPos;
  fWallOuterRadius = fWallInnerRadius + (EMC_Sector_OuterRadius - EMC_Sector_InnerRadius);
  fInnerRadius = fWallInnerRadius + (EMC_Module_side+EMC_Wall_Add)*cos(rangle);
  fEdgeRadius = fWallOuterRadius - (fWallInnerRadius + sqrt(xPos*xPos+yPos*yPos) - EMC_Sector_InnerRadius);
  fEdgeLength = fEdgeRadius - fWallInnerRadius;

  TGeoXtru *emcSectorWallShape2 = new TGeoXtru(2);
  Double_t xTru[5] = {fWallInnerRadius, fEdgeRadius, fEdgeRadius, fInnerRadius, fWallInnerRadius};
  Double_t yTru[5] = {fEMC_Z0, fEMC_Z0, fEMC_Z2, fEMC_Z2, fEMC_Z1};
  emcSectorWallShape2->DefinePolygon(5, xTru, yTru);
  emcSectorWallShape2->DefineSection(0,-0.5*EMC_Sector_wall2,0.,0.,1.0);
  emcSectorWallShape2->DefineSection(1, 0.5*EMC_Sector_wall2,0.,0.,1.0);
  TGeoVolume *emcSectorWall2 = new TGeoVolume("emcSectorWall2", emcSectorWallShape2, pMedFbGlass);
  emcSectorWall2->SetLineColor(2);

  rotation_angle = 0.0;
  for (Int_t iWall1 = 0; iWall1 <  EMC_NSector_div1+1; iWall1++) {
   TGeoCombiTrans *combi_wall1 = new TGeoCombiTrans();
   combi_wall1->RotateX(90.); combi_wall1->RotateZ(rotation_angle*TMath::RadToDeg());
   combi_wall1->SetTranslation(xPos, yPos, 0.);
   emcSector->AddNode(emcSectorWall2,iWall1,combi_wall1);
   rotation_angle += EMC_Sector_angle/EMC_NSector_div1;
  }

// Parrallel subsector (crate) (composite shape)

  Double_t fPrecWidth = 12.*fPrecision; // cm
  Double_t fCrateRad, fCrateRadius, fCrateOuterRadius, fCrateAngle, xLoc, yLoc;
  xPos = fHoleSector/tan(rotAngle); yPos = fHoleSector;
  fWallInnerRadius = EMC_Sector_InnerRadius - xPos;
  fGapWallAngle = TMath::ASin((EMC_Sector_wall2 + fPrecWidth)/fWallInnerRadius);
  fCrateAngle = (EMC_Sector_angle - (EMC_NSector_div1+1)*fGapWallAngle)/EMC_NSector_div1;
  fCrateRad = tan(0.5*fCrateAngle)*fWallInnerRadius/tan(0.5*EMC_Module_angle);
  fCrateOuterRadius = fCrateRad + fEdgeLength;
  fInnerRadius = fCrateRad + (EMC_Module_side+EMC_Wall_Add)*cos(rangle);

  TGeoPcon *emcCratePcone = new TGeoPcon(0.,EMC_Module_angle*TMath::RadToDeg(),3);
  emcCratePcone->DefineSection(0, fEMC_Z0, fCrateRad, fCrateOuterRadius);
  emcCratePcone->DefineSection(1, fEMC_Z1, fCrateRad, fCrateOuterRadius);
  emcCratePcone->DefineSection(2, fEMC_Z2, fInnerRadius, fCrateOuterRadius);
  emcCrate = new TGeoVolume("emcCrate", emcCratePcone, pMedAir);
  emcCrate->SetLineColor(12);

  rotation_angle = 0.0;
  for (Int_t iCrate = 0; iCrate < EMC_NSector_div1; iCrate++) {
    TGeoCombiTrans *combi_crate = new TGeoCombiTrans();
    combi_crate->RotateZ(rotation_angle*TMath::RadToDeg());
    xLoc = xPos + fWallInnerRadius*cos(fGapWallAngle + iCrate*(fCrateAngle+fGapWallAngle)) - 
	   fCrateRad*cos(rotation_angle);
    yLoc = yPos + fWallInnerRadius*sin(fGapWallAngle + iCrate*(fCrateAngle+fGapWallAngle)) - 
	   fCrateRad*sin(rotation_angle);
    combi_crate->SetTranslation(xLoc, yLoc, 0.0);
    emcSector->AddNode(emcCrate,iCrate,combi_crate);
    rotation_angle += EMC_Module_angle;
  }

// Transverse walls in sector

  Int_t iWall = 0, nWallSect;
  Double_t fPos[4], dZWidth, fWallLength;
  fCrateRad = ((TGeoPcon*)emcCrate->GetShape())->GetRmin()[0];
  fWallLength = EMC_Box_Length + (EMC_Tower_InnerRadius - EMC_Sector_InnerRadius);
		
  for (Int_t iWall2 = 0; iWall2 < EMC_NSector_div2+1; iWall2++) {
   rotation_angle = 2*(iWall2*8)*EMC_Box_Angle1;
   dZWidth = EMC_Sector_wall2/cos(rotation_angle);
   fSectorRadius = fCrateRad + fWallLength*cos(rotation_angle)+EMC_Box_Bottom_X*sin(rotation_angle);					
   if (iWall2 == EMC_NSector_div2)
	fSectorRadius = fCrateRad + (EMC_Module_side+EMC_Wall_Add)*cos(rotation_angle);
 
   nWallSect= 4; if (iWall2 == 0)  nWallSect = 2;
   TGeoPgon* emcSectorWallShape3 = new TGeoPgon(0.,EMC_Module_angle*TMath::RadToDeg(),2,nWallSect);

   if (iWall2 == 0) {
     dZWidth = EMC_Sector_wall1;
     emcSectorWallShape3->DefineSection(0,-EMC_Sector_wall1/2, fCrateRad, fSectorRadius);
     emcSectorWallShape3->DefineSection(1, EMC_Sector_wall1/2, fCrateRad, fSectorRadius);
   }

   if ( (iWall2 != 0) ) {
     fPos[0] = -0.5*((fSectorRadius-fCrateRad)*tan(rotation_angle) + dZWidth);
     fPos[1] = fPos[0] + dZWidth; fPos[3] = -1.*fPos[0]; fPos[2] = fPos[3] - dZWidth;
     emcSectorWallShape3->DefineSection(0,fPos[0], fCrateRad, fCrateRad);
     emcSectorWallShape3->DefineSection(1,fPos[1], fCrateRad, fCrateRad + dZWidth/tan(rotation_angle));
     emcSectorWallShape3->DefineSection(2,fPos[2],
                          fSectorRadius - dZWidth/tan(rotation_angle), fSectorRadius);
     emcSectorWallShape3->DefineSection(3,fPos[3], fSectorRadius, fSectorRadius);
    }	
   TGeoVolume *emcSectorWall3 = new TGeoVolume("emcSectorWall3", emcSectorWallShape3, pMedFbGlass);
   emcSectorWall3->SetLineColor(2);
   emcCrate->AddNode(emcSectorWall3, iWall++, new 
			  TGeoTranslation(0.,0.,-EMC_Length/2.+EMC_zWallPosition[iWall2]));
  }
  
}

//  Create ECAL shape of the different modules  

void CreateECALModShape() {

// Set specific parameters of module shape

  const Int_t NModMax = EMC_NSector_div2;
  const Int_t EMC_Half_Box = 0.5*EMC_Index_Box;
  Double_t Z12P[NModMax][EMC_Index_Box], R1P[NModMax][EMC_Index_Box], R2P[NModMax][EMC_Index_Box];
  Double_t ZtowerInCrate[NModMax*NModMax], shapeDim[NModMax], ZminiP[NModMax];

  const Int_t sizeDim = EMC_Index_Box + 1;
  Double_t LModuleBase = EMC_Box_Length;
  Double_t LModule = LModuleBase + EMC_Box_gap;
  Double_t angle = 2.0*EMC_Box_Angle1, HalfAngle = 0.5*angle;
  Double_t Lside = LModule/cos(HalfAngle), R = EMC_Sector_InnerRadius;
  Double_t hZF = EMC_Box_Bottom_X, hZB = hZF + 2.0*LModule*tan(HalfAngle);
  Double_t gapMod = EMC_Sector_wall1 + 2.*EMC_Box_gap;

  Double_t gap, AM[sizeDim], Zright[sizeDim], Zleft[sizeDim], Yleft[sizeDim], Zmin[sizeDim], Ymin[sizeDim],
           Zmax[sizeDim], Ymax[sizeDim], Z2gap[sizeDim];
  Z2gap[0] = Zmin[0]; AM[0]= HalfAngle; Zright[0]=0.;

  for(Int_t i = 0; i < EMC_Index_Box; i++) {
  AM[i]=  i*angle + HalfAngle; gap = EMC_Box_gap;
   if( (i == 7) || (i == 15) || (i == 23) || (i == 31) ||
      (i == 39) || (i == 47) || (i == 55) ) gap = 4.*EMC_Box_gap + EMC_Sector_wall2;
   if(i == 0) Zright[i] = gapMod + hZF*cos(AM[i]);

  else Zright[i] = Z2gap[i-1] + hZF*(cos(AM[i]) + sin(AM[i])*tan(AM[i] - HalfAngle));
   ZtowerInCrate[i] = Zright[i] - 0.5*hZF*cos(AM[i]) + 0.5*LModule*sin(AM[i]);
   Zleft[i] =Zright[i] - hZF*cos(AM[i]); Yleft[i] = R+sin(AM[i])*hZF;
   Zmax[i] = Zright[i] + Lside*sin(AM[i] + HalfAngle);
   Ymax[i]= R + Lside*cos(AM[i] + HalfAngle);
   Zmin[i] = Zleft[i] + Lside*sin(AM[i] - HalfAngle);
   Ymin[i] = Yleft[i] + Lside*cos(AM[i] - HalfAngle);
   Z2gap[i] = Zright[i]+gap/cos(AM[i] + HalfAngle);
  }

  Int_t j, k, iMod, Nmin, Nmax;
  Double_t Z0, gapCur, ZPgonRmin[EMC_Index_Box], ZPgonRmax[EMC_Index_Box], YPgonRmin[EMC_Index_Box],
           YPgonRmax[EMC_Index_Box], ZPgon[EMC_Index_Box], R1Pgon[EMC_Index_Box], R2Pgon[EMC_Index_Box];

  for(Int_t jk = 0; jk < NModMax; jk++) {
   iMod =jk; Nmin = iMod*NModMax; Nmax = Nmin+NModMax;
   Z0 = Zleft[Nmin] - 2.*EMC_Box_gap/cos(AM[Nmin]-HalfAngle);
   ZminiP[jk] = Z0; j=0;

  for(Int_t i = Nmin; i < Nmax;i++) {
   gapCur = EMC_Box_gap/cos(AM[i]+HalfAngle);
   if(i == Nmin) {
    ZPgonRmin[j] = 0; YPgonRmin[j] = Yleft[i] - R;
    ZPgonRmax[j] = Zmin[i]-Zleft[i]; YPgonRmax[j] = Ymin[i] - R; j++;
   }

  if(i < Nmax - 1) {
   ZPgonRmin[j] = Zleft[i]-Z0; YPgonRmin[j] = Yleft[i]-R;
   ZPgonRmax[j] = Zmin[i]-Z0; YPgonRmax[j] = Ymin[i]-R; j++;
   ZPgonRmin[j] = Zright[i]-Z0; YPgonRmin[j] = 0;
   ZPgonRmax[j] = Zmax[i]-Z0; YPgonRmax[j] = Ymax[i]-R; j++;
   ZPgonRmin[j] = Zright[i]-Z0; YPgonRmin[j] = 0;
   ZPgonRmax[j] = Zmin[i+1]-Z0 - gapCur; YPgonRmax[j] = Ymin[i+1]-R; j++;
   ZPgonRmin[j] = Zright[i]-Z0 +gapCur; YPgonRmin[j] = 0;
   ZPgonRmax[j] = Zmin[i+1]-Z0; YPgonRmax[j] = Ymin[i+1]-R; j++;
  }

  else {
   ZPgonRmin[j] = Zleft[i]-Z0; YPgonRmin[j] = Yleft[i]-R;
   ZPgonRmax[j] = Zmin[i]-Z0; YPgonRmax[j] = Ymin[i]-R; j++;
   ZPgonRmin[j] = Zright[i]-Z0; YPgonRmin[j] = 0;
   ZPgonRmax[j] = Zmax[i]-Z0; YPgonRmax[j] = Ymax[i]-R; j++;
   ZPgonRmin[j] = Zright[i]-Z0 +2.*gapCur; YPgonRmin[j] = 0;
   ZPgonRmax[j] = Zmax[i]-Z0+2.*gapCur; YPgonRmax[j] = Ymax[i]-R; j++;
   }
  }

  Double_t ZPoly[EMC_Index_Box], R1[EMC_Index_Box], R2[EMC_Index_Box], z1, z2, y1, y2;
  for( Int_t k1 = 0; k1 < NModMax; k1++)
   ZminiP[k1] = Zleft[k1*NModMax] - 2*EMC_Box_gap/cos(AM[k1*NModMax]-HalfAngle);

  for(Int_t i = 0; i < EMC_Half_Box; i++) {
   ZPoly[i] = ZPgonRmin[i]; R1[i] = YPgonRmin[i]; k = 0;
   while(ZPgonRmax[k] < ZPoly[i]) k++;
   if(i == 0) R2[i] = R1[i];
   else {
    if(k == 0) {z1 = ZPgonRmin[0]; z2 = ZPgonRmax[0]; y1 = YPgonRmin[0]; y2 = YPgonRmax[0];}
    else{z1 = ZPgonRmax[k-1]; z2 = ZPgonRmax[k]; y1 = YPgonRmax[k-1]; y2 = YPgonRmax[k];}
    R2[i] = y1 + (y2-y1)/(z2-z1)*(ZPoly[i]-z1);
   }
  }

  for(Int_t i=0; i < EMC_Half_Box; i++) {
   ZPoly[EMC_Index_Box-i-1] = ZPgonRmax[EMC_Half_Box-1-i];
   R2[EMC_Index_Box-i-1] = YPgonRmax[EMC_Half_Box-1-i]; k=0;
   while (ZPgonRmin[EMC_Half_Box-1-k] > ZPoly[EMC_Index_Box-1-i]) k++;
   if(i == 0) R1[EMC_Index_Box-1] = R2[EMC_Index_Box-1];
   else {
    if(k == 0) {z1 = ZPgonRmin[EMC_Half_Box-1]; z2 = ZPgonRmax[EMC_Half_Box-1];
                y1 = YPgonRmin[EMC_Half_Box-1]; y2 = YPgonRmax[EMC_Half_Box-1];}
    else {z1 = ZPgonRmin[EMC_Half_Box-k-1]; z2 = ZPgonRmin[EMC_Half_Box-k];
         y1= YPgonRmin[EMC_Half_Box-k-1]; y2 = YPgonRmin[EMC_Half_Box-k];}
         R1[EMC_Index_Box-i-1] = y1 +(y2-y1)/(z2-z1)*(ZPoly[EMC_Index_Box-i-1]-z1);}
   }

  for(Int_t i = 0; i < EMC_Index_Box - 1; i++){
   z1 = 100;
  for(Int_t i2=i+1; i2 < EMC_Index_Box-2; i2++){
   z2 = ZPoly[i2]-ZPoly[i];
   if(z2 <z1) {z1 = z2; k = i2; }}
  if(k-i >1) {y1 = ZPoly[i+1]; ZPoly[i+1] = ZPoly[k]; ZPoly[k] = y1;
   y1 = R1[i+1]; R1[i+1] = R1[k]; R1[k] = y1;
   y1 = R2[i+1]; R2[i+1] = R2[k]; R2[k] = y1;}
  }

   Int_t Ndel = 0;
   Z12P[iMod][0] = ZPoly[0]; R1P[iMod][0] = R1[0]; R2P[iMod][0] = R2[0];
   for(Int_t i = 1; i < EMC_Index_Box; i++){
   if((ZPoly[i] - ZPoly[i-1]) < 0.0001) Ndel++;
   Z12P[iMod][i-Ndel] = ZPoly[i]; R1P[iMod][i-Ndel] = R1[i]; R2P[iMod][i-Ndel] = R2[i];}
   shapeDim[jk] = EMC_Index_Box - Ndel;

  }

// Create ECAL module shape

  TString nameECALModule; 
  Double_t fSizeMin, fSizeMax, fEMC_Z0 = -EMC_Length/2.;
  Double_t crate_radius = ((TGeoPcon*)emcCrate->GetShape())->GetRmin()[0];
  crate_radius = crate_radius + (EMC_Tower_InnerRadius - EMC_Sector_InnerRadius);

  for(Int_t iShape =0; iShape < EMC_NSector_div2; iShape++) {
  TGeoPgon *emcModuleShape = new TGeoPgon(0.,EMC_Module_angle*TMath::RadToDeg(),2,shapeDim[iShape]);

   for (Int_t iSize = 0; iSize < shapeDim[iShape]; iSize++) {
    fSizeMin = crate_radius + R1P[iShape][iSize];
    fSizeMax = crate_radius + R2P[iShape][iSize];
    emcModuleShape->DefineSection(iSize,Z12P[iShape][iSize],fSizeMin,fSizeMax);
   }

   TGeoCombiTrans *combi_shape = new TGeoCombiTrans();
   combi_shape->SetTranslation(0.,0., fEMC_Z0 + ZminiP[iShape]);
   nameECALModule.Form("emcModule%d",iShape);
   TGeoVolume* emcModule = new TGeoVolume(nameECALModule, emcModuleShape, pMedGlue);
   listECALModules->Add(emcModule); emcModule->SetLineColor(12);
   emcCrate->AddNode(emcModule, 0, combi_shape);

 }
}


// Create ECAL module

void CreateECALModule() {

  Int_t arbType, icount = 0, icont = 0, iStep = 0, iMod = 0;
  Double_t fEMC_Z0 = -EMC_Length/2., zTrans[EMC_NSector_div2];
  Double_t xPos, yPos, xTower, yTower, rPos, fModRadius, rotation_angle, tower_angle, arbLen1, arbLen2;
  Double_t SideSize = (EMC_Box_Top_Y - EMC_Box_Bottom_Y)/(2.*tan(EMC_Box_Angle2));

  Double_t crate_radius = ((TGeoPcon*)emcCrate->GetShape())->GetRmin()[0];
  fModRadius = crate_radius + (EMC_Tower_InnerRadius - EMC_Sector_InnerRadius);
  TGeoVolume* shapeModule;

// Z coordinate of module shape

  TGeoNode* crateNode;
  TObjArray* nodeCrate = emcCrate->GetNodes();
  TIterator* crateIter = nodeCrate->MakeIterator();
   while( (crateNode = (TGeoNode*)crateIter->Next()) ) {
   TString nameECALModule(crateNode->GetName());
    if (nameECALModule.Contains("emcModule")) {
          zTrans[icont] = crateNode->GetMatrix()->GetTranslation()[2] - fEMC_Z0; icont++;
    }
   }

  for (Int_t index = 0; index < EMC_Index_Box; index++) {
   rotation_angle = (2*index+1)*EMC_Box_Angle1; arbType = 0;
   arbLen1 = (SideSize - EMC_Box_Bottom_X*sin(rotation_angle))*cos(EMC_Box_Angle1)/
	     cos(rotation_angle-EMC_Box_Angle1);
   arbLen2 = SideSize*cos(EMC_Box_Angle1)/cos(rotation_angle+EMC_Box_Angle1);
   EMC_Box_A[index] = EMC_Box_Top_Y;
   EMC_Box_C[index] = EMC_Box_Bottom_Y + 2.*EMC_Box_Bottom_X*sin(rotation_angle)*tan(EMC_Box_Angle2);
   if (arbLen1 > EMC_Box_Length) {arbLen1 = EMC_Box_Length; arbType = 1;
     EMC_Box_A[index] = EMC_Box_C[index] +
             2.*EMC_Box_Length*cos(rotation_angle-EMC_Box_Angle1)*tan(EMC_Box_Angle2);}
   if (arbLen1 < EMC_Box_Length) arbType = 2;
   if ( (arbLen1 < EMC_Box_Length) && (arbLen2 < EMC_Box_Length) ) arbType = 3;
   EMC_Box_B[index] = EMC_Box_Bottom_Y + 2.*arbLen1*cos(rotation_angle+EMC_Box_Angle1)*tan(EMC_Box_Angle2);

   if (arbType == 1) CreateECALTower(index);
   if (arbType == 2) CreateECALCompTower(index, arbLen1);
   if (arbType == 3) CreateECALCompTower(index, arbLen1, arbLen2);

   if (iStep == EMC_NSector_div2) iStep = 0;
   if (iStep == 0) {
    shapeModule = (TGeoVolume*)listECALModules->At(iMod); iMod++;
   }

   Double_t sizeB = EMC_Box_B[index];
   if (arbType == 2) sizeB = EMC_Box_B[index] + 2.*(EMC_Box_Length - arbLen1)*
                        cos(rotation_angle+EMC_Box_Angle1)*tan(EMC_Box_Angle2);
   if (arbType == 3) sizeB = EMC_Box_Top_Y;

   if (fDebug) printf("MpdRoot : Tower type, EMC_Box_A, B, C %d %d %.03f %.03f %.03f \n", index+1, arbType, 
                                 EMC_Box_A[index], sizeB, EMC_Box_C[index]);

   TGeoVolume* volModule = (TGeoVolume*)listECALTowers->At(index);
   volModule->SetLineColor(kGreen);
   tower_angle = 2.*EMC_Box_Angle2;
   rPos = 0.5*(EMC_Box_Length*cos(rotation_angle)+EMC_Box_Bottom_X*sin(rotation_angle));
   for (Int_t iXY = 0; iXY < 2; iXY++) {

    xTower = rPos*cos(iXY*tower_angle + EMC_Box_Angle2);
    yTower = rPos*sin(iXY*tower_angle + EMC_Box_Angle2);
    xPos = fModRadius*cos(tower_angle) - (2*iXY-1)*0.5*(EMC_Box_Bottom_Y+EMC_Box_gap)*sin(tower_angle);
    yPos = fModRadius*sin(tower_angle) + (2*iXY-1)*0.5*(EMC_Box_Bottom_Y+EMC_Box_gap)*cos(tower_angle);
    TGeoCombiTrans* posMatrix = new TGeoCombiTrans();
    posMatrix->RotateY(90. - rotation_angle*TMath::RadToDeg());
    posMatrix->RotateZ((iXY*tower_angle + EMC_Box_Angle2)*TMath::RadToDeg());

    posMatrix->SetTranslation(xPos + xTower, yPos + yTower, EMC_zPosition[index] - zTrans[iMod-1]);
    shapeModule->AddNode(volModule, icount, posMatrix); icount++;
   } iStep++;

  }

}
	
// Tower consisting from one trapezoid

void CreateECALTower(Int_t index) {

   TString nameECALTower; 
   TList *arbShapes = new TList();
   TGeoArb8 *arb = new TGeoArb8(EMC_Box_Length/2.);
   nameECALTower.Form("emc_box%d",index+1);
   arb->SetVertex(0,-EMC_Box_Bottom_X/2.,-EMC_Box_Bottom_Y/2.);
   arb->SetVertex(1,-EMC_Box_Bottom_X/2., EMC_Box_Bottom_Y/2.);
   arb->SetVertex(2, EMC_Box_Bottom_X/2., EMC_Box_C[index]/2.);
   arb->SetVertex(3, EMC_Box_Bottom_X/2.,-EMC_Box_C[index]/2.);
   arb->SetVertex(4,-EMC_Box_Top_X/2.,-EMC_Box_B[index]/2.);
   arb->SetVertex(5,-EMC_Box_Top_X/2., EMC_Box_B[index]/2.);
   arb->SetVertex(6, EMC_Box_Top_X/2., EMC_Box_A[index]/2.);
   arb->SetVertex(7, EMC_Box_Top_X/2.,-EMC_Box_A[index]/2.);

   arbShapes->AddAt(arb,0);
   TGeoVolume *emc_tower = new TGeoVolume(nameECALTower, arb, pMedTiPaint);
   CreateECALStructure(emc_tower, arbShapes);
   listECALTowers->Add(emc_tower);

}

// Tower consisting from two trapezoids

void CreateECALCompTower(Int_t index, Double_t Length1) {

   Double_t rotation_angle = (2*index+1)*EMC_Box_Angle1;
   TString nameECALTower, rot1Name, arb1Name, rot2Name, arb2Name, transName;
   nameECALTower.Form("emc_box%d",index+1);
   Double_t fXYSize = 0.5*(EMC_Box_Bottom_X + 2.*Length1*tan(EMC_Box_Angle1));
   Double_t fBSize = EMC_Box_B[index]/2. + (EMC_Box_Length - Length1)*
			cos(rotation_angle+EMC_Box_Angle1)*tan(EMC_Box_Angle2);

   TList *arbShapes = new TList();
   TGeoArb8 *arb1 = new TGeoArb8(Length1/2.);
   arb1Name.Form("arbLower%d",index+1); arb1->SetName(arb1Name);
   arb1->SetVertex(0,-EMC_Box_Bottom_X/2.,-EMC_Box_Bottom_Y/2.);
   arb1->SetVertex(1,-EMC_Box_Bottom_X/2., EMC_Box_Bottom_Y/2.);
   arb1->SetVertex(2, EMC_Box_Bottom_X/2., EMC_Box_C[index]/2.);
   arb1->SetVertex(3, EMC_Box_Bottom_X/2.,-EMC_Box_C[index]/2.);
   arb1->SetVertex(4,-fXYSize, -EMC_Box_B[index]/2.);
   arb1->SetVertex(5,-fXYSize, EMC_Box_B[index]/2.);
   arb1->SetVertex(6, fXYSize, EMC_Box_Top_Y/2.);
   arb1->SetVertex(7, fXYSize,-EMC_Box_Top_Y/2.);

   TGeoArb8 *arb2 = new TGeoArb8((EMC_Box_Length - Length1)/2.);
   arb2Name.Form("arbUpper%d",index+1); arb2->SetName(arb2Name);
   arb2->SetVertex(0,-fXYSize, -EMC_Box_B[index]/2.);
   arb2->SetVertex(1,-fXYSize, EMC_Box_B[index]/2.);
   arb2->SetVertex(2, fXYSize, EMC_Box_Top_Y/2.);
   arb2->SetVertex(3, fXYSize,-EMC_Box_Top_Y/2.);
   arb2->SetVertex(4,-EMC_Box_Top_X/2.,-fBSize);
   arb2->SetVertex(5,-EMC_Box_Top_X/2., fBSize);
   arb2->SetVertex(6, EMC_Box_Top_X/2., EMC_Box_Top_Y/2.);
   arb2->SetVertex(7, EMC_Box_Top_X/2.,-EMC_Box_Top_Y/2.);

   rot1Name.Form("trLower%d",index+1); rot2Name.Form("trUpper%d",index+1);
   TGeoTranslation *trArb1 = new TGeoTranslation(rot1Name,0.,0.,-(EMC_Box_Length - Length1)/2.);
   trArb1->RegisterYourself();
   TGeoTranslation *trArb2 = new TGeoTranslation(rot2Name,0.,0., Length1/2.);
   trArb2->RegisterYourself();
   transName.Form("%s:%s+%s:%s",arb1Name.Data(),rot1Name.Data(),arb2Name.Data(),rot2Name.Data());
   TGeoCompositeShape *arb = new TGeoCompositeShape("arb",transName);

   arbShapes->AddAt(arb1, 0); arbShapes->AddAt(arb2, 1);
   TGeoVolume *emc_tower = new TGeoVolume(nameECALTower, arb, pMedTiPaint);
   CreateECALStructure(emc_tower, arbShapes); 
   listECALTowers->Add(emc_tower);

}

// Tower consisting from three trapezoids

void CreateECALCompTower(Int_t index, Double_t Length1, Double_t Length2) {

   TString nameECALTower, rot1Name, arb1Name, rot2Name, arb2Name, rot3Name, arb3Name, transName;
   Double_t rotation_angle = (2*index+1)*EMC_Box_Angle1;
   nameECALTower.Form("emc_box%d",index+1);
   Double_t fXYSize1 = 0.5*(EMC_Box_Bottom_X + 2.*Length1*tan(EMC_Box_Angle1));
   Double_t fXYSize2 = 0.5*(EMC_Box_Bottom_X + 2.*Length2*tan(EMC_Box_Angle1));

   TList *arbShapes = new TList();
   TGeoArb8 *arb1 = new TGeoArb8(Length1/2.);
   arb1Name.Form("arbLower%d",index+1); arb1->SetName(arb1Name);
   arb1->SetVertex(0,-EMC_Box_Bottom_X/2.,-EMC_Box_Bottom_Y/2.);
   arb1->SetVertex(1,-EMC_Box_Bottom_X/2., EMC_Box_Bottom_Y/2.);
   arb1->SetVertex(2, EMC_Box_Bottom_X/2., EMC_Box_C[index]/2.);
   arb1->SetVertex(3, EMC_Box_Bottom_X/2.,-EMC_Box_C[index]/2.);
   arb1->SetVertex(4,-fXYSize1,-EMC_Box_B[index]/2);
   arb1->SetVertex(5,-fXYSize1, EMC_Box_B[index]/2.);
   arb1->SetVertex(6, fXYSize1, EMC_Box_Top_Y/2.);
   arb1->SetVertex(7, fXYSize1,-EMC_Box_Top_Y/2.);

   TGeoArb8 *arb2 = new TGeoArb8((Length2 - Length1)/2.);
   arb2Name.Form("arbMiddle%d",index+1); arb2->SetName(arb2Name);
   arb2->SetVertex(0,-fXYSize1,-EMC_Box_B[index]/2);
   arb2->SetVertex(1,-fXYSize1, EMC_Box_B[index]/2.);
   arb2->SetVertex(2, fXYSize1, EMC_Box_Top_Y/2.);
   arb2->SetVertex(3, fXYSize1,-EMC_Box_Top_Y/2.);
   arb2->SetVertex(4,-fXYSize2,-EMC_Box_Top_Y/2.);
   arb2->SetVertex(5,-fXYSize2, EMC_Box_Top_Y/2.);
   arb2->SetVertex(6, fXYSize2, EMC_Box_Top_Y/2.);
   arb2->SetVertex(7, fXYSize2,-EMC_Box_Top_Y/2.);

   TGeoArb8 *arb3 = new TGeoArb8((EMC_Box_Length - Length2)/2.);
   arb3Name.Form("arbUpper%d",index+1); arb3->SetName(arb3Name);
   arb3->SetVertex(0,-fXYSize2,-EMC_Box_Top_Y/2.);
   arb3->SetVertex(1,-fXYSize2, EMC_Box_Top_Y/2.);
   arb3->SetVertex(2, fXYSize2, EMC_Box_Top_Y/2.);
   arb3->SetVertex(3, fXYSize2,-EMC_Box_Top_Y/2.);
   arb3->SetVertex(4,-EMC_Box_Top_X/2.,-EMC_Box_Top_Y/2.);
   arb3->SetVertex(5,-EMC_Box_Top_X/2., EMC_Box_Top_Y/2.);
   arb3->SetVertex(6, EMC_Box_Top_X/2., EMC_Box_Top_Y/2.);
   arb3->SetVertex(7, EMC_Box_Top_X/2.,-EMC_Box_Top_Y/2.);

   rot1Name.Form("trLower%d",index+1); rot2Name.Form("trMiddle%d",index+1); rot3Name.Form("trUpper%d",index+1);
   TGeoTranslation *trArb1 = new TGeoTranslation(rot1Name,0.,0.,-(EMC_Box_Length - Length1)/2.);
   trArb1->RegisterYourself();
   TGeoTranslation *trArb2 = new TGeoTranslation(rot2Name,0.,0.,-(EMC_Box_Length - Length2  - Length1)/2.);
   trArb2->RegisterYourself();
   TGeoTranslation *trArb3 = new TGeoTranslation(rot3Name,0.,0., Length2/2.);
   trArb3->RegisterYourself();
   transName.Form("%s:%s+%s:%s+%s:%s",arb1Name.Data(),rot1Name.Data(),arb2Name.Data(),rot2Name.Data(),
		arb3Name.Data(),rot3Name.Data());

   TGeoCompositeShape *arbComp = new TGeoCompositeShape("arb",transName);
   arbShapes->AddAt(arb1, 0); arbShapes->AddAt(arb2, 1); arbShapes->AddAt(arb3, 2);
   TGeoVolume *emc_tower = new TGeoVolume(nameECALTower, arbComp, pMedTiPaint);
   CreateECALStructure(emc_tower, arbShapes); 
   listECALTowers->Add(emc_tower);

}


// Create ECAL module structure

void CreateECALStructure(TGeoVolume* topECALModule, TList* arbShapes) {

   TGeoArb8* topECALArb1;
   TString nameBoxSc, nameBoxPb;
   Double_t zPos0 = -EMC_Box_Length/2., xLow, xHigh;
   Double_t zPosSc, zPosPb, zPosPl, zPosAdd, arbLen1, arbLen2, arbLen3;
   Double_t points_low[8], points_high[8];
   Int_t iCounter = 0, inext1 = 0, inext2 = 0; 
   EMC_Box_Z = 0; 

   if (arbShapes->GetSize() > 0) {
     topECALArb1 = (TGeoArb8*)arbShapes->At(0);
     arbLen1 = 2.*topECALArb1->GetDz(); 
   }

// Add first fixing plate

    TGeoArb8 *PlPlate1 = new TGeoArb8(EMC_Box_Plastic1/2.);
    zPosPl = zPos0 + EMC_Box_Plastic1/2.;

    topECALArb1->SetPlaneVertices(-arbLen1/2., points_low);
    topECALArb1->SetPlaneVertices(-arbLen1/2. + EMC_Box_Plastic1, points_high); 
    for (int ivert = 0; ivert < 4; ivert++)
           PlPlate1->SetVertex(ivert, points_low[2*ivert], points_low[2*ivert+1]);
    for (int ivert = 0; ivert < 4; ivert++)
           PlPlate1->SetVertex(ivert+4, points_high[2*ivert], points_high[2*ivert+1]);   
    TGeoVolume* emc_cl_pl1 = new TGeoVolume("emc_cl_pl1",PlPlate1,pMedKapton);
    emc_cl_pl1->SetLineColor(1); emc_cl_pl1->SetTransparency(0);
    topECALModule->AddNode(emc_cl_pl1, 0, new TGeoTranslation(0., 0., zPosPl));

    iCounter++; EMC_Box_Z += EMC_Box_Plastic1; 
    zPos0 = zPos0 + EMC_Box_Plastic1;

   for (int ik = 0; ik <  EMC_Box_Layers; ik++) {

//  Scintillator box

      zPosSc = zPos0 + (EMC_Box_Sc)/2. + ik*EMC_Box_Cell;
      TGeoArb8 *ScPlate = new TGeoArb8(EMC_Box_Sc/2.);
	  xLow = zPosSc - EMC_Box_Sc/2.; xHigh = zPosSc + EMC_Box_Sc/2.;
      CreateECALTowerEdges(arbShapes, xLow, xHigh, points_low, points_high); 
      for (int ivert = 0; ivert < 4; ivert++)
           ScPlate->SetVertex(ivert, points_low[2*ivert], points_low[2*ivert+1]);
      for (int ivert = 0; ivert < 4; ivert++)
           ScPlate->SetVertex(ivert+4, points_high[2*ivert], points_high[2*ivert+1]);
      nameBoxSc.Form("emc_cl_sc%d",ik+1);

      TGeoVolume* emc_cl_sc = new TGeoVolume(nameBoxSc,ScPlate,pMedFscScint);
      emc_cl_sc->SetLineColor(kGreen);
      topECALModule->AddNode(emc_cl_sc, iCounter, new TGeoTranslation(0., 0., zPosSc));
      iCounter++; EMC_Box_Z += EMC_Box_Sc;

// Pb box 
	  
      zPosPb = zPosSc + (EMC_Box_Sc + EMC_Box_Pb)/2. + EMC_Box_Col;
      TGeoArb8 *PbPlate = new TGeoArb8(EMC_Box_Pb/2.);
	  xLow = zPosPb - EMC_Box_Pb/2.; xHigh = zPosPb + EMC_Box_Pb/2.;
      CreateECALTowerEdges(arbShapes, xLow, xHigh, points_low, points_high);
      for (int ivert = 0; ivert < 4; ivert++)
           PbPlate->SetVertex(ivert, points_low[2*ivert], points_low[2*ivert+1]);
      for (int ivert = 0; ivert < 4; ivert++)
           PbPlate->SetVertex(ivert+4, points_high[2*ivert], points_high[2*ivert+1]);
      nameBoxPb.Form("emc_cl_pb%d",ik+1);
      TGeoVolume* emc_cl_pb = new TGeoVolume(nameBoxPb,PbPlate,pMedLead);
      emc_cl_pb->SetLineColor(kGray);
      topECALModule->AddNode(emc_cl_pb, iCounter, new TGeoTranslation(0., 0., zPosPb));
      iCounter++; EMC_Box_Z += EMC_Box_Pb;

   }

//  Add last scintillator box   
   
    zPosSc = zPos0 + (EMC_Box_Sc)/2. + EMC_Box_Layers*EMC_Box_Cell;
    TGeoArb8 *ScPlate = new TGeoArb8(EMC_Box_Sc/2.);
	xLow = zPosSc - EMC_Box_Sc/2.; xHigh = zPosSc + EMC_Box_Sc/2.;
    CreateECALTowerEdges(arbShapes, xLow, xHigh, points_low, points_high); 
    for (int ivert = 0; ivert < 4; ivert++)
        ScPlate->SetVertex(ivert, points_low[2*ivert], points_low[2*ivert+1]);
    for (int ivert = 0; ivert < 4; ivert++)
        ScPlate->SetVertex(ivert+4, points_high[2*ivert], points_high[2*ivert+1]);
    nameBoxSc.Form("emc_cl_sc%d",EMC_Box_Layers+1);

    TGeoVolume* emc_cl_sc = new TGeoVolume(nameBoxSc,ScPlate,pMedFscScint);
    emc_cl_sc->SetLineColor(kGreen);
    topECALModule->AddNode(emc_cl_sc, iCounter, new TGeoTranslation(0., 0., zPosSc));
    iCounter++; EMC_Box_Z += EMC_Box_Sc;
   
// Add second plastic

    TGeoArb8 *PlPlate2 = new TGeoArb8(EMC_Box_Plastic2/2.);    
    zPosPl = zPosSc + (EMC_Box_Sc + EMC_Box_Plastic2)/2.;
    xLow = zPosPl - EMC_Box_Plastic2/2.; xHigh = zPosPl + EMC_Box_Plastic2/2.;
    CreateECALTowerEdges(arbShapes, xLow, xHigh, points_low, points_high);   
    for (int ivert = 0; ivert < 4; ivert++)
           PlPlate2->SetVertex(ivert, points_low[2*ivert], points_low[2*ivert+1]);
     for (int ivert = 0; ivert < 4; ivert++)
           PlPlate2->SetVertex(ivert+4, points_high[2*ivert], points_high[2*ivert+1]);

     TGeoVolume* emc_cl_pl2 = new TGeoVolume("emc_cl_pl2",PlPlate2,pMedKapton);
     emc_cl_pl2->SetLineColor(1); emc_cl_pl2->SetTransparency(0);
     topECALModule->AddNode(emc_cl_pl2, iCounter, new TGeoTranslation(0., 0., zPosPl));
     iCounter++; EMC_Box_Z += EMC_Box_Plastic2;

// Addition air space in tower

/*
    Double_t addLength = EMC_Box_Length - EMC_Box_Z;
    TGeoArb8 *PlAdd = new TGeoArb8(addLength/2.);    
    zPosAdd = zPosPl + (EMC_Box_Plastic + addLength)/2.;
	xLow = zPosAdd - addLength/2.; xHigh = zPosAdd + addLength/2.;
    CreateECALTowerEdges(arbShapes, xLow, xHigh, points_low, points_high);
    for (int ivert = 0; ivert < 4; ivert++)
           PlAdd->SetVertex(ivert, points_low[2*ivert], points_low[2*ivert+1]);
     for (int ivert = 0; ivert < 4; ivert++)
           PlAdd->SetVertex(ivert+4, points_high[2*ivert], points_high[2*ivert+1]);

     TGeoVolume* emc_cl_air = new TGeoVolume("emc_cl_air",PlAdd,pMedAir);
     emc_cl_air->SetLineColor(1); emc_cl_air->SetTransparency(0);
     topECALModule->AddNode(emc_cl_air, iCounter, new TGeoTranslation(0., 0., zPosAdd));
     iCounter++; EMC_Box_Z += addLength;
*/

}

// Create low and high positions of the trapezoid intersection

void CreateECALTowerEdges(TList* arbShapes, Double_t xLow, Double_t xHigh,
                          Double_t* point_low, Double_t* point_high) {

   Double_t zPosArb0, zPosArb1, zPosArb2, dSize;
   Double_t arbLen, arbLen1, arbLen2, arbLen3;
   TGeoArb8 *topECALArb1, *topECALArb2, *topECALArb3;

   dSize = xHigh - xLow; arbLen = xLow + EMC_Box_Length/2.;
   if (arbLen > 0) arbLen = 0.5*EMC_Box_Length + xLow;

// One arb trapezoid

   if (arbShapes->GetSize() == 1) {
     topECALArb1 = (TGeoArb8*)arbShapes->At(0);
     topECALArb1->SetPlaneVertices(xLow, point_low);
     topECALArb1->SetPlaneVertices(xHigh, point_high);
   }

// Two arbs trapezoid

   if (arbShapes->GetSize() == 2) {
     topECALArb1 = (TGeoArb8*)arbShapes->At(0);
     topECALArb2 = (TGeoArb8*)arbShapes->At(1);
     arbLen1 = 2.*topECALArb1->GetDz();
     arbLen2 = 2.*topECALArb2->GetDz();

     if (arbLen < arbLen1 - dSize) {
       topECALArb1->SetPlaneVertices(xLow + (EMC_Box_Length - arbLen1)/2., point_low);
       topECALArb1->SetPlaneVertices(xHigh + (EMC_Box_Length - arbLen1)/2., point_high);
     }
     else if ( (arbLen > arbLen1 - dSize) && (arbLen < arbLen1) ) {
       topECALArb1->SetPlaneVertices(xLow + (EMC_Box_Length - arbLen1)/2., point_low);
       topECALArb2->SetPlaneVertices(xHigh + EMC_Box_Length/2. - arbLen1 - arbLen2/2., point_high);
     }
     else {
       topECALArb2->SetPlaneVertices(xLow + EMC_Box_Length/2. - arbLen1 - arbLen2/2., point_low);
       topECALArb2->SetPlaneVertices(xHigh + EMC_Box_Length/2. - arbLen1 - arbLen2/2., point_high);
     }

   }

// Three arbs trapezoid

   if (arbShapes->GetSize() == 3) {
     topECALArb1 = (TGeoArb8*)arbShapes->At(0);
     topECALArb2 = (TGeoArb8*)arbShapes->At(1);
     topECALArb3 = (TGeoArb8*)arbShapes->At(2);
     arbLen1 = 2.*topECALArb1->GetDz(); zPosArb0 = -arbLen1/2.;
     arbLen2 = 2.*topECALArb2->GetDz(); zPosArb1 = -arbLen2/2.;
     arbLen3 = 2.*topECALArb3->GetDz(); zPosArb2 = -arbLen3/2.;

     if (arbLen < arbLen1 - dSize) {
       topECALArb1->SetPlaneVertices(xLow + (EMC_Box_Length - arbLen1)/2., point_low);
       topECALArb1->SetPlaneVertices(xHigh + (EMC_Box_Length - arbLen1)/2., point_high);
     }
     else if ( (arbLen > arbLen1 - dSize) && (arbLen < arbLen1) ) {
       topECALArb1->SetPlaneVertices(xLow + (EMC_Box_Length - arbLen1)/2., point_low);
     if ( (arbLen + dSize) <  arbLen1 + arbLen2)
       topECALArb2->SetPlaneVertices(xHigh + EMC_Box_Length/2. - arbLen1 - arbLen2/2., point_high);
     else
	topECALArb3->SetPlaneVertices(xHigh + EMC_Box_Length/2. - arbLen1 - arbLen2 - arbLen3/2., point_high);
     }
     else if ( (arbLen > arbLen1) && (arbLen < arbLen1 + arbLen2 - dSize) ) {
       topECALArb2->SetPlaneVertices(xLow + EMC_Box_Length/2. - arbLen1 - arbLen2/2., point_low);
       topECALArb2->SetPlaneVertices(xHigh + EMC_Box_Length/2. - arbLen1 - arbLen2/2., point_high);
     }
     else if ( (arbLen > arbLen1 + arbLen2 - dSize) && (arbLen < arbLen1 + arbLen2) ) {
       topECALArb2->SetPlaneVertices(xLow + EMC_Box_Length/2. - arbLen1 - arbLen2/2., point_low);
       topECALArb3->SetPlaneVertices(xHigh + EMC_Box_Length/2. - arbLen1 - arbLen2 - arbLen3/2., point_high);
    }
     else {
       topECALArb3->SetPlaneVertices(xLow + EMC_Box_Length/2. - arbLen1 - arbLen2 - arbLen3/2., point_low);
       topECALArb3->SetPlaneVertices(xHigh + EMC_Box_Length/2. - arbLen1 - arbLen2-arbLen3/2., point_high);
     }   
   }

}
