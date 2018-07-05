// ********************************************************************
// *                                                                  *
// *  Author :   M. Martemianov, V. Kulikov (ITEP)		      *
// *  Project:   mpd.jinr.ru			                      *
// *  Created:   1-Feb-2018				              *
// *								      *
// ********************************************************************

#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"
#include "TFile.h"

// #include "mpdloadlibs.C"

// Media

TGeoMedium *pMedAir = 0;
TGeoMedium *pMedAluminium = 0;
TGeoMedium *pMedFscScint = 0;
TGeoMedium *pMedLead = 0;
TGeoMedium *pMedKapton = 0;

// emc1Chamber1

Double_t EMC_InnerRadius = 172.0;
Double_t EMC_OuterRadius = 221.0; // EMC_InnerRadius + EMC_Box_Length
Double_t EMC_Length = 314.0;

// Detector coordinates and position

const Double_t EMC_Xpos = 0.0;
const Double_t EMC_Ypos = 0.0;
const Double_t EMC_Zpos = 0.0;

// Crate parameters 

Int_t emc1NSectors = 8;
Double_t emc1Sector_gap = 0.63, emc1Module_gap = 0.02;
Int_t emc1NSectors1 = 48, emc1NSectors2 = 24;

// EMC module parameters

TList* listEMCModules = new TList();

Double_t EMC_Box_Sc = 0.15; // Scintillator size
Double_t EMC_Box_Pb = 0.03; // Pb size
Double_t EMC_Box_Cell = EMC_Box_Sc + EMC_Box_Pb; // Total size of cell
Double_t EMC_Box_Plastic = 0.8; // Plastic size

Double_t EMC_Box_Top_X = 3.99; 
Double_t EMC_Box_Bottom_Y = 3.182, EMC_Box_Bottom_X = 3.238;
Double_t EMC_Box_Z = 39.78;
Double_t EMC_Box_Length = 43.095;

Double_t EMC_Box_gap1 = 0.52; 
Double_t EMC_Box_gap2 = 0.02; // Equal to emc1Module_gap

Int_t EMC_Index_Box;
const Int_t numBoxSize = 100;
Double_t* EMC_Box_A = new Double_t[numBoxSize];
Double_t* EMC_Box_B = new Double_t[numBoxSize];
Double_t* EMC_Box_C = new Double_t[numBoxSize];

Double_t EMC_Box_angle;

class FairGeoMedia;
class FairGeoBuilder;

void DefineMediaParameters(FairGeoMedia* , FairGeoBuilder*);
void xmlReadDataFile(const char*);
void ParseNode(TXMLEngine*, XMLNodePointer_t);
void CreateECALModule();
void CreateECALStructure(TGeoVolume*, TGeoArb8*);
TGeoCombiTrans* CalculateModulePosition(Int_t);
TGeoCombiTrans* CalculateModuleTranslation(Int_t );

// Settings to check geometry quality

Bool_t fDebug = kFALSE;
Double_t currentPrecision = 0.0000001;

//
// Root function to create EMC/MPD geometry for root - file
//

void create_rootgeom_emc_v2() {

// Load MPD libraries

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  mpdloadlibs();

// Set working directory  
  TString gPath = gSystem->Getenv("VMCWORKDIR");
  const TString geoDetectorName = "emc1Chamber1";
  const TString geoDetectorVersion = "v2";

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

// Fill EMC_Box_A, EMC_Box_B, EMC_Box_C (ECAL module parameters)

  xmlReadDataFile("MpdECALData.xml");

// Create geometry and global top volume

  gGeoManager = (TGeoManager*)gROOT->FindObject("FAIRGeom");
  gGeoManager->SetName(geoDetectorName + "_geom");
  TGeoVolume* top = new TGeoVolumeAssembly("TOP");
  top->SetMedium(pMedAir);
  gGeoManager->SetTopVolume(top);

// emc1Chamber1

//  TGeoVolume *EMC_ChamberVolume = new TGeoVolumeAssembly("emc1Chamber1");

  TGeoTube *EMC_ChamberShape = new TGeoTube("EMC_ChamberShape",
	EMC_InnerRadius,EMC_OuterRadius,EMC_Length);
  TGeoVolume *EMC_ChamberVolume = new TGeoVolume("emc1Chamber1",EMC_ChamberShape);
	
  EMC_ChamberVolume->SetMedium(pMedAir);
  EMC_ChamberVolume->SetLineColor(2);  
 
// emc1Ch1H

  TGeoTube *emc1ChHTube = new TGeoTube("emc1ChH",EMC_InnerRadius,EMC_OuterRadius,EMC_Length/2.);
  TGeoVolume *emc1ChH = new TGeoVolume("emc1ChH",emc1ChHTube,pMedAir);
  emc1ChH->SetLineColor(TColor::GetColor("#008000"));
  emc1ChH->SetTransparency(0); emc1ChH->SetVisLeaves(kFALSE);

  TGeoCombiTrans* rotBarrelPlus = new TGeoCombiTrans();
  rotBarrelPlus->SetTranslation(EMC_Xpos, EMC_Ypos, EMC_Length/2.);

  TGeoCombiTrans* rotBarrelMinus = new TGeoCombiTrans();
  rotBarrelMinus->ReflectZ(true);
  rotBarrelMinus->SetTranslation(EMC_Xpos, EMC_Ypos, -EMC_Length/2.);

  EMC_ChamberVolume->AddNode(emc1ChH,0,rotBarrelPlus);
  EMC_ChamberVolume->AddNode(emc1ChH,1,rotBarrelMinus);
 
// emc1Sector1 (48 x 64 modules) and emc1Sector2 (24 x 64 modules);

  Double_t emc1Sector_gap_angle = 2.*TMath::ASin(0.5*emc1Sector_gap/EMC_InnerRadius);  
  Double_t emc1Module_gap_angle = 2.*TMath::ASin(0.5*emc1Module_gap/EMC_InnerRadius);
  Double_t emc1Sector_angle = (TMath::TwoPi() - emc1NSectors*(emc1Sector_gap_angle))/
			       (emc1NSectors-1)*TMath::RadToDeg()/2.;

  TGeoTubeSeg *emc1Sector1Tube = new TGeoTubeSeg("emc1Sector1Tube",EMC_InnerRadius,EMC_OuterRadius,
                        EMC_Length/2.,0.0,2.*emc1Sector_angle);   
  TGeoTubeSeg *emc1Sector2Tube = new TGeoTubeSeg("emc1Sector2Tube",EMC_InnerRadius,EMC_OuterRadius,
                        EMC_Length/2.,0.,emc1Sector_angle);

  TGeoVolume *emc1Sector1 = new TGeoVolume("emc1Sector1", emc1Sector1Tube, pMedAir);
  TGeoVolume *emc1Sector2 = new TGeoVolume("emc1Sector2", emc1Sector2Tube, pMedAir);

  emc1Sector1->SetVisibility(kTRUE); emc1Sector1->SetLineColor(kBlue);
  emc1Sector2->SetVisibility(kTRUE); emc1Sector2->SetLineColor(kGreen);
  emc1Sector1->SetVisContainers(kTRUE); emc1Sector2->SetVisContainers(kTRUE);

  Double_t sectorAngle; 
  Double_t rotation_next = 0., rotation_angle = -emc1Sector_angle;  
  Int_t icount1 = 0, icount2 = 0;

  for (Int_t istep = 0; istep < emc1NSectors; istep++) {
    rotation_angle = rotation_angle + rotation_next;
    rotation_next = 2.*emc1Sector_angle + emc1Sector_gap_angle*TMath::RadToDeg();
    TGeoCombiTrans *combi_trans = new TGeoCombiTrans();
    combi_trans->SetTranslation(0.,0.,0.);

   sectorAngle = emc1Sector_angle;
   if (istep == 2) combi_trans->RotateZ(90.0 - 0.5*emc1Sector_angle);
   if (istep == 6) combi_trans->RotateZ(270.0 - 0.5*emc1Sector_angle);
   if ( (istep == 2) || (istep == 6) ) {
      rotation_angle = rotation_angle - emc1Sector_angle;
      emc1ChH->AddNode(emc1Sector2,icount2++,combi_trans);
    }
   else {
      combi_trans->RotateZ(rotation_angle);
      emc1ChH->AddNode(emc1Sector1,icount1++,combi_trans);	
    } 

   sectorAngle = combi_trans->GetRotation()->GetPhiRotation() + emc1Sector_angle;
   if ( (istep == 2) || (istep == 6) ) sectorAngle = sectorAngle - 0.5*emc1Sector_angle;
   if (sectorAngle < 0 ) sectorAngle += 360; 
 
   if (fDebug) 
      printf("Test : sector rotation angle %d %f \n", istep, sectorAngle); 

  }

// Create crates (emc1Sector1 and emc1Sector2 division)
  
  Double_t emc1Crate_angle = (2.*emc1Sector_angle - (emc1NSectors1)*(emc1Module_gap_angle)*TMath::RadToDeg())/emc1NSectors1;
  Double_t checkBox_Size = 2.*sin(emc1Crate_angle*TMath::DegToRad()/2.)*EMC_InnerRadius;

  if (fDebug) printf("Test : constructed and calculated sizes for module lower XY size  and diffrerence : %f %f %f \n", 
	    EMC_Box_Bottom_Y, checkBox_Size, fabs(EMC_Box_Bottom_Y-checkBox_Size));

  emc1Crate_angle = 2.*emc1Sector_angle/emc1NSectors1;
  EMC_Box_angle = emc1Crate_angle; 

  TGeoTubeSeg *emc1CrateSeg = new TGeoTubeSeg(EMC_InnerRadius, EMC_OuterRadius, EMC_Length/2, 0., emc1Crate_angle);			 
  TGeoVolume *emc1Crate = new TGeoVolume("emc1Crate",emc1CrateSeg, pMedAir);

  if (fDebug) emc1Crate->Print();
  emc1Crate->SetLineColor(2);

  rotation_angle = 0;
  for (int ik1 = 0; ik1 < emc1NSectors1; ik1++) {
    TGeoCombiTrans *combi_tr = new TGeoCombiTrans();
    combi_tr->SetTranslation(0.,0.,0.);
     combi_tr->RotateZ(rotation_angle);
     emc1Sector1->AddNode(emc1Crate, ik1, combi_tr);
     rotation_angle += emc1Crate_angle;
  }

  rotation_angle = 0.;
  for (int ik2 = 0; ik2 < emc1NSectors2; ik2++) {
    TGeoCombiTrans *combi_tr = new TGeoCombiTrans();
    combi_tr->SetTranslation(0.,0.,0.);
     combi_tr->RotateZ(rotation_angle);
     emc1Sector2->AddNode(emc1Crate, ik2, combi_tr);
     rotation_angle += emc1Crate_angle;
  }

// Create ECAL shapes (class TGeoVolume) and stored in TList

   CreateECALModule(); 

// Print structure of module number index
   Int_t index = 0;

   TGeoVolume* volModule = (TGeoVolume*)listEMCModules->At(index);
   volModule->SetLineColor(1); volModule->SetLineWidth(1);
   if (fDebug) volModule->Print();
   for (int ik = 0; ik < volModule->GetNdaughters(); ik++) {
      TGeoNode* child = volModule->GetNode(ik);
      TGeoVolume* subVolume  = child->GetVolume();
      Double_t* position = (Double_t*)child->GetMatrix()->GetTranslation();
     if ( fDebug && (ik < 10) ) {
      subVolume->Print();
      printf("MpdRoot : ik / name : %d / %s \n", ik, subVolume->GetName());
      printf("MpdRoot : Position : %f %f %f \n", position[0], position[1], position[2]);
     }
   }


// Add emc1_box module position and rotation

  for (int ip = 0; ip < EMC_Index_Box; ip++ ) {
    TGeoVolume* volModule = (TGeoVolume*)listEMCModules->At(ip);
    TGeoCombiTrans* posMatrix = CalculateModuleTranslation(ip+1); 
     emc1Crate->AddNode(volModule, ip, posMatrix);
   
   }


  top->AddNode(EMC_ChamberVolume,0,new TGeoTranslation( 0.0, 0.0, 0.0));
  top->SetVisContainers(kTRUE);

  gGeoManager->CloseGeometry();
  gGeoManager->CheckOverlaps(currentPrecision);
  gGeoManager->PrintOverlaps();
  gGeoManager->Test();
  gGeoManager->SetTopVisible(kTRUE);


  TCanvas *c1 = new TCanvas("c1","Total view / ecal",10,10,585,585);
  c1->cd();

  gGeoManager->SetVisLevel(3);
  gGeoManager->SetNsegments(10);
  c1->SetTheta(130.); c1->SetPhi(0.);
   top->Draw();
  c1->GetView()->ZoomView(0, 1.3);
  c1->Update();


// Write to root - file 

  TFile* geomFile = new TFile("$VMCWORKDIR/geometry/emc_v2.root", "RECREATE");
    top->Write();
  geomFile->Close();

}


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

}

// Read XML file 

void xmlReadDataFile(const char* filename) {

   TXMLEngine* xml = new TXMLEngine;
   XMLDocPointer_t xmldoc = xml->ParseFile(filename);
   if (xmldoc==0) {
      delete xml;
      return;
   }

   EMC_Index_Box = 0;
   ParseNode(xml, xml->DocGetRootElement(xmldoc));
   xml->FreeDoc(xmldoc);
   delete xml;

}

void ParseNode(TXMLEngine* xml, XMLNodePointer_t node)  {

   char* nodeName = (char*)xml->GetNodeName(node);
   char* content = (char*)xml->GetNodeContent(node);
   if (content != 0) {
        if (strcmp(nodeName, "Column1") == 0) EMC_Index_Box = atoi(content);
        if (strcmp(nodeName, "Column2") == 0) EMC_Box_A[EMC_Index_Box-1] = 0.1*atof(content);
        if (strcmp(nodeName, "Column3") == 0) EMC_Box_B[EMC_Index_Box-1] = 0.1*atof(content);
        if (strcmp(nodeName, "Column4") == 0) EMC_Box_C[EMC_Index_Box-1] = 0.1*atof(content);
    }

   XMLNodePointer_t child = xml->GetChild(node);
   while (child != 0 ) {
      ParseNode(xml, child);
      child = xml->GetNext(child);
   }

}

// Create ECAL module

void CreateECALModule() {

  char nameECLModule[100];
  for (int ik = 0; ik < EMC_Index_Box; ik++) {
    TGeoArb8 *arb = new TGeoArb8(EMC_Box_Length/2.);
    sprintf(nameECLModule,"emc1_box%d",ik+1);
    arb->SetVertex(0,-EMC_Box_Bottom_X/2.,-EMC_Box_Bottom_Y/2.);
    arb->SetVertex(1,-EMC_Box_Bottom_X/2., EMC_Box_Bottom_Y/2.);
    arb->SetVertex(2, EMC_Box_Bottom_X/2., EMC_Box_C[ik]/2.);
    arb->SetVertex(3, EMC_Box_Bottom_X/2.,-EMC_Box_C[ik]/2.);
    arb->SetVertex(4,-EMC_Box_Top_X/2.,-EMC_Box_B[ik]/2);
    arb->SetVertex(5,-EMC_Box_Top_X/2., EMC_Box_B[ik]/2.);
    arb->SetVertex(6, EMC_Box_Top_X/2., EMC_Box_A[ik]/2.);
    arb->SetVertex(7, EMC_Box_Top_X/2.,-EMC_Box_A[ik]/2.);

    TGeoVolume *emc1_box = new TGeoVolume(nameECLModule, arb, pMedAir);
    CreateECALStructure(emc1_box, arb);
    listEMCModules->Add(emc1_box);

  }

}

// Create ECAL module structure

void CreateECALStructure(TGeoVolume* topECALModule, TGeoArb8* topECALArb) {

   char nameBoxPb[100], nameBoxSc[100];
   Int_t nECALDiv = EMC_Box_Z/EMC_Box_Cell;

   Double_t zPos0 = -EMC_Box_Length/2.;

   Double_t zPosPb, zPosSc, zPosPl;
   Double_t points_low[8], points_high[8];

   Int_t iCounter = 0;
   
// Add first plastic

    TGeoArb8 *PlPlate1 = new TGeoArb8(EMC_Box_Plastic/2.);
    zPosPl = zPos0 + EMC_Box_Plastic/2.;
    topECALArb->SetPlaneVertices(zPosPl - EMC_Box_Plastic/2, points_low);
    topECALArb->SetPlaneVertices(zPosPl + EMC_Box_Plastic/2., points_high);
     for (int ivert = 0; ivert < 4; ivert++)
           PlPlate1->SetVertex(ivert, points_low[2*ivert], points_low[2*ivert+1]);
     for (int ivert = 0; ivert < 4; ivert++)
           PlPlate1->SetVertex(ivert+4, points_high[2*ivert], points_high[2*ivert+1]);   

     TGeoVolume* emc1_cl_pl1 = new TGeoVolume("emc1_cl_pl1",PlPlate1,pMedKapton);
     emc1_cl_pl1->SetLineColor(1); emc1_cl_pl1->SetTransparency(0);
     topECALModule->AddNode(emc1_cl_pl1, 0, new TGeoTranslation(0., 0., zPosPl));

   iCounter++;
   zPos0 = zPos0 + EMC_Box_Plastic;

   for (int ik = 0; ik < nECALDiv; ik++) {

//  Pb box

      zPosPb = zPos0 + (EMC_Box_Pb)/2. + ik*EMC_Box_Cell;
      TGeoArb8 *PbPlate = new TGeoArb8(EMC_Box_Pb/2.);
      TGeoArb8 *ScPlate = new TGeoArb8(EMC_Box_Sc/2.);

      topECALArb->SetPlaneVertices(zPosPb - EMC_Box_Pb/2., points_low);
      topECALArb->SetPlaneVertices(zPosPb + EMC_Box_Pb/2., points_high);
      for (int ivert = 0; ivert < 4; ivert++)
           PbPlate->SetVertex(ivert, points_low[2*ivert], points_low[2*ivert+1]);
      for (int ivert = 0; ivert < 4; ivert++)
           PbPlate->SetVertex(ivert+4, points_high[2*ivert], points_high[2*ivert+1]);
      sprintf(nameBoxPb,"emc1_cl_pb%d",ik+1);
      TGeoVolume* emc1_cl_pb = new TGeoVolume(nameBoxPb,PbPlate,pMedLead);
      emc1_cl_pb->SetLineColor(4);
      topECALModule->AddNode(emc1_cl_pb, iCounter, new TGeoTranslation(0., 0., zPosPb));
      iCounter++;        

// Sc box 

      zPosSc = zPosPb + (EMC_Box_Pb + EMC_Box_Sc)/2.;
      topECALArb->SetPlaneVertices(zPosSc - EMC_Box_Sc/2., points_low);
      topECALArb->SetPlaneVertices(zPosSc + EMC_Box_Sc/2., points_high);
      for (int ivert = 0; ivert < 4; ivert++)
           ScPlate->SetVertex(ivert, points_low[2*ivert], points_low[2*ivert+1]);
      for (int ivert = 0; ivert < 4; ivert++)
           ScPlate->SetVertex(ivert+4, points_high[2*ivert], points_high[2*ivert+1]);
      sprintf(nameBoxSc,"emc1_cl_sc%d",ik+1);
      TGeoVolume* emc1_cl_sc = new TGeoVolume(nameBoxSc,ScPlate,pMedFscScint);
      emc1_cl_sc->SetLineColor(3);
      topECALModule->AddNode(emc1_cl_sc, iCounter, new TGeoTranslation(0., 0., zPosSc));
      iCounter++;

   }

// Add second plastic

    TGeoArb8 *PlPlate2 = new TGeoArb8(EMC_Box_Plastic/2.);    

    zPosPl = zPos0 + EMC_Box_Z+ EMC_Box_Plastic/2.;
    topECALArb->SetPlaneVertices(zPosPl - EMC_Box_Plastic/2, points_low);
    topECALArb->SetPlaneVertices(zPosPl + EMC_Box_Plastic/2., points_high); 
    for (int ivert = 0; ivert < 4; ivert++)
           PlPlate2->SetVertex(ivert, points_low[2*ivert], points_low[2*ivert+1]);
     for (int ivert = 0; ivert < 4; ivert++)
           PlPlate2->SetVertex(ivert+4, points_high[2*ivert], points_high[2*ivert+1]);

     TGeoVolume* emc1_cl_pl2 = new TGeoVolume("emc1_cl_pl2",PlPlate2,pMedKapton);
     emc1_cl_pl2->SetLineColor(1); emc1_cl_pl2->SetTransparency(0);
     topECALModule->AddNode(emc1_cl_pl2, iCounter, new TGeoTranslation(0., 0., zPosPl));

}


// Create module position and rotation in emc1Crate sector

TGeoCombiTrans* CalculateModuleTranslation(Int_t index) {

  Double_t xPos = 0., yPos = 0., zPos = 0., rotation_angle1 = 0., rotation_angle2 = 0.;
  Double_t* zPositions = new Double_t[numBoxSize];
  Double_t gap = EMC_Box_gap2;
  TGeoCombiTrans* posMatrix = new TGeoCombiTrans();

  Int_t icount = 0;
  Double_t hAngle = TMath::ATan(0.5*(EMC_Box_Top_X - EMC_Box_Bottom_X)/EMC_Box_Length);

   zPositions[0] = 0.0;
   for (int ip = 0; ip < EMC_Index_Box; ip++) {
    gap = EMC_Box_gap2;

    icount = ip + 1; rotation_angle1 = (2*icount - 1)*hAngle;
    if (icount == 1) gap = 0.5*EMC_Box_gap2;
    if ( (icount == 9) || (icount == 25) || (icount == 41) || (icount == 57) ) gap = EMC_Box_gap1;
    zPositions[icount] = zPositions[icount-1] +
                (gap + EMC_Box_Bottom_X)*(cos(rotation_angle1)+sin(rotation_angle1)*tan(rotation_angle1-hAngle));
  }

  rotation_angle1 = (2*index - 1)*hAngle;
//  rotation_angle2 = TMath::ATan(0.5*EMC_Box_Bottom_Y/EMC_InnerRadius);
  rotation_angle2 = 0.5*EMC_Box_angle*TMath::DegToRad();

  xPos = EMC_InnerRadius + 0.5*(EMC_Box_Length*cos(rotation_angle1)+EMC_Box_Bottom_X*sin(rotation_angle1));
  zPos = zPositions[index] - 0.5*(EMC_Box_Bottom_X*cos(rotation_angle1)-EMC_Box_Length*sin(rotation_angle1));
  posMatrix->RotateY(90.);
  posMatrix->RotateY(-1.*rotation_angle1*TMath::RadToDeg());
  posMatrix->RotateZ(rotation_angle2*TMath::RadToDeg());
  posMatrix->SetTranslation(xPos*cos(rotation_angle2),xPos*sin(rotation_angle2),-0.5*EMC_Length+zPos);

  return posMatrix;

}
