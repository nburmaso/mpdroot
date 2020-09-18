// Geometry for pipe: aluminium + beryllium
// Demezhan Myktybekov
// August 2020
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
//Pipe's parameters
const Double_t beryllium_pipe_length = 169.4;
const Double_t beryllium_pipe_inner_radius = 3.1;
const Double_t beryllium_pipe_outer_radius = 3.225;

const Double_t aluminium_pipe_small_length = 3.95;
const Double_t aluminium_pipe_small_inner_radius = 3.1;
const Double_t aluminium_pipe_small_outer_radius = 3.225;

const Double_t aluminium_adapter_small_length = 7.15;
const Double_t aluminium_adapter_small_inner_radius_top = 3.1;
const Double_t aluminium_adapter_small_outer_radius_top = 3.225;
const Double_t aluminium_adapter_small_inner_radius_bottom = 4.0;
const Double_t aluminium_adapter_small_outer_radius_bottom = 4.15;

const Double_t aluminium_pipe_middle_inside_length = 36.7;//35.9; gap with FFD
const Double_t aluminium_pipe_middle_inside_inner_radius = 4.0;
const Double_t aluminium_pipe_middle_inside_outer_radius = 4.15;

const Double_t aluminium_inner_flange_small_length = 1.5;//2.3; gap with FFD
const Double_t aluminium_inner_flange_small_inner_radius = 4.;
const Double_t aluminium_inner_flange_small_outer_radius = 7.5;

const Double_t aluminium_outer_flange_small_length = 2.3;
const Double_t aluminium_outer_flange_small_inner_radius = 4.;
const Double_t aluminium_outer_flange_small_outer_radius = 7.5;

const Double_t aluminium_pipe_middle_outside_length = 55.0 * 2;
const Double_t aluminium_pipe_middle_outside_inner_radius = 4.0;
const Double_t aluminium_pipe_middle_outside_outer_radius = 4.15;

const Double_t aluminium_adapter_big_length = 35.9 * 2;
const Double_t aluminium_adapter_big_inner_radius_top = 4.0;
const Double_t aluminium_adapter_big_outer_radius_top = 4.15;
const Double_t aluminium_adapter_big_inner_radius_bottom = 6.35;
const Double_t aluminium_adapter_big_outer_radius_bottom = 6.5;

const Double_t aluminium_pipe_big_outside_length = 55.0 * 2;
const Double_t aluminium_pipe_big_outside_inner_radius = 6.35;
const Double_t aluminium_pipe_big_outside_outer_radius = 6.5;

const Double_t aluminium_outer_flange_big_length = 2.3;
const Double_t aluminium_outer_flange_big_inner_radius = 6.35;
const Double_t aluminium_outer_flange_big_outer_radius = 10.;


//media
TGeoMedium *pMedAir = 0;
TGeoMedium *pMedAluminium = 0;
TGeoMedium *pMedBeryllium = 0;
TGeoMedium *pMedVacuum = 0;
TGeoMedium *pMedSteel = 0;

class FairGeoMedia;
class FairGeoBuilder;

void DefineRequiredMedia(FairGeoMedia* geoMedia, FairGeoBuilder* geoBuild);
void pipe_v3()
{   
   // Load necessary libraries
   //gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
   //mpdloadlibs(); // load libraries

   // ----  set working directory  --------------------------------------------
   TString gPath = gSystem->Getenv("VMCWORKDIR");

   // -------   Module file name (output)   ---------------------------------
   const TString geoModuleName = "pipe";
   const TString geoModuleVersion = "v3";
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
   // Define mother volume
   //TGeoTranslation* mother_vol_pos = new TGeoTranslation("mother_vol_pos", PIPE_Xpos, PIPE_Ypos, PIPE_Zpos);
   TGeoVolume *mother_vol = gGeoManager->MakePcon("MOTHER_PCON_VOLUME", pMedVacuum, 0.0, 360.0, 22);
   TGeoPcon *mother_pcon = (TGeoPcon*)(mother_vol->GetShape());
   mother_pcon->DefineSection(0, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length + aluminium_pipe_big_outside_length + aluminium_outer_flange_big_length),
                              aluminium_outer_flange_big_inner_radius, aluminium_outer_flange_big_outer_radius);

   mother_pcon->DefineSection(1, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length + aluminium_pipe_big_outside_length),
                              aluminium_outer_flange_big_inner_radius, aluminium_outer_flange_big_outer_radius);

   mother_pcon->DefineSection(2, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length + aluminium_pipe_big_outside_length),
                              aluminium_pipe_big_outside_inner_radius, aluminium_pipe_big_outside_outer_radius);

   mother_pcon->DefineSection(3, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length),
                              aluminium_adapter_big_inner_radius_bottom, aluminium_adapter_big_outer_radius_bottom);

   mother_pcon->DefineSection(4, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length),
                              aluminium_adapter_big_inner_radius_top, aluminium_adapter_big_outer_radius_top);

   mother_pcon->DefineSection(5, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length),
                              aluminium_pipe_middle_outside_inner_radius, aluminium_pipe_middle_outside_outer_radius);

   mother_pcon->DefineSection(6, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length),
                              aluminium_outer_flange_small_inner_radius, aluminium_outer_flange_small_outer_radius);

   mother_pcon->DefineSection(7, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length),
                              aluminium_inner_flange_small_inner_radius, aluminium_inner_flange_small_outer_radius);

   mother_pcon->DefineSection(8, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length),
                              aluminium_pipe_middle_inside_inner_radius, aluminium_pipe_middle_inside_outer_radius);

   mother_pcon->DefineSection(9, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length),
                              aluminium_adapter_small_inner_radius_bottom, aluminium_adapter_small_outer_radius_bottom);
   mother_pcon->DefineSection(10, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length),
                              aluminium_pipe_small_inner_radius, aluminium_pipe_small_outer_radius);
   mother_pcon->DefineSection(11, beryllium_pipe_length / 2 + aluminium_pipe_small_length,
                              aluminium_pipe_small_inner_radius, aluminium_pipe_small_outer_radius);
   mother_pcon->DefineSection(12, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length,
                              aluminium_adapter_small_inner_radius_bottom, aluminium_adapter_small_outer_radius_bottom);
   mother_pcon->DefineSection(13, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length,
                              aluminium_pipe_middle_inside_inner_radius, aluminium_pipe_middle_inside_outer_radius);

   mother_pcon->DefineSection(14, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length,
                              aluminium_inner_flange_small_inner_radius, aluminium_inner_flange_small_outer_radius);

   mother_pcon->DefineSection(15, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length  + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length,
                              aluminium_outer_flange_small_inner_radius, aluminium_outer_flange_small_outer_radius);

   mother_pcon->DefineSection(16, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length  + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length,
                              aluminium_pipe_middle_outside_inner_radius, aluminium_pipe_middle_outside_outer_radius);

   mother_pcon->DefineSection(17, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length  + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length,
                              aluminium_adapter_big_inner_radius_top, aluminium_adapter_big_outer_radius_top);

   mother_pcon->DefineSection(18, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length  + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length,
                              aluminium_adapter_big_inner_radius_bottom, aluminium_adapter_big_outer_radius_bottom);

   mother_pcon->DefineSection(19, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length  + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length + aluminium_pipe_big_outside_length,
                              aluminium_pipe_big_outside_inner_radius, aluminium_pipe_big_outside_outer_radius);

   mother_pcon->DefineSection(20, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length  + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length + aluminium_pipe_big_outside_length,
                              aluminium_outer_flange_big_inner_radius, aluminium_outer_flange_big_outer_radius);

   mother_pcon->DefineSection(21, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length  + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length + aluminium_pipe_big_outside_length + aluminium_outer_flange_big_length,
                              aluminium_outer_flange_big_inner_radius, aluminium_outer_flange_big_outer_radius);
   mother_vol->SetLineColor(kRed);
   mother_vol->SetTransparency(95);
   top->AddNode(mother_vol,1);
   
   // Define PIPE Geometry
   TGeoTube* pipe_beryllium_inner = new TGeoTube("pipe_beryllium_inner", 3.1, 3.225, 169.4/2);
   TGeoVolume* pipe_beryllium = new TGeoVolume("pipe_beryllium", pipe_beryllium_inner);
   pipe_beryllium->SetLineColor(kRed);
   pipe_beryllium->SetMedium(pMedBeryllium);
   pipe_beryllium->SetTransparency(0);

   TGeoTranslation* pipe_beryllium_pos = new TGeoTranslation("pipe_beryllium_pos", PIPE_Xpos, PIPE_Ypos, PIPE_Zpos);
   mother_vol->AddNode(pipe_beryllium, 0, pipe_beryllium_pos);

   TGeoVolume *vol1 = gGeoManager->MakePcon("PCON1", pMedAluminium, 0.0, 360.0, 6);
   TGeoPcon *pcon1 = (TGeoPcon*)(vol1->GetShape());
   pcon1->DefineSection(0,beryllium_pipe_length / 2, aluminium_pipe_small_inner_radius, aluminium_pipe_small_outer_radius);
   pcon1->DefineSection(1,beryllium_pipe_length / 2 + aluminium_pipe_small_length, aluminium_adapter_small_inner_radius_top, aluminium_adapter_small_outer_radius_top);
   pcon1->DefineSection(2,beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length, aluminium_pipe_middle_inside_inner_radius, aluminium_pipe_middle_inside_outer_radius);
   pcon1->DefineSection(3,beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length, aluminium_pipe_middle_inside_inner_radius, aluminium_pipe_middle_inside_outer_radius);
   pcon1->DefineSection(4,beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length, aluminium_inner_flange_small_inner_radius, aluminium_inner_flange_small_outer_radius);
   pcon1->DefineSection(5,beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length, aluminium_inner_flange_small_inner_radius, aluminium_inner_flange_small_outer_radius);


   TGeoVolume *vol2 = gGeoManager->MakePcon("PCON2", pMedAluminium, 0.0, 360.0, 6);
   TGeoPcon *pcon2 = (TGeoPcon*)(vol2->GetShape());
   pcon2->DefineSection(0, -(beryllium_pipe_length / 2), aluminium_pipe_small_inner_radius, aluminium_pipe_small_outer_radius);
   pcon2->DefineSection(1, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length), aluminium_adapter_small_inner_radius_top, aluminium_adapter_small_outer_radius_top);
   pcon2->DefineSection(2, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length), aluminium_pipe_middle_inside_inner_radius, aluminium_pipe_middle_inside_outer_radius);
   pcon2->DefineSection(3, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length), aluminium_pipe_middle_inside_inner_radius, aluminium_pipe_middle_inside_outer_radius);
   pcon2->DefineSection(4, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length), aluminium_inner_flange_small_inner_radius, aluminium_inner_flange_small_outer_radius);
   pcon2->DefineSection(5, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length), aluminium_inner_flange_small_inner_radius, aluminium_inner_flange_small_outer_radius);

   TGeoVolume *vol3 = gGeoManager->MakePcon("PCON3", pMedAluminium, 0.0, 360.0, 8);
   TGeoPcon *pcon3 = (TGeoPcon*)(vol3->GetShape());
   pcon3->DefineSection(0, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length, aluminium_outer_flange_small_inner_radius, aluminium_outer_flange_small_outer_radius);
   pcon3->DefineSection(1, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length, aluminium_outer_flange_small_inner_radius, aluminium_outer_flange_small_outer_radius);
   pcon3->DefineSection(2, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length, aluminium_pipe_middle_outside_inner_radius, aluminium_pipe_middle_outside_outer_radius);
   pcon3->DefineSection(3, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length, aluminium_pipe_middle_outside_inner_radius, aluminium_pipe_middle_outside_outer_radius);
   pcon3->DefineSection(4, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length, aluminium_adapter_big_inner_radius_bottom, aluminium_adapter_big_outer_radius_bottom);
   pcon3->DefineSection(5, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length + aluminium_pipe_big_outside_length, aluminium_pipe_big_outside_inner_radius, aluminium_pipe_big_outside_outer_radius);
   pcon3->DefineSection(6, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length + aluminium_pipe_big_outside_length, aluminium_outer_flange_big_inner_radius, aluminium_outer_flange_big_outer_radius);
   pcon3->DefineSection(7, beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length + aluminium_pipe_big_outside_length + aluminium_outer_flange_big_length, aluminium_outer_flange_big_inner_radius, aluminium_outer_flange_big_outer_radius);


   TGeoVolume *vol4 = gGeoManager->MakePcon("PCON4", pMedAluminium, 0.0, 360.0, 8);
   TGeoPcon *pcon4 = (TGeoPcon*)(vol4->GetShape());
   pcon4->DefineSection(0, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length), aluminium_outer_flange_small_inner_radius, aluminium_outer_flange_small_outer_radius);
   pcon4->DefineSection(1, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length), aluminium_outer_flange_small_inner_radius, aluminium_outer_flange_small_outer_radius);
   pcon4->DefineSection(2, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length), aluminium_pipe_middle_outside_inner_radius, aluminium_pipe_middle_outside_outer_radius);
   pcon4->DefineSection(3, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length), aluminium_pipe_middle_outside_inner_radius, aluminium_pipe_middle_outside_outer_radius);
   pcon4->DefineSection(4, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length), aluminium_adapter_big_inner_radius_bottom, aluminium_adapter_big_outer_radius_bottom);
   pcon4->DefineSection(5, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length + aluminium_pipe_big_outside_length), aluminium_pipe_big_outside_inner_radius, aluminium_pipe_big_outside_outer_radius);
   pcon4->DefineSection(6, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length + aluminium_pipe_big_outside_length), aluminium_outer_flange_big_inner_radius, aluminium_outer_flange_big_outer_radius);
   pcon4->DefineSection(7, -(beryllium_pipe_length / 2 + aluminium_pipe_small_length + aluminium_adapter_small_length + aluminium_pipe_middle_inside_length + aluminium_inner_flange_small_length + aluminium_outer_flange_small_length + aluminium_pipe_middle_outside_length + aluminium_adapter_big_length + aluminium_pipe_big_outside_length + aluminium_outer_flange_big_length), aluminium_outer_flange_big_inner_radius, aluminium_outer_flange_big_outer_radius);

   vol1->SetLineColor(kYellow);
   vol2->SetLineColor(kYellow);
   vol3->SetLineColor(kGreen);
   vol4->SetLineColor(kGreen);

   mother_vol->AddNode(vol1,1);
   mother_vol->AddNode(vol2,1);
   mother_vol->AddNode(vol3,1);
   mother_vol->AddNode(vol4,1);
   
   gGeoManager->SetNsegments(30);
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

   top->Draw("ogl");
   TGLViewer *v = (TGLViewer*)gPad->GetViewer3D();
   v->SetStyle(TGLRnrCtx::kOutline);
}

//__________________________________________________________________________
void DefineRequiredMedia(FairGeoMedia* geoMedia, FairGeoBuilder* geoBuild) 
{
   //air medium
   FairGeoMedium* mAir = geoMedia->getMedium("air");
   //if ( ! mAir ) Fatal("Main", "FairMedium air not found");
   geoBuild->createMedium(mAir);
   pMedAir = gGeoManager->GetMedium("air");
   //if ( ! pMedAir ) Fatal("Main", "Medium air not found");

   //aluminium medium
   FairGeoMedium* mAluminium = geoMedia->getMedium("aluminium");
   //if ( ! mAluminium  ) Fatal("Main", "FairMedium aluminium not found");
   geoBuild->createMedium(mAluminium);
   pMedAluminium  = gGeoManager->GetMedium("aluminium");
   //if ( ! pMedAluminium  ) Fatal("Main", "Medium aluminium not found");

   //beryllium medium
   FairGeoMedium* mBeryllium = geoMedia->getMedium("beryllium");
   //if ( ! mBeryllium  ) Fatal("Main", "FairMedium beryllium not found");
   geoBuild->createMedium(mBeryllium);
   pMedBeryllium = gGeoManager->GetMedium("beryllium");
   //if ( ! pMedBeryllium  ) Fatal("Main", "Medium beryllium not found");

   //vacuum medium
   FairGeoMedium* mVacuum = geoMedia->getMedium("vacuum");
   //if ( ! mVacuum  ) Fatal("Main", "FairMedium vacuum not found");
   geoBuild->createMedium(mVacuum);
   pMedVacuum = gGeoManager->GetMedium("vacuum");
   //if ( ! pMedVacuum  ) Fatal("Main", "Medium Vacuum not found");

   //steel medium
   FairGeoMedium* mSteel = geoMedia->getMedium("steel");
   //if ( ! mSteel  ) Fatal("Main", "FairMedium steel not found");
   geoBuild->createMedium(mSteel);
   pMedSteel = gGeoManager->GetMedium("steel");
   //if ( ! pMedSteel  ) Fatal("Main", "Medium steel not found");
}
