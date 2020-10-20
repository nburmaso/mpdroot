#include <vector>
//---------------------------
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"
#include "TFile.h"
#include "TMath.h"

R__ADD_INCLUDE_PATH($VMCWORKDIR)
#include "macro/mpd/mpdloadlibs.C"


TGeoMedium *medBMDCAlu = 0;
TGeoMedium *medBMDCCar = 0;
TGeoMedium *medBMDCSci = 0;
TGeoMedium *pMedBMDFscScint = 0;
TGeoMedium *medBMDPlusSci = 0;
TGeoMedium *pMedAir = 0;
TGeoMedium *medBMDvacumm = 0;
TGeoMedium *pMedTPCmixture = 0;

class FairGeoMedia;
class FairGeoBuilder;
void DefineRequiredMedia(FairGeoMedia* geoMedia, FairGeoBuilder* geoBuild);
//void sectors_geom(TGeoVolume* mother_volume, Int_t A_or_B_side);

void cpcgeom2()
{
  mpdloadlibs(); // load libraries

  // ----  set working directory  --------------------------------------------
  TString gPath = gSystem->Getenv("VMCWORKDIR");

  // -------   Geometry file name (output)   ---------------------------------
  const TString geoDetectorName = "simple1";
  const TString geoDetectorVersion = "v2";
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
  // --------------------------------------------------------------------------------



  // --------------   Create geometry and global top volume  ------------------------
  TGeoManager* geom = (TGeoManager*)gROOT->FindObject("FAIRGeom");
  geom->SetName(geoDetectorName + "_geom");

  //--- define some materials
  /*
  FairGeoMedium* mAir = geoMedia->getMedium("air");
  if ( ! mAir ) Fatal("Main", "FairMedium air not found");
  geoBuild->createMedium(mAir);
  TGeoMedium* pMedAir = gGeoManager->GetMedium("air");
  if ( ! pMedAir ) Fatal("Main", "Medium air not found");

  //scintillator medium

  FairGeoMedium* mScintillator = geoMedia->getMedium("NDetScin");
  if ( ! mScintillator  ) Fatal("Main", "FairMedium NDetScin not found");
  geoBuild->createMedium(mScintillator);
  TGeoMedium* medBMDCSci  = gGeoManager->GetMedium("NDetScin");
  if ( ! medBMDCSci  ) Fatal("Main", "Medium NDetScin not found");

  //Fast scintillator medium

  FairGeoMedium* mFscScint  = geoMedia->getMedium("FscScint");
  if( !mFscScint ) Fatal("Main", "Medium FscScint not found");
  geoBuild->createMedium(mFscScint);
  TGeoMedium* pMedBMDFscScint = gGeoManager->GetMedium("FscScint");
  if( ! pMedBMDFscScint ) Fatal("Main", "Medium FscScint not found");
  */    

  //--- make the top container volume
  TGeoVolume *top1 =new TGeoVolumeAssembly("TOP1");
  geom->SetTopVolume(top1);
  TGeoVolume *top2 = new TGeoVolumeAssembly("TOP2");
  top1->SetMedium(pMedAir);
  top1->SetTransparency(0);
  top2->SetMedium(pMedAir);
  top2->SetTransparency(80);

  //int A_side = 1;
  //int B_side = 2;


  //--- define the transformations
  TGeoTranslation *tr1 = new TGeoTranslation("tr1", 121.72/10, 0., 0.);
  tr1->RegisterYourself();
  TGeoTranslation *tr2 = new TGeoTranslation("tr2", 169.03705/10, 0, 0.);
  tr2->RegisterYourself();
  TGeoTranslation *tr3 = new TGeoTranslation("tr3", 217.33335/10, 0., 0.);
  tr3->RegisterYourself();
  TGeoTranslation *tr4 = new TGeoTranslation("tr4", 265.6296/10, 0., 0.);
  tr4->RegisterYourself();
  TGeoTranslation *tr5 = new TGeoTranslation("tr5", 313.9259/10, 0., 0.);
  tr5->RegisterYourself();
  TGeoTranslation *tr6 = new TGeoTranslation("tr6", 362.22215/10, 0., 0.);
  tr6->RegisterYourself();
  TGeoTranslation *tr7 = new TGeoTranslation("tr7", 410.51845/10, 0., 0.);
  tr7->RegisterYourself();
  TGeoTranslation *tr8 = new TGeoTranslation("tr8", 459.5392/10, 0., 0.);
  tr8->RegisterYourself();
  TGeoTranslation *cr11 = new TGeoTranslation("cr11", 121.72/10, 8.6286/10, 0.);
  cr11->RegisterYourself();
  TGeoTranslation *cr12 = new TGeoTranslation("cr12", 121.72/10, -8.6286/10, 0.);
  cr12->RegisterYourself();
  TGeoTranslation *cr21 = new TGeoTranslation("cr21", 169.0370/10, 20.2933/10, 0.);
  cr21->RegisterYourself();
  TGeoTranslation *cr22 = new TGeoTranslation("cr22", 169.0370/10, -20.2933/10, 0.);   
  cr22->RegisterYourself();
  TGeoTranslation *cr31 = new TGeoTranslation("cr31", 217.3333/10, 33.2343/10, 0.);
  cr31->RegisterYourself();
  TGeoTranslation *cr32 = new TGeoTranslation("cr32", 217.3333/10,-33.2343/10, 0.);
  cr32->RegisterYourself();
  TGeoTranslation *cr41 = new TGeoTranslation("cr41", 265.6296/10, 46.1752/10, 0.);
  cr41->RegisterYourself();
  TGeoTranslation *cr42 = new TGeoTranslation("cr42", 265.6296/10, -46.1752/10, 0.);
  cr42->RegisterYourself();
  TGeoTranslation *cr51 = new TGeoTranslation("cr51", 313.9259/10, 59.1162/10, 0.);
  cr51->RegisterYourself();
  TGeoTranslation *cr52 = new TGeoTranslation("cr52", 313.9259/10, -59.1162/10, 0.);
  cr52->RegisterYourself();
  TGeoTranslation *cr61 = new TGeoTranslation("cr61", 362.2222/10, 72.0571/10, 0.);
  cr61->RegisterYourself();
  TGeoTranslation *cr62 = new TGeoTranslation("cr62", 362.2222/10, -72.0571/10, 0.);
  cr62->RegisterYourself();
  TGeoTranslation *cr71 = new TGeoTranslation("cr71", 410.5185/10, 84.9981/10, 0.);
  cr71->RegisterYourself();
  TGeoTranslation *cr72 = new TGeoTranslation("cr72", 410.5185/10, -84.9981/10, 0.);
  cr72->RegisterYourself();
  TGeoTranslation *cr81 = new TGeoTranslation("cr81", 459.5392/10, 97.3832/10, 0.);
  cr81->RegisterYourself();
  TGeoTranslation *cr82 = new TGeoTranslation("cr82", 459.5392/10, -97.3832/10, 0.);
  cr82->RegisterYourself();
  TGeoTranslation *base1 = new TGeoTranslation("base1", 0.,0.,200.);
  TGeoTranslation *base2 = new TGeoTranslation("base2", 0.,0., -200.);

  TGeoTube *rep_shape = new TGeoTube ("name", 6.5, 55, 2.2);
  TGeoVolume *replica1 = new TGeoVolume ("replica1", rep_shape, pMedAir);//geom->MakeTubs("REPLICA1", pMedAir, 6.5, 60., 2.2, 0., 360.);
  replica1->SetTransparency(0);
  TGeoVolume *replica2 = new TGeoVolume ("replica2", rep_shape, pMedAir);//geom->MakeTubs("REPLICA1", pMedAir, 6.5, 60., 2.2, 0., 360.);
  replica2->SetTransparency(0);
  //replica1->SetLineColor(kGreen);



  //--- make boxes level 1

  TGeoBBox *box1 = new TGeoBBox("box1", 21.72/10, 8.6286/10, 1.);
  TGeoBBox *box2 = new TGeoBBox("box2", 22.69925/10, 20.2933/10, 1.);
  TGeoBBox *box3 = new TGeoBBox("box3", 22.69925/10, 33.2343/10, 1.);
  TGeoBBox *box4 = new TGeoBBox("box4", 22.69925/10, 46.1752/10, 1.);
  TGeoBBox *box5 = new TGeoBBox("box5", 22.69925/10, 59.1162/10, 1.);
  TGeoBBox *box6 = new TGeoBBox("box6", 22.69925/10, 72.0571/10, 1.);
  TGeoBBox *box7 = new TGeoBBox("box7", 22.69925/10, 84.9981/10, 1.);
  TGeoBBox *box8 = new TGeoBBox("box8", 22.69925/10, 97.3832/10, 1.);
  //--- make half-rings level 1
  TGeoTubeSeg *circle11 = new TGeoTubeSeg("circle11", 0., 21.72/10, 1., 0., 180.);
  TGeoTubeSeg *circle12 = new TGeoTubeSeg("circle12", 0., 21.72/10, 1., 180., 360.);
  TGeoTubeSeg *circle21 = new TGeoTubeSeg("circle21", 0., 22.6992/10, 1., 0., 180.);
  TGeoTubeSeg *circle22 = new TGeoTubeSeg("circle22", 0., 22.6992/10, 1., 180., 360.);
  //--- make compositeshapes level 1
  TGeoCompositeShape *segment1 = new TGeoCompositeShape("segment1", "box1:tr1+circle11:cr11+circle12:cr12");
  TGeoCompositeShape *segment2 = new TGeoCompositeShape("segment2", "(circle21:cr21+box2:tr2+circle22:cr22)");
  TGeoCompositeShape *segment3 = new TGeoCompositeShape("segment3", "(circle21:cr31+box3:tr3+circle22:cr32)");
  TGeoCompositeShape *segment4 = new TGeoCompositeShape("segment4", "(circle21:cr41+box4:tr4+circle22:cr42)");
  TGeoCompositeShape *segment5 = new TGeoCompositeShape("segment5", "(circle21:cr51+box5:tr5+circle22:cr52)");
  TGeoCompositeShape *segment6 = new TGeoCompositeShape("segment6", "(circle21:cr61+box6:tr6+circle22:cr62)");
  TGeoCompositeShape *segment7 = new TGeoCompositeShape("segment7", "(circle21:cr71+box7:tr7+circle22:cr72)");
  TGeoCompositeShape *segment8 = new TGeoCompositeShape("segment8", "(circle21:cr81+box8:tr8+circle22:cr82)");
  

for (int k=1; k < 3; k++)
{
  if (k==2) 
    {
      top2->AddNode(replica2, k, base2);
      for (int i=1; i < 9; i++)
      {
        for (int j=1; j < 13; j++)
        {
          if (i==1) 
          {
          TGeoVolume *segmentvolume11 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment1, pMedBMDFscScint);
          replica2->AddNode(segmentvolume11, 96 + i * 12 - (12 - j), new TGeoRotation(Form("rot_%d_%d", i, j), (j-1)*30., 0., 0));
          replica2->SetLineColor(kGreen);
          }
          if (i==2) 
          {
          TGeoVolume *segmentvolume12 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment2, pMedBMDFscScint);
          replica2->AddNode(segmentvolume12, 96 + i * 12 - (12 - j), new TGeoRotation(Form("rot2_%d_%d", i, j), (j-1)*30., 0., 0));
          replica2->SetLineColor(kYellow);
          }
          if (i==3) 
          {
          TGeoVolume *segmentvolume13 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment3, pMedBMDFscScint);
          replica2->AddNode(segmentvolume13, 96 + i * 12 - (12 - j), new TGeoRotation(Form("rot3_%d_%d", i, j), (j-1)*30., 0., 0));
          replica2->SetLineColor(kGreen);
          }
          if (i==4) 
          {
          TGeoVolume *segmentvolume14 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment4, pMedBMDFscScint);
          replica2->AddNode(segmentvolume14, 96 + i * 12 - (12 - j), new TGeoRotation(Form("rot4_%d_%d", i, j), (j-1)*30., 0., 0));
          replica2->SetLineColor(kYellow);
          }
          if (i==5) 
          {
          TGeoVolume *segmentvolume15 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment5, pMedBMDFscScint);
          replica2->AddNode(segmentvolume15, 96 + i * 12 - (12 - j), new TGeoRotation(Form("rot5_%d_%d_", i, j), (j-1)*30., 0., 0));
          replica2->SetLineColor(kGreen);
          }
          if (i==6) 
          {
          TGeoVolume *segmentvolume16 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment6, pMedBMDFscScint);
          replica2->AddNode(segmentvolume16, 96 + i * 12 - (12 - j), new TGeoRotation(Form("rot6_%d_%d", i, j), (j-1)*30., 0., 0));
          replica2->SetLineColor(kYellow);
          }
          if (i==7) 
          {
          TGeoVolume *segmentvolume17 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment7, pMedBMDFscScint);
          replica2->AddNode(segmentvolume17, 96 + i * 12 - (12 - j), new TGeoRotation(Form("rot7_%d_%d", i, j), (j-1)*30., 0., 0));
          replica2->SetLineColor(kGreen);
          }
          if (i==8) 
          {
          TGeoVolume *segmentvolume18 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment8, pMedBMDFscScint);
          replica2->AddNode(segmentvolume18, 96 + i * 12 - (12 - j), new TGeoRotation(Form("rot8_%d_%d", i, j), (j-1)*30., 0., 0));
          replica2->SetLineColor(kGreen);
          }
        }
      }
    }

    if (k==1) 
    {
      top2->AddNode(replica1, k, base1);
      for (int i=1; i < 9; i++)
      {
        for (int j=1; j < 13; j++)
        {
          if (i==1) 
          {
          TGeoVolume *segmentvolume21 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment1, pMedBMDFscScint);
          replica1->AddNode(segmentvolume21, i * 12 - (12 - j), new TGeoRotation(Form("rot1_%d_%d", i, j), (j-1)*30., 0., 0));
          }
          if (i==2) 
          {
          TGeoVolume *segmentvolume22 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment2, pMedBMDFscScint);
          replica1->AddNode(segmentvolume22, i * 12 - (12 - j), new TGeoRotation(Form("rot2_%d_%d", i, j), (j-1)*30., 0., 0));
          }
          if (i==3) 
          {
          TGeoVolume *segmentvolume23 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment3, pMedBMDFscScint);
          replica1->AddNode(segmentvolume23, i * 12 - (12 - j), new TGeoRotation(Form("rot3_%d_%d", i, j), (j-1)*30., 0., 0));
          }
          if (i==4) 
          {
          TGeoVolume *segmentvolume24 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment4, pMedBMDFscScint);
          replica1->AddNode(segmentvolume24, i * 12 - (12 - j), new TGeoRotation(Form("rot4_%d_%d", i, j), (j-1)*30., 0., 0));
          }
          if (i==5) 
          {
          TGeoVolume *segmentvolume25 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment5, pMedBMDFscScint);
          replica1->AddNode(segmentvolume25, i * 12 - (12 - j), new TGeoRotation(Form("rot5_%d_%d", i, j), (j-1)*30., 0., 0));
          }
          if (i==6) 
          {
          TGeoVolume *segmentvolume26 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment6, pMedBMDFscScint);
          replica1->AddNode(segmentvolume26, i * 12 - (12 - j), new TGeoRotation(Form("rot6_%d_%d_", i, j), (j-1)*30., 0., 0));
          }
          if (i==7) 
          {
          TGeoVolume *segmentvolume27 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment7, pMedBMDFscScint);
          replica1->AddNode(segmentvolume27, i * 12 - (12 - j), new TGeoRotation(Form("rot7_%d_%d", i, j), (j-1)*30., 0., 0));
          }
          if (i==8) 
          {
          TGeoVolume *segmentvolume28 = new TGeoVolume (Form("segmentvolume_%d_%d_%d", k, i, j), segment8, pMedBMDFscScint);
          replica1->AddNode(segmentvolume28, i * 12 - (12 - j), new TGeoRotation(Form("rot_%d_%d",i, j), (j-1)*30., 0., 0));
          }
        }
      }
    }
}
  //top2->AddNode(replica1,1, base1);
  //top2->AddNode(replica1,1, base2);
  top1->AddNode(top2,1);
  top1->Draw("ogl");
  TGLViewer *v = (TGLViewer*)gPad->GetViewer3D();
  v->SetStyle(TGLRnrCtx::kOutline);
  top1->Draw("");

  //--- close the geometry

  geom->CloseGeometry();

  //--- draw the ROOT box.
  // by default the picture will appear in the standard ROOT TPad.
  //if you have activated the following line in system.rootrc,
  //it will appear in the GL viewer
  //#Viewer3D.DefaultDrawOption:   ogl

  geom->SetVisLevel(5);

  // ---------------   Finish   ----------------------------------------------
  gGeoManager->CloseGeometry();
  gGeoManager->CheckOverlaps(0.001);
  gGeoManager->PrintOverlaps();

  gGeoManager->Test();

  TFile* geoFile = new TFile(geoFileName, "RECREATE");
  top1->Write();
  geoFile->Close();

}



void DefineRequiredMedia(FairGeoMedia* geoMedia, FairGeoBuilder* geoBuild)
{
  //--- define some materials

  FairGeoMedium* mAir = geoMedia->getMedium("air");
  if ( ! mAir ) Fatal("Main", "FairMedium air not found");
  geoBuild->createMedium(mAir);
  pMedAir = gGeoManager->GetMedium("air");
  if ( ! pMedAir ) Fatal("Main", "Medium air not found");

  //scintillator medium

  FairGeoMedium* mScintillator = geoMedia->getMedium("NDetScin");
  if ( ! mScintillator  ) Fatal("Main", "FairMedium NDetScin not found");
  geoBuild->createMedium(mScintillator);
  medBMDCSci  = gGeoManager->GetMedium("NDetScin");
  if ( ! medBMDCSci  ) Fatal("Main", "Medium NDetScin not found");

  //Fast scintillator medium

  FairGeoMedium* mFscScint  = geoMedia->getMedium("FscScint");
  if( !mFscScint ) Fatal("Main", "Medium FscScint not found");
  geoBuild->createMedium(mFscScint);
  pMedBMDFscScint = gGeoManager->GetMedium("FscScint");
  if( ! pMedBMDFscScint ) Fatal("Main", "Medium FscScint not found");

}
