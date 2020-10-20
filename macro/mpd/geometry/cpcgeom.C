#include <vector>
//---------------------------
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"
#include "TFile.h"
#include "TMath.h"

R__ADD_INCLUDE_PATH($VMCWORKDIR)
#include "macro/mpd/mpdloadlibs.C"

void cpcgeom()
{
	mpdloadlibs(); // load libraries

    // ----  set working directory  --------------------------------------------
    TString gPath = gSystem->Getenv("VMCWORKDIR");

    // -------   Geometry file name (output)   ---------------------------------
    const TString geoDetectorName = "simple1";
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
    //DefineRequiredMedia(geoMedia, geoBuild);
    // --------------------------------------------------------------------------------
    
        // --------------   Create geometry and global top volume  ------------------------
    TGeoManager* geom = (TGeoManager*)gROOT->FindObject("FAIRGeom");
    geom->SetName(geoDetectorName + "_geom");
    
   //--- define some materials

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
    
    
    
   //--- define the transformations
   TGeoTranslation *tr1_left = new TGeoTranslation(121.72/10, 0., -200.);
   TGeoTranslation *tr2_left = new TGeoTranslation(169.03705/10, 0, -200.);
   TGeoTranslation *tr3_left = new TGeoTranslation(217.33335/10, 0., -200.);
   TGeoTranslation *tr4_left = new TGeoTranslation(265.6296/10, 0., -200.);
   TGeoTranslation *tr5_left = new TGeoTranslation(313.9259/10, 0., -200.);
   TGeoTranslation *tr6_left = new TGeoTranslation(362.22215/10, 0., -200.);
   TGeoTranslation *tr7_left = new TGeoTranslation(410.51845/10, 0., -200.);
   TGeoTranslation *tr8_left = new TGeoTranslation(459.5392/10, 0., -200.);
   TGeoTranslation *cr11_left = new TGeoTranslation(121.72/10, 8.6286/10, -200.);
   TGeoTranslation *cr12_left = new TGeoTranslation(121.72/10, -8.6286/10, -200.);
   TGeoTranslation *cr21_left = new TGeoTranslation(169.0370/10, 20.2933/10, -200.);
   TGeoTranslation *cr22_left = new TGeoTranslation(169.0370/10, -20.2933/10, -200.);   
   TGeoTranslation *cr31_left = new TGeoTranslation(217.3333/10,33.2343/10, -200.);
   TGeoTranslation *cr32_left = new TGeoTranslation(217.3333/10,-33.2343/10, -200.);
   TGeoTranslation *cr41_left = new TGeoTranslation(265.6296/10, 46.1752/10, -200.);
   TGeoTranslation *cr42_left = new TGeoTranslation(265.6296/10, -46.1752/10, -200.);
   TGeoTranslation *cr51_left = new TGeoTranslation(313.9259/10, 59.1162/10, -200.);
   TGeoTranslation *cr52_left = new TGeoTranslation(313.9259/10, -59.1162/10, -200.);
   TGeoTranslation *cr61_left = new TGeoTranslation(362.2222/10, 72.0571/10, -200.);
   TGeoTranslation *cr62_left = new TGeoTranslation(362.2222/10, -72.0571/10, -200.);
   TGeoTranslation *cr71_left = new TGeoTranslation(410.5185/10, 84.9981/10, -200.);
   TGeoTranslation *cr72_left = new TGeoTranslation(410.5185/10, -84.9981/10, -200.);
   TGeoTranslation *cr81_left = new TGeoTranslation(459.5392/10, 97.3832/10, -200.);
   TGeoTranslation *cr82_left = new TGeoTranslation(459.5392/10, -97.3832/10, -200.);
	//TGeoTranslation *base1 = new TGeoTranslation(0.,0.,200.);
	//TGeoTranslation *base2 = new TGeoTranslation(0.,0., -200.);

   TGeoTranslation *tr1_right = new TGeoTranslation(121.72/10, 0., 200.);
   TGeoTranslation *tr2_right = new TGeoTranslation(169.03705/10, 0, 200.);
   TGeoTranslation *tr3_right = new TGeoTranslation(217.33335/10, 0., 200.);
   TGeoTranslation *tr4_right = new TGeoTranslation(265.6296/10, 0., 200.);
   TGeoTranslation *tr5_right = new TGeoTranslation(313.9259/10, 0., 200.);
   TGeoTranslation *tr6_right = new TGeoTranslation(362.22215/10, 0., 200.);
   TGeoTranslation *tr7_right = new TGeoTranslation(410.51845/10, 0., 200.);
   TGeoTranslation *tr8_right = new TGeoTranslation(459.5392/10, 0., 200.);
   TGeoTranslation *cr11_right = new TGeoTranslation(121.72/10, 8.6286/10, 200.);
   TGeoTranslation *cr12_right= new TGeoTranslation(121.72/10, -8.6286/10, 200.);
   TGeoTranslation *cr21_right = new TGeoTranslation(169.0370/10, 20.2933/10, 200.);
   TGeoTranslation *cr22_right = new TGeoTranslation(169.0370/10, -20.2933/10, 200.);   
   TGeoTranslation *cr31_right = new TGeoTranslation(217.3333/10,33.2343/10, 200.);
   TGeoTranslation *cr32_right = new TGeoTranslation(217.3333/10,-33.2343/10, 200.);
   TGeoTranslation *cr41_right = new TGeoTranslation(265.6296/10, 46.1752/10, 200.);
   TGeoTranslation *cr42_right = new TGeoTranslation(265.6296/10, -46.1752/10, 200.);
   TGeoTranslation *cr51_right = new TGeoTranslation(313.9259/10, 59.1162/10, 200.);
   TGeoTranslation *cr52_right = new TGeoTranslation(313.9259/10, -59.1162/10, 200.);
   TGeoTranslation *cr61_right = new TGeoTranslation(362.2222/10, 72.0571/10, 200.);
   TGeoTranslation *cr62_right = new TGeoTranslation(362.2222/10, -72.0571/10, 200.);
   TGeoTranslation *cr71_right = new TGeoTranslation(410.5185/10, 84.9981/10, 200.);
   TGeoTranslation *cr72_right = new TGeoTranslation(410.5185/10, -84.9981/10, 200.);
   TGeoTranslation *cr81_right = new TGeoTranslation(459.5392/10, 97.3832/10, 200.);
   TGeoTranslation *cr82_right = new TGeoTranslation(459.5392/10, -97.3832/10, 200.);


   //--- make the top container volume
   TGeoVolume *top =new TGeoVolumeAssembly("TOP1");
   geom->SetTopVolume(top);
   TGeoVolume *location1 = gGeoManager->MakeBox("location1",pMedAir ,100.,100.,500.);//new TGeoVolumeAssembly("LOCATION1");
   //top->AddNode(location1,1);
   TGeoVolume *location2 = gGeoManager->MakeBox("location2",pMedAir ,100.,100.,500.);//new TGeoVolumeAssembly("LOCATION2");
   //top->AddNode(location2,1);
   top->SetMedium(pMedAir);
   top->SetTransparency(99);
   location1->SetMedium(pMedAir);
   location1->SetTransparency(99);
   location2->SetMedium(pMedAir);
   location2->SetTransparency(99);
   TGeoTube *rep_shape = new TGeoTube ("name", 6.5, 55, 2.2);
   TGeoVolume *replica = new TGeoVolume ("replica", rep_shape);//geom->MakeTubs("REPLICA1", pMedAir, 6.5, 60., 2.2, 0., 360.);
   //TGeoTubeSeg *sect_shape = new TGeoTubeSeg ("name2", 6.5, 55, 2.2, -15, 15);
   //TGeoVolume *sector_replica = new TGeoVolume ("sector_replica", sect_shape); // geom->MakeTubs("sector_replica1", pMedAir, 6.5, 60., 2.2, -15., 15.);
	replica->SetTransparency(99);
	//sector_replica->SetTransparency(99);
  replica->AddNode(location1,1);
  replica->AddNode(location2,1);
  top->AddNode(replica,1);
  //location2->AddNode(replica,1, base2);
  //top1->AddNode(replica,1, base2);
   //TGeoVolume *replica2 = geom->MakeTubs("REPLICA2", Vacuum, 100., 500., 12., 30., 60.);
   //TGeoVolume *replica3 = geom->MakeTubs("REPLICA3", Vacuum, 100., 500., 12., 60., 90.);
   //TGeoVolume *replica4 = geom->MakeTubs("REPLICA4", Vacuum, 100., 500., 12., 90., 120.);
   //TGeoVolume *replica5 = geom->MakeTubs("REPLICA5", Vacuum, 100., 500., 12., 120., 150.);
   //TGeoVolume *replica6 = geom->MakeTubs("REPLICA6", Vacuum, 100., 500., 12., 150., 180.);
   //TGeoVolume *replica7 = geom->MakeTubs("REPLICA7", Vacuum, 100., 500., 12., 180., 210.);
   //TGeoVolume *replica8 = geom->MakeTubs("REPLICA8", Vacuum, 100., 500., 12., 210., 240.);
   //TGeoVolume *replica9 = geom->MakeTubs("REPLICA9", Vacuum, 100., 500., 12., 240., 270.);
   //TGeoVolume *replica10 = geom->MakeTubs("REPLICA10", Vacuum, 100., 500., 12., 270., 300.);
   //TGeoVolume *replica11 = geom->MakeTubs("REPLICA11", Vacuum, 100., 500., 12., 300., 330.);
   //TGeoVolume *replica12 = geom->MakeTubs("REPLICA12", Vacuum, 100., 500., 12., 330., 360.);
   replica->SetVisibility(kFALSE);


   //--- make boxes level 1
   TGeoVolume *box1 = geom->MakeBox("Active-box1", pMedBMDFscScint, 21.72/10, 8.6286/10, 1.);
   TGeoVolume *circle11 = geom->MakeTubs("Active-circle11", pMedBMDFscScint, 0., 21.72/10, 1., 0., 180.);
   TGeoVolume *circle12 = geom->MakeTubs("Active-circle12", pMedBMDFscScint, 0., 21.72/10, 1., 180., 360.);
   TGeoVolume *box2 = geom->MakeBox("Active-box2", pMedBMDFscScint, 22.69925/10, 20.2933/10, 1.);
   TGeoVolume *circle21 = geom->MakeTubs("Active-circle11", pMedBMDFscScint, 0., 22.6992/10, 1., 0., 180.);
   TGeoVolume *circle22 = geom->MakeTubs("Active-circle12", pMedBMDFscScint, 0., 22.6992/10, 1., 180., 360.);
   TGeoVolume *box3 = geom->MakeBox("Active-box3", pMedBMDFscScint, 22.69925/10, 33.2343/10, 1.);
   TGeoVolume *box4 = geom->MakeBox("Active-box4", pMedBMDFscScint, 22.69925/10, 46.1752/10, 1.);
   TGeoVolume *box5 = geom->MakeBox("Active-box5", pMedBMDFscScint, 22.69925/10, 59.1162/10, 1.);
   TGeoVolume *box6 = geom->MakeBox("Active-box6", pMedBMDFscScint, 22.69925/10, 72.0571/10, 1.);
   TGeoVolume *box7 = geom->MakeBox("Active-box7", pMedBMDFscScint, 22.69925/10, 84.9981/10, 1.);
   TGeoVolume *box8 = geom->MakeBox("Active-box8", pMedBMDFscScint, 22.69925/10, 97.3832/10, 1.);
	box1->SetTransparency(0);
	circle11->SetTransparency(0);
	circle12->SetTransparency(0);
	box2->SetTransparency(0);
	circle21->SetTransparency(0);
	circle22->SetTransparency(0);
	box3->SetTransparency(0);
	box4->SetTransparency(0);
	box5->SetTransparency(0);
	box6->SetTransparency(0);
	box7->SetTransparency(0);
	box8->SetTransparency(0);

	location1->AddNode(circle11, 1, cr11_right);
	location1->AddNode(circle12, 1, cr12_right);
	location1->AddNode(circle21, 1, cr21_right);
	location1->AddNode(circle22, 1, cr22_right);
	location1->AddNode(circle21, 1, cr31_right);
	location1->AddNode(circle22, 1, cr32_right);
	location1->AddNode(circle21, 1, cr41_right);
	location1->AddNode(circle22, 1, cr42_right);
	location1->AddNode(circle21, 1, cr51_right);
	location1->AddNode(circle22, 1, cr52_right);
	location1->AddNode(circle21, 1, cr61_right);
	location1->AddNode(circle22, 1, cr62_right);
	location1->AddNode(circle21, 1, cr71_right);
	location1->AddNode(circle22, 1, cr72_right);
	location1->AddNode(circle21, 1, cr81_right);
	location1->AddNode(circle22, 1, cr82_right);


	location1->AddNode(box1, 1, tr1_right);
	location1->AddNode(box2, 1, tr2_right);
	location1->AddNode(box3, 1, tr3_right);
	location1->AddNode(box4, 1, tr4_right);
	location1->AddNode(box5, 1, tr5_right);
	location1->AddNode(box6, 1, tr6_right);
	location1->AddNode(box7, 1, tr7_right);
	location1->AddNode(box8, 1, tr8_right);


    location2->AddNode(circle11, 1, cr11_left);
  location2->AddNode(circle12, 1, cr12_left);
  location2->AddNode(circle21, 1, cr21_left);
  location2->AddNode(circle22, 1, cr22_left);
  location2->AddNode(circle21, 1, cr31_left);
  location2->AddNode(circle22, 1, cr32_left);
  location2->AddNode(circle21, 1, cr41_left);
  location2->AddNode(circle22, 1, cr42_left);
  location2->AddNode(circle21, 1, cr51_left);
  location2->AddNode(circle22, 1, cr52_left);
  location2->AddNode(circle21, 1, cr61_left);
  location2->AddNode(circle22, 1, cr62_left);
  location2->AddNode(circle21, 1, cr71_left);
  location2->AddNode(circle22, 1, cr72_left);
  location2->AddNode(circle21, 1, cr81_left);
  location2->AddNode(circle22, 1, cr82_left);


  location2->AddNode(box1, 1, tr1_left);
  location2->AddNode(box2, 1, tr2_left);
  location2->AddNode(box3, 1, tr3_left);
  location2->AddNode(box4, 1, tr4_left);
  location2->AddNode(box5, 1, tr5_left);
  location2->AddNode(box6, 1, tr6_left);
  location2->AddNode(box7, 1, tr7_left);
  location2->AddNode(box8, 1, tr8_left);


   replica->AddNode(location1, 1);
   replica->AddNode(location1,1, new TGeoRotation("rot1", 30., 0., 0));
   replica->AddNode(location1,1, new TGeoRotation("rot2", 60., 0., 0.));
   replica->AddNode(location1,1, new TGeoRotation("rot3", 90., 0., 0.));
   replica->AddNode(location1,1, new TGeoRotation("rot4", 120., 0., 0.));
   replica->AddNode(location1,1, new TGeoRotation("rot5", 150., 0., 0.));
   replica->AddNode(location1,1, new TGeoRotation("rot6", 180., 0., 0.));
   replica->AddNode(location1,1, new TGeoRotation("rot7", 210., 0., 0.));
   replica->AddNode(location1,1, new TGeoRotation("rot8", 240., 0., 0.));
   replica->AddNode(location1,1, new TGeoRotation("rot9", 270., 0., 0.));
   replica->AddNode(location1,1, new TGeoRotation("rot10", 300., 0., 0.));
   replica->AddNode(location1,1, new TGeoRotation("rot11", 330., 0., 0.));


   replica->AddNode(location2, 1);
   replica->AddNode(location2,1, new TGeoRotation("rot1", 30., 0., 0));
   replica->AddNode(location2,1, new TGeoRotation("rot2", 60., 0., 0.));
   replica->AddNode(location2,1, new TGeoRotation("rot3", 90., 0., 0.));
   replica->AddNode(location2,1, new TGeoRotation("rot4", 120., 0., 0.));
   replica->AddNode(location2,1, new TGeoRotation("rot5", 150., 0., 0.));
   replica->AddNode(location2,1, new TGeoRotation("rot6", 180., 0., 0.));
   replica->AddNode(location2,1, new TGeoRotation("rot7", 210., 0., 0.));
   replica->AddNode(location2,1, new TGeoRotation("rot8", 240., 0., 0.));
   replica->AddNode(location2,1, new TGeoRotation("rot9", 270., 0., 0.));
   replica->AddNode(location2,1, new TGeoRotation("rot10", 300., 0., 0.));
   replica->AddNode(location2,1, new TGeoRotation("rot11", 330., 0., 0.));

	//top1->SetVisContainers(kTRUE);
       top->Draw();



   //--- draw the ROOT box.
   // by default the picture will appear in the standard ROOT TPad.
   //if you have activated the following line in system.rootrc,
   //it will appear in the GL viewer
   //#Viewer3D.DefaultDrawOption:   ogl

   //geom->SetVisLevel(5);

       // ---------------   Finish   ----------------------------------------------
    gGeoManager->CloseGeometry();
    gGeoManager->CheckOverlaps(0.001);
    gGeoManager->PrintOverlaps();

    gGeoManager->Test();

    TFile* geoFile = new TFile(geoFileName, "RECREATE");
    top->Write();
    geoFile->Close();

}
