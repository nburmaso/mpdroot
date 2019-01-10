#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"
#include "TFile.h"

#include "TPC_geom_par.h"

#include "../mpdloadlibs.C"

void create_rootgeom_magnet_v5()
{

	mpdloadlibs(); // load libraries

	//-------------------- set working directory  ---------------------------------
	TString gPath = gSystem->Getenv("VMCWORKDIR");

	//-------------------- Geometry file name (output)   --------------------------
	const TString geoDetectorName = "magnet";
	const TString geoDetectorVersion = "v5";
	TString geoFileName = gPath + "/geometry/" + geoDetectorName + "_"+ geoDetectorVersion + ".root";
	// ----------------------------------------------------------------------------

	//-------------------- global geometry parameters  ----------------------------
	FairGeoLoader*    geoLoad = new FairGeoLoader("TGeo","FairGeoLoader");
	FairGeoInterface* geoFace = geoLoad->getGeoInterface();


	//-------------------- Load media from media file   ---------------------------

  //------------------ Magnet ---------------------------------------------------
	TString medFile = gPath + "/geometry/media.geo";
	geoFace->setMediaFile(medFile);
	geoFace->readMedia();
	FairGeoMedia*   geoMedia = geoFace->getMedia();
	FairGeoBuilder* geoBuild = geoLoad->getGeoBuilder();

	FairGeoMedium* mAir = geoMedia->getMedium("air");
	if ( ! mAir ) Fatal("Main", "FairMedium air not found");
	geoBuild->createMedium(mAir);
	TGeoMedium * pMedAir = gGeoManager->GetMedium("air");
	if ( ! pMedAir ) Fatal("Main", "Medium air not found");

	FairGeoMedium* mIron = geoMedia->getMedium("iron");
	if ( ! mIron ) Fatal("Main", "FairMedium iron not found");
	geoBuild->createMedium(mIron);
	TGeoMedium * pMedIron = gGeoManager->GetMedium("iron");
	if ( ! pMedIron ) Fatal("Main", "Medium iron not found");

	//----- Cryostat --------------------------------------------------------------
	FairGeoMedium* mVacuum = geoMedia->getMedium("vacuum");
	if ( ! mVacuum ) Fatal("Main", "FairMedium vacuum not found");
	geoBuild->createMedium(mVacuum);
	TGeoMedium * pMedVacuum = gGeoManager->GetMedium("vacuum");
	if ( ! pMedVacuum ) Fatal("Main", "Medium vacuum not found");

	FairGeoMedium* mAluminium = geoMedia->getMedium("aluminium");
	if ( ! mAluminium ) Fatal("Main", "FairMedium aluminium not found");
	geoBuild->createMedium(mAluminium);
	TGeoMedium * pMedAluminium = gGeoManager->GetMedium("aluminium");
	if ( ! pMedAluminium ) Fatal("Main", "Medium aluminium not found");

	FairGeoMedium* mCopper = geoMedia->getMedium("copper");
	if ( ! mCopper ) Fatal("Main", "FairMedium copper not found");
	geoBuild->createMedium(mCopper);
	TGeoMedium * pMedCopper = gGeoManager->GetMedium("copper");
	if ( ! pMedCopper ) Fatal("Main", "Medium copper not found");

/*
	//--- The material of the space between the solenoid and the cryostat walls ---
  FairGeoMedium* mHelium = geoMedia->getMedium("RICHgas_He+CH4");
  if ( ! mHelium ) Fatal("Main", "FairMedium RICHgas_He+CH4 not found");
  geoBuild->createMedium(mHelium);
  TGeoMedium * pMedHelium = gGeoManager->GetMedium("RICHgas_He+CH4r");
  if ( ! pMedHelium ) Fatal("Main", "Medium RICHgas_He+CH4 not found");
// or use this
	FairGeoMedium* mHelium = geoMedia->getMedium("RICHgas_He+CH4+");
  if ( ! mHelium ) Fatal("Main", "FairMedium RICHgas_He+CH4+ not found");
  geoBuild->createMedium(mHelium);
  TGeoMedium * pMedHelium = gGeoManager->GetMedium("RICHgas_He+CH4+");
  if ( ! pMedHelium ) Fatal("Main", "Medium RICHgas_He+CH4+ not found");
*/

	//-------------------- Create geometry and global top volume  -----------------
	gGeoManager = (TGeoManager*)gROOT->FindObject("FAIRGeom");
	gGeoManager->SetName(geoDetectorName + "_geom");
	TGeoVolume* top = new TGeoVolumeAssembly("TOP");
	top->SetMedium(pMedAir);
	gGeoManager->SetTopVolume(top);


  //-------------------- Mother volume  -----------------------------------------
  TString volnameb = "MAGNET";
  TGeoPcon *boxes = new TGeoPcon (volnameb, 0, 360, 10);

	boxes->DefineSection(0, -451, 65, 320);
	boxes->DefineSection(1, -379, 65, 320);
  boxes->DefineSection(2, -379, 135, 320);
  boxes->DefineSection(3, -374, 135, 320);
	boxes->DefineSection(4, -374, 229.8, 320);
	boxes->DefineSection(5, 374, 229.8, 320);
	boxes->DefineSection(6, 374, 135, 320);
	boxes->DefineSection(7, 379, 135, 320);
	boxes->DefineSection(8, 379, 65, 320);
	boxes->DefineSection(9, 451, 65, 320);

	TGeoVolume *box = new TGeoVolume(volnameb, boxes);
  box->SetMedium(pMedAir);
  box->SetTransparency(50);
  box->SetLineColor(TColor::GetColor("#ffffff"));

  TGeoCombiTrans *posb = new TGeoCombiTrans();
  posb->SetTranslation(0, 0, 0);

  top->AddNode(box, 0, posb);



	//-------------------- volumes at the ends of the barrel ----------------------
	TString volname1 = "ms01yokeend1";
	TGeoPcon *ms01yokeend1 = new TGeoPcon("ms01yokeend1",0,360,8);
	ms01yokeend1->DefineSection(0, -5, 135, 155);
	ms01yokeend1->DefineSection(1, 0, 135, 155);
	ms01yokeend1->DefineSection(2, 0, 65, 155);
	ms01yokeend1->DefineSection(3, 23, 65, 155);
	ms01yokeend1->DefineSection(4, 23, 140, 176);
	ms01yokeend1->DefineSection(5, 41, 140, 176);
	ms01yokeend1->DefineSection(6, 41, 145, 195);
	ms01yokeend1->DefineSection(7, 71, 145, 195);

	TGeoVolume *spcon = new TGeoVolume(volname1, ms01yokeend1);
	spcon->SetMedium(pMedIron);
	spcon->SetTransparency(0);
	spcon->SetLineColor(TColor::GetColor("#c7fcec"));

	TGeoCombiTrans *pos = new TGeoCombiTrans();
	pos->SetTranslation(0, 0, 379.5);

	box->AddNode(spcon, 0, pos);

  //-----------------------------------------------------------------------------

	TGeoCombiTrans *pos1 = new TGeoCombiTrans(*pos);
	pos1->ReflectZ(true);
	box->AddNode(spcon, 1, pos1);



	//-------------------- Barrel -------------------------------------------------
	TString volname4 = "ms01yokeendeom";
	TGeoPgon *ms01yokeendeom = new TGeoPgon("ms01yokeendeom", 0., 360., 12,6);
	ms01yokeendeom->DefineSection(0,0,200,305);
	ms01yokeendeom->DefineSection(1,30,200,305);
	ms01yokeendeom->DefineSection(2,30,278,305);
	ms01yokeendeom->DefineSection(3,871,278,305);
	ms01yokeendeom->DefineSection(4,871,200,305);
	ms01yokeendeom->DefineSection(5,901,200,305);


	TGeoVolume *spgon = new TGeoVolume(volname4, ms01yokeendeom);
	spgon->SetMedium(pMedIron);
	spgon->SetTransparency(0);
	spgon->SetLineColor(TColor::GetColor("#c7fcec"));

	TGeoCombiTrans *pos2 = new TGeoCombiTrans();
	pos2->SetTranslation(0, 0, -450.5);

	box->AddNode(spgon, 0, pos2);


	//-------------------- Cryostat -----------------------------------------------

	TString volnamecrybar = "ms01cryostat1";
	TGeoTube *ms01cryostat1 = new TGeoTube(volnamecrybar, 229.8,264,403);

	TGeoVolume *spconcry = new TGeoVolume(volnamecrybar, ms01cryostat1);
	spconcry->SetMedium(pMedAluminium);
	spconcry->SetTransparency(0);
	spconcry->SetLineColor(TColor::GetColor("#00ffff"));

	TGeoCombiTrans *pos7 = new TGeoCombiTrans();
	pos7->SetTranslation(0, 0, 0);

	box->AddNode(spconcry, 0, pos7);

  //---------- Еhe space between the solenoid and the cryostat walls ------------

	TString volnamecry = "ms01cryostat";
	TGeoTube *ms01cryostat = new TGeoTube(volnamecry, 230.3,263.6,402.5);
	TGeoVolume *tubecry = new TGeoVolume(volnamecry, ms01cryostat);
	tubecry->SetMedium(pMedVacuum);
	tubecry->SetTransparency(0);
	tubecry->SetLineColor(TColor::GetColor("#ff00ff"));

	TGeoCombiTrans *pos4 = new TGeoCombiTrans();
	pos4->SetTranslation(0, 0, 0);

	spconcry->AddNode(tubecry, 0, pos4);

  //----------------- Solenoid --------------------------------------------------

	TString volnamesoli = "ms01solenoid";
	TGeoTube *ms01solenoid = new TGeoTube(volnamesoli, 236.9,256.9,387.5);
	TGeoVolume *tubesoli = new TGeoVolume(volnamesoli, ms01solenoid);
	tubesoli->SetMedium(pMedCopper);
	tubesoli->SetTransparency(0);
	tubesoli->SetLineColor(TColor::GetColor("#00ff00"));

	TGeoCombiTrans *pos5 = new TGeoCombiTrans();
	pos5->SetTranslation(0, 0, 0);

	tubecry->AddNode(tubesoli, 0, pos5);



	top->SetVisContainers(kTRUE);

	//-------------------- Finish -------------------------------------------------

	gGeoManager->CloseGeometry();
	gGeoManager->CheckOverlaps(0.001);
	gGeoManager->PrintOverlaps();

	gGeoManager->Test();

	TFile* geoFile = new TFile(geoFileName, "RECREATE");
	top->Write();
	geoFile->Close();

	top->Draw("ogl");
	TGLViewer *v = (TGLViewer*)gPad->GetViewer3D();
	v->SetStyle(TGLRnrCtx::kOutline);

}
