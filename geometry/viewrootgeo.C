#include "FairGeoLoader.h"
#include "FairGeoInterface.h"
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TFile.h"
#include "TString.h"

void viewrootgeo(TString file = "tpc_v7.root")
{
    TString gPath = gSystem->Getenv("VMCWORKDIR");
	
	// ----  global geometry parameters  ---------------------------------------
    FairGeoLoader*    geoLoad = new FairGeoLoader("TGeo","FairGeoLoader");
    FairGeoInterface* geoFace = geoLoad->getGeoInterface();

    // -------   Load media from media file   ----------------------------------
    TString medFile = gPath + "/geometry/media.geo";
    geoFace->setMediaFile(medFile);
    geoFace->readMedia();
	
	// --------------   Create geometry and global top volume  ------------------------
    gGeoManager = (TGeoManager*)gROOT->FindObject("FAIRGeom");
    gGeoManager->SetName("rootGeom");
	
	TString filePath;
	if(file.Contains("/"))
		filePath = file;
	else
		filePath = gPath + "/geometry/" + file;
	TFile* rFile = new TFile(filePath);
	
	//TGeoVolume* top = new TGeoVolumeAssembly("TOP");
	//rFile->ReadTObject(top, "TOP");
	
	TGeoVolume* top = (TGeoVolumeAssembly*)rFile->Get("TOP");
	
	gGeoManager->SetTopVolume(top);
	
	//top->Draw("par");
    top->Draw("ogl");
    TGLViewer *v = (TGLViewer*)gPad->GetViewer3D();
    v->SetStyle(TGLRnrCtx::kOutline);
}
