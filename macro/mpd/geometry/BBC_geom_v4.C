//Demezhan Myktybekov
//Master's degree project 2020
//myktybekov@jinr.ru


#include <vector>
//---------------------------
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"
#include "TFile.h"
#include "TMath.h"

R__ADD_INCLUDE_PATH($VMCWORKDIR)
#include "macro/mpd/mpdloadlibs.C"

//------------------------- parameter const ----------------------------------------------------
//Detector's position
const Double_t BBC_Xpos = 0.0;
const Double_t BBC_Ypos = 0.0;
const Double_t BBC_center_Zpos = 200;

//------------------------- mother volume ------------------------------------------------------
const Double_t BBC_mother_vol_inner_radius = 5.5;
const Double_t BBC_mother_vol_outter_radius = 40;
const Double_t BBC_mother_vol_thickness = 2;

//------------------------- hexagon ------------------------------------------------------------
const Double_t hex_radius_small = 2.5;
const Double_t hex_thikness = 2.;
const Double_t angle_deg = 30;
Double_t angle_rad = angle_deg * TMath::DegToRad();
Double_t hex_radius_big = hex_radius_small/TMath::Cos(angle_rad);

//----------------------------------------------------------------------------------------------

//------------------------- chto-to esche ------------------------------------------------------

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


void ring(TGeoVolume* mother_volume, Int_t A_or_B_side, Int_t nring);


void DefineRequiredMedia(FairGeoMedia* geoMedia, FairGeoBuilder* geoBuild);

//------------------------- geometry ----------------------------------------------------------
void BBC_geom_v4()
{
    mpdloadlibs(); // load libraries

    // ----  set working directory  --------------------------------------------
    TString gPath = gSystem->Getenv("VMCWORKDIR");

    // -------   Geometry file name (output)   ---------------------------------
    const TString geoDetectorName = "bbc_hexagon";
    const TString geoDetectorVersion = "v3";
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
    gGeoManager = (TGeoManager*)gROOT->FindObject("FAIRGeom");
    gGeoManager->SetName(geoDetectorName + "_geom");
    TGeoVolume* top = new TGeoVolumeAssembly("TOP");
    top->SetMedium(pMedAir);
    gGeoManager->SetTopVolume(top);
    //gGeoMan->SetTopVisible(1);
    // --------------------------------------------------------------------------------

    //--------------------- global mother volumes -------------------------------------------

    TString bbc_common_name1 = "BBC_common1";
    TString bbc_common_name2 = "BBC_common2";

    TGeoTube *bbc_tube = new TGeoTube("BBC_common_0", BBC_mother_vol_inner_radius, BBC_mother_vol_outter_radius, BBC_mother_vol_thickness);

    TGeoVolume *BBC_common1 = new TGeoVolume("BBC_common1", bbc_tube);
    BBC_common1->SetMedium(pMedAir);
    BBC_common1->SetTransparency(99);

    TGeoVolume *BBC_common2 = new TGeoVolume("BBC_common2", bbc_tube);
    BBC_common2->SetMedium(pMedAir);
    BBC_common2->SetTransparency(99);

//    TGeoVolume *BBC_common2 = new TGeoVolume("BBC_common_2", bbc_tube);
//    BBC_common2->SetMedium(pMedAir);
//    BBC_common2->SetTransparency(50);

    TString bbc_common_of_tubes_name = "BBC_common_of_tubes";
    TGeoVolume *BBC_common_of_tubes = new TGeoVolumeAssembly(bbc_common_of_tubes_name);
    BBC_common_of_tubes->SetMedium(pMedAir);


    //---------------------------------------------------------------------------------------

    Int_t A_side = 1;
    Int_t B_side = 2;
    for (Int_t count = 1; count < 7; count++)
    {
        ring(BBC_common1, A_side, count);
    }

    for (Int_t count = 1; count < 7; count++)
    {
        ring(BBC_common2, B_side, count);
    }

    TGeoTranslation * position_of_tube = new TGeoTranslation(BBC_Xpos, BBC_Ypos, BBC_center_Zpos);

    TGeoCombiTrans *position_of_tube2 = new TGeoCombiTrans(*position_of_tube);
    position_of_tube2->ReflectZ(true);

    BBC_common_of_tubes->AddNode(BBC_common1, 1, position_of_tube);
    BBC_common_of_tubes->AddNode(BBC_common2, 2, position_of_tube2);

    top->AddNode(BBC_common_of_tubes, 0);

    top->SetVisContainers(kTRUE);

    // ---------------   Finish   ----------------------------------------------
    gGeoManager->CloseGeometry();
    gGeoManager->CheckOverlaps(0.001);
    gGeoManager->PrintOverlaps();

    gGeoManager->Test();

    TFile* geoFile = new TFile(geoFileName, "RECREATE");
    top->Write();
    geoFile->Close();

    //TString gdmlname = gPath + "/geometry/tpc_v7.gdml";
    //gGeoManager->Export(gdmlname);

    top->Draw("ogl");
    TGLViewer *v = (TGLViewer*)gPad->GetViewer3D();
    v->SetStyle(TGLRnrCtx::kOutline);
    top->Draw("");

}

void ring(TGeoVolume* mother_volume, Int_t A_or_B_side, Int_t nring)
{
    Double_t Xaxid = 0.;
    Double_t Yaxid = 0.;
    Double_t Zaxid = 0.;


    Int_t cell_count;
    if(nring==1) cell_count =12;
    if(nring==2) cell_count =18;
    if(nring==3) cell_count =24;
    if(nring==4) cell_count =30;
    if(nring==5) cell_count =36;
    if(nring==6) cell_count =42;

    Int_t side_offset;
    if(A_or_B_side == 1) {side_offset = 0;}
    if(A_or_B_side == 2) {side_offset = 162;}
    Int_t smeschenie;
    if(nring==1) smeschenie =0;
    if(nring==2) smeschenie =12;
    if(nring==3) smeschenie =30;
    if(nring==4) smeschenie =54;
    if(nring==5) smeschenie =84;
    if(nring==6) smeschenie =120;


    for(Int_t cell_number = 0; cell_number < cell_count; cell_number++)
    {
        Int_t modNb = 1;
        modNb += side_offset + smeschenie + cell_number;

//        cout<< "modNb = " << modNb << endl;

        TString hexagons_sensA_name = Form("Hexagon_cell_%d_%d_%d", A_or_B_side, nring, cell_number+1);

//        cout << "resl name   " << hexagons_sensA_name << endl;

        TGeoPgon *hex=new TGeoPgon(0,360,6,2);
        hex->DefineSection(0,-hex_thikness/2, 0, hex_radius_small);//1.611*cm);
        hex->DefineSection(1, hex_thikness/2, 0, hex_radius_small);//1.611*cm);

        TGeoVolume *hex_vol = new TGeoVolume (hexagons_sensA_name, hex);
        hex_vol->SetMedium(pMedBMDFscScint);
        hex_vol->SetTransparency(0);

        if (nring==1)
        {
            hex_vol->SetLineColor(kYellow);

            if (cell_number<3 )
            {
                Xaxid =- 1.5*cell_number * hex_radius_big;
                Yaxid = (4 - cell_number) * hex_radius_small;
            }
            if (cell_number == 3)
            {
                Xaxid = -3 * hex_radius_big;
                Yaxid = 0.;
            }
            if (cell_number == 9)
            {
                Xaxid = 3 * hex_radius_big;
                Yaxid = 0.;
            }
            if (cell_number > 3 && cell_number < 7)
            {
                Xaxid = -(3 - 1.5*(cell_number-4))*hex_radius_big;
                Yaxid = -(cell_number-2) * hex_radius_small;
            }
            if (cell_number > 6 && cell_number < 9)
            {
                Xaxid = -(3 - 1.5*(cell_number-4))*hex_radius_big;
                Yaxid = -(4-(cell_number-6)) * hex_radius_small;
            }
            if (cell_number > 9)
            {
                Xaxid = (3 - 1.5*(cell_number-10))*hex_radius_big;
                Yaxid = (2 + (cell_number-10)) * hex_radius_small;
            }
        }

        if (nring==2)
        {
            hex_vol->SetLineColor(kGreen);

            if (cell_number<4 )
            {
                Xaxid =- 1.5*cell_number * hex_radius_big;
                Yaxid = (6 - cell_number) * hex_radius_small;
            }
            if (cell_number == 4 || cell_number == 5)
            {
                Xaxid = -4.5 * hex_radius_big;
                Yaxid = (4.5-cell_number)*2*hex_radius_small;
            }
            if (cell_number == 13 || cell_number == 14)
            {
                Xaxid = 4.5 * hex_radius_big;;
                Yaxid = -(13.5-cell_number)*2*hex_radius_small;
            }
            if (cell_number > 5 && cell_number < 10)
            {
                Xaxid = -(4.5 - 1.5*(cell_number-6))*hex_radius_big;
                Yaxid = -(3 + (cell_number-6))*hex_radius_small;
            }
            if (cell_number > 9 && cell_number < 13)
            {
                Xaxid = -(3 - 1.5*(cell_number-7))*hex_radius_big;
                Yaxid = -(6-(cell_number-9)) * hex_radius_small;
            }
            if (cell_number > 14)
            {
                Xaxid = (6-1.5*(cell_number-14))*hex_radius_big;
                Yaxid = (3 + (cell_number-15)) * hex_radius_small;
            }
        }

        if (nring==3)
        {
            hex_vol->SetLineColor(kYellow);

            if (cell_number <= 4)
            {
                Xaxid =- 1.5*cell_number * hex_radius_big;
                Yaxid = (8 - cell_number) * hex_radius_small;
            }
            if (cell_number > 4 && cell_number < 8)
            {
                Xaxid = -6*hex_radius_big;
                Yaxid = (6-cell_number)*2*hex_radius_small;
            }
            if (cell_number >16 && cell_number < 20)
            {
                Xaxid = 6*hex_radius_big;;
                Yaxid = -(18-cell_number)*2*hex_radius_small;
            }
            if (cell_number >=8 && cell_number < 13)
            {
                Xaxid = -(6 - 1.5*(cell_number-8))*hex_radius_big;
                Yaxid = -(3 + (cell_number-7))*hex_radius_small;
            }
            if (cell_number >= 13 && cell_number <= 16)
            {
                Xaxid = -(3 - 1.5*(cell_number-10))*hex_radius_big;
                Yaxid = -(7-(cell_number-13)) * hex_radius_small;
            }
            if (cell_number >= 20)
            {
                Xaxid = (6-1.5*(cell_number-20))*hex_radius_big;
                Yaxid = (3 + (cell_number-19)) * hex_radius_small;
            }
        }

        if (nring==4)
        {
            hex_vol->SetLineColor(kGreen);

            if (cell_number <= 5)
            {
                Xaxid =- 1.5*cell_number * hex_radius_big;
                Yaxid = (10 - cell_number) * hex_radius_small;
            }
            if (cell_number > 5 && cell_number < 10)
            {
                Xaxid = -7.5*hex_radius_big;
                Yaxid = (7.5-cell_number)*2*hex_radius_small;
            }
            if (cell_number >20 && cell_number < 25)
            {
                Xaxid = 7.5*hex_radius_big;;
                Yaxid = -(22.5-cell_number)*2*hex_radius_small;
            }
            if (cell_number >=10 && cell_number < 16)
            {
                Xaxid = -(7.5 - 1.5*(cell_number-10))*hex_radius_big;
                Yaxid = -(3 + (cell_number-8))*hex_radius_small;
            }
            if (cell_number >= 16 && cell_number <= 20)
            {
                Xaxid = -(3 - 1.5*(cell_number-13))*hex_radius_big;
                Yaxid = -(9-(cell_number-16)) * hex_radius_small;
            }
            if (cell_number >= 25)
            {
                Xaxid = (7.5-1.5*(cell_number-25))*hex_radius_big;
                Yaxid = (3 + (cell_number-23)) * hex_radius_small;
            }
        }

        if (nring==5)
        {
            hex_vol->SetLineColor(kYellow);

            if (cell_number <= 6)
            {
                Xaxid =- 1.5*cell_number * hex_radius_big;
                Yaxid = (12 - cell_number) * hex_radius_small;
            }
            if (cell_number > 6 && cell_number < 12)
            {
                Xaxid = -9*hex_radius_big;
                Yaxid = (9-cell_number)*2*hex_radius_small;
            }
            if (cell_number >24 && cell_number < 30)
            {
                Xaxid = 9*hex_radius_big;;
                Yaxid = -(27-cell_number)*2*hex_radius_small;
            }
            if (cell_number >=12 && cell_number < 19)
            {
                Xaxid = -(9 - 1.5*(cell_number-12))*hex_radius_big;
                Yaxid = -(3 + (cell_number-9))*hex_radius_small;
            }
            if (cell_number >= 19 && cell_number <= 24)
            {
                Xaxid = -(3 - 1.5*(cell_number-16))*hex_radius_big;
                Yaxid = -(14-(cell_number-16)) * hex_radius_small;
            }
            if (cell_number >= 30)
            {
                Xaxid = (9-1.5*(cell_number-30))*hex_radius_big;
                Yaxid = (3 + (cell_number-27)) * hex_radius_small;
            }
        }

        if (nring==6)
        {
            hex_vol->SetLineColor(kGreen);

            if (cell_number <= 7)
            {
                Xaxid =- 1.5*cell_number * hex_radius_big;
                Yaxid = (14 - cell_number) * hex_radius_small;
            }
            if (cell_number > 7 && cell_number < 14)
            {
                Xaxid = -10.5*hex_radius_big;
                Yaxid = (10.5-cell_number)*2*hex_radius_small;
            }
            if (cell_number >28 && cell_number < 35)
            {
                Xaxid = 10.5*hex_radius_big;;
                Yaxid = -(31.5-cell_number)*2*hex_radius_small;
            }
            if (cell_number >=14 && cell_number < 22)
            {
                Xaxid = -(10.5 - 1.5*(cell_number-14))*hex_radius_big;
                Yaxid = -(3 + (cell_number-10))*hex_radius_small;
            }
            if (cell_number >= 22 && cell_number <= 28)
            {
                Xaxid = -(3 - 1.5*(cell_number-19))*hex_radius_big;
                Yaxid = -(15-(cell_number-20)) * hex_radius_small;
            }
            if (cell_number >= 35)
            {
                Xaxid = (10.5-1.5*(cell_number-35))*hex_radius_big;
                Yaxid = (3 + (cell_number-31)) * hex_radius_small;
            }
        }


        TGeoTranslation *hex_position = new TGeoTranslation(Xaxid, Yaxid, Zaxid);
        mother_volume->AddNode(hex_vol,modNb,hex_position);




    }

}


void DefineRequiredMedia(FairGeoMedia* geoMedia, FairGeoBuilder* geoBuild)
{

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
