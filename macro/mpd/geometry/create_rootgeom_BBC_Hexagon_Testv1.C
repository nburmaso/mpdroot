//Author: Pedro Gonzalez Zamora, pedro.gonzalez.zamora@cern.ch
//-----------------------------------------------------
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"
#include "TFile.h"

#include "/opt/mpdroot/macro/mpd/mpdloadlibs.C"
//#include "TPC_geom_par.h" NOTE same for BMD

//Detector's position
//Detector's construct parameters
//Global dimensions for BMD in cm

//media
TGeoMedium *medBMDCAlu;
TGeoMedium *medBMDCCar;
TGeoMedium *medBMDCSci;
TGeoMedium *medBMDFscScint;
TGeoMedium *medBMDPlusSci; 
TGeoMedium *medAir;
TGeoMedium  *medBMDvacumm;
TGeoMedium *pMedTPCmixture;




class FairGeoMedia;
class FairGeoBuilder;

void DefineRequiredMedia(FairGeoMedia* geoMedia, FairGeoBuilder* geoBuild) {
  
  
    FairGeoMedium* mAir = geoMedia->getMedium("air");
    if ( ! mAir ) Fatal("Main", "FairMedium air not found");
    geoBuild->createMedium(mAir);
    medAir = gGeoManager->GetMedium("air");
    if ( ! medAir ) Fatal("Main", "Medium air not found");
  
      //aluminium medium
    FairGeoMedium* mAluminium = geoMedia->getMedium("aluminium");
    if ( ! mAluminium  ) Fatal("Main", "FairMedium aluminium not found");
    geoBuild->createMedium(mAluminium);
    medBMDCAlu  = gGeoManager->GetMedium("aluminium");
    if ( ! medBMDCAlu  ) Fatal("Main", "Medium aluminium not found");
    
    //carbon medium
    
    FairGeoMedium* mCarbon = geoMedia->getMedium("carbon");
    if ( ! mCarbon  ) Fatal("Main", "FairMedium carbon not found");
    geoBuild->createMedium(mCarbon);
    medBMDCCar  = gGeoManager->GetMedium("carbon");
    if ( ! medBMDCCar  ) Fatal("Main", "Medium carbon not found");
    
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
    medBMDFscScint = gGeoManager->GetMedium("FscScint");
    if( !medBMDFscScint ) Fatal("Main", "Medium FscScint not found");
    
    FairGeoMedium* mvacuum   = geoMedia->getMedium("vacuum");
    if( !mvacuum ) Fatal("Main","Medium vacuum not found");
    geoBuild->createMedium(mvacuum);
    medBMDvacumm = gGeoManager->GetMedium("vacuum");
    if( !medBMDvacumm ) Fatal("Main", "Medium vacuum not found");
    cout<<"Definio vacum"<<endl;
    
    FairGeoMedium* mTPCmixture = geoMedia->getMedium("TPCmixture");
    if ( ! mTPCmixture ) Fatal("Main", "FairMedium TPCmixture not found");
    geoBuild->createMedium(mTPCmixture);
    pMedTPCmixture = gGeoManager->GetMedium("TPCmixture");
    if ( ! pMedTPCmixture ) Fatal("Main", "Medium TPCmixture not found");
    
}

Int_t GetLayerIDOffset (TString side = "A", Int_t nRing = 0 ) {
 
   
    Int_t   offSet = 0;
    
    if( side.CompareTo("A") == 0 ) {
        
        if( nRing == 2  ) offSet = 6;
        if( nRing == 3  ) offSet = 18;
        if( nRing == 4  ) offSet = 36;
        if( nRing == 5  ) offSet = 60;
        if( nRing == 6  ) offSet = 90;
        if( nRing == 7  ) offSet = 126;
        
    } else if ( side.CompareTo("C") == 0) {
     
        if( nRing == 1  ) offSet = 168 ;
        if( nRing == 2  ) offSet = 168 + 6;
        if( nRing == 3  ) offSet = 168 + 18;
        if( nRing == 4  ) offSet = 168 + 36;
        if( nRing == 5  ) offSet = 168 + 60;
        if( nRing == 6  ) offSet = 168 + 90;
        if( nRing == 7  ) offSet = 168 + 126;
        
    }
    

    return offSet;

}


void create_rootgeom_BBC_Hexagon_Testv1(TString mediumBMD="NDetScin")
{
    // Load necessary libraries
    //gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(); // load libraries
    

    
    //---Units: using centimeters as base, i.e. 1 position unit=1 centimeter
    Double_t cm=1;//,micron=1e-4;
    Double_t AdjustX=0*cm,AdjustY=0*cm;
    //Int_t ntooth = 5;
    Double_t xplate = 2.5;
    //Double_t yplate = 50;
    Double_t xtooth = .01;
    
    // Double_t ytooth = 0.5*yplate/ntooth;
    Double_t dshiftx  = 0.4;
    //Double_t dshifty =  2.*xplate + xtooth;
    
    Double_t dshifty =  2.*xplate;
    
    Double_t ytooth = 0.;
     
    
    
    // Double_t xt,yt;
    

    // ----  set working directory  --------------------------------------------
    TString gPath = gSystem->Getenv("VMCWORKDIR");
    // -------   Geometry file name (output)   ---------------------------------
    const TString geoDetectorName = "bbc_hex";
    const TString geoDetectorVersion = "v1";
    const TString geoDetectorMedium  = mediumBMD;
    TString geoFileName = gPath + "/geometry/" + geoDetectorName + "_"+ geoDetectorMedium + "_"+geoDetectorVersion + ".root";
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
    gGeoManager->SetName(geoDetectorName + "_geom");
    TGeoVolume* top = new TGeoVolumeAssembly("TOP");
    top->SetMedium(medAir); 
    gGeoManager->SetTopVolume(top);
    
    
    ///////////////////////////////////////////////////////////////////////////
  // BMDPlus

    Int_t kBMDPlusColorSci   = kGreen;//5;
    
    TGeoMedium *medBMDDetector = 0x0;
    
    
    if(  mediumBMD.EqualTo("NDetScin") ) {
      
	medBMDDetector = medBMDCSci;
        	      
    } else if ( mediumBMD.EqualTo("FscScint") ){
      
	medBMDDetector = medBMDFscScint;
	
    } else if ( mediumBMD.EqualTo("air") ){
      
	medBMDDetector = medAir;
	
    } else if ( mediumBMD.EqualTo("vacuum") ){
      
      medBMDDetector = medBMDvacumm;
      
    }
    else {
      Fatal("Main","Medium not defined");
      return;
    }
    
    if( ! medBMDDetector ){
      
      Fatal("Main", Form("Medium %s not found",mediumBMD.Data()));
      return;
    }
   
    
    
    



    TGeoPgon *hex=new TGeoPgon(0,360,6,2);
    hex->DefineSection(0,-1*cm, 0, 5*cm);//1.611*cm);
    hex->DefineSection(1, 1*cm, 0, 5*cm);//1.611*cm);


   
   
   TGeoVolume *rowA0 = new TGeoVolumeAssembly("rowA0");
   TGeoVolume *rowC0 = new TGeoVolumeAssembly("rowC0");
   
   Int_t ncells = 15;
   Int_t cellId = 31;
   Int_t nring  = 7;
   Int_t nodeId = 1;
   
   Int_t layerIDA = -1;
   Int_t layerIDC = -1;
   Int_t layerOffsetA = 0;
   Int_t layerOffsetC = 0;
   
   
   TString     cellsName[400];
   Double_t    cellsPosXYZ[400][3];
   Int_t       offsetSideC = 86;
   
   
   
   
   for(Int_t i=0; i<ncells; i++) {
       
      Double_t ycell = (2*i)*(dshifty);
      
       
      
      if( i > 0 ) ycell+=ytooth;
      
       
      if( i != 7 ) {
      
        TString cellAIdName = Form("HexagonA%d_%d",nring,cellId);
        TString cellCIdName = Form("HexagonC%d_%d",nring,cellId);
        layerOffsetA = GetLayerIDOffset("A",nring);
        layerOffsetC = GetLayerIDOffset("C",nring);
        layerIDA = layerOffsetA + cellId + 1;
        layerIDC = layerOffsetC + cellId + 1; 
        
        cellsName[layerIDA] = "rowA0";
        cellsName[layerIDC] = "rowC0";
        
        cellsPosXYZ[layerIDA][0] = 0;  cellsPosXYZ[layerIDA][1] = ycell; cellsPosXYZ[layerIDA][2]  = 0;
        cellsPosXYZ[layerIDC][0] = 0;  cellsPosXYZ[layerIDC][1] = ycell; cellsPosXYZ[layerIDC][2]  = 0;
        
          
          
          
          if( i < ( (ncells-1) / 2 ) ) {
              
              if ( i%2 == 1 ) cellId-=5;
              else cellId-=4;
              nring--;
              
          } else {
              
              if ( i%2 == 1 ) cellId+=1;
              else cellId+=2;
              nring++;
          }
          
        TGeoVolume* cellA = new TGeoVolume(cellAIdName.Data(),hex,medBMDDetector); 
        TGeoVolume* cellC = new TGeoVolume(cellCIdName.Data(),hex,medBMDDetector);
        
        cout<<cellAIdName.Data()<<" "<<layerIDA<<" "<<cellCIdName.Data()<<" "<<layerIDC<<endl;
    
        if( i%2 == 0 ) {
            cellA->SetLineColor(kYellow);
            cellC->SetLineColor(kYellow);
        }
        else {
            cellA->SetLineColor(kGreen);
            cellC->SetLineColor(kGreen);
        }
        rowA0->AddNode(cellA, nodeId, new TGeoTranslation(0, ycell, 0));
        rowC0->AddNode(cellC, nodeId, new TGeoTranslation(0, ycell, 0));
        nodeId++;
        
     } else {
         
            nring  = 1;
            cellId = 1;
        }
    }
   
   
   cout<<"///////////////////////////////////////////"<<endl;
   
   
   TGeoVolume *rowAR1 = new TGeoVolumeAssembly("rowAR1");
   TGeoVolume *rowAL1 = new TGeoVolumeAssembly("rowAL1");
   TGeoVolume *rowCR1 = new TGeoVolumeAssembly("rowCR1");
   TGeoVolume *rowCL1 = new TGeoVolumeAssembly("rowCL1");
   
   ncells        = 14;
   Int_t cellIdR = 32;
   Int_t cellIdL = 30;
   nring         = 7;
   nodeId        = 1;
   
   Int_t layerIDAL = -1;
   Int_t layerIDCL = -1;
   Int_t layerIDAR = -1;
   Int_t layerIDCR = -1;
   
   
   
   for (Int_t i=0; i<ncells; i++) {
       
        Double_t ycell = (2*i)*(dshifty);
         if( i > 0 ) ycell+=ytooth;
      
       
        
        TString cellARIdName = Form("HexagonA%d_%d",nring,cellIdR);
        TString cellALIdName = Form("HexagonA%d_%d",nring,cellIdL);
        TString cellCRIdName = Form("HexagonC%d_%d",nring,cellIdR);
        TString cellCLIdName = Form("HexagonC%d_%d",nring,cellIdL);
        
        TGeoVolume* cellAR = new TGeoVolume(cellARIdName.Data(),hex,medBMDDetector); 
        TGeoVolume* cellAL = new TGeoVolume(cellALIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCR = new TGeoVolume(cellCRIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCL = new TGeoVolume(cellCLIdName.Data(),hex,medBMDDetector);
        
        layerOffsetA = GetLayerIDOffset("A",nring);
        layerOffsetC = GetLayerIDOffset("C",nring);
        
        
        layerIDAR = layerOffsetA + cellIdR + 1;
        layerIDCR = layerOffsetC + cellIdR + 1; 
        layerIDAL = layerOffsetA + cellIdL + 1;
        layerIDCL = layerOffsetC + cellIdL + 1; 
        
         
        cellsName[layerIDAR] = "rowAR1";
        cellsName[layerIDCR] = "rowCR1";
        cellsPosXYZ[layerIDAR][0] = 0; cellsPosXYZ[layerIDAR][1] = ycell; cellsPosXYZ[layerIDAR][2] = 0;
        cellsPosXYZ[layerIDCR][0] = 0; cellsPosXYZ[layerIDCR][1] = ycell; cellsPosXYZ[layerIDCR][2] = 0;
        cellsName[layerIDAL] = "rowAL1";
        cellsName[layerIDCL] = "rowCL1";
        cellsPosXYZ[layerIDAL][0] = 0;  cellsPosXYZ[layerIDAL][1] = ycell; cellsPosXYZ[layerIDAL][2] = 0;
        cellsPosXYZ[layerIDCL][0] = 0;  cellsPosXYZ[layerIDCL][1] = ycell; cellsPosXYZ[layerIDCL][2] = 0;
        
        cout<<cellARIdName.Data()<<" "<<layerIDAR<<" "<<cellCRIdName.Data()<<" "<<layerIDCR<<endl;
        cout<<cellALIdName.Data()<<" "<<layerIDAL<<" "<<cellCLIdName.Data()<<" "<<layerIDCL<<endl;
        
        
        
      
        if( i > 4 && i < 7) {
   
        if( i > 5 ) {  
            cellAR->SetLineColor(kYellow);
            cellAL->SetLineColor(kYellow);
            cellCR->SetLineColor(kYellow);
            cellCL->SetLineColor(kYellow);
        } else {
            cellAR->SetLineColor(kGreen);
            cellAL->SetLineColor(kGreen);
            cellCR->SetLineColor(kGreen);
            cellCL->SetLineColor(kGreen);
        }
        
        nring = 1;
        
            if( i == 5) {
                cellIdR = 5;
                cellIdL = 3;
            }
            else if ( i == 6) {
                cellIdR = 0;
                cellIdL = 2;
            }
              
        } else {
          if( i <= 4 ) {
            if( i%2 == 0 ) {
                cellAR->SetLineColor(kYellow);
                cellAL->SetLineColor(kYellow);
                cellCR->SetLineColor(kYellow);
                cellCL->SetLineColor(kYellow);
                cellIdR-=4;
                cellIdL-=4;
            }
            else {
                cellAR->SetLineColor(kGreen);
                cellAL->SetLineColor(kGreen);
                cellCR->SetLineColor(kGreen);
                cellCL->SetLineColor(kGreen);
                cellIdR-=5;
                cellIdL-=5;
            }
            nring--;
          } else {
            if( i%2 == 0 ) {
                cellAR->SetLineColor(kGreen);
                cellAL->SetLineColor(kGreen);
                cellCR->SetLineColor(kGreen);
                cellCL->SetLineColor(kGreen);
                cellIdR+=1;
                cellIdL+=1;
            }
            else { 
                cellAR->SetLineColor(kYellow);
                cellAL->SetLineColor(kYellow);
                cellCR->SetLineColor(kYellow);
                cellCL->SetLineColor(kYellow);
                cellIdR+=2;
                cellIdL+=2;
            }
            nring++;
          }
      }
          
      rowAR1->AddNode(cellAR, nodeId, new TGeoTranslation(0,ycell,0));
      rowAL1->AddNode(cellAL, nodeId, new TGeoTranslation(0,ycell,0));
      rowCR1->AddNode(cellCR, nodeId, new TGeoTranslation(0,ycell,0));
      rowCL1->AddNode(cellCL, nodeId, new TGeoTranslation(0,ycell,0));
      nodeId++;
    
   }
    
   cout<<"////////////////////////////////////////////"<<endl;
   
   TGeoVolume *rowAR2 = new TGeoVolumeAssembly("rowAR2");
   TGeoVolume *rowAL2 = new TGeoVolumeAssembly("rowAL2");
   TGeoVolume *rowCR2 = new TGeoVolumeAssembly("rowCR2");
   TGeoVolume *rowCL2 = new TGeoVolumeAssembly("rowCL2");
   
   
   ncells   = 13;
   cellIdR  = 33;
   cellIdL  = 29;
   nring    = 7;
   nodeId   = 1;
    
    for (Int_t i=0; i<ncells; i++) {
        
        Double_t ycell = (2*i)*(dshifty);
         if( i > 0 ) ycell+=ytooth;
      
        
        TString cellARIdName = Form("HexagonA%d_%d",nring,cellIdR);
        TString cellALIdName = Form("HexagonA%d_%d",nring,cellIdL);
        TString cellCRIdName = Form("HexagonC%d_%d",nring,cellIdR);
        TString cellCLIdName = Form("HexagonC%d_%d",nring,cellIdL);
            
        TGeoVolume* cellAR = new TGeoVolume(cellARIdName.Data(),hex,medBMDDetector); 
        TGeoVolume* cellAL = new TGeoVolume(cellALIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCR = new TGeoVolume(cellCRIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCL = new TGeoVolume(cellCLIdName.Data(),hex,medBMDDetector);
        
        
        layerOffsetA = GetLayerIDOffset("A",nring);
        layerOffsetC = GetLayerIDOffset("C",nring);
        
        
        layerIDAR = layerOffsetA + cellIdR + 1;
        layerIDCR = layerOffsetC + cellIdR + 1; 
        layerIDAL = layerOffsetA + cellIdL + 1;
        layerIDCL = layerOffsetC + cellIdL + 1; 
        
         
        cellsName[layerIDAR] = "rowAR2";
        cellsName[layerIDCR] = "rowCR2";
        cellsPosXYZ[layerIDAR][0] = 0; cellsPosXYZ[layerIDAR][1] = ycell; cellsPosXYZ[layerIDAR][2] = 0;
        cellsPosXYZ[layerIDCR][0] = 0; cellsPosXYZ[layerIDCR][1] = ycell; cellsPosXYZ[layerIDCR][2] = 0;
        cellsName[layerIDAL] = "rowAL2";
        cellsName[layerIDCL] = "rowCL2";
        cellsPosXYZ[layerIDAL][0] = 0;  cellsPosXYZ[layerIDAL][1] = ycell; cellsPosXYZ[layerIDAL][2] = 0;
        cellsPosXYZ[layerIDCL][0] = 0;  cellsPosXYZ[layerIDCL][1] = ycell; cellsPosXYZ[layerIDCL][2] = 0;
        
        cout<<cellARIdName.Data()<<" "<<layerIDAR<<" "<<cellCRIdName.Data()<<" "<<layerIDCR<<endl;
        cout<<cellALIdName.Data()<<" "<<layerIDAL<<" "<<cellCLIdName.Data()<<" "<<layerIDCL<<endl;
        
        
        
        
    
        
        
        
    if( i > 3 && i < 7) {
        
        cellAR->SetLineColor(kGreen);
        cellAL->SetLineColor(kGreen);
        cellCR->SetLineColor(kGreen);
        cellCL->SetLineColor(kGreen);
   
        if( i  == 4 ) {
        cellAR->SetLineColor(kYellow);
        cellAL->SetLineColor(kYellow);
        cellCR->SetLineColor(kYellow);
        cellCL->SetLineColor(kYellow);
        }
        
        nring = 2;
        if( i == 4 ) {
            cellIdR = 11;
            cellIdL = 7;
        }
        if( i == 5 ) {
            cellIdR = 0;
            cellIdL = 6;
        }
        if( i == 6 ) {
            
            cellIdR = 1;
            cellIdL = 5;
            
        }

    } else {
          if( i <= 3 ) {
            if( i%2 == 0 ) {
                cellAR->SetLineColor(kYellow);
                cellAL->SetLineColor(kYellow);
                cellCR->SetLineColor(kYellow);
                cellCL->SetLineColor(kYellow);
                cellIdR-=4;
                cellIdL-=4;
            }
            else {
                cellAR->SetLineColor(kGreen);
                cellAL->SetLineColor(kGreen);
                cellCR->SetLineColor(kGreen);
                cellCL->SetLineColor(kGreen);
                cellIdR-=5;
                cellIdL-=5;
            }
            nring--;
          } else {
            if( i%2 == 0 ) {
                cellAR->SetLineColor(kYellow);
                cellAL->SetLineColor(kYellow);
                cellCR->SetLineColor(kYellow);
                cellCL->SetLineColor(kYellow);
                cellIdR+=2;
                cellIdL+=2;
            }
            else {
                cellAR->SetLineColor(kGreen);
                cellAL->SetLineColor(kGreen);
                cellCR->SetLineColor(kGreen);
                cellCL->SetLineColor(kGreen);
                cellIdR+=1;
                cellIdL+=1;
            }
            nring++;
          }
      }
      rowAR2->AddNode(cellAR, nodeId, new TGeoTranslation(0, ycell,0));
      rowAL2->AddNode(cellAL, nodeId, new TGeoTranslation(0, ycell,0));
      rowCR2->AddNode(cellCR, nodeId, new TGeoTranslation(0, ycell,0));
      rowCL2->AddNode(cellCL, nodeId, new TGeoTranslation(0, ycell,0));
      nodeId++;
    }
   
   
   cout<<"//////////////////////////////////////////////////"<<endl;
   TGeoVolume *rowAR3 = new TGeoVolumeAssembly("rowAR3");
   TGeoVolume *rowAL3 = new TGeoVolumeAssembly("rowAL3");
   TGeoVolume *rowCR3 = new TGeoVolumeAssembly("rowCR3");
   TGeoVolume *rowCL3 = new TGeoVolumeAssembly("rowCL3");
   
   ncells   = 12;
   cellIdR  = 34;
   cellIdL  = 28;
   nring    = 7;
   nodeId   = 1;
   
   
   for (Int_t i=0; i<ncells; i++) {
      Double_t ycell = (2*i)*(dshifty);
       if( i > 0 ) ycell+=ytooth;
      
      
        TString cellARIdName = Form("HexagonA%d_%d",nring,cellIdR);
        TString cellALIdName = Form("HexagonA%d_%d",nring,cellIdL);
        TString cellCRIdName = Form("HexagonC%d_%d",nring,cellIdR);
        TString cellCLIdName = Form("HexagonC%d_%d",nring,cellIdL);
            
        TGeoVolume* cellAR = new TGeoVolume(cellARIdName.Data(),hex,medBMDDetector); 
        TGeoVolume* cellAL = new TGeoVolume(cellALIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCR = new TGeoVolume(cellCRIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCL = new TGeoVolume(cellCLIdName.Data(),hex,medBMDDetector);
        
        
        
        layerOffsetA = GetLayerIDOffset("A",nring);
        layerOffsetC = GetLayerIDOffset("C",nring);
        
        
        layerIDAR = layerOffsetA + cellIdR + 1;
        layerIDCR = layerOffsetC + cellIdR + 1; 
        layerIDAL = layerOffsetA + cellIdL + 1;
        layerIDCL = layerOffsetC + cellIdL + 1; 
        
         
        cellsName[layerIDAR] = "rowAR3";
        cellsName[layerIDCR] = "rowCR3";
        cellsPosXYZ[layerIDAR][0] = 0; cellsPosXYZ[layerIDAR][1] = ycell; cellsPosXYZ[layerIDAR][2] = 0;
        cellsPosXYZ[layerIDCR][0] = 0; cellsPosXYZ[layerIDCR][1] = ycell; cellsPosXYZ[layerIDCR][2] = 0;
        cellsName[layerIDAL] = "rowAL3";
        cellsName[layerIDCL] = "rowCL3";
        cellsPosXYZ[layerIDAL][0] = 0;  cellsPosXYZ[layerIDAL][1] = ycell; cellsPosXYZ[layerIDAL][2] = 0;
        cellsPosXYZ[layerIDCL][0] = 0;  cellsPosXYZ[layerIDCL][1] = ycell; cellsPosXYZ[layerIDCL][2] = 0;
        
        cout<<cellARIdName.Data()<<" "<<layerIDAR<<" "<<cellCRIdName.Data()<<" "<<layerIDCR<<endl;
        cout<<cellALIdName.Data()<<" "<<layerIDAL<<" "<<cellCLIdName.Data()<<" "<<layerIDCL<<endl;
        
        
        
       
       if( i > 2 && i < 7) {
   
        cellAR->SetLineColor(kYellow);
        cellAL->SetLineColor(kYellow);
        cellCR->SetLineColor(kYellow);
        cellCL->SetLineColor(kYellow);
        
        nring=3; 
        
        if( i == 3 ) {
            cellIdR=16;
            cellIdL= 10;
            
            cellAR->SetLineColor(kGreen);
            cellAL->SetLineColor(kGreen);
            cellCR->SetLineColor(kGreen);
            cellCL->SetLineColor(kGreen);
        }
        if( i == 4 ) {
            cellIdR=17;
            cellIdL=9;
        }
        if( i == 5 ) {
            cellIdR=0;
            cellIdL=8;
        }
        if( i == 6 ) {
            
            cellIdR=1;
            cellIdL=7;
            
        }
    
      
      } else {
          if( i <= 2 ) {
            if( i%2 == 0 ) {
                cellAR->SetLineColor(kYellow);
                cellAL->SetLineColor(kYellow);
                cellCR->SetLineColor(kYellow);
                cellCL->SetLineColor(kYellow);
                cellIdR-=4;
                cellIdL-=4;
            }
            else {
                cellAR->SetLineColor(kGreen);
                cellAL->SetLineColor(kGreen);
                cellCR->SetLineColor(kGreen);
                cellCL->SetLineColor(kGreen);
                cellIdR-=5;
                cellIdL-=5;
            }
            nring--;
          } else {
            if( i%2 == 0 ) {
                cellAR->SetLineColor(kGreen);
                cellAL->SetLineColor(kGreen);
                cellCR->SetLineColor(kGreen);
                cellCL->SetLineColor(kGreen);
                cellIdR+=1;
                cellIdL+=1;
            }
            else {
                cellAR->SetLineColor(kYellow);
                cellAL->SetLineColor(kYellow);
                cellCR->SetLineColor(kYellow);
                cellCL->SetLineColor(kYellow);
                cellIdR+=2;
                cellIdL+=2;
            }
            nring++;
            
          }
      }
      
      rowAR3->AddNode(cellAR, nodeId, new TGeoTranslation(0, ycell,0));
      rowAL3->AddNode(cellAL, nodeId, new TGeoTranslation(0, ycell,0));
      rowCR3->AddNode(cellCR, nodeId, new TGeoTranslation(0, ycell,0));
      rowCL3->AddNode(cellCL, nodeId, new TGeoTranslation(0, ycell,0));
      nodeId++;
   }
   
   
   
   cout<<"//////////////////////////////////////////////////"<<endl;
   TGeoVolume *rowAR4 = new TGeoVolumeAssembly("rowAR4");
   TGeoVolume *rowAL4 = new TGeoVolumeAssembly("rowAL4");
   TGeoVolume *rowCR4 = new TGeoVolumeAssembly("rowCR4");
   TGeoVolume *rowCL4 = new TGeoVolumeAssembly("rowCL4");
   
   ncells  = 11;
   cellIdR = 35;
   cellIdL = 27;
   nring   = 7;
   nodeId  = 1;
   
   
   for (Int_t i=0; i<ncells; i++) {
      Double_t ycell = (2*i)*(dshifty);
      
       if( i > 0 ) ycell+=ytooth;
      
      
       
        TString cellARIdName = Form("HexagonA%d_%d",nring,cellIdR);
        TString cellALIdName = Form("HexagonA%d_%d",nring,cellIdL);
        TString cellCRIdName = Form("HexagonC%d_%d",nring,cellIdR);
        TString cellCLIdName = Form("HexagonC%d_%d",nring,cellIdL);
            
        TGeoVolume* cellAR = new TGeoVolume(cellARIdName.Data(),hex,medBMDDetector); 
        TGeoVolume* cellAL = new TGeoVolume(cellALIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCR = new TGeoVolume(cellCRIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCL = new TGeoVolume(cellCLIdName.Data(),hex,medBMDDetector);
        
        
        layerOffsetA = GetLayerIDOffset("A",nring);
        layerOffsetC = GetLayerIDOffset("C",nring);
        
        
        layerIDAR = layerOffsetA + cellIdR + 1;
        layerIDCR = layerOffsetC + cellIdR + 1; 
        layerIDAL = layerOffsetA + cellIdL + 1;
        layerIDCL = layerOffsetC + cellIdL + 1; 
        
         
        cellsName[layerIDAR] = "rowAR4";
        cellsName[layerIDCR] = "rowCR4";
        cellsPosXYZ[layerIDAR][0] = 0; cellsPosXYZ[layerIDAR][1] = ycell; cellsPosXYZ[layerIDAR][2] = 0;
        cellsPosXYZ[layerIDCR][0] = 0; cellsPosXYZ[layerIDCR][1] = ycell; cellsPosXYZ[layerIDCR][2] = 0;
        cellsName[layerIDAL] = "rowAL4";
        cellsName[layerIDCL] = "rowCL4";
        cellsPosXYZ[layerIDAL][0] = 0;  cellsPosXYZ[layerIDAL][1] = ycell; cellsPosXYZ[layerIDAL][2] = 0;
        cellsPosXYZ[layerIDCL][0] = 0;  cellsPosXYZ[layerIDCL][1] = ycell; cellsPosXYZ[layerIDCL][2] = 0;
        
        cout<<cellARIdName.Data()<<" "<<layerIDAR<<" "<<cellCRIdName.Data()<<" "<<layerIDCR<<endl;
        cout<<cellALIdName.Data()<<" "<<layerIDAL<<" "<<cellCLIdName.Data()<<" "<<layerIDCL<<endl;
        
     
        
      
      if( i > 1 && i < 7) {
   
          cellAR->SetLineColor(kGreen);
          cellAL->SetLineColor(kGreen);
          cellCR->SetLineColor(kGreen);
          cellCL->SetLineColor(kGreen);
          
          nring=4;
          
          if( i == 2 ) {
              cellIdR=22;
              cellIdL=14;
              cellAR->SetLineColor(kYellow);
              cellAL->SetLineColor(kYellow);
              cellCR->SetLineColor(kYellow);
              cellCL->SetLineColor(kYellow);
          }
          if( i == 3 ) {
              cellIdR=23;
              cellIdL=13;
          }
          if( i == 4 ) {
              cellIdR=0;
              cellIdL=12;
          }
          if( i == 5 ) {
              cellIdR=1;
              cellIdL=11;
          }
          if( i == 6 ) {
              
              cellIdR=2;
              cellIdL=10;
          }

      } else {
          if( i <= 1 ) {
            if( i%2 == 0 ){
                cellAR->SetLineColor(kYellow);
                cellAL->SetLineColor(kYellow);
                cellCR->SetLineColor(kYellow);
                cellCL->SetLineColor(kYellow);
                cellIdR-=4;
                cellIdL-=4;
            }
            else {
                cellAR->SetLineColor(kGreen);
                cellAL->SetLineColor(kGreen);
                cellCR->SetLineColor(kGreen);
                cellCL->SetLineColor(kGreen);
                cellIdR-=5;
                cellIdL-=5;
            }
            nring--;
          } else {
            if( i%2 == 0 ) {
                cellAR->SetLineColor(kYellow);
                cellAL->SetLineColor(kYellow);
                cellCR->SetLineColor(kYellow);
                cellCL->SetLineColor(kYellow);
                cellIdR+=2;
                cellIdL+=2;
            }
            else {
                cellAR->SetLineColor(kGreen);
                cellAL->SetLineColor(kGreen);
                cellCR->SetLineColor(kGreen);
                cellCL->SetLineColor(kGreen);
                cellIdR+=1;
                cellIdL+=1;
            }
            nring++;
          }
      }
      
      rowAR4->AddNode(cellAR, nodeId, new TGeoTranslation(0, ycell,0));
      rowAL4->AddNode(cellAL, nodeId, new TGeoTranslation(0, ycell,0));
      rowCR4->AddNode(cellCR, nodeId, new TGeoTranslation(0, ycell,0));
      rowCL4->AddNode(cellCL, nodeId, new TGeoTranslation(0, ycell,0));
      nodeId++;
   }
   
   
   cout<<"//////////////////////////////////////////////////"<<endl;
   TGeoVolume *rowAR5 = new TGeoVolumeAssembly("rowAR5");
   TGeoVolume *rowAL5 = new TGeoVolumeAssembly("rowAL5");
   TGeoVolume *rowCR5 = new TGeoVolumeAssembly("rowCR5");
   TGeoVolume *rowCL5 = new TGeoVolumeAssembly("rowCL5");
   
   ncells  = 10;
   cellIdR = 36;
   cellIdL = 26;
   nring   = 7;
   nodeId  = 1;

   
   
   for (Int_t i=0; i<ncells; i++) {
      Double_t ycell = (2*i)*(dshifty);
      
       if( i > 0 ) ycell+=ytooth;
      
      
      
        TString cellARIdName = Form("HexagonA%d_%d",nring,cellIdR);
        TString cellALIdName = Form("HexagonA%d_%d",nring,cellIdL);
        TString cellCRIdName = Form("HexagonC%d_%d",nring,cellIdR);
        TString cellCLIdName = Form("HexagonC%d_%d",nring,cellIdL);
            
        TGeoVolume* cellAR = new TGeoVolume(cellARIdName.Data(),hex,medBMDDetector); 
        TGeoVolume* cellAL = new TGeoVolume(cellALIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCR = new TGeoVolume(cellCRIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCL = new TGeoVolume(cellCLIdName.Data(),hex,medBMDDetector);
        
        layerOffsetA = GetLayerIDOffset("A",nring);
        layerOffsetC = GetLayerIDOffset("C",nring);
        
        
        layerIDAR = layerOffsetA + cellIdR + 1;
        layerIDCR = layerOffsetC + cellIdR + 1; 
        layerIDAL = layerOffsetA + cellIdL + 1;
        layerIDCL = layerOffsetC + cellIdL + 1; 
        
         
        cellsName[layerIDAR] = "rowAR5";
        cellsName[layerIDCR] = "rowCR5";
        cellsPosXYZ[layerIDAR][0] = 0; cellsPosXYZ[layerIDAR][1] = ycell; cellsPosXYZ[layerIDAR][2] = 0;
        cellsPosXYZ[layerIDCR][0] = 0; cellsPosXYZ[layerIDCR][1] = ycell; cellsPosXYZ[layerIDCR][2] = 0;
        cellsName[layerIDAL] = "rowAL5";
        cellsName[layerIDCL] = "rowCL5";
        cellsPosXYZ[layerIDAL][0] = 0;  cellsPosXYZ[layerIDAL][1] = ycell; cellsPosXYZ[layerIDAL][2] = 0;
        cellsPosXYZ[layerIDCL][0] = 0;  cellsPosXYZ[layerIDCL][1] = ycell; cellsPosXYZ[layerIDCL][2] = 0;
        
        cout<<cellARIdName.Data()<<" "<<layerIDAR<<" "<<cellCRIdName.Data()<<" "<<layerIDCR<<endl;
        cout<<cellALIdName.Data()<<" "<<layerIDAL<<" "<<cellCLIdName.Data()<<" "<<layerIDCL<<endl;
     
       
        
      
      
      if( i > 0 && i < 7) {
   
        cellAR->SetLineColor(kYellow);
        cellAL->SetLineColor(kYellow);
        cellCR->SetLineColor(kYellow);
        cellCL->SetLineColor(kYellow);
        
        nring=5;
        
        if( i == 1 )
        {
            cellIdL=17;
            cellIdR=27;
            cellAR->SetLineColor(kGreen);
            cellAL->SetLineColor(kGreen);
            cellCR->SetLineColor(kGreen);
            cellCL->SetLineColor(kGreen);
        
            
            
        } else if( i != 4 ){
            cellIdR++;
            cellIdL--;
        }
        else {
            cellIdR = 0; 
            cellIdL--;
            
        }

      } else {
          if( i <= 0 ) {
            if( i%2 == 0 ) {
                cellAR->SetLineColor(kYellow);
                cellAL->SetLineColor(kYellow);
                cellCR->SetLineColor(kYellow);
                cellCL->SetLineColor(kYellow);
                cellIdR-=4;
                cellIdL-=4;
            }
            else { 
                cellAR->SetLineColor(kGreen);
                cellAL->SetLineColor(kGreen);
                cellCR->SetLineColor(kGreen);
                cellCL->SetLineColor(kGreen);
                cellIdR-=5;
                cellIdL-=5;
            }
            nring--;
          } else {
            if( i%2 == 0 ){
                cellAR->SetLineColor(kGreen);
                cellAL->SetLineColor(kGreen);
                cellCR->SetLineColor(kGreen);
                cellCL->SetLineColor(kGreen);
                cellIdR+=1;
                cellIdL+=1;
            }
            else {
                cellAR->SetLineColor(kYellow);
                cellAL->SetLineColor(kYellow);
                cellCR->SetLineColor(kYellow);
                cellCL->SetLineColor(kYellow);
                cellIdR+=2;
                cellIdL+=2;
            }
            nring++;
          }
      }
      
      rowAR5->AddNode(cellAR, nodeId, new TGeoTranslation(0, ycell, 0));
      rowAL5->AddNode(cellAL, nodeId, new TGeoTranslation(0, ycell, 0));
      rowCR5->AddNode(cellCR, nodeId, new TGeoTranslation(0, ycell, 0));
      rowCL5->AddNode(cellCL, nodeId, new TGeoTranslation(0, ycell, 0));
      nodeId++;
   }
   

   cout<<"//////////////////////////////////////////////////"<<endl;
   TGeoVolume *rowAR6 = new TGeoVolumeAssembly("rowAR6");
   TGeoVolume *rowAL6 = new TGeoVolumeAssembly("rowAL6");
   TGeoVolume *rowCR6 = new TGeoVolumeAssembly("rowCR6");
   TGeoVolume *rowCL6 = new TGeoVolumeAssembly("rowCL6");
   
   
   ncells  = 9;
   cellIdR = 37;
   cellIdL = 25;
   nring   = 7;
   nodeId  = 1;
   

   
   for (Int_t i=0; i<ncells; i++) {
      Double_t ycell = (2*i)*(dshifty);
      
       if( i > 0 ) ycell+=ytooth;
      
      
      
        TString cellARIdName = Form("HexagonA%d_%d",nring,cellIdR);
        TString cellALIdName = Form("HexagonA%d_%d",nring,cellIdL);
        TString cellCRIdName = Form("HexagonC%d_%d",nring,cellIdR);
        TString cellCLIdName = Form("HexagonC%d_%d",nring,cellIdL);
            
        TGeoVolume* cellAR = new TGeoVolume(cellARIdName.Data(),hex,medBMDDetector); 
        TGeoVolume* cellAL = new TGeoVolume(cellALIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCR = new TGeoVolume(cellCRIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCL = new TGeoVolume(cellCLIdName.Data(),hex,medBMDDetector);
     
        layerOffsetA = GetLayerIDOffset("A",nring);
        layerOffsetC = GetLayerIDOffset("C",nring);
        
        
        layerIDAR = layerOffsetA + cellIdR + 1;
        layerIDCR = layerOffsetC + cellIdR + 1; 
        layerIDAL = layerOffsetA + cellIdL + 1;
        layerIDCL = layerOffsetC + cellIdL + 1; 
        
         
        cellsName[layerIDAR] = "rowAR6";
        cellsName[layerIDCR] = "rowCR6";
        cellsPosXYZ[layerIDAR][0] = 0; cellsPosXYZ[layerIDAR][1] = ycell; cellsPosXYZ[layerIDAR][2] = 0;
        cellsPosXYZ[layerIDCR][0] = 0; cellsPosXYZ[layerIDCR][1] = ycell; cellsPosXYZ[layerIDCR][2] = 0;
        cellsName[layerIDAL] = "rowAL6";
        cellsName[layerIDCL] = "rowCL6";
        cellsPosXYZ[layerIDAL][0] = 0;  cellsPosXYZ[layerIDAL][1] = ycell; cellsPosXYZ[layerIDAL][2] = 0;
        cellsPosXYZ[layerIDCL][0] = 0;  cellsPosXYZ[layerIDCL][1] = ycell; cellsPosXYZ[layerIDCL][2] = 0;
        
        cout<<cellARIdName.Data()<<" "<<layerIDAR<<" "<<cellCRIdName.Data()<<" "<<layerIDCR<<endl;
        cout<<cellALIdName.Data()<<" "<<layerIDAL<<" "<<cellCLIdName.Data()<<" "<<layerIDCL<<endl;
      
     
     
      
      if(  i < 7) {
   
        nring=6;
        cellAR->SetLineColor(kGreen);
        cellAL->SetLineColor(kGreen);
        cellCR->SetLineColor(kGreen);
        cellCL->SetLineColor(kGreen);
        
        
        if( i == 0 ){
           cellIdR=33;
           cellIdL=21;  
           cellAR->SetLineColor(kYellow);
           cellAL->SetLineColor(kYellow);
           cellCR->SetLineColor(kYellow);
           cellCL->SetLineColor(kYellow);
            
        } else if( i!= 3 ) {
            cellIdR++;
            cellIdL--;
        }
        else {
            cellIdR=0;
            cellIdL--;
        }
      
      }else {
          
             
            Color_t color = kGreen;
            if( i == 7 ) color = kGreen;
            if( i == 8 ) color = kYellow;
            cellAR->SetLineColor(color);
            cellCR->SetLineColor(color);
            cellAL->SetLineColor(color);
            cellCL->SetLineColor(color);
              
            cellIdR+=1;
            cellIdL+=1;
            nring=7;
         
      }
      
      rowAR6->AddNode(cellAR, nodeId, new TGeoTranslation(0, ycell,0));
      rowAL6->AddNode(cellAL, nodeId, new TGeoTranslation(0, ycell,0));
      rowCR6->AddNode(cellCR, nodeId, new TGeoTranslation(0, ycell,0));
      rowCL6->AddNode(cellCL, nodeId, new TGeoTranslation(0, ycell,0));
      nodeId++;
   }
   
   cout<<"//////////////////////////////////////////////////"<<endl;
   TGeoVolume *rowAR7 = new TGeoVolumeAssembly("rowAR7");
   TGeoVolume *rowAL7 = new TGeoVolumeAssembly("rowAL7");
   TGeoVolume *rowCR7 = new TGeoVolumeAssembly("rowCR7");
   TGeoVolume *rowCL7 = new TGeoVolumeAssembly("rowCL7");
   
   ncells  = 8;
   cellIdR = 38;
   cellIdL = 24;
   nring   = 7;
   nodeId  = 1;
   
   
   for (Int_t i=0; i<ncells; i++) {
      Double_t ycell = (2*i)*(dshifty);
      
       if( i > 0 ) ycell+=ytooth;
      
      
      
     TString cellARIdName = Form("HexagonA%d_%d",nring,cellIdR);
     TString cellALIdName = Form("HexagonA%d_%d",nring,cellIdL);
     TString cellCRIdName = Form("HexagonC%d_%d",nring,cellIdR);
     TString cellCLIdName = Form("HexagonC%d_%d",nring,cellIdL);
            
     TGeoVolume* cellAR = new TGeoVolume(cellARIdName.Data(),hex,medBMDDetector); 
     TGeoVolume* cellAL = new TGeoVolume(cellALIdName.Data(),hex,medBMDDetector);
     TGeoVolume* cellCR = new TGeoVolume(cellCRIdName.Data(),hex,medBMDDetector);
     TGeoVolume* cellCL = new TGeoVolume(cellCLIdName.Data(),hex,medBMDDetector);
     
     layerOffsetA = GetLayerIDOffset("A",nring);
     layerOffsetC = GetLayerIDOffset("C",nring);
        
        
        layerIDAR = layerOffsetA + cellIdR + 1;
        layerIDCR = layerOffsetC + cellIdR + 1; 
        layerIDAL = layerOffsetA + cellIdL + 1;
        layerIDCL = layerOffsetC + cellIdL + 1; 
        
         
        cellsName[layerIDAR] = "rowAR7";
        cellsName[layerIDCR] = "rowCR7";
        cellsPosXYZ[layerIDAR][0] = 0; cellsPosXYZ[layerIDAR][1] = ycell; cellsPosXYZ[layerIDAR][2] = 0;
        cellsPosXYZ[layerIDCR][0] = 0; cellsPosXYZ[layerIDCR][1] = ycell; cellsPosXYZ[layerIDCR][2] = 0;
        cellsName[layerIDAL] = "rowAL7";
        cellsName[layerIDCL] = "rowCL7";
        cellsPosXYZ[layerIDAL][0] = 0;  cellsPosXYZ[layerIDAL][1] = ycell; cellsPosXYZ[layerIDAL][2] = 0;
        cellsPosXYZ[layerIDCL][0] = 0;  cellsPosXYZ[layerIDCL][1] = ycell; cellsPosXYZ[layerIDCL][2] = 0;
        
        cout<<cellARIdName.Data()<<" "<<layerIDAR<<" "<<cellCRIdName.Data()<<" "<<layerIDCR<<endl;
        cout<<cellALIdName.Data()<<" "<<layerIDAL<<" "<<cellCLIdName.Data()<<" "<<layerIDCL<<endl;
     
     
      
     cellAR->SetLineColor(kYellow);
     cellAL->SetLineColor(kYellow);
     cellCR->SetLineColor(kYellow);
     cellCL->SetLineColor(kYellow);
     
     if( i == 3 ) {
         cellIdR=0;
         cellIdL--;
     }
     else {
         cellIdR++;
         cellIdL--;
     }
      
     rowAR7->AddNode(cellAR, nodeId, new TGeoTranslation(0, ycell,0));
     rowAL7->AddNode(cellAL, nodeId, new TGeoTranslation(0, ycell,0));
     rowCR7->AddNode(cellCR, nodeId, new TGeoTranslation(0, ycell,0));
     rowCL7->AddNode(cellCL, nodeId, new TGeoTranslation(0, ycell,0));
     nodeId++;
   }
   
   
   
    Double_t translationRowXYZR[8][3];
    Double_t translationRowXYZL[8][3];
    Double_t translationRowXYZC[3];
   

    TGeoVolume *BBCA = new TGeoVolumeAssembly("BBCA");
    TGeoVolume *BBCC = new TGeoVolumeAssembly("BBCC");
   
   
   
   
   
    Double_t dxrow = 3.0*(dshiftx+.10)*TMath::Tan(30.*TMath::DegToRad());
    Double_t dyrow = dshifty;
    Int_t nrows = 1;
    Int_t i=0;
    Double_t shiftx = -4.3305;
    Double_t xrow = 5*(2*i+1)*dxrow + shiftx;
    
    Double_t ytooth2  = - ((2*dshifty)*14)/2;  //.0;  
    Double_t yrow = 0 + ytooth2;  
       
    BBCA->AddNode(rowA0,1,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowC0,1,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZC[0] = xrow;
    translationRowXYZC[1] = yrow;
    translationRowXYZC[2] = 0;
    
    
    i=1;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = (1)*dyrow + ytooth2;  
    
    BBCA->AddNode(rowAR1,2,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCR1,2,new TGeoTranslation(xrow,yrow,0)); 
    
    
    translationRowXYZR[1][0] = xrow;
    translationRowXYZR[1][1] = yrow;
    translationRowXYZR[1][2] = 0;
    

    i=2;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 2*dyrow + ytooth2;  

    BBCA->AddNode(rowAR2,3,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCR2,3,new TGeoTranslation(xrow,yrow,0));
   
    
    translationRowXYZR[2][0] = xrow;
    translationRowXYZR[2][1] = yrow;
    translationRowXYZR[2][2] = 0;
    
    
    i=3;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 3*dyrow + ytooth2;  
 
    BBCA->AddNode(rowAR3,4,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCR3,4,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZR[3][0] = xrow;
    translationRowXYZR[3][1] = yrow;
    translationRowXYZR[3][2] = 0;
    

    i=4;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 4*dyrow + ytooth2;  

    BBCA->AddNode(rowAR4,5,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCR4,5,new TGeoTranslation(xrow,yrow,0));   
    
    translationRowXYZR[4][0] = xrow;
    translationRowXYZR[4][1] = yrow;
    translationRowXYZR[4][2] = 0;
    

    i=5;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 5*dyrow + ytooth2;  
    
    BBCA->AddNode(rowAR5,6,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCR5,6,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZR[5][0] = xrow;
    translationRowXYZR[5][1] = yrow;
    translationRowXYZR[5][2] = 0;
     
    i=6;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 6*dyrow + ytooth2;  
     
    BBCA->AddNode(rowAR6,7,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCR6,7,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZR[6][0] = xrow;
    translationRowXYZR[6][1] = yrow;
    translationRowXYZR[6][2] = 0;
    
     
    i=7;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 7*dyrow + ytooth2;  
     
    BBCA->AddNode(rowAR7,8,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCR7,8,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZR[7][0] = xrow;
    translationRowXYZR[7][1] = yrow;
    translationRowXYZR[7][2] = 0;
    
    

    /////////////////////////////////////////////////////// 
     
    i=-1;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 1*dyrow + ytooth2; 
    
    BBCA->AddNode(rowAL1,9,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCL1,9,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZL[1][0] = xrow;
    translationRowXYZL[1][1] = yrow;
    translationRowXYZL[1][2] = 0;
    
    
    i=-2;
    xrow = 5*(2*i+1)*dxrow +shiftx;
    yrow = 2*dyrow + ytooth2;  
    
    BBCA->AddNode(rowAL2,10,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCL2,10,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZL[2][0] = xrow;
    translationRowXYZL[2][1] = yrow;
    translationRowXYZL[2][2] = 0;
    
    
    i=-3;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 3*dyrow + ytooth2;  
    
    BBCA->AddNode(rowAL3,11,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCL3,11,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZL[3][0] = xrow;
    translationRowXYZL[3][1] = yrow;
    translationRowXYZL[3][2] = 0;
   
    
    i=-4;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 4*dyrow + ytooth2;  
    
    BBCA->AddNode(rowAL4,12,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCL4,12,new TGeoTranslation(xrow,yrow,0));
    
    
    translationRowXYZL[4][0] = xrow;
    translationRowXYZL[4][1] = yrow;
    translationRowXYZL[4][2] = 0;
    
    
    i=-5;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 5*dyrow + ytooth2;  
     
    BBCA->AddNode(rowAL5,13,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCL5,13,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZL[5][0] = xrow;
    translationRowXYZL[5][1] = yrow;
    translationRowXYZL[5][2] = 0;
    
    
    i=-6;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 6*dyrow + ytooth2;  
    
    BBCA->AddNode(rowAL6,14,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCL6,14,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZL[6][0] = xrow;
    translationRowXYZL[6][1] = yrow;
    translationRowXYZL[6][2] = 0;
    
    
    i=-7;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 7*dyrow + ytooth2;  
    
    BBCA->AddNode(rowAL7,15,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCL7,15,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZL[7][0] = xrow;
    translationRowXYZL[7][1] = yrow;
    translationRowXYZL[7][2] = 0;

      
    TGeoVolume *BMD = new TGeoVolumeAssembly("BMD");
    
    
    BMD->AddNode(BBCA,1,new TGeoTranslation(0,0,-200));
    BMD->AddNode(BBCC,2,new TGeoTranslation(0,0, 200));
    
    
    cout<<"/////////////////////////"<<endl;
    cout<<" (x,y,z) "<<endl;
    
    for(Int_t index = 1; index <= 336; index++){
        
        Double_t x = 0;
        Double_t y = 0;
        Double_t z = 0;
        Bool_t flag = kTRUE;
        
       // cout<<"Pedrito: "<<cellsName[0].Data()<<endl;
        
        if( cellsName[index].CompareTo("rowA0") == 0 || cellsName[index].CompareTo("rowC0") == 0 ){
            
            x = translationRowXYZC[0];
            y = translationRowXYZC[1];
            
        }   else if( cellsName[index].Contains("rowAR")  || cellsName[index].Contains("rowCR") ){
            if( cellsName[index].Contains("R1") ) {
                x = translationRowXYZR[1][0];
                y = translationRowXYZR[1][1];
            } else if ( cellsName[index].Contains("R2")  ) {
                x = translationRowXYZR[2][0];
                y = translationRowXYZR[2][1];
            } else if ( cellsName[index].Contains("R3") ) {
                x = translationRowXYZR[3][0];
                y = translationRowXYZR[3][1];
            } else if ( cellsName[index].Contains("R4") ) {
                x = translationRowXYZR[4][0];
                y = translationRowXYZR[4][1];
            } else if ( cellsName[index].Contains("R5") ) {
                x = translationRowXYZR[5][0];
                y = translationRowXYZR[5][1];
            } else if ( cellsName[index].Contains("R6") ) {
                x = translationRowXYZR[6][0];
                y = translationRowXYZR[6][1];
            } else if ( cellsName[index].Contains("R7") ) {
                x = translationRowXYZR[7][0];
                y = translationRowXYZR[7][1];
            }  
            
        }  else if( cellsName[index].Contains("rowAL") || cellsName[index].Contains("rowCL") ){
            
            if( cellsName[index].Contains("L1") ) {
                x = translationRowXYZL[1][0];
                y = translationRowXYZL[1][1];
            } else if ( cellsName[index].Contains("L2") ) {
                x = translationRowXYZL[2][0];
                y = translationRowXYZL[2][1];
            } else if ( cellsName[index].Contains("L3") ) {
                x = translationRowXYZL[3][0];
                y = translationRowXYZL[3][1];
            } else if ( cellsName[index].Contains("L4") ) {
                x = translationRowXYZL[4][0];
                y = translationRowXYZL[4][1];
            } else if ( cellsName[index].Contains("L5") ) {
                x = translationRowXYZL[5][0];
                y = translationRowXYZL[5][1];
            } else if ( cellsName[index].Contains("L6") ) {
                x = translationRowXYZL[6][0];
                y = translationRowXYZL[6][1];
            } else if ( cellsName[index].Contains("L7") ) {
                x = translationRowXYZL[7][0];
                y = translationRowXYZL[7][1];
            }
            
        } else {
        
                flag = kFALSE;
        }
        
        
        if( cellsName[index].Contains("rowC") )      z =  200;
        else if ( cellsName[index].Contains("rowA")) z = -200;
        
        if ( flag ) {
                cellsPosXYZ[index][0]+=x;  cellsPosXYZ[index][1]+=y;  cellsPosXYZ[index][2]+= z; 
                cout<<index<<" "<<" = ( "<<cellsPosXYZ[index][0]<<","<<cellsPosXYZ[index][1]<<","<<cellsPosXYZ[index][2]<<" )"<<endl;
        }
            
    }
    
    
    
    
    top->AddNode(BMD,1);
    

    top->SetVisContainers(kTRUE);
    
    // ---------------   Finish   ----------------------------------------------
   
    gGeoManager->SetVisLevel(5);
    gGeoManager->CloseGeometry();
    gGeoManager->CheckOverlaps(0.0001);
    gGeoManager->PrintOverlaps();
    
    gGeoManager->Test();

    //gGeoManager->Test();
    
      top->Draw();

    TFile* geoFile = new TFile(geoFileName, "RECREATE");
    top->Write();
    geoFile->Close();


    
    
    //geom->CloseGeometry();
    //geom->SetVisLevel(4);
    //geom->SetVisOption(0);
    //top->Draw();


}