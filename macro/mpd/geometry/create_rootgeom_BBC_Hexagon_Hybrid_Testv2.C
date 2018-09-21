//Author: Pedro Gonzalez Zamora, pedro.gonzalez.zamora@cern.ch

//-----------------------------------------------------
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"
#include "TFile.h"
#include "../mpdloadlibs.C"



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
TGeoMedium *medBMDvacumm;
TGeoMedium *pMedTPCmixture;

const Double_t fBMDPlusR0		= 5.0;//4.2;
const Double_t fBMDPlusR1		= 8.4;//7.6;
const Double_t fBMDPlusR2		= 14.6;//13.8;
const Double_t fBMDPlusR3		= 22.5; //17.5;//23.5;//22.7;
const Double_t fBMDPlusR4		= 37.5;//32.5;//41.3;
const Double_t fBMDPlusR5		= 52.5;//47.5;//75.93;
const Double_t fBMDPlusR6		= 80.0;
const Double_t fBMDPlusSciWd		= 0.002; 
const Double_t fBMDPlusFraWd		= 0.2;
const Double_t fBMDPlusnMeters		= fBMDPlusR6*0.01; //Check 


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
    
    cout<<"////// FairGeoMedium //////////"<<endl;
    mScintillator->print();
    cout<<"/////  TGeoMedium /////"<<endl;
    Double_t paramChkv[4];
    Double_t component0[3];
    Double_t component1[3];
    mScintillator->getCerenkovPar(1,paramChkv);
    mScintillator->getComponent(0,component0);
    mScintillator->getComponent(1,component1);
    cout<<"Cerenkov param 0: "<<paramChkv[0]<<" "<<paramChkv[1]<<" "<<paramChkv[2]<<" "<<paramChkv[3]<<endl; 
    cout<<"Radiation length: "<<mScintillator->getRadiationLength()<<endl;
    cout<<"Density: "<<mScintillator->getDensity()<<endl;
    cout<<"WeightFac: "<<mScintillator->getWeightFac()<<endl;
    cout<<"NComponents: "<<mScintillator->getNComponents()<<endl;
    cout<<"Component0:  "<<component0[0]<<" "<<component0[1]<<" "<<component0[2]<<endl;
    cout<<"component1:  "<<component1[1]<<" "<<component1[1]<<" "<<component1[2]<<endl;
    
   
    
    //medBMDFscScint->
    
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


void create_rootgeom_BBC_Hexagon_Hybrid_Testv2(TString mediumBMD="NDetScin",Bool_t wrGeoWithMaterials = false)
{

    // Load necessary libraries
    //gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(); // load libraries
    

    
    //---Units: using centimeters as base, i.e. 1 position unit=1 centimeter
    
    Double_t cm=1;//,micron=1e-4;
    Double_t xplate = 1.25;
    Double_t xtooth = .01;
    Double_t dshiftx  = 0.1;
    Double_t dshifty =  2.*xplate;
    Double_t ytooth = 0.;
     
    

    // ----  set working directory  --------------------------------------------
    TString gPath = gSystem->Getenv("VMCWORKDIR");
    // -------   Geometry file name (output)   ---------------------------------
    const TString geoDetectorName = "bbc_hex_hybrid";
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
    //top->SetMedium(medBMDvacumm);
    
    gGeoManager->SetTopVolume(top);
    
    
    ///////////////////////////////////////////////////////////////////////////
    // Hybrid Plus

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
   
   
    Double_t Pi = TMath::Pi();
    Double_t Sin22p5    = TMath::Sin(Pi/8.); 
    Double_t Cos22p5    = TMath::Cos(Pi/8.); 
    Double_t bmdPlusPts[16];
    
    TString bmd_detectorA_name = "BBCPLUSA";
    TGeoTube  *bmdPlusAS = new TGeoTube(Form("%s_TGeoTube",bmd_detectorA_name.Data()),fBMDPlusR3+fBMDPlusFraWd/2.,fBMDPlusR5-fBMDPlusFraWd/2.,fBMDPlusSciWd/2.);
    //TGeoTube  *bmdPlusAS = new TGeoTube(bmd_detectorA_name.Data(),fBMDPlusR3+fBMDPlusFraWd/2.,fBMDPlusR5-fBMDPlusFraWd/2.,fBMDPlusSciWd/2.);
    
    TGeoVolume *bmdPlusA = new TGeoVolume(bmd_detectorA_name.Data(),bmdPlusAS);
    bmdPlusA->SetMedium(medBMDDetector);
    
    TString bmd_detectorC_name = "BBCPLUSC";
    TGeoTube  *bmdPlusCS = new TGeoTube(Form("%s_TGeoTube",bmd_detectorC_name.Data()),fBMDPlusR3+fBMDPlusFraWd/2.,fBMDPlusR5-fBMDPlusFraWd/2.,fBMDPlusSciWd/2.);
     //TGeoTube  *bmdPlusCS = new TGeoTube(bmd_detectorC_name.Data(),fBMDPlusR3+fBMDPlusFraWd/2.,fBMDPlusR5-fBMDPlusFraWd/2.,fBMDPlusSciWd/2.);
    
    TGeoVolume *bmdPlusC = new TGeoVolume(bmd_detectorC_name.Data(),bmdPlusCS);
    bmdPlusC->SetMedium(medBMDDetector);
   
   

    /// For boolean sustraction
    for (Int_t i = 0; i < 2; i++) 
      {
	bmdPlusPts[0+8*i] = fBMDPlusR3 - fBMDPlusFraWd/2. - fBMDPlusFraWd;  bmdPlusPts[1+8*i] = -fBMDPlusFraWd;
	bmdPlusPts[2+8*i] = fBMDPlusR3 - fBMDPlusFraWd/2. - fBMDPlusFraWd;  bmdPlusPts[3+8*i] =  fBMDPlusFraWd/2.;
	bmdPlusPts[4+8*i] = fBMDPlusR5 + fBMDPlusFraWd/2. + fBMDPlusFraWd;  bmdPlusPts[5+8*i] =  fBMDPlusFraWd/2.;
	bmdPlusPts[6+8*i] = fBMDPlusR5 + fBMDPlusFraWd/2. + fBMDPlusFraWd;  bmdPlusPts[7+8*i] = -fBMDPlusFraWd;
      }
    new TGeoArb8("sBMDPlusCha1",fBMDPlusSciWd/1.5,bmdPlusPts);
    for (Int_t i = 0; i < 2; i++) 
      {
	bmdPlusPts[0+8*i] = fBMDPlusR3*Cos22p5-fBMDPlusFraWd;
	bmdPlusPts[1+8*i] = (fBMDPlusR3 - fBMDPlusFraWd)*Sin22p5-fBMDPlusFraWd;
	bmdPlusPts[2+8*i] = (fBMDPlusR3 - fBMDPlusFraWd/2.)*Cos22p5-fBMDPlusFraWd;
	bmdPlusPts[3+8*i] = (fBMDPlusR3 - fBMDPlusFraWd/2.)*Sin22p5;
	bmdPlusPts[4+8*i] = (fBMDPlusR5 + fBMDPlusFraWd/2.)*Cos22p5+fBMDPlusFraWd;
	bmdPlusPts[5+8*i] = (fBMDPlusR5 + fBMDPlusFraWd/2.)*Sin22p5+2.*fBMDPlusFraWd;
	bmdPlusPts[6+8*i] = (fBMDPlusR5 + fBMDPlusFraWd)*Cos22p5+fBMDPlusFraWd;
	bmdPlusPts[7+8*i] = fBMDPlusR5*Sin22p5+fBMDPlusFraWd;
      }
    new TGeoArb8("sBMDPlusCha2", fBMDPlusSciWd/2.+2.*fBMDPlusFraWd, bmdPlusPts);
    new TGeoCompositeShape("sBMDPlusCha","sBMDPlusCha1+sBMDPlusCha2");

    //Sensitive scintillator
    new TGeoTubeSeg( "sBMDPlusR4b", fBMDPlusR3+fBMDPlusFraWd/2., fBMDPlusR4-fBMDPlusFraWd/2., fBMDPlusSciWd/2., 0, 22.5);
    new TGeoTubeSeg( "sBMDPlusR5b", fBMDPlusR4+fBMDPlusFraWd/2., fBMDPlusR5-fBMDPlusFraWd/2., fBMDPlusSciWd/2., 0, 22.5);
    
    TGeoCompositeShape *sBMDPlusR4 = new TGeoCompositeShape("sBMDPlusR4","sBMDPlusR4b-sBMDPlusCha");
    TGeoCompositeShape *sBMDPlusR5 = new TGeoCompositeShape("sBMDPlusR5","sBMDPlusR5b-sBMDPlusCha");
    

    Double_t rR4min =  fBMDPlusR3+fBMDPlusFraWd/2.;Double_t rR4max =  fBMDPlusR4-fBMDPlusFraWd/2.;
    Double_t rR5min =  fBMDPlusR4+fBMDPlusFraWd/2.;Double_t rR5max =  fBMDPlusR5-fBMDPlusFraWd/2.;
    
    cout<<"Ring4: "<<rR4min<<" "<<rR4max<<endl;
    cout<<"Ring5: "<<rR5min<<" "<<rR5max<<endl;
   
    
    for(Int_t iSec=1;iSec<=16;iSec++){
    
    /// Definition sector 1
    
        TGeoVolume *bmdPlusA4Sec = new TGeoVolume(Form("HybridPayPlusA4Sec%d",iSec), sBMDPlusR4,medBMDDetector);
        TGeoVolume *bmdPlusA5Sec = new TGeoVolume(Form("HybridPayPlusA5Sec%d",iSec), sBMDPlusR5,medBMDDetector); 
    
        bmdPlusA4Sec->SetLineColor(kBMDPlusColorSci);
        bmdPlusA5Sec->SetLineColor(kBMDPlusColorSci);
 
        //TGeoVolume *bmdPlusASciSec = new TGeoVolumeAssembly(Form("HybridPayPlusASciSec%d",iSec));
        //bmdPlusASciSec->SetMedium(medBMDDetector);
        //bmdPlusASciSec->AddNode(bmdPlusA4Sec,1);
        //bmdPlusASciSec->AddNode(bmdPlusA5Sec,2);
        //bmdPlusASciSec->SetLineColor(kBMDPlusColorSci);
    
        TGeoRotation *RotASec = new TGeoRotation(Form("RotASec%d",iSec), 90.,(iSec-1)*22.5, 90., 90.+(iSec-1)*22.5, 0., 0.);
        bmdPlusA->AddNode(bmdPlusA4Sec,iSec,RotASec);
        bmdPlusA->AddNode(bmdPlusA5Sec,iSec,RotASec);
    
        //bmdPlusA->AddNode(bmdPlusASciSec,1,RotASec);  
    
        //Defining BMD side C
    
        TGeoVolume *bmdPlusC4Sec = new TGeoVolume(Form("HybridPayPlusC4Sec%d",iSec), sBMDPlusR4,medBMDDetector);
        TGeoVolume *bmdPlusC5Sec = new TGeoVolume(Form("HybridPayPlusC5Sec%d",iSec), sBMDPlusR5,medBMDDetector); 
    
        bmdPlusC4Sec->SetLineColor(kBMDPlusColorSci);
        bmdPlusC5Sec->SetLineColor(kBMDPlusColorSci);
    
    
        //TGeoVolume *bmdPlusCSciSec = new TGeoVolumeAssembly(Form("HybridPayPlusCSciSec%d",iSec));
        //bmdPlusCSciSec->SetMedium(medBMDDetector);
        //bmdPlusCSciSec->AddNode(bmdPlusC4Sec,1);
        //bmdPlusCSciSec->AddNode(bmdPlusC5Sec,2);
        //bmdPlusCSciSec->SetLineColor(kBMDPlusColorSci);
    
       
        TGeoRotation *RotCSec = new TGeoRotation(Form("RotCSec%d",iSec), 90.,(iSec-1)*22.5, 90., 90.+(iSec-1)*22.5, 0., 0.);
        bmdPlusC->AddNode(bmdPlusC4Sec,iSec,RotCSec);
        bmdPlusC->AddNode(bmdPlusC5Sec,iSec,RotCSec);
        //bmdPlusC->AddNode(bmdPlusCSciSec,1,RotCSec);
    }
   
   
   
    TGeoPgon *hex=new TGeoPgon(0,360,6,2);
    hex->DefineSection(0,-0.001*cm, 0, 2.5*cm);//1.611*cm);
    hex->DefineSection(1, 0.001*cm, 0, 2.5*cm);//1.611*cm);


    // Make a row as an assembly of cells, then combine rows in a honeycomb
    // structure. This again works without any need to define rows as 
    // "overlapping"
    
    TString     cellsName[200];
    Double_t    cellsPosXYZ[200][3];
    Int_t  offsetSideC = 86;
    
    
    TGeoVolume *rowA0 = new TGeoVolumeAssembly("rowA0");
    TGeoVolume *rowC0 = new TGeoVolumeAssembly("rowC0");
   
    Int_t ncells = 9;
    Int_t cellId = 49;
    Int_t nring  = 3;
    Int_t nodeId = 1;
   
    Color_t   cellColor = kYellow;
   
   
   for(Int_t i=0; i<ncells; i++) {
       
      Double_t ycell = (2*i)*(dshifty);
      
      if( i > 0 ) ycell+=ytooth;
      
       
      if( i != 3 && i != 4 && i != 5 ) {
      
          TString cellAIdName = Form("HybridHexA%d_%d",nring,cellId);
          TString cellCIdName = Form("HybridHexC%d_%d",nring,cellId);
          
           
        
         cellsName[cellId]             = "rowA0";
         cellsName[cellId+offsetSideC] = "rowC0";
         cellsPosXYZ[cellId][0] = 0;              cellsPosXYZ[cellId][1]             = ycell; cellsPosXYZ[cellId][2]    = 0;
         cellsPosXYZ[cellId+offsetSideC][0] = 0;  cellsPosXYZ[cellId+offsetSideC][1] = ycell; cellsPosXYZ[cellId+offsetSideC][2] = 0;
         
         
          
             
          TGeoVolume* cellA = new TGeoVolume(cellAIdName.Data(),hex,medBMDDetector); 
          TGeoVolume* cellC = new TGeoVolume(cellCIdName.Data(),hex,medBMDDetector);
       
          
         
          
          if ( i  == 0 ) {
              
                    cellId = 26;
                    nring  = 2;
          } else if ( i == 1 ) {
                    cellId = 10;
                    nring  = 1;
          } else if ( i == 6 ) {
                    cellId = 17;
                    nring  = 2;
          } else if ( i == 7 ) {
                    cellId = 37;
                    nring  = 3;
          }
      
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
        
     }  else if ( i == 5 ) {
         
            nring  = 1;
            cellId = 4;
        }
    }
   
   
   cout<<"///////////////////////////////////////////"<<endl;
   
   TGeoVolume *rowAR1 = new TGeoVolumeAssembly("rowAR1");
   TGeoVolume *rowAL1 = new TGeoVolumeAssembly("rowAL1");
   TGeoVolume *rowCR1 = new TGeoVolumeAssembly("rowCR1");
   TGeoVolume *rowCL1 = new TGeoVolumeAssembly("rowCL1");
   
   ncells = 8;
   Int_t cellIdR = 50;
   Int_t cellIdL = 48;
   nring  = 3;
   nodeId = 1;
   cellColor = kYellow;
   
   
   for (Int_t i=0; i<ncells; i++) {
       
        Double_t ycell = (2*i)*(dshifty);
        if( i > 0 ) ycell+=ytooth;
      
        if( i != 3 && i != 4  ) {
         
        TString cellARIdName = Form("HybridHexA%d_%d",nring,cellIdR);
        TString cellALIdName = Form("HybridHexA%d_%d",nring,cellIdL);
        TString cellCRIdName = Form("HybridHexC%d_%d",nring,cellIdR);
        TString cellCLIdName = Form("HybridHexC%d_%d",nring,cellIdL);
        
        cellsName[cellIdR]             = "rowAR1";
        cellsName[cellIdR+offsetSideC] = "rowCR1";
        cellsPosXYZ[cellIdR][0] = 0;     cellsPosXYZ[cellIdR][1]    = ycell; cellsPosXYZ[cellIdR][2]    = 0;
        cellsPosXYZ[cellIdR+offsetSideC][0] = 0;  cellsPosXYZ[cellIdR+offsetSideC][1] = ycell; cellsPosXYZ[cellIdR+offsetSideC][2] = 0;
        cellsName[cellIdL]             = "rowAL1";
        cellsName[cellIdL+offsetSideC] = "rowCL1";
        cellsPosXYZ[cellIdL][0] = 0;     cellsPosXYZ[cellIdL][1]    = ycell; cellsPosXYZ[cellIdL][2]    = 0;
        cellsPosXYZ[cellIdL+offsetSideC][0] = 0;  cellsPosXYZ[cellIdL+offsetSideC][1] = ycell; cellsPosXYZ[cellIdL+offsetSideC][2] = 0;
        
        
        
        TGeoVolume* cellAR = new TGeoVolume(cellARIdName.Data(),hex,medBMDDetector); 
        TGeoVolume* cellAL = new TGeoVolume(cellALIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCR = new TGeoVolume(cellCRIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCL = new TGeoVolume(cellCLIdName.Data(),hex,medBMDDetector);
        cellAR->SetLineColor(cellColor);
        cellAL->SetLineColor(cellColor);
        cellCR->SetLineColor(cellColor);
        cellCL->SetLineColor(cellColor);   
        
        //cout<<i<<" "<<nodeId<<" "<<cellARIdName.Data()<<" "<<cellCRIdName.Data()<<endl;
        //cout<<i<<" "<<nodeId<<" "<<cellALIdName.Data()<<" "<<cellCLIdName.Data()<<endl;
        
        if ( i == 0 ) {
            
                cellColor  = kGreen;
                cellIdR= 27;
                cellIdL= 25;
                nring  = 2;
                
        } else if ( i == 1 ) {
                 
                cellColor  = kYellow;
                cellIdR= 11;
                cellIdL= 9;
                nring  = 1;
                
        } else if ( i == 2 ) {
            
                cellColor  = kYellow;
                cellIdR= 5;
                cellIdL= 5;
                nring  = 1;
                
        } else if ( i == 5 ) {
            
                cellColor  = kGreen;
                cellIdR= 16;
                cellIdL= 18;
                nring  = 2;
                
        } else if ( i  == 6 ) {
            
                cellColor  = kYellow;
                cellIdR= 36;
                cellIdL= 38;
                nring  = 3;
        }
        
    
        rowAR1->AddNode(cellAR, nodeId, new TGeoTranslation(0,ycell,0));
        rowAL1->AddNode(cellAL, nodeId, new TGeoTranslation(0,ycell,0));
        rowCR1->AddNode(cellCR, nodeId, new TGeoTranslation(0,ycell,0));
        rowCL1->AddNode(cellCL, nodeId, new TGeoTranslation(0,ycell,0));
        nodeId++;
        
        } else if ( i == 4 ){
                
                cellColor  = kYellow;
                cellIdR= 3;
                cellIdL= 5;
                nring  = 1;
        }
    
   }
    
   cout<<"////////////////////////////////////////////"<<endl;
   
   TGeoVolume *rowAR2 = new TGeoVolumeAssembly("rowAR2");
   TGeoVolume *rowAL2 = new TGeoVolumeAssembly("rowAL2");
   TGeoVolume *rowCR2 = new TGeoVolumeAssembly("rowCR2");
   TGeoVolume *rowCL2 = new TGeoVolumeAssembly("rowCL2");
   
   
   ncells  = 7;
   cellIdR = 51;
   cellIdL = 47;
   nring   = 3;
   nodeId  = 1;
   cellColor = kYellow;
   
    
    for (Int_t i=0; i<ncells; i++) {
        
        Double_t ycell = (2*i)*(dshifty);
        if( i > 0 ) ycell+=ytooth;
      
        
        TString cellARIdName = Form("HybridHexA%d_%d",nring,cellIdR);
        TString cellALIdName = Form("HybridHexA%d_%d",nring,cellIdL);
        TString cellCRIdName = Form("HybridHexC%d_%d",nring,cellIdR);
        TString cellCLIdName = Form("HybridHexC%d_%d",nring,cellIdL);
        
        cellsName[cellIdR]             = "rowAR2";
        cellsName[cellIdR+offsetSideC] = "rowCR2";
        cellsPosXYZ[cellIdR][0] = 0;              cellsPosXYZ[cellIdR][1]    = ycell; cellsPosXYZ[cellIdR][2]    = 0;
        cellsPosXYZ[cellIdR+offsetSideC][0] = 0;  cellsPosXYZ[cellIdR+offsetSideC][1] = ycell; cellsPosXYZ[cellIdR+offsetSideC][2] = 0;
        cellsName[cellIdL]             = "rowAL2";
        cellsName[cellIdL+offsetSideC] = "rowCL2";
        cellsPosXYZ[cellIdL][0] = 0;     cellsPosXYZ[cellIdL][1]    = ycell; cellsPosXYZ[cellIdL][2]    = 0;
        cellsPosXYZ[cellIdL+offsetSideC][0] = 0;  cellsPosXYZ[cellIdL+offsetSideC][1] = ycell; cellsPosXYZ[cellIdL+offsetSideC][2] = 0;
        
            
        TGeoVolume* cellAR = new TGeoVolume(cellARIdName.Data(),hex,medBMDDetector); 
        TGeoVolume* cellAL = new TGeoVolume(cellALIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCR = new TGeoVolume(cellCRIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCL = new TGeoVolume(cellCLIdName.Data(),hex,medBMDDetector);
        cellAR->SetLineColor(cellColor);
        cellAL->SetLineColor(cellColor);
        cellCR->SetLineColor(cellColor);
        cellCL->SetLineColor(cellColor);   
        
        //cout<<i<<" "<<nodeId<<" "<<cellARIdName.Data()<<" "<<cellCRIdName.Data()<<endl;
        //cout<<i<<" "<<nodeId<<" "<<cellALIdName.Data()<<" "<<cellCLIdName.Data()<<endl;
      
        if ( i == 0 ) {
            
                cellColor  = kGreen;
                cellIdR= 28;
                cellIdL= 24;
                nring  = 2;
                
        } else if ( i == 1 ) {
                 
                cellColor  = kYellow;
                cellIdR= 12;
                cellIdL= 8;
                nring  = 1;
                
        } else if ( i == 2 ) {
            
                cellColor  = kYellow;
                cellIdR= 1;
                cellIdL= 7;
                nring  = 1;
                
        } else if ( i == 3 ) {
            
                cellColor  = kYellow;
                cellIdR= 2;
                cellIdL= 6;
                nring  = 1;
                
        } else if ( i  == 4 ) {
            
                cellColor  = kGreen;
                cellIdR= 15;
                cellIdL= 19;
                nring  = 2;
                
        } else if ( i  == 5 ) {
            
                cellColor  = kYellow;
                cellIdR= 35;
                cellIdL= 39;
                nring  = 3;
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
   
   ncells  = 6;
   cellIdR = 52;
   cellIdL = 46;
   nring   = 3;
   nodeId  = 1;
   cellColor = kYellow;
   
   for (Int_t i=0; i<ncells; i++) {
      Double_t ycell = (2*i)*(dshifty);
       if( i > 0 ) ycell+=ytooth;
      
      
        TString cellARIdName = Form("HybridHexA%d_%d",nring,cellIdR);
        TString cellALIdName = Form("HybridHexA%d_%d",nring,cellIdL);
        TString cellCRIdName = Form("HybridHexC%d_%d",nring,cellIdR);
        TString cellCLIdName = Form("HybridHexC%d_%d",nring,cellIdL);
       
        cellsName[cellIdR]             = "rowAR3";
        cellsName[cellIdR+offsetSideC] = "rowCR3";
        cellsPosXYZ[cellIdR][0] = 0;              cellsPosXYZ[cellIdR][1]    = ycell; cellsPosXYZ[cellIdR][2]    = 0;
        cellsPosXYZ[cellIdR+offsetSideC][0] = 0;  cellsPosXYZ[cellIdR+offsetSideC][1] = ycell; cellsPosXYZ[cellIdR+offsetSideC][2] = 0;
        cellsName[cellIdL]             = "rowAL3";
        cellsName[cellIdL+offsetSideC] = "rowCL3";
        cellsPosXYZ[cellIdL][0] = 0;     cellsPosXYZ[cellIdL][1]    = ycell; cellsPosXYZ[cellIdL][2]    = 0;
        cellsPosXYZ[cellIdL+offsetSideC][0] = 0;  cellsPosXYZ[cellIdL+offsetSideC][1] = ycell; cellsPosXYZ[cellIdL+offsetSideC][2] = 0;
            
        TGeoVolume* cellAR = new TGeoVolume(cellARIdName.Data(),hex,medBMDDetector); 
        TGeoVolume* cellAL = new TGeoVolume(cellALIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCR = new TGeoVolume(cellCRIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCL = new TGeoVolume(cellCLIdName.Data(),hex,medBMDDetector);
        cellAR->SetLineColor(cellColor);
        cellAL->SetLineColor(cellColor);
        cellCR->SetLineColor(cellColor);
        cellCL->SetLineColor(cellColor);   
        
        //cout<<i<<" "<<nodeId<<" "<<cellARIdName.Data()<<" "<<cellCRIdName.Data()<<endl;
        cout<<i<<" "<<nodeId<<" "<<cellALIdName.Data()<<" "<<cellCLIdName.Data()<<endl;
        
       
        if ( i == 0 ) {
            
                cellColor  = kGreen;
                cellIdR= 29;
                cellIdL= 23;
                nring  = 2;
                
        } else if ( i == 1 ) {
                 
                cellColor  = kGreen;
                cellIdR= 30;
                cellIdL= 22;
                nring  = 2;
                
        } else if ( i == 2 ) {
            
                cellColor  = kGreen;
                cellIdR= 13;
                cellIdL= 21;
                nring  = 2;
                
        } else if ( i == 3 ) {
            
                cellColor  = kGreen;
                cellIdR= 14;
                cellIdL= 20;
                nring  = 2;
                
        } else if ( i  == 4 ) {
            
                cellColor  = kYellow;
                cellIdR= 34;
                cellIdL= 40;
                nring  = 3;
                
        } 
      
      rowAR3->AddNode(cellAR, nodeId, new TGeoTranslation(0, ycell,0));
      rowAL3->AddNode(cellAL, nodeId, new TGeoTranslation(0, ycell,0));
      rowCR3->AddNode(cellCR, nodeId, new TGeoTranslation(0, ycell,0));
      rowCL3->AddNode(cellCL, nodeId, new TGeoTranslation(0, ycell,0));
      nodeId++;
   }
   
   
   
   ncells  = 5;
   cellIdR = 53;
   cellIdL = 45;
   nring   = 3;
   nodeId  = 1;
   cellColor = kYellow;
   
   cout<<"//////////////////////////////////////////////////"<<endl;
   TGeoVolume *rowAR4 = new TGeoVolumeAssembly("rowAR4");
   TGeoVolume *rowAL4 = new TGeoVolumeAssembly("rowAL4");
   TGeoVolume *rowCR4 = new TGeoVolumeAssembly("rowCR4");
   TGeoVolume *rowCL4 = new TGeoVolumeAssembly("rowCL4");
   
   
   for (Int_t i=0; i<ncells; i++) {
      Double_t ycell = (2*i)*(dshifty);
       if( i > 0 ) ycell+=ytooth;
      
      
        TString cellARIdName = Form("HybridHexA%d_%d",nring,cellIdR);
        TString cellALIdName = Form("HybridHexA%d_%d",nring,cellIdL);
        TString cellCRIdName = Form("HybridHexC%d_%d",nring,cellIdR);
        TString cellCLIdName = Form("HybridHexC%d_%d",nring,cellIdL);
       
        cellsName[cellIdR]             = "rowAR4";
        cellsName[cellIdR+offsetSideC] = "rowCR4";
        cellsPosXYZ[cellIdR][0] = 0;              cellsPosXYZ[cellIdR][1]    = ycell; cellsPosXYZ[cellIdR][2]    = 0;
        cellsPosXYZ[cellIdR+offsetSideC][0] = 0;  cellsPosXYZ[cellIdR+offsetSideC][1] = ycell; cellsPosXYZ[cellIdR+offsetSideC][2] = 0;
        cellsName[cellIdL]             = "rowAL4";
        cellsName[cellIdL+offsetSideC] = "rowCL4";
        cellsPosXYZ[cellIdL][0] = 0;     cellsPosXYZ[cellIdL][1]    = ycell; cellsPosXYZ[cellIdL][2]    = 0;
        cellsPosXYZ[cellIdL+offsetSideC][0] = 0;  cellsPosXYZ[cellIdL+offsetSideC][1] = ycell; cellsPosXYZ[cellIdL+offsetSideC][2] = 0;
            
        TGeoVolume* cellAR = new TGeoVolume(cellARIdName.Data(),hex,medBMDDetector); 
        TGeoVolume* cellAL = new TGeoVolume(cellALIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCR = new TGeoVolume(cellCRIdName.Data(),hex,medBMDDetector);
        TGeoVolume* cellCL = new TGeoVolume(cellCLIdName.Data(),hex,medBMDDetector);
        
        cellAR->SetLineColor(cellColor);
        cellAL->SetLineColor(cellColor);
        cellCR->SetLineColor(cellColor);
        cellCL->SetLineColor(cellColor);  
        
        //cout<<i<<" "<<nodeId<<" "<<cellARIdName.Data()<<" "<<cellCRIdName.Data()<<endl;
        //cout<<i<<" "<<nodeId<<" "<<cellALIdName.Data()<<" "<<cellCLIdName.Data()<<endl;
        
       
        if ( i == 0 ) {
            
                cellColor  = kYellow;
                cellIdR= 54;
                cellIdL= 44;
                nring  = 3;
                
        } else if ( i == 1 ) {
                 
                cellColor  = kYellow;
                cellIdR= 31;
                cellIdL= 43;
                nring  = 3;
                
        } else if ( i == 2 ) {
            
                cellColor  = kYellow;
                cellIdR= 32;
                cellIdL= 42;
                nring  = 3;
                
        } else if ( i == 3 ) {
            
                cellColor  = kYellow;
                cellIdR= 33;
                cellIdL= 41;
                nring  = 3;
                
        } 
      
      
      rowAR4->AddNode(cellAR, nodeId, new TGeoTranslation(0, ycell,0));
      rowAL4->AddNode(cellAL, nodeId, new TGeoTranslation(0, ycell,0));
      rowCR4->AddNode(cellCR, nodeId, new TGeoTranslation(0, ycell,0));
      rowCL4->AddNode(cellCL, nodeId, new TGeoTranslation(0, ycell,0));
      nodeId++;
   }
   
   
    
    for(Int_t index = 1; index < 86*2; index++){
   
            cout<<index<<" "<<cellsName[index]<<" "<<cellsPosXYZ[index][0]<<" "<<cellsPosXYZ[index][1]<<" "<<cellsPosXYZ[index][2]<<endl;
       
   }       
   
   
    Double_t translationRowXYZR[5][3];
    Double_t translationRowXYZL[5][3];
    Double_t translationRowXYZC[3];
   
   
    TGeoVolume *BBCA = new TGeoVolumeAssembly("BBCA");
    TGeoVolume *BBCC = new TGeoVolumeAssembly("BBCC");
   

    
    Double_t dxrow = 3.0*(dshiftx+.15)*TMath::Tan(30.*TMath::DegToRad());
    Double_t dyrow = dshifty;
    Int_t nrows = 1;
    Double_t shiftx = -2.1650635;
    Int_t i=0;
    
    Double_t xrow       = 5*(2*i+1)*dxrow + shiftx;
    Double_t ytooth2    = - ((2*dshifty)*6)/2;  
    Double_t yrow       = (-2)*dyrow + ytooth2;   
       
    BBCA->AddNode(rowA0,1,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowC0,1,new TGeoTranslation(xrow,yrow,0));
   
    translationRowXYZC[0] = xrow;
    translationRowXYZC[1] = yrow;
    translationRowXYZC[2] = 0;
    
    
    
    
   
    i=1;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = (-1)*dyrow + ytooth2;  
    
    BBCA->AddNode(rowAR1,2,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCR1,2,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZR[1][0] = xrow;
    translationRowXYZR[1][1] = yrow;
    translationRowXYZR[1][2] = 0;
   
    i=2;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 0*dyrow + ytooth2;  

    BBCA->AddNode(rowAR2,3,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCR2,3,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZR[2][0] = xrow;
    translationRowXYZR[2][1] = yrow;
    translationRowXYZR[2][2] = 0;
    
    i=3;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 1*dyrow + ytooth2;  
 
    BBCA->AddNode(rowAR3,4,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCR3,4,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZR[3][0] = xrow;
    translationRowXYZR[3][1] = yrow;
    translationRowXYZR[3][2] = 0;
    
    
    i=4;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 2*dyrow + ytooth2;  
 
    BBCA->AddNode(rowAR4,5,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCR4,5,new TGeoTranslation(xrow,yrow,0));

    translationRowXYZR[4][0] = xrow;
    translationRowXYZR[4][1] = yrow;
    translationRowXYZR[4][2] = 0;
    
  
    i=-1;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = -1*dyrow + ytooth2; 
    
    BBCA->AddNode(rowAL1,5,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCL1,5,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZL[1][0] = xrow;
    translationRowXYZL[1][1] = yrow;
    translationRowXYZL[1][2] = 0;
    
   
    i=-2;
    xrow = 5*(2*i+1)*dxrow +shiftx;
    yrow = 0*dyrow + ytooth2;  
    
    BBCA->AddNode(rowAL2,6,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCL2,6,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZL[2][0] = xrow;
    translationRowXYZL[2][1] = yrow;
    translationRowXYZL[2][2] = 0;
    
    
    i=-3;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 1*dyrow + ytooth2;  
    
    BBCA->AddNode(rowAL3,7,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCL3,7,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZL[3][0] = xrow;
    translationRowXYZL[3][1] = yrow;
    translationRowXYZL[3][2] = 0;
   
    i=-4;
    xrow = 5*(2*i+1)*dxrow + shiftx;
    yrow = 2*dyrow + ytooth2;  
    
    BBCA->AddNode(rowAL4,8,new TGeoTranslation(xrow,yrow,0));
    BBCC->AddNode(rowCL4,8,new TGeoTranslation(xrow,yrow,0));
    
    translationRowXYZL[4][0] = xrow;
    translationRowXYZL[4][1] = yrow;
    translationRowXYZL[4][2] = 0;
    
    
    TGeoVolume *BBCHybridA = new TGeoVolumeAssembly("BBCA");
    
    BBCHybridA->AddNode(bmdPlusA,1);
    BBCHybridA->AddNode(BBCA,2);
    
    TGeoVolume *BBCHybridC = new TGeoVolumeAssembly("BBCC");
    
    BBCHybridC->AddNode(bmdPlusC,1);
    BBCHybridC->AddNode(BBCC,2);
    
    TGeoVolume *BBC = new TGeoVolumeAssembly("BBC");
    
   
    BBC->AddNode(BBCHybridA,1,new TGeoTranslation(0, 0, -200.0));
    BBC->AddNode(BBCHybridC,2,new TGeoTranslation(0, 0,  200.0));
    
    
    cout<<"/////////////////////////"<<endl;
    cout<<" (x,y,z) "<<endl;
    
    for(Int_t index = 0; index < 86*2; index++){
        
        Double_t x = 0;
        Double_t y = 0;
        Double_t z = 0;
        Bool_t flag = kTRUE;
        
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
    
    
    
    
    
    top->AddNode(BBC,0);
    

    top->SetVisContainers(kTRUE);
    
    // ---------------   Finish   ----------------------------------------------
   
    //gGeoManager->SetVisLevel(5);
    gGeoManager->CloseGeometry();
    gGeoManager->CheckOverlaps(0.001);
    gGeoManager->PrintOverlaps();
    
    gGeoManager->Test();

    //gGeoManager->Test();
    
    //top->Draw();

    TFile* geoFile = new TFile(geoFileName, "RECREATE");
    top->Write();
    geoFile->Close();


    if (wrGeoWithMaterials)
	{
		TString geoFile_wMat = gPath + "/geometry/" + geoDetectorName+"_"+ geoDetectorMedium + "_"+ geoDetectorVersion + "_with_materials.root";
                 //gPath + "/geometry/" + geoDetectorName + "_"+ geoDetectorMedium + "_"+geoDetectorVersion + ".root";
		gGeoManager->Export(geoFile_wMat);
	}
    
    //geom->CloseGeometry();
    //geom->SetVisLevel(4);
    //geom->SetVisOption(0);
    //top->Draw();


}