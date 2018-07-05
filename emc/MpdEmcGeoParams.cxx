////////////////////////////////////////////////////////////////
//                                                            //
//  MpdEmcGeoParams                                           //
//  EMC geometry in MpdEmcHitCreation, v01                    //
//  Author List : Martemianov M., ITEP, 2017                  //
//                                                            //
////////////////////////////////////////////////////////////////

#include "MpdEmcGeoParams.h"
#include "FairParamList.h"
#include "TObjArray.h"
#include <iostream>

#include "TGeoNode.h"
#include "TGeoManager.h"
#include "TGeoTube.h"
#include "TGeoArb8.h"

ClassImp(MpdEmcGeoParams)

MpdEmcGeoParams ::MpdEmcGeoParams(const char* name, const char* title, const char* context)
  : FairParGenericSet(name,title,context) {

  fGeoSensNodes = new TObjArray();
  fGeoPassNodes = new TObjArray();

}

MpdEmcGeoParams::MpdEmcGeoParams() {

// New EMC version 

    TString pathUp; 
    TString path1 = "/cave_1/emc1Chamber1_0/emc1ChH_0";
    TString path2 = "/cave_1/emc1Chamber1_0/emc1ChH_1";     
    TString pathSector, pathCrate, pathBox;

    gGeoManager->cd(path1);

// Total barell 

    length = 2.*((TGeoTube*)gGeoManager->GetCurrentVolume()->GetShape())->GetDz();
    rMin = ((TGeoTube*)gGeoManager->GetCurrentVolume()->GetShape())->GetRmin();    
    rMax = ((TGeoTube*)gGeoManager->GetCurrentVolume()->GetShape())->GetRmax();
  
    TGeoVolume* sector1 = (TGeoVolume*)gGeoManager->GetVolume("emc1Sector1");
    TGeoVolume* sector2 = (TGeoVolume*)gGeoManager->GetVolume("emc1Sector2");
    Double_t phiMin1 = ((TGeoTubeSeg*)sector1->GetShape())->GetPhi1();
    Double_t phiMax1 = ((TGeoTubeSeg*)sector1->GetShape())->GetPhi2();
    fAngleSector1 = fabs(phiMax1 - phiMin1);
    Double_t phiMin2 = ((TGeoTubeSeg*)sector2->GetShape())->GetPhi1();
    Double_t phiMax2 = ((TGeoTubeSeg*)sector2->GetShape())->GetPhi2();
    fAngleSector2 = fabs(phiMax2 - phiMin2);

    TGeoVolume* emcCrate = (TGeoVolume*)gGeoManager->GetVolume("emc1Crate");
    Double_t phiCrateMin = ((TGeoTubeSeg*)emcCrate->GetShape())->GetPhi1();
    Double_t phiCrateMax = ((TGeoTubeSeg*)emcCrate->GetShape())->GetPhi2();

    Int_t iSector = 0, iRow1 = 0, iRow2 = 0, iBox = 0;
    const Double_t* fRotMatrix; 
    const Double_t* fTransMatrix;
    TGeoNode *sectorNode, *rowNode, *boxNode;
    TGeoMatrix *rowMatrix; 
    TGeoShape* boxShape; 
    TIterator* sectorIter; 
    Double_t anglePhi1, anglePhi2; 
    Double_t *master = new Double_t[3];

// Left / Right barell side 

    for (Int_t iSide = 0; iSide < 2; iSide++) {

      if (iSide == 0)  {
        pathUp = path1; gGeoManager->cd(path1);     	
        TObjArray *nodeChH1 = gGeoManager->GetCurrentVolume()->GetNodes();
        sectorIter = nodeChH1->MakeIterator();
      }
      if (iSide == 1) {
        pathUp = path2; gGeoManager->cd(path2);
        TObjArray *nodeChH2 = gGeoManager->GetCurrentVolume()->GetNodes();
        sectorIter = nodeChH2->MakeIterator();
      }
     	
// Sector level

       while( (sectorNode = (TGeoNode*) sectorIter->Next()) ) { 
         iSector++; pathSector = pathUp+"/"+sectorNode->GetName();
         
         TIterator *rowIter = sectorNode->GetNodes()->MakeIterator();
         gGeoManager->cd(pathSector);
         rowMatrix = gGeoManager->GetCurrentMatrix();
         fRotMatrix = rowMatrix->GetRotationMatrix();
         anglePhi1 = phiMin1; anglePhi2 = phiMax1; 
         if (strcmp(sectorNode->GetName(),"emc1Sector2") > 0) { 
          anglePhi1 = phiMin2; anglePhi2 = phiMax2; 
         }
         Double_t rotSector1 = ATan2(fRotMatrix[3],fRotMatrix[0])*RadToDeg();         
         phiSector.push_back(rotSector1+0.5*(anglePhi2-anglePhi1));
         if (phiSector[iSector-1] < 0) phiSector[iSector-1] += 360.;
         
         
// Row level
       
        while( (rowNode = (TGeoNode*)rowIter->Next()) ) { 
         pathCrate = pathSector+"/"+rowNode->GetName();
         gGeoManager->cd(pathCrate); 
         if (iSide == 0) {iRow1++; 
          rowMatrix = gGeoManager->GetCurrentMatrix();
          rowMatrix->LocalToMaster(rowNode->GetMatrix()->GetTranslation(), master);        
          fRotMatrix = rowMatrix->GetRotationMatrix();
          Double_t rotSector2 = ATan2(fRotMatrix[3],fRotMatrix[0])*RadToDeg();
          phiRow.push_back(rotSector2+0.5*(phiCrateMax - phiCrateMin));
          if (phiRow[iRow1-1] < 0) phiRow[iRow1-1] += 360.; 
         }
         if (iSide == 1) iRow2++;

// Box level (only one row)
	if ( ( iRow1 == 1) || (iRow2 == 1) ) { 
         TIterator *boxIter = rowNode->GetNodes()->MakeIterator();
	  while((boxNode = (TGeoNode*)boxIter->Next()) ) {
	   iBox++; pathBox = pathCrate+"/"+boxNode->GetName();
	   gGeoManager->cd(pathBox);
	   rowMatrix = gGeoManager->GetCurrentMatrix();
	   rowMatrix->LocalToMaster(boxNode->GetMatrix()->GetTranslation(), master);  	  
 	   fRotMatrix = rowMatrix->GetRotationMatrix();
	   fTransMatrix = rowMatrix->GetTranslation();
           boxShape = boxNode->GetVolume()->GetShape();

           if ( !boxShape->IsReflected() ) {
             Double_t *vertices = ((TGeoArb8*)boxShape)->GetVertices();	
	     sizeLowBox = 0.5*(vertices[4] - vertices[0]);
	     sizeHighBox = 0.5*(vertices[12] - vertices[8]);
             lengthBox = ((TGeoArb8*)boxShape)->GetDz();
           }

	   thetaBox.push_back(ACos(fRotMatrix[8])*RadToDeg());
           rhoCenterBox.push_back(sqrt(fTransMatrix[0]*fTransMatrix[0] + 
					fTransMatrix[1]*fTransMatrix[1]));
           zCenterBox.push_back(fTransMatrix[2]);
          } 
 	 }
       }
     }

   }

     nSec = phiSector.size(); nRows = phiRow.size(); nMod = thetaBox.size();

// Define simple rows' enumeration

   sort(phiRow.begin(), phiRow.end());

   vector<double> vTemp1, vTemp2,  vTemp3;
   vTemp1 = zCenterBox; vTemp2 = rhoCenterBox; vTemp3 = thetaBox; 
   Int_t iter = 0; 
   for (Int_t ind = 0; ind < nMod; ind++) {
     iter = ind-0.5*nMod;
     if (ind < 0.5*nMod) iter = nMod-ind-1;
     zCenterBox[ind] = vTemp1[iter]; 
     rhoCenterBox[ind] = vTemp2[iter]; 
     thetaBox[ind] = vTemp3[iter];
   } 


}

MpdEmcGeoParams::~MpdEmcGeoParams(void) {
}

void MpdEmcGeoParams::clear(void) {
  if(fGeoSensNodes) delete fGeoSensNodes;
  if(fGeoPassNodes) delete fGeoPassNodes;
}

void MpdEmcGeoParams::putParams(FairParamList* l) {
  if (!l) return;
  l->addObject("FairGeoNodes Sensitive List", fGeoSensNodes);
  l->addObject("FairGeoNodes Passive List", fGeoPassNodes);
}

Bool_t MpdEmcGeoParams::getParams(FairParamList* l) {
  if (!l) return kFALSE;
  if (!l->fillObject("FairGeoNodes Sensitive List", fGeoSensNodes)) return kFALSE;
  if (!l->fillObject("FairGeoNodes Passive List", fGeoPassNodes)) return kFALSE;
  return kTRUE;
}
