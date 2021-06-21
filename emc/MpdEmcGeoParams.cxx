////////////////////////////////////////////////////////////////
//                                                            //
//  MpdEmcGeoParams                                           //
//  EMC geometry in MpdEmcHitCreation, v02                    //
//  Author List : Martemianov M., 2019 		              //
//                                                            //
////////////////////////////////////////////////////////////////

#include "MpdEmcGeoParams.h"
#include "FairParamList.h"
#include "TObjArray.h"
#include <iostream>

#include "TGeoNode.h"
#include "TGeoManager.h"
#include "TGeoTube.h"
#include "TGeoPgon.h"
#include "TGeoPcon.h"
#include "TGeoArb8.h"

ClassImp(MpdEmcGeoParams)

MpdEmcGeoParams ::MpdEmcGeoParams(const char* name, const char* title, const char* context)
  : FairParGenericSet(name,title,context) {

  fGeoSensNodes = new TObjArray();
  fGeoPassNodes = new TObjArray();

}

MpdEmcGeoParams::MpdEmcGeoParams() {

   TString pathCave= "/cave_1";
   TString pathChamber = pathCave + "/emcChamber_0";
  
// New EMC version 

    TString path1, path2, pathSector, pathRow, pathBox;
    TGeoNode *sectorNode, *rowNode, *modNode, *boxNode;
    path1 = pathChamber+"/emcChH_0"; path2 = pathChamber+"/emcChH_1";
   
    gGeoManager->cd(path1);

// Total barell 

    length = 2.*((TGeoTube*)gGeoManager->GetCurrentVolume()->GetShape())->GetDz();
    rMin = ((TGeoTube*)gGeoManager->GetCurrentVolume()->GetShape())->GetRmin();    
    rMax = ((TGeoTube*)gGeoManager->GetCurrentVolume()->GetShape())->GetRmax();

    TGeoVolume* emcRow = (TGeoVolume*)gGeoManager->GetVolume("emcCrate");
    fAngleCrate = ((TGeoPgon*)emcRow->GetShape())->GetDphi();

    Double_t rotAngle;  
    Int_t nSector = 0, nRow = 0, nBox = 0;  
    TGeoVolume* emcChH = (TGeoVolume*)gGeoManager->GetCurrentVolume();
    TIterator* sectorIter = emcChH->GetNodes()->MakeIterator();
    const Double_t* fRotMatrix; 
    const Double_t* fTransMatrix;  
    Double_t master[3];
    TGeoMatrix *rowMatrix;
    TGeoShape* boxShape;

    while( (sectorNode = (TGeoNode*) sectorIter->Next()) )
      if ( ((TString)(sectorNode->GetName())).Contains("emcSector")) nSector++;
    fAngleSector = 360./nSector; nSec = nSector; 
  
    nSector = 0; sectorIter->Reset(); 
    while( (sectorNode = (TGeoNode*)sectorIter->Next()) ) {
      pathSector = path1+"/"+sectorNode->GetName();
      if (((TString)(sectorNode->GetName())).Contains("emcSector")) { nSector++;
        gGeoManager->cd(pathSector);
        fRotMatrix = gGeoManager->GetCurrentMatrix()->GetRotationMatrix();
        rotAngle = ATan2(fRotMatrix[3],fRotMatrix[0])*RadToDeg();         
        phiSector.push_back(rotAngle + 0.5*fAngleSector);
        if (phiSector[nSector-1] < 0) phiSector[nSector-1] += 360.;

// Row level

       TIterator* rowIter = sectorNode->GetNodes()->MakeIterator();
        while( (rowNode = (TGeoNode*)rowIter->Next()) ) { 
        pathRow = pathSector+"/"+rowNode->GetName();
        if (((TString)(rowNode->GetName())).Contains("emcCrate")){nRow++;
         gGeoManager->cd(pathRow);
         rowMatrix = gGeoManager->GetCurrentMatrix();
         rowMatrix->LocalToMaster(rowNode->GetMatrix()->GetTranslation(), master);
         fRotMatrix = rowMatrix->GetRotationMatrix(); 
         rotAngle = ATan2(fRotMatrix[3],fRotMatrix[0])*RadToDeg();
         if (rotAngle < 0) rotAngle += 360.; 
         fTransMatrix = rowMatrix->GetTranslation(); 
         phiRow.push_back(rotAngle + 0.5*fAngleCrate);
         xRow.push_back(fTransMatrix[0]); yRow.push_back(fTransMatrix[1]);
         if (phiRow[nRow-1] < 0) phiRow[nRow-1] += 360.;

// Box level 

         TIterator* moduleIter = rowNode->GetNodes()->MakeIterator(); 
          while ( (modNode = (TGeoNode*)moduleIter->Next()) ) {
	   	
           if (((TString)(modNode->GetName())).Contains("emcModule")){
           TIterator* boxIter = modNode->GetNodes()->MakeIterator();
            while ( (boxNode = (TGeoNode*)boxIter->Next()) ) {nBox++;        
             pathBox = pathRow+"/"+modNode->GetName()+"/"+boxNode->GetName();
              if (((TString)(boxNode->GetName())).Contains("emc_box")){
	       gGeoManager->cd(pathBox);
               rowMatrix = gGeoManager->GetCurrentMatrix();
               rowMatrix->LocalToMaster(boxNode->GetMatrix()->GetTranslation(), master);
               fRotMatrix = rowMatrix->GetRotationMatrix();
	       fTransMatrix = rowMatrix->GetTranslation();
	       phiBox.push_back(ATan2(fRotMatrix[3],fRotMatrix[0])*RadToDeg());
               thetaBox.push_back(ACos(fRotMatrix[8])*RadToDeg()); 
               xBox.push_back(fTransMatrix[0]); yBox.push_back(fTransMatrix[1]);                
               zBox.push_back(fTransMatrix[2]); 
               rhoBox.push_back(sqrt(fTransMatrix[0]*fTransMatrix[0]+fTransMatrix[1]*fTransMatrix[1]));
               boxShape = boxNode->GetVolume()->GetShape();
               if  (((TString)(boxShape->GetName())).Contains("TGeoArb8")) {
                  Double_t *vertices = ((TGeoArb8*)boxShape)->GetVertices();
                  sizeLowBox = 0.5*(vertices[4] - vertices[0]);
                  sizeHighBox = 0.5*(vertices[12] - vertices[8]);
                  lengthBox = ((TGeoArb8*)boxShape)->GetDz();
               }
	      }
            }
           }
          }

//// end box level

        }
       }  
      }
     }  nRows = nRow; nTowers = nBox;  

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