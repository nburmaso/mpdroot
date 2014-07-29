//------------------------------------------------------------------------------------------------------------------------
#include <iostream>
#include <algorithm>

#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TParticle.h"
#include "TVirtualMC.h"
#include "TObjArray.h"
#include "TGeoBBox.h"
#include "TGeoMatrix.h"

#include "FairGeoInterface.h"
#include "FairGeoLoader.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "FairRootManager.h"
#include "FairStack.h"
#include "FairRuntimeDb.h"
#include "FairRunAna.h"
#include "FairVolume.h"
#include "MpdTofGeo.h"
#include "MpdTofGeoPar.h"
#include "MpdTofPoint.h"

#include "MpdTof.h"
//------------------------------------------------------------------------------------------------------------------------
MpdTof::MpdTof(const char* name, Bool_t active)
 : FairDetector(name, active)
{  
	fTofCollection = new TClonesArray("MpdTofPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdTof::~MpdTof() 
{
	if(fTofCollection){ fTofCollection->Delete(); delete fTofCollection; }
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t  MpdTof::ProcessHits(FairVolume* vol)
{
	Int_t  region, module, pad; 

	// Set parameters at entrance of volume. Reset ELoss.
	if(gMC->IsTrackEntering()) 
	{
		fELoss  = 0.;
		fTime   = gMC->TrackTime() * 1.0e09;
		fLength = gMC->TrackLength();
		gMC->TrackPosition(fPos);
		gMC->TrackMomentum(fMom);		
	}

	// Sum energy loss for all steps in the active volume
	fELoss += gMC->Edep();
	
	// Create MpdTofPoint at exit of active volume
	if(fELoss > 0 &&  (gMC->IsTrackExiting() || gMC->IsTrackStop() || gMC->IsTrackDisappeared()) ) 
	{	
		fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
		
		gMC->CurrentVolOffID(1, pad);
		gMC->CurrentVolOffID(2, module);
		gMC->CurrentVolOffID(4, region);
		fVolumeID = MpdTofPoint::GetVolumeUID(region, module, pad);

		AddHit(fTrackID, fVolumeID, TVector3(fPos.X(),  fPos.Y(),  fPos.Z()),
	   		TVector3(fMom.Px(), fMom.Py(), fMom.Pz()), fTime, fLength, fELoss);

		((FairStack*)gMC->GetStack())->AddPoint(kTOF);

    		ResetParameters();
  	}

return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::EndOfEvent() 
{
	if(fVerboseLevel) Print();
  	fTofCollection->Delete();
  	fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::Register(){ FairRootManager::Instance()->Register("TOFPoint", "Tof", fTofCollection, kTRUE); }
//------------------------------------------------------------------------------------------------------------------------
TClonesArray* MpdTof::GetCollection(Int_t iColl) const 
{
	if(iColl == 0) 	return fTofCollection;
	
return NULL;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::Print() const 
{
	Int_t nHits = fTofCollection->GetEntriesFast();
	cout << "-I- MpdTof: " << nHits << " points registered in this event." << endl;
	
	if(fVerboseLevel > 1)
    		for(Int_t i=0; i<nHits; i++) (*fTofCollection)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::Reset(){ fTofCollection->Delete(); ResetParameters(); }
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
	Int_t nEntries = cl1->GetEntriesFast();
	cout << "-I- MpdTof: " << nEntries << " entries to add." << endl;
	TClonesArray& clref = *cl2;
	MpdTofPoint* oldpoint = NULL;
	
	for(Int_t i=0; i<nEntries; i++) 
	{
		oldpoint = (MpdTofPoint*) cl1->At(i);
		Int_t index = oldpoint->GetTrackID() + offset;
		oldpoint->SetTrackID(index);
		new (clref[fPosIndex]) MpdTofPoint(*oldpoint);
		fPosIndex++;
	}
	
	cout << "-I- MpdTof: " << cl2->GetEntriesFast() << " merged entries."  << endl;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::ConstructGeometry() 
{
        FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
        FairGeoInterface* geoFace = geoLoad->getGeoInterface();
	MpdTofGeo*       tofGeo  = new MpdTofGeo();
	tofGeo->setGeomFile(GetGeometryFileName());
	geoFace->addGeoModule(tofGeo);

	Bool_t rc = geoFace->readSet(tofGeo);
	if(rc) tofGeo->create(geoLoad->getGeoBuilder());
	TList* volList = tofGeo->getListOfVolumes();

	// store geo parameter
        FairRun *fRun = FairRun::Instance();
        FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
	MpdTofGeoPar* par =(MpdTofGeoPar*)(rtdb->getContainer("MpdTofGeoPar"));
	TObjArray *fSensNodes = par->GetGeoSensitiveNodes();
	TObjArray *fPassNodes = par->GetGeoPassiveNodes();

        FairGeoNode *node   = NULL;
        FairGeoVolume *aVol = NULL;
	TListIter iter(volList);			
	
        while((node = (FairGeoNode*)iter.Next()))
	{
                aVol = dynamic_cast<FairGeoVolume*> (node);
		if(node->isSensitive()) 	fSensNodes->AddLast(aVol); 
		else           		 	fPassNodes->AddLast(aVol);	
  	}

	par->setChanged();
	par->setInputVersion(fRun->GetRunId(), 1);  
	ProcessNodes(volList);
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofPoint* MpdTof::AddHit(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t time, Double_t length, Double_t eLoss) 
{	
	return new((*fTofCollection)[fTofCollection->GetEntriesFast()]) MpdTofPoint(trackID, detID, pos, mom, time, length, eLoss);
}
//------------------------------------------------------------------------------------------------------------------------
#include "TGeoManager.h"
Bool_t	MpdTof::ParseTGeoManager(TH2D *h2, MpdTofUtils::RegVec& _Regions, MpdTofUtils::ModMMap& _Modules, MpdTofUtils::PadMap& _Pads)
{
	cout<<"\n-I- [MpdTofUtils::ParseTGeoManager] Get volumes parameters from TGeoManager.\n"<<flush; 

	FairRunAna::Instance()->GetRuntimeDb()->getContainer("FairBaseParSet");
	assert(gGeoManager);

	static const double Pi = TMath::Pi();

 	MpdTofUtils::PadMap tmpMap; MpdTofUtils::padPar  paramC; MpdTofUtils::modPar paramM; MpdTofUtils::regionData region;
  	TGeoNode *reg, *mod, *cell, *node; TGeoMatrix *matrix; TGeoBBox *box;
        Double_t  *local = new Double_t[3], master[3];
  	Double_t *pointA = new Double_t[3], *pointB = new Double_t[3], CavePointA[3], CavePointB[3], dX, dY, dZ;
 	Double_t *X0Y0Z0 = new Double_t[3]; X0Y0Z0[0] = X0Y0Z0[1] = X0Y0Z0[2] = 0.;		
	X0Y0Z0[1] = -0.75; //AZ
	Double_t shift = -0.75; //AZ
	const Double_t *loc1, *XYZ;
  	Int_t volumeUID, regID, modID, cellID;
	
  	TString PATH1,PATH2,  PATH3, PATH0 = "/cave_1/tof1_0";
  	gGeoManager->cd(PATH0);
	
	TString RegGasName, PadGasName;
	RegGasName = "/t1regGas_1"; PadGasName = "t1gas_1"; 
	

	
  	TObjArray *array = gGeoManager->GetCurrentVolume()->GetNodes();
  	TIterator *it1 = array->MakeIterator(); int nNodes = 0;	
  	while( (reg = (TGeoNode*) it1->Next()) )			// REGIONS
    	{		
      		PATH1 = PATH0 + "/" + reg->GetName() + RegGasName.Data(); regID = reg->GetNumber(); nNodes++;
      		cout<<"\n REGION: "<<reg->GetName()<<", copy# "<<regID<<" ..."<<flush;

		XYZ = reg->GetMatrix()->GetTranslation();		// regions - only translation in MRS
		box = (TGeoBBox*)reg->GetVolume()->GetShape();
		dX = box->GetDX(); dY = box->GetDY();dZ = box->GetDZ();

		pointA[0] = dX; pointA[1] = 0.; pointA[2] = dZ;  		// region middle plane( Y = 0.)
		reg->GetMatrix()->LocalToMaster(pointA, CavePointA);

		pointB[0] = -dX; pointB[1] = 0.; pointB[2] = dZ;
		reg->GetMatrix()->LocalToMaster(pointB, CavePointB);

		region.ID = regID;
		region.center.SetXYZ(XYZ[0], XYZ[1], XYZ[2]);
		region.point[0].SetXYZ(CavePointA[0], CavePointA[1], CavePointA[2]);
		region.point[1].SetXYZ(CavePointB[0], CavePointB[1], CavePointB[2]);
	      	region.point[2] = MpdTofUtils::_zerkalo(region.center, region.point[0]);
	      	region.point[3] = MpdTofUtils::_zerkalo(region.center, region.point[1]);

		double phi0 = region.point[0].Phi()+Pi, phi1 = region.point[1].Phi()+Pi, phi2 =region.point[2].Phi()+Pi, phi3 =region.point[3].Phi()+Pi;
		region.minPhi =  min(phi0, 	min(phi1, 	min(phi2, phi3)));
		region.maxPhi = max(phi0, 	max(phi1, 	max(phi2, phi3)));

		_Regions.push_back(region);

      		TIterator *it2 = reg->GetDaughter(0)->GetNodes()->MakeIterator(); nNodes++;		
      		while( (mod = (TGeoNode*) it2->Next()) )		// MODULES
		{
	  		PATH2 = PATH1 + "/" + mod->GetName(); modID = mod->GetNumber(); nNodes++;
	  		//cout<<"\n MODULE: "<<mod->GetName()<<", copy# "<<modID; 		

			gGeoManager->cd(PATH2);				// cd to module node
      			matrix = gGeoManager->GetCurrentMatrix();	// calculate global TGeoHMatrix for current node
			matrix->LocalToMaster(X0Y0Z0, master);		// 0.0.0. --> MRS

			box = (TGeoBBox*)mod->GetVolume()->GetShape();
			dX = box->GetDX(); dY = box->GetDY();dZ = box->GetDZ();

			pointA[0] = dX; pointA[1] = 0.; pointA[2] = dZ;  	// module middle plane( Y = 0.)
			pointA[1] += shift; //AZ
			matrix->LocalToMaster(pointA, CavePointA);

			pointB[0] = -dX; pointB[1] = 0.; pointB[2] = dZ;
			pointB[1] += shift; //AZ
			matrix->LocalToMaster(pointB, CavePointB);

			paramM.module = modID;
			paramM.center.SetXYZ(master[0], master[1], master[2]);
			paramM.point[0].SetXYZ(CavePointA[0], CavePointA[1], CavePointA[2]);
			paramM.point[1].SetXYZ(CavePointB[0], CavePointB[1], CavePointB[2]);
	      		paramM.point[2] = MpdTofUtils::_zerkalo(paramM.center, paramM.point[0]);
	      		paramM.point[3] = MpdTofUtils::_zerkalo(paramM.center, paramM.point[1]);
			paramM.perp = ( (paramM.point[0]-paramM.center).Cross(paramM.point[1]-paramM.center) ).Unit(); // unit perp.to module	

			_Modules.insert(MpdTofUtils::ModMMap::value_type(regID, paramM));
	  		tmpMap.clear();

	  		TIterator *it3 = mod->GetNodes()->MakeIterator();
	  		while( (cell = (TGeoNode*) it3->Next()) )	// CELLs
	    		{
	      			PATH3 = PATH2 + "/" + cell->GetName() + "/" + PadGasName.Data(); cellID = cell->GetNumber(); nNodes+=3;
	      			//cout<<"\n SELL: "<<cell->GetName()<<", copy# "<<cellID; 
	
	      			node = cell->GetVolume()->GetNode(PadGasName.Data()); assert(node);
								
				box = (TGeoBBox*)cell->GetVolume()->GetShape(); 		// cell<->node (sensitive gas volume)
				dX = box->GetDX(); dY = box->GetDY(); dZ = box->GetDZ();
				
	      			gGeoManager->cd(PATH3);
	      			matrix = gGeoManager->GetCurrentMatrix();

	      			loc1 = node->GetMatrix()->GetTranslation(); 		
	      			// if(detType == IsBarrel)	{local[0] = loc1[0];	local[1] = loc1[1] + dY; 	local[2] = loc1[2];}
	      			local[0] = loc1[0];	local[1] = loc1[1] + dY - 0.2; 	local[2] = loc1[2]; //AZ
	      			matrix->LocalToMaster(local, master);
								
	      			pointA[0] = local[0] + dX; pointA[1] = local[1];	pointA[2] = local[2] + dZ; // TOF XZ pad 2.5 x 3.5 cm
	      			matrix->LocalToMaster(pointA, CavePointA);				
				
				
	      			pointB[0] = local[0] - dX; pointB[1] = local[1]; 	pointB[2] = local[2] + dZ; // TOF XZ pad 2.5 x 3.5 cm
	      			matrix->LocalToMaster(pointB, CavePointB);				
				
	      			volumeUID =  MpdTofPoint::GetVolumeUID(regID, modID, cellID);
				
	      			paramC.region 	= regID;
	      			paramC.module 	= modID;
	      			paramC.pad 	= cellID;				
	      			paramC.center.SetXYZ(master[0], master[1], master[2]);				
	      			paramC.point[0].SetXYZ(CavePointA[0], CavePointA[1], CavePointA[2]);
	      			paramC.point[1].SetXYZ(CavePointB[0], CavePointB[1], CavePointB[2]);
	      			paramC.point[2] = MpdTofUtils::_zerkalo(paramC.center, paramC.point[0]);
	      			paramC.point[3] = MpdTofUtils::_zerkalo(paramC.center, paramC.point[1]);
	      			tmpMap.insert(MpdTofUtils::PadMap::value_type(volumeUID, paramC));				
	    		}

	  		MpdTofUtils::FindCrossPads(tmpMap, h2);
			_Pads.insert(tmpMap.begin(), tmpMap.end()); // copy entry from tmp container
		}	
		cout<<" done."<<flush;
    	}		
  	cout<<"\n Processed "<<nNodes<<" nodes.";

return true;		
}
//------------------------------------------------------------------------------------------------------------------------

ClassImp(MpdTof)
