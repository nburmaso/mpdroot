#include <iostream>

#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TParticle.h"
#include "TVirtualMC.h"
#include "TGeoBBox.h"
#include "TGeoMatrix.h"

#include "FairGeoInterface.h"
#include "FairGeoLoader.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "MpdEtofGeo.h"
#include "FairRootManager.h"
#include "FairVolume.h"
#include "MpdEtofPoint.h"
#include "FairRuntimeDb.h"
#include "MpdEtofGeoPar.h"
#include "TObjArray.h"
#include "FairStack.h"
#include "FairRunAna.h"

#include "MpdEtof.h"
//------------------------------------------------------------------------------------------------------------------------
MpdEtof::MpdEtof(const char* name, Bool_t active)
 : FairDetector(name, active)
{  
	fTofCollection = new TClonesArray("MpdEtofPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdEtof::~MpdEtof() 
{
	if(fTofCollection){ fTofCollection->Delete(); delete fTofCollection; }
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t  MpdEtof::ProcessHits(FairVolume* vol)
{ 
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
		Int_t  region, module, pad;
				
		TString padname = vol->GetName(); padname.Remove(0, 7); pad = padname.Atoi();//gMC->CurrentVolOffID(1, pad);
		gMC->CurrentVolOffID(2, module);
		gMC->CurrentVolOffID(3, region);					
		fVolumeID = MpdEtofPoint::GetVolumeUID(region, module, pad);

		///cout<<"\n UID: "<<region<<" "<<module<<" "<<pad<<"   "<<fVolumeID<<" "<<vol->GetName();
		
		AddHit(fTrackID, fVolumeID, TVector3(fPos.X(),  fPos.Y(),  fPos.Z()), TVector3(fMom.Px(), fMom.Py(), fMom.Pz()), fTime, fLength, fELoss);

		((FairStack*)gMC->GetStack())->AddPoint(kETOF);

    		ResetParameters();
  	}

return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdEtof::EndOfEvent() 
{
	if(fVerboseLevel) Print();
  	fTofCollection->Delete();
  	fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdEtof::Register(){ FairRootManager::Instance()->Register("ETOFPoint", "Etof", fTofCollection, kTRUE); }
//------------------------------------------------------------------------------------------------------------------------
TClonesArray* MpdEtof::GetCollection(Int_t iColl) const 
{
	if(iColl == 0) 	return fTofCollection;
	
return NULL;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdEtof::Print() const 
{
	Int_t nHits = fTofCollection->GetEntriesFast();
	cout << "-I- MpdEtof: " << nHits << " points registered in this event." << endl;
	
	if(fVerboseLevel > 1) for(Int_t i=0; i<nHits; i++) (*fTofCollection)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------
void MpdEtof::Reset(){ fTofCollection->Delete(); ResetParameters(); }
//------------------------------------------------------------------------------------------------------------------------
void MpdEtof::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
	Int_t nEntries = cl1->GetEntriesFast();
	cout << "-I- MpdEtof: " << nEntries << " entries to add." << endl;
	TClonesArray& clref = *cl2;
	MpdEtofPoint* oldpoint = NULL;
	
	for(Int_t i=0; i<nEntries; i++) 
	{
		oldpoint = (MpdEtofPoint*) cl1->At(i);
		Int_t index = oldpoint->GetTrackID() + offset;
		oldpoint->SetTrackID(index);
		new (clref[fPosIndex]) MpdEtofPoint(*oldpoint);
		fPosIndex++;
	}
	
	cout << "-I- MpdEtof: " << cl2->GetEntriesFast() << " merged entries."  << endl;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdEtof::ConstructGeometry() 
{  
        FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
        FairGeoInterface* geoFace = geoLoad->getGeoInterface();
	MpdEtofGeo*       tofGeo  = new MpdEtofGeo();
	tofGeo->setGeomFile(GetGeometryFileName());
	geoFace->addGeoModule(tofGeo);

	Bool_t rc = geoFace->readSet(tofGeo);
	if(rc) tofGeo->create(geoLoad->getGeoBuilder());
	TList* volList = tofGeo->getListOfVolumes();

	// store geo parameter
        FairRun *fRun = FairRun::Instance();
        FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
	MpdEtofGeoPar* par =(MpdEtofGeoPar*)(rtdb->getContainer("MpdEtofGeoPar"));
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
MpdEtofPoint* MpdEtof::AddHit(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t time, Double_t length, Double_t eLoss) 
{
	return new((*fTofCollection)[fTofCollection->GetEntriesFast()]) MpdEtofPoint(trackID, detID, pos, mom, time, length, eLoss);
}
//------------------------------------------------------------------------------------------------------------------------
#include "TGeoManager.h"
Bool_t		MpdEtof::ParseTGeoManager(MpdTofUtils::RegVec& _Regions, MpdTofUtils::ModMMap& _Modules, MpdTofUtils::PadMap& _Pads)  
{
	cout<<"\n-I- [MpdEtof::ParseTGeoManager] Get volumes parameters from TGeoManager.\n"<<flush; 
	FairRunAna::Instance()->GetRuntimeDb()->getContainer("FairBaseParSet");
	assert(gGeoManager);

	MpdTofUtils::PadMap tmpMap; MpdTofUtils::padPar  paramP; MpdTofUtils::modPar paramM; MpdTofUtils::regionData paramR;
  	TGeoNode *reg, *mod, *pad, *node; TGeoMatrix *matrix; TGeoBBox *box; //TGeoTrap *trap; TGeoTube *tube;
  	Double_t  *local = new Double_t[3], master[3];
  	Double_t *pointA = new Double_t[3], *pointB = new Double_t[3], CavePointA[3], CavePointB[3], dX, dY, dZ;
	const Double_t *loc;
  	Int_t volumeUID, regID, modID, padID, nNodes=0;
	TString PATH0, PATH1, PATH2;
	TString buf, postfix="_0", RegGasName = "/et1m_",  PadGasName = "et1gas_"; 	
	
	const char* RegNames[]={"/cave_1/etof1_1", "/cave_1/etof1_2"};	
	const int RegNmb = 2;	
		
	for(int region = 0; region < RegNmb; region++) 					// REGIONs
	{
  		PATH0 = RegNames[region];
		regID = region + 1;
		
  		gGeoManager->cd(PATH0);	
		matrix = gGeoManager->GetCurrentMatrix();				// calculate global TGeoHMatrix for current node
				
		reg = gGeoManager->GetCurrentNode();
      		cout<<"\n REGION: "<<reg->GetName()<<", copy# "<<regID<<" ..."<<flush;

		loc = matrix->GetTranslation();		
		box = (TGeoBBox*)reg->GetVolume()->GetShape();
		dX = box->GetDX(); dY = box->GetDY();dZ = box->GetDZ();

		paramR.ID = regID;
		paramR.center.SetXYZ(loc[0], loc[1], loc[2]);
		paramR.point[0].SetXYZ(0,0,0);						// NOT USED(DUMMY)
		paramR.point[1].SetXYZ(0,0,0);
	      	paramR.point[2].SetXYZ(0,0,0);
	      	paramR.point[3].SetXYZ(0,0,0);
				
		_Regions.push_back(paramR);
				
  		TIterator *it1 = reg->GetNodes()->MakeIterator(); 
  		while( (mod = (TGeoNode*) it1->Next()) ) 				// MODULEs				
		{	
	  		modID = mod->GetNumber(); nNodes++;
			PATH1 = PATH0 + "/" + mod->GetName();
	  		///cout<<"\n MODULE: "<<mod->GetName()<<", copy# "<<modID<<"   "<<PATH1.Data(); 		

			gGeoManager->cd(PATH1);						// cd to module node
			node = gGeoManager->GetCurrentNode();			
      			matrix = gGeoManager->GetCurrentMatrix();			// calculate global TGeoHMatrix for current node
							
			box = (TGeoBBox*)mod->GetVolume()->GetShape();
			dX = box->GetDX(); dY = box->GetDY();dZ = box->GetDZ();
					
			local[0] = local[1] =  local[2] = 0.; 	
			matrix->LocalToMaster(local, master);				// module center point

			pointA[0] = local[0] + dX; pointA[1] = local[1]; pointA[2] = local[2] + dZ;  	// pad edge point A
			matrix->LocalToMaster(pointA, CavePointA);

			pointB[0] = local[0] - dX; pointB[1] = local[1]; pointB[2] = local[2] + dZ;  	// pad edge point B
			matrix->LocalToMaster(pointB, CavePointB);

			paramM.module = modID;
			paramM.center.SetXYZ(master[0], master[1], master[2]);
			paramM.point[0].SetXYZ(CavePointA[0], CavePointA[1], CavePointA[2]);
			paramM.point[1].SetXYZ(CavePointB[0], CavePointB[1], CavePointB[2]);
	      		paramM.point[2] = MpdTofUtils::_zerkalo(paramM.center, paramM.point[0]);
	      		paramM.point[3] = MpdTofUtils::_zerkalo(paramM.center, paramM.point[1]);
			paramM.perp = ( (paramM.point[0]-paramM.center).Cross(paramM.point[1]-paramM.center) ).Unit(); // unit perp.to module	

			_Modules.insert(MpdTofUtils::ModMMap::value_type(regID, paramM));

	  		TIterator *it2 = mod->GetNodes()->MakeIterator();
	  		while( (pad = (TGeoNode*) it2->Next()) )				// PADs
	    		{
	      			buf = TString(pad->GetName()).Remove(0, 7); buf.Remove(TString::kTrailing, '0'); buf.Remove(TString::kTrailing, '_');
				padID = buf.Atoi();				
				
				nNodes++;
				PATH2 = PATH1 + "/" + pad->GetName() + "/" + PadGasName.Data();  PATH2 += padID; PATH2 += postfix;
	      			///cout<<"\n Pad: "<<pad->GetName()<<", copy# "<<padID<<"   "<<buf.Data(); 	
	
				gGeoManager->cd(PATH2);
				node = gGeoManager->GetCurrentNode();
								
				box = (TGeoBBox*) node->GetVolume()->GetShape(); 		// pad<->node(sensitive gas volume)
				dX = box->GetDX(); dY = box->GetDY(); dZ = box->GetDZ();

	      			matrix = gGeoManager->GetCurrentMatrix();
	      			loc = node->GetMatrix()->GetTranslation(); 		

				local[0] = loc[0]; local[1] = loc[1] - dY; local[2] = loc[2]; 	// pad center point(inside plane) 
	      			matrix->LocalToMaster(local, master);							

				pointA[0] = local[0] + dX; pointA[1] = local[1]; pointA[2] = local[2] + dZ;   	// pad edge point A
	      			matrix->LocalToMaster(pointA, CavePointA);				
				
				pointB[0] = local[0] - dX; pointB[1] = local[1]; pointB[2] = local[2] + dZ;   	// pad edge point B
	      			matrix->LocalToMaster(pointB, CavePointB);				
				
				volumeUID =  MpdEtofPoint::GetVolumeUID(regID, modID, padID);
				
	      			paramP.region 	= regID;
	      			paramP.module 	= modID;
	      			paramP.pad 	= padID;				
	      			paramP.center.SetXYZ(master[0], master[1], master[2]);				
	      			paramP.point[0].SetXYZ(CavePointA[0], CavePointA[1], CavePointA[2]);
	      			paramP.point[1].SetXYZ(CavePointB[0], CavePointB[1], CavePointB[2]);
	      			paramP.point[2] = MpdTofUtils::_zerkalo(paramP.center, paramP.point[0]);
	      			paramP.point[3] = MpdTofUtils::_zerkalo(paramP.center, paramP.point[1]);
				paramP.neighboring[MpdTofUtils::Up] = MpdTofUtils::Absent;			// NOT USED(DUMMY)
				paramP.neighboring[MpdTofUtils::Right] = MpdTofUtils::Absent;				
				paramP.neighboring[MpdTofUtils::Down] = MpdTofUtils::Absent;
				paramP.neighboring[MpdTofUtils::Left] = MpdTofUtils::Absent;
								
	      			_Pads.insert(MpdTofUtils::PadMap::value_type(volumeUID, paramP));
												
	    		} // PADs
			
		} // MODULEs
			
		cout<<" done."<<flush;
    			
	} // REGIONs
		
  	cout<<"\n Processed "<<nNodes<<" nodes.\n";

return true;		
}
//------------------------------------------------------------------------------------------------------------------------

ClassImp(MpdEtof)
