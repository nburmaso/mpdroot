//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                       MpdCpc source file                      -----
// -------------------------------------------------------------------------

#include "iostream"

#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TParticle.h"
#include "TVirtualMC.h"
#include "MpdMCTrack.h"

#include "FairGeoInterface.h"
#include "FairGeoLoader.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "FairRootManager.h"
#include "MpdStack.h"
#include "FairRuntimeDb.h"
#include "TObjArray.h"
#include "FairRun.h"
#include "FairVolume.h"

#include "MpdCpc.h"
#include "MpdCpcGeo.h"
#include "MpdCpcPoint.h"
#include "MpdCpcGeoPar.h"

class FairVolume;

//------------------------------------------------------------------------------------------------------------------------
MpdCpc::MpdCpc() 
 : FairDetector("CPC", kTRUE)
{
	fCpcCollection = new TClonesArray("MpdCpcPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdCpc::MpdCpc(const char* name, Bool_t active)
 : FairDetector(name, active)
{  
	fCpcCollection = new TClonesArray("MpdCpcPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdCpc::~MpdCpc() 
{
	if(fCpcCollection){ fCpcCollection->Delete(); delete fCpcCollection; }
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t  MpdCpc::ProcessHits(FairVolume* vol)
{
	Int_t gap, cell, module, region;
	TString Volname;

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

	// Create MpdCpcPoint at ENTER of active volume; fELoss INVALID!!!
	//AZ if(gMC->IsTrackEntering()) 

	// Create MpdCpcPoint at exit of active volume
	if ((gMC->IsTrackExiting() || gMC->IsTrackStop() || gMC->IsTrackDisappeared()) && fELoss > 0) 
	{
		fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
		//Volname = vol->getRealName();         // EL
		//region = Volname[5] - '0';   //?????????????????????????
		//gMC->CurrentVolID(gap);
		//gMC->CurrentVolOffID(1, cell);
		//gMC->CurrentVolOffID(2, module);
    
		//fVolumeID = ((region-1)<<24);////////////// + ((module-1)<<14) + ((cell-1)<<4) + (gap-1);
		fVolumeID = vol->getMCid();

		AddHit(fTrackID, fVolumeID, TVector3(fPos.X(),  fPos.Y(),  fPos.Z()),
	   		TVector3(fMom.Px(), fMom.Py(), fMom.Pz()), fTime, fLength, fELoss);

		((MpdStack*)gMC->GetStack())->AddPoint(kCPC);

    		ResetParameters();
  	}


return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdCpc::EndOfEvent() 
{
	if(fVerboseLevel) Print();
  	fCpcCollection->Delete();
  	fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdCpc::Register(){ FairRootManager::Instance()->Register("CPCPoint", "Cpc", fCpcCollection, kTRUE); }
//------------------------------------------------------------------------------------------------------------------------
TClonesArray* MpdCpc::GetCollection(Int_t iColl) const 
{
	if(iColl == 0) 	return fCpcCollection;
	
return NULL;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdCpc::Print() const 
{
	Int_t nHits = fCpcCollection->GetEntriesFast();
	cout << "-I- MpdCpc: " << nHits << " points registered in this event." << endl;
	
	if(fVerboseLevel > 1)
    		for(Int_t i=0; i<nHits; i++) (*fCpcCollection)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------
void MpdCpc::Reset(){ fCpcCollection->Delete(); ResetParameters(); }
//------------------------------------------------------------------------------------------------------------------------
void MpdCpc::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
	Int_t nEntries = cl1->GetEntriesFast();
	cout << "-I- MpdCpc: " << nEntries << " entries to add." << endl;
	TClonesArray& clref = *cl2;
	MpdCpcPoint* oldpoint = NULL;
	
	for(Int_t i=0; i<nEntries; i++) 
	{
		oldpoint = (MpdCpcPoint*) cl1->At(i);
		Int_t index = oldpoint->GetTrackID() + offset;
		oldpoint->SetTrackID(index);
		new (clref[fPosIndex]) MpdCpcPoint(*oldpoint);
		fPosIndex++;
	}
	
	cout << "-I- MpdCpc: " << cl2->GetEntriesFast() << " merged entries."  << endl;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdCpc::ConstructGeometry() 
{
	TString fileName = GetGeometryFileName();

	if ( fileName.EndsWith(".root") ) {
		LOG(INFO) << "Constructing CPC geometry from ROOT file " << fileName.Data() << endl;
		ConstructRootGeometry();
	}
	else if ( fileName.EndsWith(".geo") ) {
		LOG(INFO) << "Constructing CPC geometry from ASCII file " << fileName.Data() << endl;
		ConstructAsciiGeometry();
	}
	/*else if ( fileName.EndsWith(".gdml") )
	{
		LOG(INFO) << "Constructing CPC geometry from GDML file " << fileName.Data() << endl;
		ConstructGDMLGeometry();
	}*/
	else
	{
		LOG(FATAL) << "Geometry format of CPC file " << fileName.Data() << " not supported." << endl;
	}
}
//------------------------------------------------------------------------------------------------------------------------
void MpdCpc::ConstructAsciiGeometry()
{
  
	int count=0;
	int count_tot=0;

        FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
        FairGeoInterface* geoFace = geoLoad->getGeoInterface();
	MpdCpcGeo*       Geo  = new MpdCpcGeo();
	Geo->setGeomFile(GetGeometryFileName());
	geoFace->addGeoModule(Geo);

	Bool_t rc = geoFace->readSet(Geo);
	if(rc) Geo->create(geoLoad->getGeoBuilder());
	else std::cerr<<"CpcDetector:: geometry could not be read!"<<std::endl;

	TList* volList = Geo->getListOfVolumes();

	// store geo parameter
        FairRun *fRun = FairRun::Instance();
        FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
	MpdCpcGeoPar* par =(MpdCpcGeoPar*)(rtdb->getContainer("MpdCpcGeoPar"));
	TObjArray *fSensNodes = par->GetGeoSensitiveNodes();
	TObjArray *fPassNodes = par->GetGeoPassiveNodes();

	TListIter iter(volList);
	FairGeoNode *node   = NULL;
        FairGeoVolume *aVol = NULL;
	
        while((node = (FairGeoNode*)iter.Next()))
	{
                aVol = dynamic_cast<FairGeoVolume*> (node);
		if(node->isSensitive()){ 	fSensNodes->AddLast(aVol); count++; }
		else           		 	fPassNodes->AddLast(aVol);      
       		count_tot++;
  	}
	
	par->setChanged();
	par->setInputVersion(fRun->GetRunId(), 1);

	ProcessNodes(volList);
}
//------------------------------------------------------------------------------------------------------------------------
MpdCpcPoint* MpdCpc::AddHit(Int_t trackID, Int_t detID, TVector3 pos,
			    TVector3 mom, Double_t time, Double_t length, 
			    Double_t eLoss) 
{
	TClonesArray& clref = *fCpcCollection;
	Int_t size = clref.GetEntriesFast();
	
return new(clref[size]) MpdCpcPoint(trackID, detID, pos, mom, time, length, eLoss);
}
//------------------------------------------------------------------------------------------------------------------------
//Check if Sensitive-----------------------------------------------------------
Bool_t MpdCpc::CheckIfSensitive(std::string name) {
	TString tsname = name;
	if (tsname.Contains("Active") || tsname.Contains("cpc01al")) {
		return kTRUE;
	}
	return kFALSE;
}
//------------------------------------------------------------------------------------------------------------------------

ClassImp(MpdCpc)
