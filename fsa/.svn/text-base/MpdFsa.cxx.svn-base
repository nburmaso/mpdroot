//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                       MpdFsa source file                      -----
// -------------------------------------------------------------------------

#include <iostream>

#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TParticle.h"
#include "TVirtualMC.h"

//#include "FairDetectorList.h"
#include "FairGeoInterface.h"
#include "FairGeoLoader.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "FairRootManager.h"
#include "FairStack.h"
#include "FairRuntimeDb.h"
#include "TObjArray.h"
#include "FairRun.h"
#include "FairVolume.h"

#include "MpdFsa.h"
#include "MpdFsaGeo.h"
#include "MpdFsaPoint.h"
#include "MpdFsaGeoPar.h"

class FairVolume;

//------------------------------------------------------------------------------------------------------------------------
MpdFsa::MpdFsa() 
 : FairDetector("FSA", kTRUE)
{
	fFsaCollection = new TClonesArray("MpdFsaPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdFsa::MpdFsa(const char* name, Bool_t active)
 : FairDetector(name, active)
{  
	fFsaCollection = new TClonesArray("MpdFsaPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdFsa::~MpdFsa() 
{
	if(fFsaCollection){ fFsaCollection->Delete(); delete fFsaCollection; }
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t  MpdFsa::ProcessHits(FairVolume* vol)
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


	// Create MpdFsaPoint at exit of active volume
//	if(gMC->TrackCharge() != 0 &&  (gMC->IsTrackExiting() || gMC->IsTrackStop() || gMC->IsTrackDisappeared()) ) 
	
	// Create MpdFsaPoint at ENTER of active volume; fELoss INVALID!!!
	if(gMC->IsTrackEntering()) 
	{
		fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
		Volname = vol->getRealName();     // EL
		region = Volname[5] - '0';   //?????????????????????????
		gMC->CurrentVolID(gap);
		gMC->CurrentVolOffID(1, cell);
		gMC->CurrentVolOffID(2, module);
    
		fVolumeID = ((region-1)<<24);////////////// + ((module-1)<<14) + ((cell-1)<<4) + (gap-1);

		AddHit(fTrackID, fVolumeID, TVector3(fPos.X(),  fPos.Y(),  fPos.Z()),
	   		TVector3(fMom.Px(), fMom.Py(), fMom.Pz()), fTime, fLength, fELoss);

		((FairStack*)gMC->GetStack())->AddPoint(kFSA);

    		ResetParameters();
  	}


return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFsa::EndOfEvent() 
{
	if(fVerboseLevel) Print();
  	fFsaCollection->Clear();
  	fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFsa::Register(){ FairRootManager::Instance()->Register("FSAPoint", "Fsa", fFsaCollection, kTRUE); }
//------------------------------------------------------------------------------------------------------------------------
TClonesArray* MpdFsa::GetCollection(Int_t iColl) const 
{
	if(iColl == 0) 	return fFsaCollection;
	
return NULL;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFsa::Print() const 
{
	Int_t nHits = fFsaCollection->GetEntriesFast();
	cout << "-I- MpdFsa: " << nHits << " points registered in this event." << endl;
	
	if(fVerboseLevel > 1)
    		for(Int_t i=0; i<nHits; i++) (*fFsaCollection)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFsa::Reset(){ fFsaCollection->Clear(); ResetParameters(); }
//------------------------------------------------------------------------------------------------------------------------
void MpdFsa::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
	Int_t nEntries = cl1->GetEntriesFast();
	cout << "-I- MpdFsa: " << nEntries << " entries to add." << endl;
	TClonesArray& clref = *cl2;
	MpdFsaPoint* oldpoint = NULL;
	
	for(Int_t i=0; i<nEntries; i++) 
	{
		oldpoint = (MpdFsaPoint*) cl1->At(i);
		Int_t index = oldpoint->GetTrackID() + offset;
		oldpoint->SetTrackID(index);
		new (clref[fPosIndex]) MpdFsaPoint(*oldpoint);
		fPosIndex++;
	}
	
	cout << "-I- MpdFsa: " << cl2->GetEntriesFast() << " merged entries."  << endl;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFsa::ConstructGeometry() 
{
  
	Int_t count=0;
	Int_t count_tot=0;

        FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
        FairGeoInterface* geoFace = geoLoad->getGeoInterface();
	MpdFsaGeo*       fsaGeo  = new MpdFsaGeo();
	fsaGeo->setGeomFile(GetGeometryFileName());
	geoFace->addGeoModule(fsaGeo);

	Bool_t rc = geoFace->readSet(fsaGeo);
	if(rc) fsaGeo->create(geoLoad->getGeoBuilder());
	TList* volList = fsaGeo->getListOfVolumes();

	// store geo parameter
        FairRun *fRun = FairRun::Instance();
        FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
	MpdFsaGeoPar* par =(MpdFsaGeoPar*)(rtdb->getContainer("MpdFsaGeoPar"));
	TObjArray *fSensNodes = par->GetGeoSensitiveNodes();
	TObjArray *fPassNodes = par->GetGeoPassiveNodes();

        FairGeoNode *node   = NULL;
        FairGeoVolume *aVol = NULL;
	TListIter iter(volList);
	
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
MpdFsaPoint* MpdFsa::AddHit(Int_t trackID, Int_t detID, TVector3 pos,
			    TVector3 mom, Double_t time, Double_t length, 
			    Double_t eLoss) 
{
	TClonesArray& clref = *fFsaCollection;
	Int_t size = clref.GetEntriesFast();
	
return new(clref[size]) MpdFsaPoint(trackID, detID, pos, mom, time, length, eLoss);
}
//------------------------------------------------------------------------------------------------------------------------

ClassImp(MpdFsa)
