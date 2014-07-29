//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                       MpdFfd source file                      -----
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

#include "MpdFfd.h"
#include "MpdFfdGeo.h"
#include "MpdFfdPoint.h"
#include "MpdFfdGeoPar.h"

class FairVolume;

//------------------------------------------------------------------------------------------------------------------------
MpdFfd::MpdFfd()
 : FairDetector("Ffd", kTRUE)
{
        fFfdCollection = new TClonesArray("MpdFfdPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdFfd::MpdFfd(const char* name, Bool_t active)
 : FairDetector(name, active)
{  
        fFfdCollection = new TClonesArray("MpdFfdPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdFfd::~MpdFfd()
{
        if(fFfdCollection){ fFfdCollection->Delete(); delete fFfdCollection; }
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t  MpdFfd::ProcessHits(FairVolume* vol)
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


        // Create MpdFfdPoint at exit of active volume
//	if(gMC->TrackCharge() != 0 &&  (gMC->IsTrackExiting() || gMC->IsTrackStop() || gMC->IsTrackDisappeared()) ) 
	
        // Create MpdFfdPoint at ENTER of active volume; fELoss INVALID!!!
	if(gMC->IsTrackEntering()) 
	{
		fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
		Volname = vol->getRealName();        // EL
		region = Volname[5] - '0';   //?????????????????????????
		gMC->CurrentVolID(gap);
		gMC->CurrentVolOffID(1, cell);
		gMC->CurrentVolOffID(2, module);
    
		fVolumeID = ((region-1)<<24);////////////// + ((module-1)<<14) + ((cell-1)<<4) + (gap-1);

		AddHit(fTrackID, fVolumeID, TVector3(fPos.X(),  fPos.Y(),  fPos.Z()),
	   		TVector3(fMom.Px(), fMom.Py(), fMom.Pz()), fTime, fLength, fELoss);

		((FairStack*)gMC->GetStack())->AddPoint(kFFD);

    		ResetParameters();
  	}


return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFfd::EndOfEvent()
{
	if(fVerboseLevel) Print();
        fFfdCollection->Clear();
  	fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFfd::Register(){ FairRootManager::Instance()->Register("FfdPoint", "Ffd", fFfdCollection, kTRUE); }
//------------------------------------------------------------------------------------------------------------------------
TClonesArray* MpdFfd::GetCollection(Int_t iColl) const
{
        if(iColl == 0) 	return fFfdCollection;
	
return NULL;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFfd::Print() const
{
        Int_t nHits = fFfdCollection->GetEntriesFast();
        cout << "-I- MpdFfd: " << nHits << " points registered in this event." << endl;
	
	if(fVerboseLevel > 1)
                for(Int_t i=0; i<nHits; i++) (*fFfdCollection)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFfd::Reset(){ fFfdCollection->Clear(); ResetParameters(); }
//------------------------------------------------------------------------------------------------------------------------
void MpdFfd::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
	Int_t nEntries = cl1->GetEntriesFast();
        cout << "-I- MpdFfd: " << nEntries << " entries to add." << endl;
	TClonesArray& clref = *cl2;
        MpdFfdPoint* oldpoint = NULL;
	
	for(Int_t i=0; i<nEntries; i++) 
	{
                oldpoint = (MpdFfdPoint*) cl1->At(i);
		Int_t index = oldpoint->GetTrackID() + offset;
		oldpoint->SetTrackID(index);
                new (clref[fPosIndex]) MpdFfdPoint(*oldpoint);
		fPosIndex++;
	}
	
        cout << "-I- MpdFfd: " << cl2->GetEntriesFast() << " merged entries."  << endl;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdFfd::ConstructGeometry()
{
  
	Int_t count=0;
	Int_t count_tot=0;

        FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
        FairGeoInterface* geoFace = geoLoad->getGeoInterface();
        MpdFfdGeo*       FfdGeo  = new MpdFfdGeo();
        FfdGeo->setGeomFile(GetGeometryFileName());
        geoFace->addGeoModule(FfdGeo);

        Bool_t rc = geoFace->readSet(FfdGeo);
        if(rc) FfdGeo->create(geoLoad->getGeoBuilder());
        TList* volList = FfdGeo->getListOfVolumes();

	// store geo parameter
        FairRun *fRun = FairRun::Instance();
        FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
        MpdFfdGeoPar* par =(MpdFfdGeoPar*)(rtdb->getContainer("MpdFfdGeoPar"));
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
MpdFfdPoint* MpdFfd::AddHit(Int_t trackID, Int_t detID, TVector3 pos,
			    TVector3 mom, Double_t time, Double_t length, 
			    Double_t eLoss) 
{
        TClonesArray& clref = *fFfdCollection;
	Int_t size = clref.GetEntriesFast();
	
return new(clref[size]) MpdFfdPoint(trackID, detID, pos, mom, time, length, eLoss);
}
//------------------------------------------------------------------------------------------------------------------------

ClassImp(MpdFfd)
