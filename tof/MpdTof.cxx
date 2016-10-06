
//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTof, LRectangle, LStrip
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include <iostream>
#include <algorithm>
#include <limits>

#include <TClonesArray.h>
#include <TVirtualMC.h>
#include <TGeoManager.h>
#include <TGeoBBox.h>
#include <TGeoMatrix.h>

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

ClassImp(MpdTof)
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
MpdTof::MpdTof(const char* name, Bool_t active)
 : FairDetector(name, active), nan(std::numeric_limits<double>::quiet_NaN())
{  
	aTofHits = new TClonesArray("MpdTofPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdTof::~MpdTof() 
{
	if(aTofHits){ aTofHits->Delete(); delete aTofHits; }
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t  MpdTof::ProcessHits(FairVolume* vol)
{
	Int_t  strip, detector, box, sector; 

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
		
		gMC->CurrentVolOffID(0, strip);
		gMC->CurrentVolOffID(1, detector);
		box = 1;//gMC->CurrentVolOffID(3, box);
		gMC->CurrentVolOffID(4, sector);		
					
		fVolumeID = MpdTofPoint::GetVolumeUID(sector, box, detector, strip);

		AddPoint(fTrackID, fVolumeID, fPos.Vect(), fMom.Vect(), fTime, fLength, fELoss);

		((FairStack*)gMC->GetStack())->AddPoint(kTOF);

    		ResetParameters();
  	}

return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::EndOfEvent() 
{
	if(fVerboseLevel) Print();
  	aTofHits->Delete();
  	fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::Register(){ FairRootManager::Instance()->Register("TOFPoint", "Tof", aTofHits, kTRUE); }
//------------------------------------------------------------------------------------------------------------------------
TClonesArray* MpdTof::GetCollection(Int_t iColl) const 
{
	if(iColl == 0) 	return aTofHits;
	
return nullptr;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::Print() const 
{
	Int_t nHits = aTofHits->GetEntriesFast();
	cout << "-I- MpdTof: " << nHits << " points registered in this event." << endl;
	
	if(fVerboseLevel > 1)
    		for(Int_t i=0; i<nHits; i++) (*aTofHits)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::Reset(){ aTofHits->Delete(); ResetParameters(); }
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
	Int_t nEntries = cl1->GetEntriesFast();
	cout << "-I- MpdTof: " << nEntries << " entries to add." << endl;
	TClonesArray& clref = *cl2;
	MpdTofPoint* oldpoint = nullptr;
	
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
//--------------------------------------------------------------------------------------------------------------------------------------
void 			MpdTof::ConstructGeometry() 
{
	TString fileName = GetGeometryFileName();
	if(fileName.EndsWith(".root")) 
	{
		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "Constructing TOF geometry from ROOT file %s", fileName.Data());
		ConstructRootGeometry();
	}
	else if ( fileName.EndsWith(".geo") ) 
	{
		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "Constructing TOF geometry from ASCII file %s", fileName.Data());
		ConstructAsciiGeometry();
	}
	else	FairLogger::GetLogger()->Fatal(MESSAGE_ORIGIN, "Geometry format of TOF file %S not supported.", fileName.Data());    
}
//------------------------------------------------------------------------------------------------------------------------
void 	MpdTof::ConstructAsciiGeometry() 
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

        FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
	MpdTofGeoPar* par =(MpdTofGeoPar*)(rtdb->getContainer("MpdTofGeoPar"));
	TObjArray *fSensNodes = par->GetGeoSensitiveNodes();
	TObjArray *fPassNodes = par->GetGeoPassiveNodes();

        FairGeoNode *node   = nullptr;
        FairGeoVolume *aVol = nullptr;
	TListIter iter(volList);			
	
        while((node = (FairGeoNode*)iter.Next()))
	{
                aVol = dynamic_cast<FairGeoVolume*> (node);       
                
		if(node->isSensitive()) 	fSensNodes->AddLast(aVol); 
		else           		 	fPassNodes->AddLast(aVol);	
  	}

	par->setChanged();
	par->setInputVersion(FairRun::Instance()->GetRunId(), 1);  
	ProcessNodes(volList);
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofPoint* 		MpdTof::AddPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t time, Double_t length, Double_t eLoss) 
{	
	return new((*aTofHits)[aTofHits->GetEntriesFast()]) MpdTofPoint(trackID, detID, pos, mom, time, length, eLoss);
}
//--------------------------------------------------------------------------------------------------------------------------------------
Bool_t 			MpdTof::CheckIfSensitive(std::string name)
{
  TString tsname = name;
  if (tsname.Contains("Active")) return kTRUE;
  
return kFALSE;
}
//------------------------------------------------------------------------------------------------------------------------


