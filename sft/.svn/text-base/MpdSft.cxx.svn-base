//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                       MpdSft source file                      -----
// -----                  Created 28/07/04  by V. Friese               -----
// -------------------------------------------------------------------------

#include "MpdSft.h"
#include "MpdSftGeo.h"
#include "MpdSftGeoPar.h"
#include "MpdSftPoint.h"

#include "FairGeoInterface.h"
#include "FairGeoLoader.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "FairRootManager.h"
#include "FairStack.h"
#include "FairRuntimeDb.h"
#include "FairRun.h"
#include "FairVolume.h"

#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TParticle.h"
#include "TVirtualMC.h"
#include "TObjArray.h"

#include <iostream>

//------------------------------------------------------------------------------------------------------------------------
MpdSft::MpdSft() 
 : FairDetector("SFT", kTRUE)
{
	fTofCollection = new TClonesArray("MpdSftPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdSft::MpdSft(const char* name, Bool_t active)
 : FairDetector(name, active)
{  
	fTofCollection = new TClonesArray("MpdSftPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdSft::~MpdSft() 
{
	if(fTofCollection){ fTofCollection->Delete(); delete fTofCollection; }
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t  MpdSft::ProcessHits(FairVolume* vol)
{
	
  const Double_t pitch = 0.01; // strip pitch 100 um
  static Bool_t first = kTRUE;
  static Double_t dph[4] = {0}, radi[4] = {5., 12.5, 6., 13.}; // inner rad.
  if (first) {
    // Compute some parameters
    first = kFALSE;
    for (Int_t i = 0; i < 4; ++i) {
      dph[i] = TMath::TwoPi() * radi[i] / pitch;
      cout << " Inner radius: " << radi[i] << ", pitch number: " << dph[i] << endl; 
      dph[i] = TMath::TwoPi() / dph[i]; // angular pitch
    }
  }
  //cout << "Got Hit!\n";
  Int_t  module, strip;//, gap, cell;
  TString volname;
  
  // Set parameters at entrance of volume. Reset ELoss.
  if (gMC->IsTrackEntering()) {
    fELoss  = 0.;
    fTime   = gMC->TrackTime() * 1.0e09;
    fLength = gMC->TrackLength();
    gMC->TrackPosition(fPos);
    gMC->TrackMomentum(fMom);
    globalPos[0] = globalPos[1] = globalPos[2] = 0;
    localPos[0] = localPos[1] = localPos[2] = 0;
    gMC->Gdtom(localPos, globalPos, 1);
  }
  
  // Sum energy loss for all steps in the active volume
  fELoss += gMC->Edep();

  // Create MpdSftPoint at exit of active volume
  //if (gMC->TrackCharge() != 0 && (gMC->IsTrackExiting() || gMC->IsTrackStop() || gMC->IsTrackDisappeared())) {
  if (fELoss != 0 && (gMC->IsTrackExiting() || gMC->IsTrackStop() || gMC->IsTrackDisappeared())) {
    fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
    //gMC->CurrentVolOffID(1, strip);
    //gMC->CurrentVolOffID(3, module);
    gMC->CurrentVolOffID(1, module);
    volname = vol->getRealName(); 
    //fVolumeID =  (module<<14) + (strip<<4);
    //sscanf(&(volname[11]),"%d",&fVolumeID);
    if (volname.Length() == 12) sscanf(&(volname[11]),"%d",&fVolumeID);
    else sscanf(&(volname[11]),"%d%d",&fVolumeID);
    fVolumeID |= (module<<5);
    //cout << volname << " " << fVolumeID << " " << module << " " << fPos.Z() << endl;
    
    MpdSftPoint *p = 0x0;
    //if (fELoss > 0) AddHit(fTrackID, fVolumeID, TVector3(globalPos[0],  globalPos[1],  globalPos[2]),
    //	TVector3(fMom.Px(), fMom.Py(), fMom.Pz()), fTime, fLength, fELoss);
    if (fELoss > 0) p = AddHit(fTrackID, fVolumeID, TVector3(fPos.X(),fPos.Y(),fPos.Z()),
			       TVector3(fMom.X(),fMom.Y(),fMom.Z()), fTime, fLength, fELoss); //AZ
    Int_t i = 0, istrip, irad = 0;
    if (fVolumeID != 3) {
      if (fPos.Pt() > radi[1]) i = 1;
    } else {
      i = (fPos.Pt() > radi[3]) ? 3 : 2;
    }
    istrip = (Int_t) (fPos.Phi() / dph[i]);
    irad = i % 2;
    if (p) {
      p->SetStrip(istrip);
      p->SetRad(irad);
      ((FairStack*)gMC->GetStack())->AddPoint(kFSA);
    }
    ResetParameters();
  }
  
  return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdSft::EndOfEvent() 
{
	if(fVerboseLevel) Print();
  	fTofCollection->Delete();
  	fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdSft::Register(){ FairRootManager::Instance()->Register("SftPoint", "Sft", fTofCollection, kTRUE); }
//------------------------------------------------------------------------------------------------------------------------
TClonesArray* MpdSft::GetCollection(Int_t iColl) const 
{
	if(iColl == 0) 	return fTofCollection;
	
return NULL;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdSft::Print() const 
{
	Int_t nHits = fTofCollection->GetEntriesFast();
	cout << "-I- MpdSft: " << nHits << " points registered in this event." << endl;
	
	if(fVerboseLevel > 1)
    		for(Int_t i=0; i<nHits; i++) (*fTofCollection)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------
void MpdSft::Reset(){ fTofCollection->Delete(); ResetParameters(); }
//------------------------------------------------------------------------------------------------------------------------
void MpdSft::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
	Int_t nEntries = cl1->GetEntriesFast();
	cout << "-I- MpdSft: " << nEntries << " entries to add." << endl;
	TClonesArray& clref = *cl2;
	MpdSftPoint* oldpoint = NULL;
	
	for(Int_t i=0; i<nEntries; i++) 
	{
		oldpoint = (MpdSftPoint*) cl1->At(i);
		Int_t index = oldpoint->GetTrackID() + offset;
		oldpoint->SetTrackID(index);
		new (clref[fPosIndex]) MpdSftPoint(*oldpoint);
		fPosIndex++;
	}
	
	cout << "-I- MpdSft: " << cl2->GetEntriesFast() << " merged entries."  << endl;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdSft::ConstructGeometry() 
{
  
	Int_t count=0;
	Int_t count_tot=0;

        FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
        FairGeoInterface* geoFace = geoLoad->getGeoInterface();
	MpdSftGeo*       tofGeo  = new MpdSftGeo();
	tofGeo->setGeomFile(GetGeometryFileName());
	geoFace->addGeoModule(tofGeo);

	Bool_t rc = geoFace->readSet(tofGeo);
	if(rc) tofGeo->create(geoLoad->getGeoBuilder());
	TList* volList = tofGeo->getListOfVolumes();

	// store geo parameter
        FairRun *fRun = FairRun::Instance();
        FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
	MpdSftGeoPar* par =(MpdSftGeoPar*)(rtdb->getContainer("MpdSftGeoPar"));
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
MpdSftPoint* MpdSft::AddHit(Int_t trackID, Int_t detID, TVector3 pos,
			    TVector3 mom, Double_t time, Double_t length, 
			    Double_t eLoss) 
{
	TClonesArray& clref = *fTofCollection;
	Int_t size = clref.GetEntriesFast();
	
return new(clref[size]) MpdSftPoint(trackID, detID, pos, mom, time, length, eLoss);
}
//------------------------------------------------------------------------------------------------------------------------

ClassImp(MpdSft)
