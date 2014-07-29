//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                       MpdDch source file                      -----
// -------------------------------------------------------------------------

#include <iostream>

#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TParticle.h"
#include "TVirtualMC.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"

//#include "FairDetectorList.h"
#include "FairGeoInterface.h"
#include "FairGeoLoader.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "MpdDchGeo.h"
#include "FairRootManager.h"
#include "MpdDch.h"
#include "MpdDchPoint.h"
#include "FairRuntimeDb.h"
#include "MpdDchGeoPar.h"
#include "TObjArray.h"
#include "FairRun.h"
#include "FairVolume.h"
#include "TMath.h"

#include "TParticlePDG.h"

class FairVolume;

//------------------------------------------------------------------------------------------------------------------------
MpdDch::MpdDch() 
 : FairDetector("Dch", kTRUE)
{
	fPointCollection = new TClonesArray("MpdDchPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
	ResetParameters();
}
//------------------------------------------------------------------------------------------------------------------------
MpdDch::MpdDch(const char* name, Bool_t active)
 : FairDetector(name, active)
{  
	fPointCollection = new TClonesArray("MpdDchPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
	ResetParameters();
}
//------------------------------------------------------------------------------------------------------------------------
MpdDch::~MpdDch() 
{
	if(fPointCollection){fPointCollection->Delete(); delete fPointCollection; }
}
//------------------------------------------------------------------------------------------------------------------------
int MpdDch::DistAndPoints(TVector3 p3, TVector3 p4, TVector3& pa, TVector3& pb) {                                         
    pa=(p3+p4)*0.5;
    pb=pa;
    
    //pa=p3; //del
    //pb=pa; //del
    return 0;
} 
//------------------------------------------------------------------------------------------------------------------------
TVector3 MpdDch::GlobalToLocal(TVector3& global) {
    Double_t globPos[3];
    Double_t localPos[3];
    global.GetXYZ(globPos);
    gMC->Gmtod(globPos, localPos, 1); 
    return TVector3(localPos);
}
//------------------------------------------------------------------------------------------------------------------------
TVector3 MpdDch::LocalToGlobal(TVector3& local) {
    Double_t globPos[3];
    Double_t localPos[3];
    local.GetXYZ(localPos);
    gMC->Gdtom(localPos, globPos, 1);  
    return TVector3(globPos);
}
//----------------------------------------------------------------------------------------------------------------------
Bool_t  MpdDch::ProcessHits(FairVolume* vol)
{

  static TString proj = "xyuv";
  Int_t gap, cell, module, region, wheel, gasgap;

  // Set parameters at entrance of volume. Reset ELoss. 
  if (gMC->IsTrackEntering()) {
    TString Volname = vol->getRealName();   // EL
    sscanf(&(Volname[4]),"%d",&wheel); // chamber
    //cout << "===============-----------------Volname="<< Volname << " " << wheel << endl;

    //cout<<"************-- "<<gMC->CurrentVolPath()<<endl;
    TString path = gMC->CurrentVolPath();
    Int_t pos = path.Last('_');
    Int_t iproj = proj.First(path[pos-1]); // projection
    gMC->CurrentVolOffID(0, gasgap); // plane 1 or 2
    //cout << iproj << " " << gasgap << endl;
    
    ResetParameters();
    fELoss  = 0.;
    fTime   = gMC->TrackTime() * 1.0e09;
    fLength = gMC->TrackLength();
    fIsPrimary = 0;
    fCharge = -1;
    fPdgId = 0;
      
    TLorentzVector PosIn;
    gMC->TrackPosition(PosIn);
    fPosIn.SetXYZ(PosIn.X(), PosIn.Y(), PosIn.Z());
    gMC->TrackMomentum(fMom);
    TParticle* part = 0;
    part = gMC->GetStack()->GetCurrentTrack();
    if (part) {
      fIsPrimary = (Int_t)part->IsPrimary();
      fCharge = (Int_t)part->GetPDG()->Charge();
      fPdgId = (Int_t)part->GetPdgCode();
    }
    
    fVolumeID = (wheel - 1) * 8 + iproj * 2 + gasgap;
    
    fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
  } 
  
  // Sum energy loss for all steps in the active volume
  fELoss += gMC->Edep();

  // Create MpdDchPoint at EXIT of active volume; 
  if ((gMC->IsTrackExiting() || gMC->IsTrackStop() || gMC->IsTrackDisappeared()) && fELoss > 0) {
    TLorentzVector PosOut;
    gMC->TrackPosition(PosOut);
    fPosOut.SetXYZ(PosOut.X(), PosOut.Y(), PosOut.Z());
    
    // Line defined in local coordinates
    TVector3 p1(0, 0, 0); // 10 - arbitrary number... 
    TVector3 p2(10,0, 0);
    
    // Conversion to global coordinates
    p1 = LocalToGlobal(p1);
    p2 = LocalToGlobal(p2);
    Double_t phi = TMath::ATan2 (p2.Y()-p1.Y(),p2.X()-p1.X());
    
    // "will-be-filled-out-soon" Points of closest approach
    TVector3 trackPosition(0,0,0); // trackPosition => point on track, fPos => point on straw
    
    // calculate points of closest approach between track and straw
    //int result =
    DistAndPoints(fPosIn, fPosOut, fPos, trackPosition);

    MpdDchPoint *p = AddHit(fTrackID, fVolumeID, fPos, fPos.Perp(), 
			    TVector3(fMom.Px(), fMom.Py(), fMom.Pz()), 
			    fTime, (fLength+gMC->TrackLength())/2, fELoss, 
			    fIsPrimary, fCharge, fPdgId, trackPosition);
    p->SetPhi(phi); //AZ
    //==((FairStack*)gMC->GetStack())->AddPoint(kECT);    //==
  }

  return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdDch::EndOfEvent() 
{
	if(fVerboseLevel) Print();
  	fPointCollection->Clear();
  	fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdDch::Register(){ FairRootManager::Instance()->Register("DchPoint", "Dch", fPointCollection, kTRUE); }
//------------------------------------------------------------------------------------------------------------------------
TClonesArray* MpdDch::GetCollection(Int_t iColl) const 
{
	if(iColl == 0) 	return fPointCollection;
	
return NULL;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdDch::Print() const 
{
	Int_t nHits = fPointCollection->GetEntriesFast();
	cout << "-I- MpdDch: " << nHits << " points registered in this event." << endl;
	
	if(fVerboseLevel > 1)
    		for(Int_t i=0; i<nHits; i++) (*fPointCollection)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------
void MpdDch::Reset(){ fPointCollection->Clear(); ResetParameters(); }
//------------------------------------------------------------------------------------------------------------------------
void MpdDch::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
	Int_t nEntries = cl1->GetEntriesFast();
	cout << "-I- MpdDch: " << nEntries << " entries to add." << endl;
	TClonesArray& clref = *cl2;
	MpdDchPoint* oldpoint = NULL;
	
	for(Int_t i=0; i<nEntries; i++) 
	{
		oldpoint = (MpdDchPoint*) cl1->At(i);
		Int_t index = oldpoint->GetTrackID() + offset;
		oldpoint->SetTrackID(index);
		new (clref[fPosIndex]) MpdDchPoint(*oldpoint);
		fPosIndex++;
	}
	
	cout << "-I- MpdDch: " << cl2->GetEntriesFast() << " merged entries."  << endl;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdDch::ConstructGeometry() 
{
  
	Int_t count=0;
	Int_t count_tot=0;

        FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
        FairGeoInterface* geoFace = geoLoad->getGeoInterface();
	MpdDchGeo* geo  = new MpdDchGeo();
	geo->setGeomFile(GetGeometryFileName());
	geoFace->addGeoModule(geo);

	Bool_t rc = geoFace->readSet(geo);
	if(rc) geo->create(geoLoad->getGeoBuilder());
	TList* volList = geo->getListOfVolumes();
	volList->Print();
	// store geo parameter
        FairRun *fRun = FairRun::Instance();
        FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
	MpdDchGeoPar* par =(MpdDchGeoPar*)(rtdb->getContainer("MpdDchGeoPar"));
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
MpdDchPoint* MpdDch::AddHit(Int_t trackID, Int_t detID, TVector3 pos, Double_t radius,
			    TVector3 mom, Double_t time, Double_t length, 
			    Double_t eLoss, Int_t isPrimary, Double_t charge, Int_t pdgId, TVector3 trackPos) 
{
	TClonesArray& clref = *fPointCollection;
	Int_t size = clref.GetEntriesFast();
    //std::cout << "ELoss: " << eLoss << "\n";	
    return new(clref[size]) MpdDchPoint(trackID, detID, pos, radius, mom, time, length, eLoss, isPrimary, charge, pdgId, trackPos);
}
//------------------------------------------------------------------------------------------------------------------------

ClassImp(MpdDch)
