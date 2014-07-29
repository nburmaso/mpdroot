//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                       MpdTgem source file                      -----
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
#include "MpdTgemGeo.h"
#include "FairRootManager.h"
#include "MpdTgem.h"
#include "MpdTgemPoint.h"
#include "FairRuntimeDb.h"
#include "MpdTgemGeoPar.h"
#include "TObjArray.h"
#include "FairRun.h"
#include "FairVolume.h"
#include "TMath.h"

#include "TParticlePDG.h"

class FairVolume;

//------------------------------------------------------------------------------------------------------------------------
MpdTgem::MpdTgem() 
 : FairDetector("Tgem", kTRUE)
{
	fTgemCollection = new TClonesArray("MpdTgemPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
	ResetParameters();
}
//------------------------------------------------------------------------------------------------------------------------
MpdTgem::MpdTgem(const char* name, Bool_t active)
 : FairDetector(name, active)
{  
	fTgemCollection = new TClonesArray("MpdTgemPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
	ResetParameters();
}
//------------------------------------------------------------------------------------------------------------------------
MpdTgem::~MpdTgem() 
{
	if(fTgemCollection){fTgemCollection->Delete(); delete fTgemCollection; }
}
//------------------------------------------------------------------------------------------------------------------------
int MpdTgem::DistAndPoints(TVector3 p3, TVector3 p4, TVector3& pa, TVector3& pb) {                                         
    pa=(p3+p4)*0.5;
    pb=pa;
    
    //pa=p3; //del
    //pb=pa; //del
    return 0;
} 
//------------------------------------------------------------------------------------------------------------------------
TVector3 MpdTgem::GlobalToLocal(TVector3& global) {
    Double_t globPos[3];
    Double_t localPos[3];
    global.GetXYZ(globPos);
    gMC->Gmtod(globPos, localPos, 1); 
    return TVector3(localPos);
}
//------------------------------------------------------------------------------------------------------------------------
TVector3 MpdTgem::LocalToGlobal(TVector3& local) {
    Double_t globPos[3];
    Double_t localPos[3];
    local.GetXYZ(localPos);
    gMC->Gdtom(localPos, globPos, 1);  
    return TVector3(globPos);
}
//----------------------------------------------------------------------------------------------------------------------
Bool_t  MpdTgem::ProcessHits(FairVolume* vol)
{
  //AZ
  static Int_t first = 1, nLayMod = 30; //!!! geometry dependent
  static Double_t z0[4] = {156.5, 186.5, 0, 0};
  first=0; //=
  if (first) {
    first = 0;
    TGeoHMatrix matrix;
    if (gGeoManager) {
      if (gGeoManager->GetCurrentNavigator()->CheckPath("/cave_1/stt01layerradial_1")) {  //EL
	if (gMC->GetTransformation("/cave_1/stt01layerradial_1",matrix)) 
	  z0[0] = TMath::Abs (matrix.GetTranslation()[2]);
      }
      //matrix.Print();
      //cout << gGeoManager->GetPath() << " " << matrix.GetTranslation()[2] << endl;
      if (gGeoManager->GetCurrentNavigator()->CheckPath("/cave_1/stt02layerradial_1")) {  //EL  
	if (gMC->GetTransformation("/cave_1/stt02layerradial_1",matrix)) 
	  z0[1] = TMath::Abs (matrix.GetTranslation()[2]);
      }
      if (gGeoManager->GetCurrentNavigator()->CheckPath("/cave_1/stt03layerradial_1")) { 
        if (gMC->GetTransformation("/cave_1/stt03layerradial_1",matrix))
          z0[2] = TMath::Abs (matrix.GetTranslation()[2]);
      }
      if (gGeoManager->GetCurrentNavigator()->CheckPath("/cave_1/stt04layerradial_1")) { 
        if (gMC->GetTransformation("/cave_1/stt04layerradial_1",matrix))
          z0[3] = TMath::Abs (matrix.GetTranslation()[2]);
      }
    }
  }
  //AZ
  Int_t gap, cell, module, region, wheel, gasgap;
  static Int_t sameVol = 0;
  TString Volname = vol->getRealName();   // EL
  sscanf(&(Volname[11]),"%d",&gasgap);
  //==++cout <<gasgap<<"===============-----------------Volname="<< Volname << endl;
  sscanf(&(gMC->CurrentVolPath()[15]),"%d",&wheel);
  //=cout<<"************--"<<wheel<<"  "<<gMC->CurrentVolPath()<<endl;
  // Set parameters at entrance of volume. Reset ELoss. 
  // Check if this is not due to passing through the anode wire. 
  // If anode wire, do not reset anything. 
  if (gMC->IsTrackEntering()) {
    sameVol = 0; //== 1 == 
    gMC->CurrentVolOffID(2, module);
    
    sscanf(&(gMC->CurrentVolPath()[15]),"%d",&wheel);
    //cout<<"************--"<<wheel<<"  "<<gMC->CurrentVolPath()<<endl;
    // cout<<"-----!!!!!!!!!!!!!! "<<gMC->GetStack()->GetCurrentTrackNumber()<<" "<<fTrackID<<" "<<gMC->CurrentVolID(gap)+1000*module<<" "<<fVolumeID<<endl;
    if ((gMC->GetStack()->GetCurrentTrackNumber() != fTrackID || 
	wheel != fwheel)) {
      //if(fTrackIDTmp != -1) {ResetParameters();sameVol = 0; cout<<"          !!!!!!!!==========ERROR==========!!!!!!!!! "<<fPdgId<<endl; return kTRUE;}
//	gMC->CurrentVolID(gap)+1000*module != fVolumeID) {
      ResetParameters();
      sameVol = 0;
      fELoss  = 0.;
      fTime   = gMC->TrackTime() * 1.0e09;
      fLength = gMC->TrackLength();
      fIsPrimary = 0;
      fCharge = -1;
      fPdgId = 0;
      if(gasgap == 1){
      
      TLorentzVector PosIn;
      gMC->TrackPosition(PosIn);
      fPosIn.SetXYZ(PosIn.X(), PosIn.Y(), PosIn.Z());
      fPosInTmp.SetXYZ(PosIn.X(), PosIn.Y(), PosIn.Z());
      //==++cout <<"===============-----------------"<< Volname <<" PosIn.Z()="<<PosIn.Z()<< endl;
      gMC->TrackMomentum(fMom);
      TParticle* part = 0;
      part = gMC->GetStack()->GetCurrentTrack();
      if (part) {
	fIsPrimary = (Int_t)part->IsPrimary();
	fCharge = (Int_t)part->GetPDG()->Charge();
	fPdgId = (Int_t)part->GetPdgCode();
      }
      fVolumeID = gMC->CurrentVolID(gap) + 1000 * module;
      fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
      fTrackIDTmp = fTrackID;
      fwheel = wheel; // wheel number
      fwheelTmp = fwheel;
     } 
    } 
  }
  
  // Sum energy loss for all steps in the active volume
  fELoss += gMC->Edep();
  // Create MpdTgemPoint at EXIT of active volume; 
  //if (gMC->IsTrackExiting() && fELoss > 0) {
  
  fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
  if ((gMC->IsTrackExiting() || gMC->IsTrackStop() || gMC->IsTrackDisappeared()) && fELoss > 0 && gasgap == 2 && 
  fTrackID == fTrackIDTmp && fwheelTmp == fwheel) {
        
    //if(fTrackID != fTrackIDTmp || fwheelTmp != fwheel) {  return 0;}
    
    //==++ cout<<"************* gMC->GetStack()->GetCurrentTrackNumber()="<<fTrackID<<"    "<<"fPdgId="<<fPdgId<<endl;
    //= Volname = vol->getName();
    //= cout <<"===============-----------------"<< Volname << endl;
    //AZ region = Volname[5] - '0';   //?????????????????????????
    //gMC->CurrentVolID(gap);
    //gMC->CurrentVolOffID(1, cell);
    //gMC->CurrentVolOffID(2, module);
    
    TLorentzVector PosOut;
    gMC->TrackPosition(PosOut);
    fPosOut.SetXYZ(PosOut.X(), PosOut.Y(), PosOut.Z());
    
    //AZ Get layer number
    
    //== sscanf(&(Volname[7]),"%d",&wheel); // wheel number
    
    //==++cout <<"===============-----------------"<< Volname <<" PosOut.Z()="<<PosOut.Z()<< endl;
    //= cout <<"===============-----------------"<< wheel << endl;
     
    Double_t dz = TMath::Abs(PosOut.Z()) - z0[wheel-1];
    //Int_t lay = TMath::Nint(dz/1.) + 1;
    //Int_t lay = TMath::Nint(dz/1.) + 2; // geometry with triplets
    Int_t lay = TMath::Nint(dz/0.85) + 1; // geometry with triplets
    if (wheel == 2 || wheel == 4) lay += nLayMod;
    //= region = lay * 1000 + cell; // lay*1000+tubeID
    cell=1; //=
    //== cout<<"__________________________wheel="<<wheel<<endl;
    region = wheel * 10000 + cell; //=  fwheel*10000+segmentID 

    /*
    Double_t posG[3], posL[3];
    fPosOut.GetXYZ(posG);
    gMC->Gmtod(posG, posL, 1);
    cout << lay << " " << cell << " " << fTrackID << " " << posL[0] << " " 
    << posL[1] << " " << posL[2] << " " << gap << " " << module << endl; */
    //AZ
    
    // Straw line defined in local coordinates
    TVector3 p1(0,0, -10); // -10,10 - arbitrary number in Z axis... 
    TVector3 p2(0,0, 10);
    
    // Conversion to global coordinates
    p1 = LocalToGlobal(p1);
    p2 = LocalToGlobal(p2);
    Double_t phi = TMath::ATan2 (p2.Y()-p1.Y(),p2.X()-p1.X()); //AZ
    
    // "will-be-filled-out-soon" Points of closest approach
    TVector3 trackPosition(0,0,0); // trackPosition => point on track, fPos => point on straw
    
    // calculate points of closest approach between track and straw
    int result = DistAndPoints(fPosInTmp, fPosOut, fPos, trackPosition);
    //TVector3 dist = trackPosition - fPos;
    //std::cout << "Dist: " << dist.Mag() << std::endl;
    
    //TVector3 trackPosition(0,0,0);
    //if (result == 0) { // track and straw should not be parallel!
    // convert point coordinates to back to global system
    //    TVector3 dist = pa - pb;
    //    std::cout << "distance: " << dist.Mag() << std::endl;
    //}	
    
    //AZ fVolumeID = ((region-1)<<24);
    // Remove point if it is from the same track and in the same tube (to account for the cases 
    // when the track goes through the anode wire - probably will not work if secondaries are
    // produced inside the anode wire (they are transported before the parent track?).
    if (sameVol) fTgemCollection->RemoveAt(fTgemCollection->GetEntriesFast()-1); 
//==    cout<<"*******************************Charge="<<fCharge<<endl;
    MpdTgemPoint *p = AddHit(fTrackID, region, fPos, fPos.Perp(), 
			     TVector3(fMom.Px(), fMom.Py(), fMom.Pz()), 
			     fTime, (fLength+gMC->TrackLength())/2, fELoss, 
			     fIsPrimary, fCharge, fPdgId, trackPosition);
    //= p->SetPhi(phi); //AZ
    //==((FairStack*)gMC->GetStack())->AddPoint(kTgem);    //==
    //ResetParameters();
    fTrackIDTmp = -1;
    fwheelTmp = -1;
  }

  return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTgem::EndOfEvent() 
{
	if(fVerboseLevel) Print();
  	fTgemCollection->Clear();
  	fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTgem::Register(){ FairRootManager::Instance()->Register("TgemPoint", "Tgem", fTgemCollection, kTRUE); }
//------------------------------------------------------------------------------------------------------------------------
TClonesArray* MpdTgem::GetCollection(Int_t iColl) const 
{
	if(iColl == 0) 	return fTgemCollection;
	
return NULL;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTgem::Print() const 
{
	Int_t nHits = fTgemCollection->GetEntriesFast();
	cout << "-I- MpdTgem: " << nHits << " points registered in this event." << endl;
	
	if(fVerboseLevel > 1)
    		for(Int_t i=0; i<nHits; i++) (*fTgemCollection)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTgem::Reset(){ fTgemCollection->Clear(); ResetParameters(); }
//------------------------------------------------------------------------------------------------------------------------
void MpdTgem::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
	Int_t nEntries = cl1->GetEntriesFast();
	cout << "-I- MpdTgem: " << nEntries << " entries to add." << endl;
	TClonesArray& clref = *cl2;
	MpdTgemPoint* oldpoint = NULL;
	
	for(Int_t i=0; i<nEntries; i++) 
	{
		oldpoint = (MpdTgemPoint*) cl1->At(i);
		Int_t index = oldpoint->GetTrackID() + offset;
		oldpoint->SetTrackID(index);
		new (clref[fPosIndex]) MpdTgemPoint(*oldpoint);
		fPosIndex++;
	}
	
	cout << "-I- MpdTgem: " << cl2->GetEntriesFast() << " merged entries."  << endl;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTgem::ConstructGeometry() 
{
  
	Int_t count=0;
	Int_t count_tot=0;

        FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
        FairGeoInterface* geoFace = geoLoad->getGeoInterface();
	MpdTgemGeo* tgemGeo  = new MpdTgemGeo();
	tgemGeo->setGeomFile(GetGeometryFileName());
	geoFace->addGeoModule(tgemGeo);

	Bool_t rc = geoFace->readSet(tgemGeo);
	if(rc) tgemGeo->create(geoLoad->getGeoBuilder());
	TList* volList = tgemGeo->getListOfVolumes();
	volList->Print();
	// store geo parameter
        FairRun *fRun = FairRun::Instance();
        FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
	MpdTgemGeoPar* par =(MpdTgemGeoPar*)(rtdb->getContainer("MpdTgemGeoPar"));
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
MpdTgemPoint* MpdTgem::AddHit(Int_t trackID, Int_t detID, TVector3 pos, Double_t radius,
			    TVector3 mom, Double_t time, Double_t length, 
			    Double_t eLoss, Int_t isPrimary, Double_t charge, Int_t pdgId, TVector3 trackPos) 
{
	TClonesArray& clref = *fTgemCollection;
	Int_t size = clref.GetEntriesFast();
    //std::cout << "ELoss: " << eLoss << "\n";	
    return new(clref[size]) MpdTgemPoint(trackID, detID, pos, radius, mom, time, length, eLoss, isPrimary, charge, pdgId, trackPos);
}
//------------------------------------------------------------------------------------------------------------------------

ClassImp(MpdTgem)
