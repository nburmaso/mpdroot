/*****************************************************************************
 *
 *         Class MpdZdc
 *         
 *  Adopted for MPD by:   Elena Litvinenko (EL)
 *  e-mail:   litvin@nf.jinr.ru
 *  Version:  8-Apr-2008
 *  Last update:  22-Feb-2012 (EL) 
 * 
 *  Modified by M.Golubeva 
 *
 *****************************************************************************/

#include <iostream>

#include "TClonesArray.h"
#include "TGeoMCGeometry.h"
#include "TGeoManager.h"
#include "TLorentzVector.h"
#include "TParticle.h"
#include "TVirtualMC.h"
#include "TGeoArb8.h"

#include "FairGeoInterface.h"
#include "FairGeoLoader.h"
#include "FairGeoNode.h"
#include "MpdZdcGeo.h"
#include "FairGeoRootBuilder.h"
#include "MpdStack.h"
#include "MpdZdc.h"
#include "MpdZdcPoint.h"

#include "FairRootManager.h"
#include "FairVolume.h"
// add on for debug
//#include "FairGeoG3Builder.h"
#include "FairRuntimeDb.h"
#include "TObjArray.h"
#include "FairRun.h"

#include "TParticlePDG.h"

// -----   Default constructor   -------------------------------------------
MpdZdc::MpdZdc() {
  fZdcCollection        = new TClonesArray("MpdZdcPoint");
  volDetector = 0;
  fPosIndex   = 0; 
  fEventID=-1; 
  fHitNb=0; 
  fVSCVolId=0;
  fVSCHVolId=0;
  fVerboseLevel = 1;
}
// -------------------------------------------------------------------------


// -----   Standard constructor   ------------------------------------------
MpdZdc::MpdZdc(const char* name, Bool_t active)
  : FairDetector(name, active) {
    fZdcCollection        = new TClonesArray("MpdZdcPoint");
    fPosIndex   = 0;
    volDetector = 0;
    fEventID=-1;
    fHitNb=0; 
    fVSCVolId=0;
    fVSCHVolId=0;
    fVerboseLevel = 1;
}
// -------------------------------------------------------------------------


// -----   Destructor   ----------------------------------------------------
MpdZdc::~MpdZdc() {
  if (fZdcCollection) {
    fZdcCollection->Delete();
    delete fZdcCollection;
  } 
}
// -------------------------------------------------------------------------


// -----   Public method Intialize   ---------------------------------------
void MpdZdc::Initialize() {
  // Init function
  
  FairDetector::Initialize();
  FairRun* sim = FairRun::Instance();
  FairRuntimeDb* rtdb=sim->GetRuntimeDb();

  fVSCVolId = gMC->VolId("zdc01s");
  fVSCHVolId = gMC->VolId("ScH");
}
// -------------------------------------------------------------------------


void MpdZdc::BeginEvent(){
  // Begin of the event 
}

//__________________________________________________________________________


MpdZdcPoint* MpdZdc::GetHit(Int_t i) const
{
// Returns the hit for the specified layer.
// ---

  return (MpdZdcPoint*)fZdcCollection->At(i);
}
//_____________________________________________________________________________


MpdZdcPoint* MpdZdc::GetHit(Int_t vsc, Int_t mod, Int_t zdc) const
{
// Returns the hit for the specified vsc and module.
// ---

  MpdZdcPoint *hit;
  Int_t nofHits = fZdcCollection->GetEntriesFast();

  for (Int_t i=0; i<nofHits; i++) {
    hit =  GetHit(i);
    //cout <<"fEdep " <<i <<" " <<hit->GetEdep() <<endl;
    //int iVSCId = hit->GetVSCId();
    //int iMODId = hit->GetMODId();
    //if(iVSCId == vsc && iMODId == mod)
    if(hit->GetCopy() == vsc && hit->GetCopyMother() == mod && hit->GetCopyZdc() ==zdc)
      return hit;
  }
  return 0;
}


// -----   Public method ProcessHits  --------------------------------------
Bool_t MpdZdc::ProcessHits(FairVolume* vol) {

  Int_t copyNoVSC,copyNoVTYVEC,copyNoVMOD,copyNoVZDC;
  Int_t copyNoVSCH,copyNoVTYVECH,copyNoVMODH,copyNoVZDCH;
  Int_t copyNoVSCCom,copyNoVTYVECCom,copyNoVMODCom,copyNoVZDCCom;

  Int_t ivol;
  TLorentzVector tPos1, tMom1;
  TLorentzVector tPos, tMom;

  Int_t copyNo;
  Int_t ivol1;
  Int_t module, module_h; //module, module with hole
  Int_t slice, slice_h;
  Int_t zdc,zdc_h;
  
  Double_t time=0;
  Double_t length =0;
  
  TParticle* part;
  Double_t charge;  
  
  Double_t  QCF=1; //quenching for Birk
  Double_t  BirkConst = 12.6; //0.126 mm/MeV for polystyrene 
  //0.126 *(0.1/0.001) = 12.6 cm/GeV
  //(0.126 mm/MeV - from Wikipedia, 0.07943mm/MeV є in Geant4,G4EmSaturation.cc (push))

 //#define EDEBUG
#ifdef EDEBUG
  static Int_t lEDEBUGcounter=0;
  if (lEDEBUGcounter<1)
    std::cout << "EDEBUG-- MpdZdc::ProcessHits: entered" << gMC->CurrentVolPath() << endl;
#endif
  
  if (gMC->CurrentVolID(copyNoVSC) != fVSCVolId &&
      gMC->CurrentVolID(copyNoVSCH) != fVSCHVolId) {
    return kFALSE;
  }
  
  ivol    = vol->getMCid();

  if (gMC->CurrentVolID(copyNoVSC) == fVSCVolId || gMC->CurrentVolID(copyNoVSCH) == fVSCHVolId) {
    gMC->CurrentVolOffID(1, slice);
    gMC->CurrentVolOffID(2, module);
    gMC->CurrentVolOffID(3, zdc);
    copyNoVTYVECCom = slice; copyNoVMODCom = module; copyNoVZDCCom = zdc;
  }

  if (gMC->IsTrackEntering()) {
    
    ResetParameters();
    fELoss = 0.;
    time    = 0.;
    length  = 0.;
    gMC->TrackPosition(tPos);
    gMC->TrackMomentum(tMom);
    
#ifdef EDEBUG
      gMC->TrackPosition(tPos1);
      gMC->TrackMomentum(tMom1);
#endif
    }//if (gMC->IsTrackEntering())

    if ( gMC->IsTrackInside()) {

      gMC->TrackPosition(tPos);
      gMC->TrackMomentum(tMom);
      length += gMC->TrackStep();    
      time   += gMC->TrackTime() * 1.0e09;

      //fELoss +=gMC->Edep();
//Birk corrections
      if(gMC->TrackStep()>0) {
	QCF = 1.+(BirkConst/gMC->TrackStep())*gMC->Edep();
      }
      else QCF = 1;
      fELoss +=(gMC->Edep())/QCF; 

      if ( gMC->IsTrackStop() || gMC->IsTrackDisappeared() ) {
	
	part    = gMC->GetStack()->GetCurrentTrack();
	charge = part->GetPDG()->Charge() / 3. ;
	
	// Create MpdZdcPoint
	fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
	
	if(fELoss>0) {

	  if(copyNoVTYVECCom==slice && copyNoVMODCom==module && copyNoVZDCCom==zdc) {//module
	    if ( !GetHit(slice,module,zdc) ) {
	      AddHit(fTrackID, ivol, slice, module, zdc, TVector3(tPos.X(), tPos.Y(), tPos.Z()),TVector3(tMom.Px(), tMom.Py(), tMom.Pz()),time, length, fELoss);
	    }
	    else {
	      GetHit(slice,module,zdc)->AddVSC(fTrackID, ivol, slice, module, zdc, TVector3(tPos.X(), tPos.Y(), tPos.Z()),TVector3(tMom.Px(), tMom.Py(), tMom.Pz()),time, length, fELoss);
	    }
	  }//if(copyNoVTYVECCom==copyNoVTYVEC	  

	}//if(fELoss>0)
      }//if ( gMC->IsTrackStop() || gMC->IsTrackDisappeared() )
    }//if ( gMC->IsTrackInside())
      
    if ( gMC->IsTrackExiting()) {

      part    = gMC->GetStack()->GetCurrentTrack();

      // Create MpdZdcPoint
      fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
      time    += gMC->TrackTime() * 1.0e09;
      length  += gMC->TrackLength();
      gMC->TrackPosition(tPos);
      gMC->TrackMomentum(tMom);

      //fELoss +=gMC->Edep();
//Birk corrections
      if(gMC->TrackStep()>0) {
	QCF = 1.+(BirkConst/gMC->TrackStep())*gMC->Edep();
      }
      else QCF = 1;
      fELoss +=(gMC->Edep())/QCF; 

      if(fELoss>0) {

	if(copyNoVTYVECCom==slice && copyNoVMODCom==module && copyNoVZDCCom==zdc) {
	  if ( !GetHit(slice,module,zdc) ) {
	      AddHit(fTrackID, ivol, slice, module, zdc, TVector3(tPos.X(), tPos.Y(), tPos.Z()),TVector3(tMom.Px(), tMom.Py(), tMom.Pz()),time, length, fELoss);
	  }
	  else {
	    GetHit(slice,module,zdc)->AddVSC(fTrackID, ivol, slice, module, zdc, TVector3(tPos.X(), tPos.Y(), tPos.Z()),TVector3(tMom.Px(), tMom.Py(), tMom.Pz()),time, length, fELoss);
	  }
	}//if(copyNoVTYVECCom==copyNoVTYVEC	  

      }//if(fELoss>0)
    }//if ( gMC->IsTrackExiting()) {


      Int_t points = gMC->GetStack()->GetCurrentTrack()->GetMother(1);  
      points = ( points & ( ~ (1<<30) ) ) | (1 << 30);

      gMC->GetStack()->GetCurrentTrack()->SetMother(1,points);

      ((MpdStack*)gMC->GetStack())->AddPoint(kZDC);

    return kTRUE;
  
}
// ----------------------------------------------------------------------------


// -----   Public method EndOfEvent   -----------------------------------------
void MpdZdc::EndOfEvent() {
  if (fVerboseLevel)  Print();
  Reset();
}


// -----   Public method Register   -------------------------------------------
void MpdZdc::Register() {
  FairRootManager::Instance()->Register("ZdcPoint","Zdc", fZdcCollection, kTRUE);
}


// -----   Public method GetCollection   --------------------------------------
TClonesArray* MpdZdc::GetCollection(Int_t iColl) const {
   if (iColl == 0) return fZdcCollection;

  return NULL;
}


// -----   Public method Print   ----------------------------------------------
void MpdZdc::Print() const {
    Int_t nHits = fZdcCollection->GetEntriesFast();
    cout << "-I- MpdZdc: " << nHits << " points registered in this event."
 	<< endl;

    if (fVerboseLevel>1)
      for (Int_t i=0; i<nHits; i++) (*fZdcCollection)[i]->Print();
}


// -----   Public method Reset   ----------------------------------------------
void MpdZdc::Reset() {
   fZdcCollection->Delete();
 
  fPosIndex = 0;
}


// guarda in FairRootManager::CopyClones
// -----   Public method CopyClones   -----------------------------------------
void MpdZdc::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset ) {
  Int_t nEntries = cl1->GetEntriesFast();
  TClonesArray& clref = *cl2;
  MpdZdcPoint* oldpoint = NULL;
  for (Int_t i=0; i<nEntries; i++) {
    oldpoint = (MpdZdcPoint*) cl1->At(i);
    Int_t index = oldpoint->GetTrackID() + offset;
    oldpoint->SetTrackID(index);
    new (clref[fPosIndex]) MpdZdcPoint(*oldpoint);
    fPosIndex++;
  }
  cout << " -I- MpdZdc: " << cl2->GetEntriesFast() << " merged entries."
       << endl;
}


// -----   Public method ConstructGeometry   ----------------------------------
void MpdZdc::ConstructGeometry() {

	TString fileName = GetGeometryFileName();
	if(fileName.EndsWith(".root")) 
	{
		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "Constructing ZDC geometry from ROOT file %s", fileName.Data());
		ConstructRootGeometry();
	}
	/*
	else if ( fileName.EndsWith(".geo") ) 
	{
		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "Constructing ZDC geometry from ASCII file %s", fileName.Data());
		ConstructAsciiGeometry();
	}
	else	FairLogger::GetLogger()->Fatal(MESSAGE_ORIGIN, "Geometry format of ZDC file %S not supported.", fileName.Data());    
	*/
	
 FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
  FairGeoInterface* geoFace = geoLoad->getGeoInterface();
  MpdZdcGeo*      zdcGeo = new MpdZdcGeo();
  zdcGeo->setGeomFile(GetGeometryFileName());
  geoFace->addGeoModule(zdcGeo);
	
  Bool_t rc = geoFace->readSet(zdcGeo);
  if (rc) zdcGeo->create(geoLoad->getGeoBuilder());
  TList* volList = zdcGeo->getListOfVolumes();
	
  // store geo parameter
  FairRun *fRun = FairRun::Instance();
  FairRuntimeDb *rtdb= FairRun::Instance()->GetRuntimeDb();
  MpdZdcGeoPar* par=(MpdZdcGeoPar*)(rtdb->getContainer("MpdZdcGeoPar"));
  TObjArray *fSensNodes = par->GetGeoSensitiveNodes();
  TObjArray *fPassNodes = par->GetGeoPassiveNodes();

  TListIter iter(volList);
  FairGeoNode* node   = NULL;
  FairGeoVolume *aVol=NULL;

  while( (node = (FairGeoNode*)iter.Next()) ) {
    aVol = dynamic_cast<FairGeoVolume*> ( node );


    if ( node->isSensitive()  ) {
      fSensNodes->AddLast( aVol );
    }else{
      fPassNodes->AddLast( aVol );
    }
  }
  par->setChanged();
  par->setInputVersion(fRun->GetRunId(),1);

  ProcessNodes ( volList );
}

  
//Check if Sensitive-----------------------------------------------------------
Bool_t MpdZdc::CheckIfSensitive(std::string name) {
    TString tsname = name;
    if (tsname.Contains("zdc01s") || tsname.Contains("ScH")) {
        return kTRUE;
    }
    return kFALSE;
}
//-----------------------------------------------------------------------------
 

// -----   Private method AddHit   --------------------------------------------
MpdZdcPoint* MpdZdc::AddHit(Int_t trackID, Int_t detID, Int_t copyNo, Int_t copyNoMother, Int_t copyNoZdc,TVector3 pos, TVector3 mom, Double_t time, Double_t length, Double_t eLoss) {
  TClonesArray& clref = *fZdcCollection;
  Int_t size = clref.GetEntriesFast();

  return new(clref[size]) MpdZdcPoint(trackID, detID, copyNo, copyNoMother, copyNoZdc, pos, mom, time, length, eLoss);
 }
// ----


ClassImp(MpdZdc)
