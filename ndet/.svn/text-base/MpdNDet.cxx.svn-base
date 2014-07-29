/**  MpdNDet.cxx
 *@author Mikhail Prokudin
 **
 ** Defines the active detector NDet with geometry coded here.
 ** Layers, holes, fibers,steel tapes implemented 
 **/

#include "MpdNDet.h"

#include "MpdNDetPointLite.h"
#include "MpdNDetPoint.h"
#include "MpdNDetParam.h"

#include "FairGeoInterface.h"
#include "FairGeoLoader.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "FairRuntimeDb.h"
#include "FairRootManager.h"
#include "FairRun.h"
#include "FairRunAna.h"
#include "FairMCTrack.h"
#include "FairStack.h"
#include "FairVolume.h"
#include "FairGeoMedium.h"
#include "FairGeoMedia.h"

#include "TClonesArray.h"
#include "TGeoMCGeometry.h"
#include "TGeoManager.h"
#include "TParticle.h"
#include "TVirtualMC.h"
#include "TGeoBBox.h"
#include "TGeoPgon.h"
#include "TGeoTube.h"
#include "TGeoMatrix.h"
#include "TList.h"

#include <iostream>
#include <stdlib.h>

using namespace std;

#define kN kNumberOfNdetSensitiveVolumes  //sensitive air and scintillator

// -----   Default constructor   -------------------------------------------
MpdNDet::MpdNDet() : FairDetector("NDET", kTRUE) {
//  MpdNDetPoint::Class()    ->IgnoreTObjectStreamer();
//  MpdNDetPointLite::Class()->IgnoreTObjectStreamer();
//  FairMCTrack::Class()      ->IgnoreTObjectStreamer();
  fNDetCollection = new TClonesArray("MpdNDetPoint");
  fLiteCollection = new TClonesArray("MpdNDetPointLite");
  fPosIndex = 0;
  fVerboseLevel = 1;

  Int_t i;

  for(i=kN-1;i>-1;i--)
    fVolArr[i]=-1111;
}
// -------------------------------------------------------------------------



// -----   Standard constructor   ------------------------------------------
MpdNDet::MpdNDet(const char* name, Bool_t active, const char* fileGeo)
  : FairDetector(name, active)
{
  /** MpdNDet constructor:
   ** reads geometry parameters from the ascii file <fileGeo>,
   ** creates the Ndet geometry container MpdNdetInf
   ** and initializes basic geometry parameters needed to construct
   ** TGeo geometry
   **/

  Int_t i;
  Int_t j;
  TString nm;
  Info("MpdNDet","Geometry is read from file %s.", fileGeo);
  for(i=kN-1;i>-1;i--)
    fVolArr[i]=-1111;
  fNDetCollection = new TClonesArray("MpdNDetPoint");
  fLiteCollection = new TClonesArray("MpdNDetPointLite");
  fPosIndex = 0;
  fVerboseLevel = 1;
  MpdNDetParam* conf=new MpdNDetParam("NDetParam",fileGeo);
  fZSize=conf->GetDouble("zsize");
  fAirInnerRad=conf->GetDouble("airinnerradius");
  fAirThickness=conf->GetDouble("airthickness");
  fScinThickness=conf->GetDouble("scinthickness");
  fHCut=conf->GetDouble("hcut");
  fECut=conf->GetDouble("ecut");
  fZDivisions=conf->GetInteger("zdivisions");
  fRDivisions=conf->GetInteger("rdivisions");// phi divisions
}

// -------------------------------------------------------------------------

void MpdNDet::Initialize()
{
  FairDetector::Initialize();
/*
  FairRun* sim = FairRun::Instance();
  FairRuntimeDb* rtdb=sim->GetRuntimeDb();
  FairGeoEcalPar *par=new FairGeoEcalPar();
  fInf->FillGeoPar(par,0);
  rtdb->addContainer(par);
*/
}

// -----   Destructor   ----------------------------------------------------
MpdNDet::~MpdNDet()
{
  if (fNDetCollection) {
    fNDetCollection->Delete(); 
    delete fNDetCollection;
    fNDetCollection=NULL;
  }
  if (fLiteCollection) {
    fLiteCollection->Delete();
    delete fLiteCollection;
    fLiteCollection=NULL;
  }
}
// -------------------------------------------------------------------------

// -----   Private method SetNDetCuts   ------------------------------------
void MpdNDet::SetNdetCuts(Int_t medium)
{
  /** Set GEANT3 tracking energy cuts for selected medium **/
  if (fECut > 0) {
    gMC->Gstpar(medium,"CUTGAM",fECut);
    gMC->Gstpar(medium,"CUTELE",fECut);
    gMC->Gstpar(medium,"BCUTE" ,fECut);
    gMC->Gstpar(medium,"BCUTM" ,fECut);
  }

  if (fHCut > 0) {
    gMC->Gstpar(medium,"CUTNEU",fHCut);
    gMC->Gstpar(medium,"CUTHAD",fHCut);
    gMC->Gstpar(medium,"CUTMUO",fHCut);
    gMC->Gstpar(medium,"PPCUTM",fHCut);
  }
}
// -------------------------------------------------------------------------

void MpdNDet::FinishPrimary()
{
  fFirstNumber=fLiteCollection->GetEntriesFast();
}

//_____________________________________________________________________________
void MpdNDet::ChangeHit(MpdNDetPointLite* oldHit)
{
  Double_t edep = gMC->Edep();
  Double_t el=oldHit->GetEnergyLoss();
  Double_t ttime=gMC->TrackTime()*1.0e9;
  oldHit->SetEnergyLoss(el+edep);
  if(ttime<oldHit->GetTime())
    oldHit->SetTime(ttime);
}

//_____________________________________________________________________________
void MpdNDet::SetSpecialPhysicsCuts()
{
  /** Change the special tracking cuts for
   ** two NDet media, Scintillator and Lead
   **/


  FairRun* fRun = FairRun::Instance();
  if (strcmp(fRun->GetName(),"TGeant3") == 0) {
    Int_t mediumID;
    mediumID = gGeoManager->GetMedium("SensAir")->GetId();
    SetNdetCuts(mediumID);
    mediumID = gGeoManager->GetMedium("NDetScin")->GetId();
    SetNdetCuts(mediumID);
  }
}

// -----   Public method ProcessHits  --------------------------------------
Bool_t  MpdNDet::ProcessHits(FairVolume* vol)
{
  /** Fill MC point for sensitive NDet volumes **/

  fELoss   = gMC->Edep();
  fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
  fTime    = gMC->TrackTime()*1.0e09;
  fLength  = gMC->TrackLength();
  if (vol->getVolumeId()==fStructureId)
    if (gMC->IsTrackEntering())
    {
      FillWallPoint();
      TParticle* p=((FairStack*)gMC->GetStack())->GetParticle(fTrackID);
      Int_t points=p->GetMother(1);
      Int_t ep=(points&(15<<24))>>24;
      ep++; if (ep>15) ep=15;
      points=(points&(~(15<<24)))|(ep<<24);
      p->SetMother(1, points); 
      ResetParameters();

      return kTRUE;
    }
    else
      return kFALSE;

  if (fELoss>0)
  {
    Int_t i;
    TParticle* p=gMC->GetStack()->GetCurrentTrack();
    Double_t x, y, z;
    Double_t px;
    Double_t py;
    Double_t dx;
    Int_t mphi;
    Int_t mz;
    gMC->TrackPosition(x, y, z);
//    cout << "Id: " << p->GetPdgCode() << " (" << x << ", " << y << ", ";
//    cout << z << "): ";
//    cout << endl;
    gMC->CurrentVolOffID(0, mphi); mphi--;
    gMC->CurrentVolOffID(1, mz); mz--;
    Int_t id=mphi+1+mz*1000;
    fVolumeID=id;
    FillLitePoint(0);
//    type=fInf->GetType(mx, my);
//    cx=cell%type;
//    cy=cell/type;
//    px=mx*fModuleSize-fEcalSize[0]/2.0+cx*fModuleSize/type+1.0;
//    py=my*fModuleSize-fEcalSize[1]/2.0+cy*fModuleSize/type+1.0;
//    cout << "(" << px << ", " << py << "|" << type << "): ";
/*    for(i=0;i<5;i++)
    {
      Int_t t;
      
      gMC->CurrentVolOffID(i, t);
      cout << i << ": " << gMC->CurrentVolOffName(i) << " " << t << "; ";
   }
    cout << endl;
*/
  }
  TParticle* p=((FairStack*)gMC->GetStack())->GetParticle(fTrackID);
  Int_t points=p->GetMother(1);
  Int_t ep=(points&(15<<24))>>24;
  ep++; if (ep>15) ep=15;
  points=(points&(~(15<<24)))|(ep<<24);
  p->SetMother(1, points); 

  ResetParameters();

  return kTRUE;

}

/** returns type of volume **/
Int_t MpdNDet::GetVolType(Int_t volnum)
{
	Int_t i;
	for(i=kN-1;i>-1;i--) {
	  if (fVolArr[i]==volnum) break;
	}
        
	return i;
}

//-----------------------------------------------------------------------------
void MpdNDet::FillWallPoint()
{
  /** Fill MC points on the NDet front wall **/
  gMC->TrackPosition(fPos);
  gMC->TrackMomentum(fMom);
  fVolumeID = -1;
  Double_t mass = gMC->TrackMass();
  // Calculate kinetic energy
  Double_t etot = TMath::Sqrt( fMom.Px()*fMom.Px() +
			       fMom.Py()*fMom.Py() +
			       fMom.Pz()*fMom.Pz() +
			       mass * mass );// - mass;
  fELoss = etot;
  // Create MpdNDetPoint at the entrance of neutron detector
  // for particles with pz>0 coming through the front wall
  AddHit(fTrackID, fVolumeID, TVector3(fPos.X(),  fPos.Y(),  fPos.Z()),
	   TVector3(fMom.Px(), fMom.Py(), fMom.Pz()), fTime, fLength, 
	   fELoss);
  fTrackID=gMC->GetStack()->GetCurrentTrackNumber();

  ((FairStack*)gMC->GetStack())->AddPoint(kNDET);
}

//-----------------------------------------------------------------------------
MpdNDetPointLite* MpdNDet::FindHit(Int_t VolId, Int_t TrackId)
{
  for(Int_t i=fFirstNumber;i<fLiteCollection->GetEntriesFast();i++)
  {
    MpdNDetPointLite* point=(MpdNDetPointLite*)fLiteCollection->At(i);
    if (point->GetTrackID()==TrackId&&point->GetDetectorID()==VolId)
      return point;
  }
  return NULL;
}
//-----------------------------------------------------------------------------
Bool_t MpdNDet::FillLitePoint(Int_t volnum)
{
  /** Fill MC points inside the NDet for non-zero deposited energy **/

  //Search for input track
 
  static Double_t rmin=fAirInnerRad-0.001;
  static Double_t rmax=fAirInnerRad+fAirThickness+0.001; 
  TParticle* part=gMC->GetStack()->GetCurrentTrack();
  fTrackID=gMC->GetStack()->GetCurrentTrackNumber();
  Double_t vx=part->Vx(); vx*=vx;
  Double_t vy=part->Vy(); vy*=vy;
  Double_t r=vx; r+=vy; r=TMath::Sqrt(r); 
  
  while (part->GetFirstMother()>=0&&\
      r<=rmax&&r>=rmin&&\
      TMath::Abs(part->Vz())<=fZSize)
    {
      fTrackID=part->GetFirstMother();
      part =((FairStack*)gMC->GetStack())->GetParticle(fTrackID);
      vx=part->Vx(); vx*=vx;
      vy=part->Vy(); vy*=vy;
      r=vx; r+=vy; r=TMath::Sqrt(r); 
    }
#ifdef _DECAL
  if (fTrackID<0) cout<<"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!fTrackID="<<fTrackID<<endl;
#endif
  MpdNDetPointLite* oldHit;
  MpdNDetPointLite* newHit;
  
  if ((oldHit=FindHit(fVolumeID,fTrackID))!=NULL)
    ChangeHit(oldHit);
  else {
    // Create MpdNDetPoint for scintillator volumes
    newHit = AddLiteHit(fTrackID, fVolumeID, fTime, fELoss);
      
  ((FairStack*)gMC->GetStack())->AddPoint(kNDET);
  }

  return kTRUE;
}

// -----   Public method EndOfEvent   --------------------------------------
void MpdNDet::EndOfEvent() {
  if (fVerboseLevel) Print();
  fNDetCollection->Clear();
  fLiteCollection->Clear();
  fPosIndex = 0;
  fFirstNumber=0;
}
// -------------------------------------------------------------------------

// -----   Public method GetCollection   -----------------------------------
TClonesArray* MpdNDet::GetCollection(Int_t iColl) const
{
  if (iColl == 0) return fNDetCollection;
  return NULL;
//   if (iColl == 1) return fLiteCollection;
//   return NULL;
}
// -------------------------------------------------------------------------

// -----   Public method Reset   -------------------------------------------
void MpdNDet::Reset()
{
  fNDetCollection->Clear();
  fLiteCollection->Clear();
  ResetParameters();
  fFirstNumber=0;
}
// -------------------------------------------------------------------------

// -----   Public method Print   -------------------------------------------
void MpdNDet::Print() const 
{
  Int_t nHits = fNDetCollection->GetEntriesFast();
  Int_t nLiteHits;
  Int_t i;

  cout << "-I- MpdNDet: " << nHits << " points registered in this event.";
  cout << endl;

  nLiteHits = fLiteCollection->GetEntriesFast();
  cout << "-I- MpdNDet: " << nLiteHits << " lite points registered in this event.";
  cout << endl;

  if (fVerboseLevel>1) {
    for (i=0; i<nHits; i++) (*fNDetCollection)[i]->Print();
    for (i=0; i<nLiteHits; i++) (*fLiteCollection)[i]->Print();
  }
}
// -------------------------------------------------------------------------

// -----   Public method CopyClones   --------------------------------------
void MpdNDet::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{   
  Int_t nEntries = cl1->GetEntriesFast();
  Int_t i;
  Int_t index;
  cout << "-I- MpdNDet: " << nEntries << " entries to add." << endl;
  TClonesArray& clref = *cl2;
  if (cl1->GetClass()==MpdNDetPoint::Class()) {
    MpdNDetPoint* oldpoint = NULL;
    for (i=0; i<nEntries; i++) {
      oldpoint = (MpdNDetPoint*) cl1->At(i);
      index = oldpoint->GetTrackID()+offset;
      oldpoint->SetTrackID(index);
      new (clref[fPosIndex]) MpdNDetPoint(*oldpoint);
      fPosIndex++;
    }
    cout << "-I- MpdNDet: " << cl2->GetEntriesFast() << " merged entries."
         << endl;
  }
  else if (cl1->GetClass()==MpdNDetPointLite::Class()) {
    MpdNDetPointLite* oldpoint = NULL;
    for (i=0; i<nEntries; i++) {
      oldpoint = (MpdNDetPointLite*) cl1->At(i);
      index = oldpoint->GetTrackID()+offset;
      oldpoint->SetTrackID(index);
      new (clref[fPosIndex]) MpdNDetPointLite(*oldpoint);
      fPosIndex++;
    }
    cout << "-I- MpdNDet: " << cl2->GetEntriesFast() << " merged entries."
         << endl;
  }
}
// -------------------------------------------------------------------------

// -----   Public method Register   ----------------------------------------
void MpdNDet::Register()
{
  FairRootManager::Instance()->Register("NDetPoint","NDet",fNDetCollection,kTRUE);
  FairRootManager::Instance()->Register("NDetPointLite","NDetLite",fLiteCollection,kTRUE);
}
// -------------------------------------------------------------------------

// -----   Public method ConstructGeometry   -------------------------------
void MpdNDet::ConstructGeometry()
{
  TGeoVolume *volume;
  TGeoVolume *vol1;
  TGeoVolume *vol2;

  Float_t *buf = 0;
  Int_t i;
  Double_t par[10];

  // create SensVacuum which is defined in the media file

  /** Initialize all media **/
  InitMedia();
  par[0]=fAirInnerRad;
  par[1]=fAirInnerRad+fAirThickness;
  par[2]=fZSize+0.001;
  volume=gGeoManager->Volume("NDet", "TUBE",  gGeoManager->GetMedium("SensAir")->GetId(), par, 3);
  gGeoManager->Node("NDet", 1, "cave", 0.0, 0.0, 0.0, 0, kTRUE, buf, 0);
  // An ugly way!!!
  // Need to make a two volumes for each Ndet 
  AddSensitiveVolume(volume);
  fStructureId=volume->GetNumber();
  par[0]=fAirInnerRad+0.001;
  par[1]=fAirInnerRad+fScinThickness+0.001;
  par[2]=fZSize;
  volume=gGeoManager->Volume("NDetScin", "TUBE",  gGeoManager->GetMedium("NDetScin")->GetId(), par, 3);
  gGeoManager->Node("NDetScin", 1, "NDet", 0.0, 0.0, 0.0, 0, kTRUE, buf, 0);
  vol1=gGeoManager->Division("NDetColumn","NDetScin", 3, -1,-fZSize,fZSize*2/fZDivisions,0,"S");
  vol2=gGeoManager->Division("NDetRaw","NDetColumn" , 2,fRDivisions,0.0,-1,0,"N");
  AddSensitiveVolume(vol2);
  fScinId=vol2->GetNumber();

  //gGeoManager->Node("EcalStructure", 1, "Ecal", 0.0, 0.0, 0.0, 0, kTRUE, buf, 0);
}
// -------------------------------------------------------------------------

// ----- Public method BeginEvent  -----------------------------------------
void MpdNDet::BeginEvent()
{
}
// -------------------------------------------------------------------------


// -------------------------------------------------------------------------

// -----   Private method AddHit   -----------------------------------------    
MpdNDetPoint* MpdNDet::AddHit(Int_t trackID, Int_t detID, TVector3 pos,         
			      TVector3 mom, Double_t time, Double_t length,       
			      Double_t eLoss)
{
  TClonesArray& clref = *fNDetCollection;
  Int_t size = clref.GetEntriesFast();
  return new(clref[size]) MpdNDetPoint(trackID, detID, pos, mom,
                                      time, length, eLoss);
}                                                                               
// -------------------------------------------------------------------------

// -----   Private method AddHit   -----------------------------------------    
MpdNDetPointLite* MpdNDet::AddLiteHit(Int_t trackID, Int_t detID, Double32_t time, Double32_t eLoss)
{
  TClonesArray& clref = *fLiteCollection;
  Int_t size = clref.GetEntriesFast();
  return new(clref[size]) MpdNDetPointLite(trackID, detID, time, eLoss);
}
// -------------------------------------------------------------------------


// -----   Private method InitMedium ---------------------------------------    
Int_t MpdNDet::InitMedium(const char* name)
{
  static FairGeoLoader *geoLoad=FairGeoLoader::Instance();
  static FairGeoInterface *geoFace=geoLoad->getGeoInterface();
  static FairGeoMedia *media=geoFace->getMedia();
  static FairGeoBuilder *geoBuild=geoLoad->getGeoBuilder();

  FairGeoMedium *FairMedium=media->getMedium(name);

  if (!FairMedium)
  {
    Fatal("InitMedium","Material %s not defined in media file.", name);
    return -1111;
  }
  return geoBuild->createMedium(FairMedium);
}
// -------------------------------------------------------------------------

// -----   Private method InitMedia ----------------------------------------    
void MpdNDet::InitMedia()
{
  Info("InitMedia", "Initializing media.");
  InitMedium("SensAir");
  InitMedium("NDetScin");
}
// -------------------------------------------------------------------------

// ----- Public method GetCellCoordInf ----------------------------------------
// TODO: !!!
Bool_t MpdNDet::GetCellCoordInf(Int_t fVolID, Float_t &x, Float_t &y, Int_t& tenergy)
{
  return kFALSE;
}

// ------------------------------------------------------------------------------
Bool_t MpdNDet::GetCellCoord(Int_t fVolID, Float_t &x, Float_t &y, Int_t& tenergy)
{
  return GetCellCoordInf(fVolID, x, y, tenergy);
}


ClassImp(MpdNDet)
