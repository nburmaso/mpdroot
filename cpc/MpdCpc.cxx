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

//class FairVolume;

//------------------------------------------------------------------------------------------------------------------------
MpdCpc::MpdCpc() : FairDetector("CPC", kTRUE)
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

		TString volID = gMC->CurrentVolName();
		fCpcID = GetCpcID(volID);
    		fRingID = GetRingID(volID);
    		fCellID = GetCellID(volID);

		cout << "volID  " << volID << "  fCpcID  " << fCpcID << "  fRingID  " << fRingID << "  fCellID  " << fCellID << endl;
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
	   		TVector3(fMom.Px(), fMom.Py(), fMom.Pz()), fTime, fLength, fELoss, fCpcID, fRingID, fCellID);

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

	if (fileName.EndsWith(".root"))
	{
		gLogger->Info(MESSAGE_ORIGIN, "Constructing CPC geometry from ROOT file %s", fileName.Data());
		ConstructRootGeometry();
	}
	else if ( fileName.EndsWith(".geo") ) {
		gLogger->Info(MESSAGE_ORIGIN,
			"Constructing CPC geometry from ASCII file %s",
			fileName.Data());
		ConstructAsciiGeometry();
	}
	/*else if ( fileName.EndsWith(".gdml") )
	{
		gLogger->Info(MESSAGE_ORIGIN,
			"Constructing CPC geometry from GDML file %s",
			fileName.Data());
		ConstructGDMLGeometry();
	}*/
	else
	{
		gLogger->Fatal(MESSAGE_ORIGIN,
			"Geometry format of CPC file %s not supported.",
		fileName.Data());
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
	else std::cerr<<"MpdCpc:: geometry could not be read!"<<std::endl;

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
/*MpdCpcPoint* MpdCpc::AddHit(Int_t trackID, Int_t detID, TVector3 pos,
			    TVector3 mom, Double_t time, Double_t length,
			    Double_t eLoss, Int_t fCpcID, Int_t fRingID, Int_t fCellID)
{
	TClonesArray& clref = *fCpcCollection;
	//TClonesArray &clref = *fCpcCollection;
	Int_t size = clref.GetEntriesFast();

return new(clref[size]) MpdCpcPoint(trackID, detID, pos, mom, time, length, eLoss, fCpcID, fRingID, fCellID);
}*/
MpdCpcPoint *MpdCpc::
AddHit(Int_t trackID, Int_t detID, TVector3 pos,
       TVector3 mom, Double_t time, Double_t length,
       Double_t eLoss, Int_t CpcID, Int_t RingID, Int_t CellID) {
  TClonesArray &clref = *fCpcCollection;
  Int_t size = clref.GetEntriesFast();
  return new (clref[size])
      MpdCpcPoint(trackID, detID, pos, mom, time, length, eLoss, CpcID, RingID, CellID);
}
//------------------------------------------------------------------------------------------------------------------------
//Check if Sensitive-----------------------------------------------------------
Bool_t MpdCpc::CheckIfSensitive(std::string name) {
	TString tsname = name;
	if (tsname.Contains("Active") || tsname.Contains("segmentvolume")|| tsname.Contains("circle")){
//	|| tsname.Contains("cpc01scintcircle2") || tsname.Contains("cpc01l1") || tsname.Contains("cpc01l2")
//	|| tsname.Contains("cpc01l3")|| tsname.Contains("cpc01l4") || tsname.Contains("cpc01l5")
//	|| tsname.Contains("cpc01l6")|| tsname.Contains("cpc01l7")|| tsname.Contains("cpc01l8")) {
		return kTRUE;
	}
	return kFALSE;
}
//------------------------------------------------------------------------------------------------------------------------

Int_t MpdCpc::GetCpcID(TString volname) {

  Int_t CpcID = 0;

  TObjArray *svolnameArr = volname.Tokenize("segmentvolume_");
  if (svolnameArr->GetEntries() < 3) {
    cout << "ERROR: invalid name of volume'" << endl;
    return -1;
  }
  TObjString *CpcNameObjStr;
  TObjString *cellNameObjStr;

  CpcNameObjStr = (TObjString *)svolnameArr->At(0);

  TString CpcNameStr = CpcNameObjStr->GetString();

  Int_t offSet = 0;

  CpcID = offSet + CpcNameStr.Atoi();

  return CpcID;
}
Int_t MpdCpc::GetRingID(TString volname) {

  Int_t RingID = 0;

  TObjArray *svolnameArr = volname.Tokenize("segmentvolume_");
  if (svolnameArr->GetEntries() < 3) {
    cout << "ERROR: invalid name of volume'" << endl;
    return -1;
  }
  TObjString *ringNameObjStr;

  ringNameObjStr = (TObjString *)svolnameArr->At(1);


  TString ringNameStr = ringNameObjStr->GetString();

  Int_t offSet = 0;

  RingID = offSet + ringNameStr.Atoi();

  return RingID;
}
Int_t MpdCpc::GetCellID(TString volname) {

  Int_t detectorID = 0;

  TObjArray *svolnameArr = volname.Tokenize("segmentvolume_");
  if (svolnameArr->GetEntries() < 3) {
    cout << "ERROR: invalid name of volume'" << endl;
    return -1;
  }
  TObjString *cellNameObjStr;

  cellNameObjStr = (TObjString *)svolnameArr->At(2);

  TString cellNameStr = cellNameObjStr->GetString();

  Int_t offSet = 0;

  detectorID = offSet + cellNameStr.Atoi();

  return detectorID;
}
ClassImp(MpdCpc)
