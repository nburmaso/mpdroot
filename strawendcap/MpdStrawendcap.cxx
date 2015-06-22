//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                       MpdStrawendcap source file                      -----
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
#include "FairStack.h"
#include "MpdStrawendcapGeo.h"
#include "FairRootManager.h"
#include "MpdStrawendcap.h"
#include "MpdStrawendcapPoint.h"
#include "FairRuntimeDb.h"
#include "MpdStrawendcapGeoPar.h"
#include "TObjArray.h"
#include "FairRun.h"
#include "FairVolume.h"
#include "TMath.h"

#include "TParticlePDG.h"

#include "TGeoManager.h"
#include "TGDMLParse.h"
#include "FairGeoMedia.h"

class FairVolume;

//------------------------------------------------------------------------------------------------------------------------
MpdStrawendcap::MpdStrawendcap()
 : FairDetector("STT", kTRUE)
{
    fStrawendcapCollection = new TClonesArray("MpdStrawendcapPoint");
    fPosIndex = 0;
    fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdStrawendcap::MpdStrawendcap(const char* name, Bool_t active)
 : FairDetector(name, active)
{
    fStrawendcapCollection = new TClonesArray("MpdStrawendcapPoint");
    fPosIndex = 0;
    fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdStrawendcap::~MpdStrawendcap()
{
	if(fStrawendcapCollection){ fStrawendcapCollection->Delete(); delete fStrawendcapCollection; }
}
//------------------------------------------------------------------------------------------------------------------------
int MpdStrawendcap::DistAndPoints(TVector3 p1, TVector3 p2, TVector3 p3, TVector3 p4, TVector3& pa, TVector3& pb) {
    TVector3 A = p2 - p1;
    TVector3 B = p4 - p3;
    TVector3 C = p1 - p3;

    if (p3 == p4) {
        pb = p4;
        TVector3 unit = A.Unit();
        Double_t dist = unit.Dot(-C);
        pa = p1 + dist * unit;
        return 0;
    }

    Double_t numer = C.Dot(B) * B.Dot(A) - C.Dot(A) * B.Dot(B);
    Double_t denom = A.Dot(A) * B.Dot(B) - B.Dot(A) * B.Dot(A);
    if (denom == 0) {
        // parallel lines
        pa.SetXYZ(-10000,-10000,-10000);
        pb.SetXYZ(-10000,-10000,-10000);
        return 1;
    }
    Double_t ma = numer / denom;
    Double_t mb = ( C.Dot(B) + B.Dot(A)*ma ) / B.Dot(B);
    pa = p1 + A*ma;
    pb = p3 + B*mb;
    return 0;
}
//------------------------------------------------------------------------------------------------------------------------
TVector3 MpdStrawendcap::GlobalToLocal(TVector3& global) {
    Double_t globPos[3];
    Double_t localPos[3];
    global.GetXYZ(globPos);
    gMC->Gmtod(globPos, localPos, 1);
    return TVector3(localPos);
}
//------------------------------------------------------------------------------------------------------------------------
TVector3 MpdStrawendcap::LocalToGlobal(TVector3& local) {
    Double_t globPos[3];
    Double_t localPos[3];
    local.GetXYZ(localPos);
    gMC->Gdtom(localPos, globPos, 1);
    return TVector3(globPos);
}
//----------------------------------------------------------------------------------------------------------------------
Bool_t MpdStrawendcap::ProcessHits(FairVolume* vol) {
    //AZ
    static Int_t first = 1, nLayMod = 30; //!!! geometry dependent
    static Double_t z0[4] = {156.5, 186.5, 0, 0};
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
    Int_t gap, cell, module, region;
    static Int_t sameVol = 0;
    TString Volname;

    // Set parameters at entrance of volume. Reset ELoss.
    // Check if this is not due to passing through the anode wire.
    // If anode wire, do not reset anything.
    if (gMC->IsTrackEntering()) {
        sameVol = 1;
        gMC->CurrentVolOffID(2, module);
        if (gMC->GetStack()->GetCurrentTrackNumber() != fTrackID ||
            gMC->CurrentVolID(gap)+1000*module != fVolumeID) {
            ResetParameters();
            sameVol = 0;
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
            fVolumeID = gMC->CurrentVolID(gap) + 1000 * module;
            fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
        }
    }

    // Sum energy loss for all steps in the active volume
    fELoss += gMC->Edep();

    // Create MpdStrawendcapPoint at EXIT of active volume;
    //if (gMC->IsTrackExiting() && fELoss > 0) {
    if ( (gMC->IsTrackExiting() || gMC->IsTrackStop() || gMC->IsTrackDisappeared()) && fELoss > 0 ) {
        fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
        //   Volname = vol->getName();
        Volname = vol->getRealName();  // EL
        //cout << Volname << endl;
        //AZ region = Volname[5] - '0';   //?????????????????????????
        gMC->CurrentVolID(gap);
        gMC->CurrentVolOffID(1, cell);
        gMC->CurrentVolOffID(2, module);

        TLorentzVector PosOut;
        gMC->TrackPosition(PosOut);
        fPosOut.SetXYZ(PosOut.X(), PosOut.Y(), PosOut.Z());

        //AZ Get layer number
        Int_t wheel;
        sscanf(&(Volname[4]),"%d",&wheel); // wheel number
        Double_t dz = TMath::Abs(PosOut.Z()) - z0[wheel-1];
        //Int_t lay = TMath::Nint(dz/1.) + 1;
        //Int_t lay = TMath::Nint(dz/1.) + 2; // geometry with triplets
        Int_t lay = TMath::Nint(dz/0.85) + 1; // geometry with triplets
        if (wheel == 2 || wheel == 4) lay += nLayMod;
        region = lay * 1000 + cell; // lay*1000+tubeID
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
        int result = DistAndPoints(p1, p2, fPosIn, fPosOut, fPos, trackPosition);
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
        if (sameVol) fStrawendcapCollection->RemoveAt(fStrawendcapCollection->GetEntriesFast()-1);
        MpdStrawendcapPoint *p =
            AddHit(fTrackID, region, fPos, fPos.Perp(), TVector3(fMom.Px(), fMom.Py(), fMom.Pz()),
                 fTime, (fLength+gMC->TrackLength())/2, fELoss, fIsPrimary, fCharge, fPdgId, trackPosition);
        p->SetPhi(phi); //AZ

        ((FairStack*)gMC->GetStack())->AddPoint(kECT);
        //ResetParameters();
    }

    return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdStrawendcap::EndOfEvent()
{
    if(fVerboseLevel) Print();
    fStrawendcapCollection->Delete();
    fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdStrawendcap::Register(){ FairRootManager::Instance()->Register("STRAWPoint", "Strawendcap", fStrawendcapCollection, kTRUE); }
//------------------------------------------------------------------------------------------------------------------------
TClonesArray* MpdStrawendcap::GetCollection(Int_t iColl) const {
    if(iColl == 0) return fStrawendcapCollection;
    return NULL;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdStrawendcap::Print() const
{
    Int_t nHits = fStrawendcapCollection->GetEntriesFast();
    cout << "-I- MpdStrawendcap: " << nHits << " points registered in this event." << endl;

    if(fVerboseLevel > 1)
        for(Int_t i=0; i<nHits; i++) (*fStrawendcapCollection)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------

void MpdStrawendcap::Reset(){ fStrawendcapCollection->Delete(); ResetParameters(); }
//------------------------------------------------------------------------------------------------------------------------

void MpdStrawendcap::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
    Int_t nEntries = cl1->GetEntriesFast();
    cout << "-I- MpdStrawendcap: " << nEntries << " entries to add." << endl;
    TClonesArray& clref = *cl2;
    MpdStrawendcapPoint* oldpoint = NULL;

    for(Int_t i=0; i<nEntries; i++) {
        oldpoint = (MpdStrawendcapPoint*) cl1->At(i);
        Int_t index = oldpoint->GetTrackID() + offset;
        oldpoint->SetTrackID(index);
        new (clref[fPosIndex]) MpdStrawendcapPoint(*oldpoint);
        fPosIndex++;
    }

    cout << "-I- MpdStrawendcap: " << cl2->GetEntriesFast() << " merged entries."  << endl;
}
//------------------------------------------------------------------------------------------------------------------------

void MpdStrawendcap::ConstructGeometry() {
    TString fileName = GetGeometryFileName();

    if ( fileName.EndsWith(".root") ) {
        gLogger->Info(MESSAGE_ORIGIN,
            "Constructing TPC geometry from ROOT file %s",
            fileName.Data());
        ConstructRootGeometry();
    }
    else if ( fileName.EndsWith(".geo") ) {
        gLogger->Info(MESSAGE_ORIGIN,
            "Constructing TPC geometry from ASCII file %s",
            fileName.Data());
        ConstructAsciiGeometry();
    }
    else if ( fileName.EndsWith(".gdml") ) {
        gLogger->Info(MESSAGE_ORIGIN,
            "Constructing TPC geometry from GDML file %s",
            fileName.Data());
        //ConstructGDMLGeometry();
    }
    else {
        gLogger->Fatal(MESSAGE_ORIGIN,
            "Geometry format of TPC file %s not supported.",
            fileName.Data());
    }
}

// -----   ConstructAsciiGeometry   -------------------------------------------
void MpdStrawendcap::ConstructAsciiGeometry() {

    Int_t count=0;
    Int_t count_tot=0;

    FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
    FairGeoInterface* geoFace = geoLoad->getGeoInterface();
    MpdStrawendcapGeo* sttGeo  = new MpdStrawendcapGeo();
    sttGeo->setGeomFile(GetGeometryFileName());
    geoFace->addGeoModule(sttGeo);

    Bool_t rc = geoFace->readSet(sttGeo);
    if(rc) sttGeo->create(geoLoad->getGeoBuilder());
    TList* volList = sttGeo->getListOfVolumes();

    // store geo parameter
    FairRun *fRun = FairRun::Instance();
    FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
    MpdStrawendcapGeoPar* par =(MpdStrawendcapGeoPar*)(rtdb->getContainer("MpdStrawendcapGeoPar"));
    TObjArray *fSensNodes = par->GetGeoSensitiveNodes();
    TObjArray *fPassNodes = par->GetGeoPassiveNodes();

    FairGeoNode *node   = NULL;
    FairGeoVolume *aVol = NULL;
    TListIter iter(volList);

    while((node = (FairGeoNode*)iter.Next())) {
        aVol = dynamic_cast<FairGeoVolume*> (node);
        if(node->isSensitive()){ fSensNodes->AddLast(aVol); count++; }
        else fPassNodes->AddLast(aVol);
        count_tot++;
    }

    par->setChanged();
    par->setInputVersion(fRun->GetRunId(), 1);
    ProcessNodes(volList);
}
// ----------------------------------------------------------------------------

// -----   ConstructGDMLGeometry   -------------------------------------------
void MpdStrawendcap::ConstructGDMLGeometry()
{
    TFile *old = gFile;
    TGDMLParse parser;
    TGeoVolume* gdmlTop;

    // Before importing GDML
    Int_t maxInd = gGeoManager->GetListOfMedia()->GetEntries() - 1;

    gdmlTop = parser.GDMLReadFile(GetGeometryFileName());

    // Cheating - reassigning media indices after GDML import (need to fix this in TGDMLParse class!!!)
    //   for (Int_t i=0; i<gGeoManager->GetListOfMedia()->GetEntries(); i++)
    //      gGeoManager->GetListOfMedia()->At(i)->Dump();
    // After importing GDML
    Int_t j = gGeoManager->GetListOfMedia()->GetEntries() - 1;
    Int_t curId;
    TGeoMedium* m;
    do {
        m = (TGeoMedium*)gGeoManager->GetListOfMedia()->At(j);
        curId = m->GetId();
        m->SetId(curId+maxInd);
        j--;
    } while (curId > 1);
    //   LOG(DEBUG) << "====================================================================" << FairLogger::endl;
    //   for (Int_t i=0; i<gGeoManager->GetListOfMedia()->GetEntries(); i++)
    //      gGeoManager->GetListOfMedia()->At(i)->Dump();

    Int_t newMaxInd = gGeoManager->GetListOfMedia()->GetEntries() - 1;

    gGeoManager->GetTopVolume()->AddNode(gdmlTop, 1, 0);
    ExpandNodeForGdml(gGeoManager->GetTopVolume()->GetNode(gGeoManager->GetTopVolume()->GetNdaughters()-1));

    for (Int_t k = maxInd+1; k < newMaxInd+1; k++) {
        TGeoMedium* medToDel = (TGeoMedium*)(gGeoManager->GetListOfMedia()->At(maxInd+1));
        LOG(DEBUG) << "    removing media " << medToDel->GetName() << " with id " << medToDel->GetId() << " (k=" << k << ")" << FairLogger::endl;
        gGeoManager->GetListOfMedia()->Remove(medToDel);
    }
    gGeoManager->SetAllIndex();

    gFile = old;
}

void MpdStrawendcap::ExpandNodeForGdml(TGeoNode* node)
{
    LOG(DEBUG) << "----------------------------------------- ExpandNodeForGdml for node " << node->GetName() << FairLogger::endl;

    TGeoVolume* curVol = node->GetVolume();

    LOG(DEBUG) << "    volume: " << curVol->GetName() << FairLogger::endl;

    if (curVol->IsAssembly()) {
        LOG(DEBUG) << "    skipping volume-assembly" << FairLogger::endl;
    }
    else {
        TGeoMedium* curMed = curVol->GetMedium();
        TGeoMaterial* curMat = curVol->GetMaterial();
        TGeoMedium* curMedInGeoManager = gGeoManager->GetMedium(curMed->GetName());
        TGeoMaterial* curMatOfMedInGeoManager = curMedInGeoManager->GetMaterial();
        TGeoMaterial* curMatInGeoManager = gGeoManager->GetMaterial(curMat->GetName());

        // Current medium and material assigned to the volume from GDML
        LOG(DEBUG2) << "    curMed\t\t\t\t" << curMed << "\t" << curMed->GetName() << "\t" << curMed->GetId() << FairLogger::endl;
        LOG(DEBUG2) << "    curMat\t\t\t\t" << curMat << "\t" << curMat->GetName() << "\t" << curMat->GetIndex() << FairLogger::endl;

        // Medium and material found in the gGeoManager - either the pre-loaded one or one from GDML
        LOG(DEBUG2) << "    curMedInGeoManager\t\t" << curMedInGeoManager
                 << "\t" << curMedInGeoManager->GetName() << "\t" << curMedInGeoManager->GetId() << FairLogger::endl;
        LOG(DEBUG2) << "    curMatOfMedInGeoManager\t\t" << curMatOfMedInGeoManager
                 << "\t" << curMatOfMedInGeoManager->GetName() << "\t" << curMatOfMedInGeoManager->GetIndex() << FairLogger::endl;
        LOG(DEBUG2) << "    curMatInGeoManager\t\t" << curMatInGeoManager
                 << "\t" << curMatInGeoManager->GetName() << "\t" << curMatInGeoManager->GetIndex() << FairLogger::endl;

        TString matName = curMat->GetName();
        TString medName = curMed->GetName();

        if (curMed->GetId() != curMedInGeoManager->GetId()) {
            if (fFixedMedia.find(medName) == fFixedMedia.end()) {
                LOG(DEBUG) << "    Medium needs to be fixed" << FairLogger::endl;
                fFixedMedia[medName] = curMedInGeoManager;
                Int_t ind = curMat->GetIndex();
                gGeoManager->RemoveMaterial(ind);
                LOG(DEBUG) << "    removing material " << curMat->GetName()
                    << " with index " << ind << FairLogger::endl;
                for (Int_t i=ind; i<gGeoManager->GetListOfMaterials()->GetEntries(); i++) {
                    TGeoMaterial* m = (TGeoMaterial*)gGeoManager->GetListOfMaterials()->At(i);
                    m->SetIndex(m->GetIndex()-1);
                }

                LOG(DEBUG) << "    Medium fixed" << FairLogger::endl;
            }
            else {
                LOG(DEBUG) << "    Already fixed medium found in the list    " << FairLogger::endl;
            }
        }
        else {
            if (fFixedMedia.find(medName) == fFixedMedia.end()) {
                LOG(DEBUG) << "    There is no correct medium in the memory yet" << FairLogger::endl;

                FairGeoLoader* geoLoad = FairGeoLoader::Instance();
                FairGeoInterface* geoFace = geoLoad->getGeoInterface();
                FairGeoMedia* geoMediaBase =  geoFace->getMedia();
                FairGeoBuilder* geobuild = geoLoad->getGeoBuilder();

                FairGeoMedium* curMedInGeo = geoMediaBase->getMedium(medName);
                if (curMedInGeo == 0) {
                    LOG(FATAL) << "    Media not found in Geo file: " << medName << FairLogger::endl;
                    //! This should not happen.
                    //! This means that somebody uses material in GDML that is not in the media.geo file.
                    //! Most probably this is the sign to the user to check materials' names in the CATIA model.
                }
                else {
                    LOG(DEBUG) << "    Found media in Geo file" << medName << FairLogger::endl;
                    Int_t nmed = geobuild->createMedium(curMedInGeo);
                    fFixedMedia[medName] = (TGeoMedium*)gGeoManager->GetListOfMedia()->Last();
                    gGeoManager->RemoveMaterial(curMatOfMedInGeoManager->GetIndex());
                    LOG(DEBUG) << "    removing material " << curMatOfMedInGeoManager->GetName()
                        << " with index " << curMatOfMedInGeoManager->GetIndex() << FairLogger::endl;
                    for (Int_t i=curMatOfMedInGeoManager->GetIndex(); i<gGeoManager->GetListOfMaterials()->GetEntries(); i++) {
                        TGeoMaterial* m = (TGeoMaterial*)gGeoManager->GetListOfMaterials()->At(i);
                        m->SetIndex(m->GetIndex()-1);
                    }
                }

                if (curMedInGeo->getSensitivityFlag()) {
                    LOG(DEBUG) << "    Adding sensitive  " << curVol->GetName() << FairLogger::endl;
                    AddSensitiveVolume(curVol);
                }
            }
            else {
                LOG(DEBUG) << "    Already fixed medium found in the list" << FairLogger::endl;
                LOG(DEBUG) << "!!! Sensitivity: " << fFixedMedia[medName]->GetParam(0) << FairLogger::endl;
                if (fFixedMedia[medName]->GetParam(0) == 1) {
                    LOG(DEBUG) << "    Adding sensitive  " << curVol->GetName() << FairLogger::endl;
                    AddSensitiveVolume(curVol);
                }
            }
        }

        curVol->SetMedium(fFixedMedia[medName]);
        gGeoManager->SetAllIndex();

  //      gGeoManager->GetListOfMaterials()->Print();
  //      gGeoManager->GetListOfMedia()->Print();

    }

    //! Recursevly go down the tree of nodes
    if (curVol->GetNdaughters() != 0) {
        TObjArray* NodeChildList = curVol->GetNodes();
        TGeoNode* curNodeChild;
        for (Int_t j=0; j<NodeChildList->GetEntriesFast(); j++) {
            curNodeChild = (TGeoNode*)NodeChildList->At(j);
            ExpandNodeForGdml(curNodeChild);
        }
    }
}
//-----------------------------------------------------------------------------

//Check if Sensitive-----------------------------------------------------------
Bool_t MpdStrawendcap::CheckIfSensitive(std::string name) {
    TString tsname = name;
    if (tsname.Contains("straw_gas")) {
        return kTRUE;
    }
    return kFALSE;
}
//-----------------------------------------------------------------------------

MpdStrawendcapPoint* MpdStrawendcap::AddHit(Int_t trackID, Int_t detID, TVector3 pos, Double_t radius,
			    TVector3 mom, Double_t time, Double_t length,
			    Double_t eLoss, Int_t isPrimary, Double_t charge, Int_t pdgId, TVector3 trackPos) {
    TClonesArray& clref = *fStrawendcapCollection;
    Int_t size = clref.GetEntriesFast();
    //std::cout << "ELoss: " << eLoss << "\n";
    return new(clref[size]) MpdStrawendcapPoint(trackID, detID, pos, radius, mom, time, length, eLoss, isPrimary, charge, pdgId, trackPos);
}
//------------------------------------------------------------------------------------------------------------------------

ClassImp(MpdStrawendcap)