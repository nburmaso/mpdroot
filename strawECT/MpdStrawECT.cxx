//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                       MpdStrawECT source file                      -----
// -------------------------------------------------------------------------

#include <iostream>

#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TParticle.h"
#include "TVirtualMC.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"

#include "FairGeoInterface.h"
#include "FairGeoLoader.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "MpdStack.h"
#include "MpdStrawECTGeo.h"
#include "FairRootManager.h"
#include "MpdStrawECT.h"
#include "MpdStrawECTPoint.h"
#include "FairRuntimeDb.h"
#include "MpdStrawECTGeoPar.h"
#include "TObjArray.h"
#include "FairRun.h"
#include "FairVolume.h"
#include "TMath.h"

#include "TParticlePDG.h"

#include "TGeoManager.h"
#include "TGDMLParse.h"
#include "FairGeoMedia.h"

#include "TString.h"

class FairVolume;

//------------------------------------------------------------------------------------------------------------------------
MpdStrawECT::MpdStrawECT()
 : FairDetector("strawECT", kTRUE)
{
    fStrawECTCollection = new TClonesArray("MpdStrawECTPoint");
    fPosIndex = 0;
    fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdStrawECT::MpdStrawECT(const char* name, Bool_t active)
 : FairDetector(name, active)
{
    fStrawECTCollection = new TClonesArray("MpdStrawECTPoint");
    fPosIndex = 0;
    fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdStrawECT::~MpdStrawECT()
{
    if(fStrawECTCollection){ fStrawECTCollection->Delete(); delete fStrawECTCollection; }
}
//------------------------------------------------------------------------------------------------------------------------
int MpdStrawECT::DistAndPoints(TVector3 p1, TVector3 p2, TVector3 p3, TVector3 p4, TVector3& pa, TVector3& pb) {
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
TVector3 MpdStrawECT::GlobalToLocal(TVector3& global) {
    Double_t globPos[3];
    Double_t localPos[3];
    global.GetXYZ(globPos);
    gMC->Gmtod(globPos, localPos, 1);
    return TVector3(localPos);
}
//------------------------------------------------------------------------------------------------------------------------
TVector3 MpdStrawECT::LocalToGlobal(TVector3& local) {
    Double_t globPos[3];
    Double_t localPos[3];
    local.GetXYZ(localPos);
    gMC->Gdtom(localPos, globPos, 1);
    return TVector3(globPos);
}
//----------------------------------------------------------------------------------------------------------------------
Bool_t MpdStrawECT::ProcessHits(FairVolume* vol) {
    // Set parameters at entrance of volume. Reset ELoss.
    if(gMC->IsTrackEntering()) {
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
        fVolumeID = vol->getMCid();
        fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
    }

    // Sum energy loss for all steps in the active volume
    fELoss += gMC->Edep();

    // Create MpdStrawECTPoint at EXIT of active volume;
    if ( (gMC->IsTrackExiting() || gMC->IsTrackStop() || gMC->IsTrackDisappeared()) && fELoss > 0 ) {
        fTrackID = gMC->GetStack()->GetCurrentTrackNumber();

        TLorentzVector PosOut;
        gMC->TrackPosition(PosOut);
        fPosOut.SetXYZ(PosOut.X(), PosOut.Y(), PosOut.Z());

//--------  define id for volumes from vol. path -------------------------------
        TString vol_path = gMC->CurrentVolPath();

        Int_t module_num = -1;
        Int_t submodule_num = -1;
        Int_t layer_num = -1;
        TString layer_type = "";
        Int_t straw_num = -1;

        TString key_module = "module_";
        TString key_submodule = "sub_module_";
        TString key_layer_radial = "layer_radial_";
        TString key_layer_stereoR = "layer_stereoR_";
        TString key_layer_stereoL = "layer_stereoL_";
        TString key_straw_tube = "straw_tube_";
        TString key_delim = "/";


        Int_t current_pos = 0;
        Int_t pos_in = 0;
        Int_t pos_out = 0;

        //module number
        pos_in = vol_path.Index(key_module, current_pos);
        pos_in += key_module.Length();
        pos_out = vol_path.Index(key_delim, pos_in);
        current_pos = pos_out;
        module_num = ((TString)(vol_path(pos_in, pos_out-pos_in))).Atoi();

        //submodule number
        pos_in = vol_path.Index(key_submodule, current_pos);
        pos_in += key_submodule.Length();
        pos_out = vol_path.Index(key_delim, pos_in);
        current_pos = pos_out;
        submodule_num = ((TString)(vol_path(pos_in, pos_out-pos_in))).Atoi();

        //layer number
        pos_in = vol_path.Index(key_layer_radial, current_pos);
        if(pos_in != -1) {
            pos_in += key_layer_radial.Length();
            layer_type = "radial";
        }
        if(pos_in == -1) {
            pos_in = vol_path.Index(key_layer_stereoR, current_pos);
            if(pos_in != -1) {
                pos_in += key_layer_stereoR.Length();
                layer_type = "stereoR";
            }
        }
        if(pos_in == -1) {
            pos_in = vol_path.Index(key_layer_stereoL, current_pos);
            if(pos_in != -1) {
                pos_in += key_layer_stereoL.Length();
                layer_type = "stereoL";
            }
        }
        if(pos_in != -1) {
            pos_out = vol_path.Index(key_delim, pos_in);
            current_pos = pos_out;
            layer_num = ((TString)(vol_path(pos_in, pos_out-pos_in))).Atoi();
        }

        //straw number
        pos_in = vol_path.Index(key_straw_tube, current_pos);
        pos_in += key_straw_tube.Length();
        pos_out = vol_path.Index(key_delim, pos_in);
        current_pos = pos_out;
        straw_num = ((TString)(vol_path(pos_in, pos_out-pos_in))).Atoi();

        //cout << "vol_path = " << vol_path << "\n";
        //cout << "module_num    = " << module_num << "\n";
        //cout << "submodule_num = " << submodule_num << "\n";
        //cout << "layer_type    = " << layer_type << "\n";
        //cout << "layer_num     = " << layer_num << "\n";
        //cout << "straw_num     = " << straw_num << "\n";
//------------------------------------------------------------------------------

        MpdStrawECTPoint *p =
            AddHit(fTrackID, fVolumeID, fPosIn, TVector3(fMom.Px(), fMom.Py(), fMom.Pz()),
                 fTime, (fLength+gMC->TrackLength())/2, fELoss);

        p->SetModule(module_num);
        p->SetSubmodule(submodule_num);
        p->SetLayer(layer_num);
        p->SetLayerType(layer_type);
        p->SetStraw(straw_num);

        ((MpdStack*)gMC->GetStack())->AddPoint(kECT);
    }

    return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdStrawECT::EndOfEvent()
{
    if(fVerboseLevel) Print();
    fStrawECTCollection->Delete();
    fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdStrawECT::Register(){ FairRootManager::Instance()->Register("StrawECTPoint", "StrawECT", fStrawECTCollection, kTRUE); }
//------------------------------------------------------------------------------------------------------------------------
TClonesArray* MpdStrawECT::GetCollection(Int_t iColl) const {
    if(iColl == 0) return fStrawECTCollection;
    return NULL;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdStrawECT::Print() const
{
    Int_t nHits = fStrawECTCollection->GetEntriesFast();
    cout << "-I- MpdStrawECT: " << nHits << " points registered in this event." << endl;

    if(fVerboseLevel > 1)
        for(Int_t i=0; i<nHits; i++) (*fStrawECTCollection)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------

void MpdStrawECT::Reset(){ fStrawECTCollection->Delete(); ResetParameters(); }
//------------------------------------------------------------------------------------------------------------------------

void MpdStrawECT::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
    Int_t nEntries = cl1->GetEntriesFast();
    cout << "-I- MpdStrawECT: " << nEntries << " entries to add." << endl;
    TClonesArray& clref = *cl2;
    MpdStrawECTPoint* oldpoint = NULL;

    for(Int_t i=0; i<nEntries; i++) {
        oldpoint = (MpdStrawECTPoint*) cl1->At(i);
        Int_t index = oldpoint->GetTrackID() + offset;
        oldpoint->SetTrackID(index);
        new (clref[fPosIndex]) MpdStrawECTPoint(*oldpoint);
        fPosIndex++;
    }

    cout << "-I- MpdStrawECT: " << cl2->GetEntriesFast() << " merged entries."  << endl;
}
//------------------------------------------------------------------------------------------------------------------------

void MpdStrawECT::ConstructGeometry() {
    TString fileName = GetGeometryFileName();

    if ( fileName.EndsWith(".root") ) {
        LOG(INFO) <<
            "Constructing straw ECT geometry from ROOT file " <<
            fileName.Data();
        ConstructRootGeometry();
    }
    else if ( fileName.EndsWith(".geo") ) {
        LOG(INFO) <<
            "Constructing straw ECT geometry from ASCII file " <<
            fileName.Data();
        ConstructAsciiGeometry();
    }
    else if ( fileName.EndsWith(".gdml") ) {
        LOG(INFO) <<
            "Constructing straw ECT geometry from GDML file " <<
            fileName.Data();
        //ConstructGDMLGeometry();
    }
    else {
         LOG(FATAL) <<
            "Geometry format of straw ECT file " <<
            fileName.Data() << " not supported.";
    }
}

// -----   ConstructAsciiGeometry   -------------------------------------------
void MpdStrawECT::ConstructAsciiGeometry() {

    Int_t count=0;
    Int_t count_tot=0;

    FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
    FairGeoInterface* geoFace = geoLoad->getGeoInterface();
    MpdStrawECTGeo* sttGeo  = new MpdStrawECTGeo();
    sttGeo->setGeomFile(GetGeometryFileName());
    geoFace->addGeoModule(sttGeo);

    Bool_t rc = geoFace->readSet(sttGeo);
    if(rc) sttGeo->create(geoLoad->getGeoBuilder());
    TList* volList = sttGeo->getListOfVolumes();

    // store geo parameter
    FairRun *fRun = FairRun::Instance();
    FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
    MpdStrawECTGeoPar* par =(MpdStrawECTGeoPar*)(rtdb->getContainer("MpdStrawECTGeoPar"));
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
void MpdStrawECT::ConstructGDMLGeometry()
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
    //   LOG(DEBUG) << "====================================================================" << "";
    //   for (Int_t i=0; i<gGeoManager->GetListOfMedia()->GetEntries(); i++)
    //      gGeoManager->GetListOfMedia()->At(i)->Dump();

    Int_t newMaxInd = gGeoManager->GetListOfMedia()->GetEntries() - 1;

    gGeoManager->GetTopVolume()->AddNode(gdmlTop, 1, 0);
    ExpandNodeForGdml(gGeoManager->GetTopVolume()->GetNode(gGeoManager->GetTopVolume()->GetNdaughters()-1));

    for (Int_t k = maxInd+1; k < newMaxInd+1; k++) {
        TGeoMedium* medToDel = (TGeoMedium*)(gGeoManager->GetListOfMedia()->At(maxInd+1));
        LOG(DEBUG) << "    removing media " << medToDel->GetName() << " with id " << medToDel->GetId() << " (k=" << k << ")" << "";
        gGeoManager->GetListOfMedia()->Remove(medToDel);
    }
    gGeoManager->SetAllIndex();

    gFile = old;
}

void MpdStrawECT::ExpandNodeForGdml(TGeoNode* node)
{
    LOG(DEBUG) << "----------------------------------------- ExpandNodeForGdml for node " << node->GetName() << "";

    TGeoVolume* curVol = node->GetVolume();

    LOG(DEBUG) << "    volume: " << curVol->GetName() << "";

    if (curVol->IsAssembly()) {
        LOG(DEBUG) << "    skipping volume-assembly" << "";
    }
    else {
        TGeoMedium* curMed = curVol->GetMedium();
        TGeoMaterial* curMat = curVol->GetMaterial();
        TGeoMedium* curMedInGeoManager = gGeoManager->GetMedium(curMed->GetName());
        TGeoMaterial* curMatOfMedInGeoManager = curMedInGeoManager->GetMaterial();
        TGeoMaterial* curMatInGeoManager = gGeoManager->GetMaterial(curMat->GetName());

        // Current medium and material assigned to the volume from GDML
        LOG(DEBUG2) << "    curMed\t\t\t\t" << curMed << "\t" << curMed->GetName() << "\t" << curMed->GetId() << "";
        LOG(DEBUG2) << "    curMat\t\t\t\t" << curMat << "\t" << curMat->GetName() << "\t" << curMat->GetIndex() << "";

        // Medium and material found in the gGeoManager - either the pre-loaded one or one from GDML
        LOG(DEBUG2) << "    curMedInGeoManager\t\t" << curMedInGeoManager
                 << "\t" << curMedInGeoManager->GetName() << "\t" << curMedInGeoManager->GetId() << "";
        LOG(DEBUG2) << "    curMatOfMedInGeoManager\t\t" << curMatOfMedInGeoManager
                 << "\t" << curMatOfMedInGeoManager->GetName() << "\t" << curMatOfMedInGeoManager->GetIndex() << "";
        LOG(DEBUG2) << "    curMatInGeoManager\t\t" << curMatInGeoManager
                 << "\t" << curMatInGeoManager->GetName() << "\t" << curMatInGeoManager->GetIndex() << "";

        TString matName = curMat->GetName();
        TString medName = curMed->GetName();

        if (curMed->GetId() != curMedInGeoManager->GetId()) {
            if (fFixedMedia.find(medName) == fFixedMedia.end()) {
                LOG(DEBUG) << "    Medium needs to be fixed" << "";
                fFixedMedia[medName] = curMedInGeoManager;
                Int_t ind = curMat->GetIndex();
                gGeoManager->RemoveMaterial(ind);
                LOG(DEBUG) << "    removing material " << curMat->GetName()
                    << " with index " << ind << "";
                for (Int_t i=ind; i<gGeoManager->GetListOfMaterials()->GetEntries(); i++) {
                    TGeoMaterial* m = (TGeoMaterial*)gGeoManager->GetListOfMaterials()->At(i);
                    m->SetIndex(m->GetIndex()-1);
                }

                LOG(DEBUG) << "    Medium fixed" << "";
            }
            else {
                LOG(DEBUG) << "    Already fixed medium found in the list    " << "";
            }
        }
        else {
            if (fFixedMedia.find(medName) == fFixedMedia.end()) {
                LOG(DEBUG) << "    There is no correct medium in the memory yet" << "";

                FairGeoLoader* geoLoad = FairGeoLoader::Instance();
                FairGeoInterface* geoFace = geoLoad->getGeoInterface();
                FairGeoMedia* geoMediaBase =  geoFace->getMedia();
                FairGeoBuilder* geobuild = geoLoad->getGeoBuilder();

                FairGeoMedium* curMedInGeo = geoMediaBase->getMedium(medName);
                if (curMedInGeo == 0) {
                    LOG(FATAL) << "    Media not found in Geo file: " << medName << "";
                    //! This should not happen.
                    //! This means that somebody uses material in GDML that is not in the media.geo file.
                    //! Most probably this is the sign to the user to check materials' names in the CATIA model.
                }
                else {
                    LOG(DEBUG) << "    Found media in Geo file" << medName << "";
                    Int_t nmed = geobuild->createMedium(curMedInGeo);
                    fFixedMedia[medName] = (TGeoMedium*)gGeoManager->GetListOfMedia()->Last();
                    gGeoManager->RemoveMaterial(curMatOfMedInGeoManager->GetIndex());
                    LOG(DEBUG) << "    removing material " << curMatOfMedInGeoManager->GetName()
                        << " with index " << curMatOfMedInGeoManager->GetIndex() << "";
                    for (Int_t i=curMatOfMedInGeoManager->GetIndex(); i<gGeoManager->GetListOfMaterials()->GetEntries(); i++) {
                        TGeoMaterial* m = (TGeoMaterial*)gGeoManager->GetListOfMaterials()->At(i);
                        m->SetIndex(m->GetIndex()-1);
                    }
                }

                if (curMedInGeo->getSensitivityFlag()) {
                    LOG(DEBUG) << "    Adding sensitive  " << curVol->GetName() << "";
                    AddSensitiveVolume(curVol);
                }
            }
            else {
                LOG(DEBUG) << "    Already fixed medium found in the list" << "";
                LOG(DEBUG) << "!!! Sensitivity: " << fFixedMedia[medName]->GetParam(0) << "";
                if (fFixedMedia[medName]->GetParam(0) == 1) {
                    LOG(DEBUG) << "    Adding sensitive  " << curVol->GetName() << "";
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
Bool_t MpdStrawECT::CheckIfSensitive(std::string name) {
    TString tsname = name;
    if (tsname.Contains("straw_gas")) {
        return kTRUE;
    }
    return kFALSE;
}
//-----------------------------------------------------------------------------

MpdStrawECTPoint* MpdStrawECT::AddHit(Int_t trackID, Int_t detID, TVector3 pos,
                TVector3 mom, Double_t time, Double_t length, Double_t eLoss) {
    TClonesArray& clref = *fStrawECTCollection;
    Int_t size = clref.GetEntriesFast();
    //std::cout << "ELoss: " << eLoss << "\n";
    return new(clref[size]) MpdStrawECTPoint(trackID, detID, pos, mom, time, length, eLoss);
}
//------------------------------------------------------------------------------------------------------------------------

ClassImp(MpdStrawECT)
