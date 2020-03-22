// -------------------------------------------------------------------------
// -----                        TowerDraw source file               -----
// -------------------------------------------------------------------------

//#define DEBUG_BBC_TOWERS
#include "MpdBmdTowerDraw.h"
#include "BmdDigi.h"

#include "MpdEventManagerEditor.h"
#include "FairLogger.h"

#include "TEveManager.h"    // for gEve
#include "TEveTreeTools.h"  // for TEvePointSelectorConsumer
#include "TGeoManager.h"    // for gGeoManager
#include "TEveCaloData.h"
#include "TEveCalo.h"
#include "TEveViewer.h"
#include "TGeoBBox.h"
#include "TH2F.h"
#include "TRandom.h"
#include "TGeoPgon.h"

#include <iostream>
using namespace std;

#define BMD_PARENT "/cave_1/BBC_common_of_tubes_0"

// -----   Default constructor   -------------------------------------------
MpdBmdTowerDraw::MpdBmdTowerDraw()
  : FairTask("MpdBmdTowerDraw", 0),
    fVerbose(0),
    fShadow(kFALSE),
    fResetRequiredFlag(kFALSE),
    fDigitList(NULL),
    fEventManager(NULL),
    fEneArr(NULL),
    fBmdMinEnergyThreshold(0),
    fq(NULL)
{
}

// -----   Standard constructor   ------------------------------------------
MpdBmdTowerDraw::MpdBmdTowerDraw(const char* name, Double_t bmdMinEnergyThreshold, Bool_t shadow, Int_t verbose)
  : FairTask(name, verbose),
    fVerbose(verbose),
    fShadow(shadow),
    fResetRequiredFlag(kFALSE),
    fDigitList(NULL),
    fEventManager(NULL),
    fBmdMinEnergyThreshold(bmdMinEnergyThreshold),
    fq(NULL)
{
}

// -------------------------------------------------------------------------
InitStatus MpdBmdTowerDraw::Init()

{
cout<<"entering MpdBmdTowerDraw::Init()"<<endl;

    if (fVerbose > 0) cout<<"MpdBmdTowerDraw::Init()"<<endl;

    fEventManager = MpdEventManager::Instance();
    if (fVerbose > 1) cout<<"MpdBmdTowerDraw::Init() get instance of EventManager: "<<fEventManager<<endl;

    fEventManager->fgRedrawRecoPointsReqired = kTRUE;
    fEventManager->fgShowRecoPointsIsShow = kTRUE;

    FairRootManager* fManager = FairRootManager::Instance();
    if (fVerbose > 1) cout<<"MpdBmdTowerDraw::Init() get instance of FairRootManager: "<<fManager<<endl;

    fDigitList = (TClonesArray*) fManager->GetObject("BmdPoint");
    if (fDigitList == 0)
    {
        LOG(ERROR)<<"MpdBmdTowerDraw::Init() branch BmdPoint not found! Task will be deactivated"<<FairLogger::endl;
        SetActive(kFALSE);
    }

    SetNumModules(162);
    SetModuleZLen(40);

    fEneArr = new Double_t[GetNumModules()*2];
    for (Int_t i = 0; i < GetNumModules()*2; i++)
        SetEneArr(i,0);

    fq = 0;

    return kSUCCESS;
}

void MpdBmdTowerDraw::Exec(Option_t* option)
{
    if (IsActive())
    {
        Reset();
        if (fVerbose > 0) cout<<"MpdBmdTowerDraw::Exec() current visibility level = "<<gGeoManager->GetVisLevel()<<endl;

        if (fEventManager->fgShowRecoPointsIsShow)
        {
            UInt_t fNhits = fDigitList->GetEntriesFast();
            if (fVerbose > 0) cout<<"MpdBmdTowerDraw::Exec() Number of BMD hits = " << fNhits << endl;

            for (Int_t i = 0; i < GetNumModules()*2; i++)
                SetEneArr(i,0);

            for (UInt_t iPnt = 0; iPnt < fNhits; iPnt++)
            {
                BmdPoint* dgt = (BmdPoint*) fDigitList->At(iPnt);
                //cout<<"GetELoss() = "<<dgt->GetELoss()<<endl;

                //cout<<"GetDetectorID() = "<<dgt->GetDetectorID()<<endl;
                //cout<<"GetModuleID() = "<<dgt->GetModuleID()<<endl;
                Int_t ringID = dgt->GetRingID();
                Int_t offset = 0;
                if( ringID == 2  ) offset = 12; //6;
                if( ringID == 3  ) offset = 30;
                if( ringID == 4  ) offset = 54;
                if( ringID == 5  ) offset = 84;
                if( ringID == 6  ) offset = 120;
                Int_t moduleID = offset + dgt->GetCellID();
                UInt_t iCur = GetNumModules() * (dgt->GetBmdID()-1) + moduleID; // + dgt->GetRingID() + dgt->GetCellID();
                //UInt_t iCur = dgt->GetDetectorID();
                //if (ringID == 5 || ringID == 6) cout << "iCur = " << iCur  << "___" << "GetBmdID = " << dgt->GetBmdID()  << "___" << "GetRingID = " << dgt->GetRingID()  << "___" << "GetCellID = " << dgt->GetCellID()  << "___" << "GetNumModules  " <<  GetNumModules () << endl;
                //cout<<"iCur = " <<iCur<<endl;
                SetEneArr(iCur, GetEneArrValue(iCur) + dgt->GeteLoss());
            }

            //cut off energies under threshold
            //if (GetBmdMinEnergyThreshold()!=0)
            //    for (UInt_t i = 0; i < GetNumModules()*2; ++i) {
            //        if (GetEneArrValue(i) < GetBmdMinEnergyThreshold()) SetEneArr(i,0);
            //}

            //search for maximum bin
            SetMaxE(0);
            for (UInt_t i = 0; i < 2*GetNumModules(); i++)
            {
                Double_t E = GetEneArrValue(i);
                //cout<<"E = "<<E<<endl;
                if (E > GetMaxE()) SetMaxE(E);
            }
            if (fVerbose > 0) cout<<"MpdBmdTowerDraw::Exec() maxE = "<<GetMaxE()<<endl;

            DrawTowers();

            //gGeoManager->cd("/cave_1/bmd01Empty_1");
            //TGeoNode *bmd01 = gGeoManager->GetCurrentNode();
            //bmd01->GetVolume()->SetTransparency(60);
            //gGeoManager->cd("/cave_1/bmd01Empty_2");
            //bmd01 = gGeoManager->GetCurrentNode();
            //bmd01->GetVolume()->SetTransparency(60);

            //_test
            /*TEveCalo3D* calo3d = new TEveCalo3D();
            //calo3d->SetBarrelRadius(129.00);
            calo3d->SetBarrelRadius(50.0);
            //calo3d->SetEndCapPos(268.36);
            Double_t bmdZPos = 410.5;
            Double_t bmdZLen = 40.0;

            calo3d->SetEndCapPos(bmdZPos+bmdZLen);
            //calo3d->SetEta(-100,100);
      //      cout<<"calo3d->GetEndCapPos() = "<<calo3d->GetEndCapPos()<<endl;
      //      cout<<"calo3d->GetBackwardEndCapPos() = "<<calo3d->GetBackwardEndCapPos()<<endl;
      //      cout<<"calo3d->GetEta() = "<<calo3d->GetEta()<<endl;
      //      cout<<"calo3d->GetEtaMax() = "<<calo3d->GetEtaMax()<<endl;//6.28319
      //      cout<<"calo3d->GetEtaMin() = "<<calo3d->GetEtaMin()<<endl;//-6.28319
      //      cout<<"calo3d->GetEtaRng() = "<<calo3d->GetEtaRng()<<endl;
      //      cout<<"calo3d->GetPhi() = "<<calo3d->GetPhi()<<endl;
      //      cout<<"calo3d->GetPhiMax() = "<<calo3d->GetPhiMax()<<endl;
      //      cout<<"calo3d->GetPhiMin() = "<<calo3d->GetPhiMin()<<endl;
      //      cout<<"calo3d->GetPhiRng() = "<<calo3d->GetPhiRng()<<endl;
      //      cout<<"calo3d->GetForwardEndCapPos() = "<<calo3d->GetForwardEndCapPos()<<endl;
      //      cout<<"calo3d->GetTransitionEta() = "<<calo3d->GetTransitionEta()<<endl;
      //      cout<<"calo3d->GetTransitionEtaBackward() = "<<calo3d->GetTransitionEtaBackward()<<endl;
      //      cout<<"calo3d->GetTransitionEtaForward() = "<<calo3d->GetTransitionEtaForward()<<endl;
      //      cout<<"calo3d->GetTransitionTheta() = "<<calo3d->GetTransitionTheta()<<endl;
      //      cout<<"calo3d->GetTransitionThetaBackward() = "<<calo3d->GetTransitionThetaBackward()<<endl;
      //      cout<<"calo3d->GetTransitionThetaForward() = "<<calo3d->GetTransitionThetaForward()<<endl;
      //      cout<<"calo3d->GetMaxValAbs() = "<<calo3d->GetMaxValAbs()<<endl;
      //      cout<<"calo3d->GetMaxTowerH() = "<<calo3d->GetMaxTowerH()<<endl;

            //calo3d->SetFrameTransparency(100);

            // Create, fill and project a 2D histogram.
            TH2F *h2 = new TH2F("h2","",200,-10,10,200,-TwoPi(),TwoPi());
            Float_t px, py;
            for (Int_t i = 0; i < 160000; i++) {
                gRandom->Rannor(px,py);
                if (Abs(px)>calo3d->GetTransitionEta())
                    h2->Fill(px,py);
            }

            TEveCaloDataHist* data = new TEveCaloDataHist();
            data->AddHistogram(h2);//ecalHist);
            data->RefSliceInfo(0).Setup("ECAL", 0.3, kRed);
            //data->RefSliceInfo(1).Setup("HCAL", 0.1, kBlue);
            data->GetEtaBins()->SetTitleFont(120);
            data->GetEtaBins()->SetTitle("h");
            data->GetPhiBins()->SetTitleFont(120);
            data->GetPhiBins()->SetTitle("f");
            data->IncDenyDestroy();
            gEve->AddToListTree(data, kFALSE);

            TEveViewer* v= gEve->GetDefaultViewer();

            // Create a scene and a viewer in the given slot.
            TEveScene* s = gEve->SpawnNewScene("Scene BMD");
            v->AddScene(s);
            calo3d->SetData(data);
            s->AddElement(calo3d);

            //gEve->GetBrowser()->GetTabRight()->SetTab(1);
            gEve->Redraw3D(kFALSE);*/
            //_____
        }
        else
        {
            cout << "!!!!!!!" << endl;
            if (GetResetRequiredFlag())
            {
                for (Int_t i = 0; i < GetNumModules()*2; i++)
                    SetEneArr(i,0);

                SetMaxE(1);
                DrawTowers();
                for (Int_t i = 0; i < GetNumModules()*2; i++)
                    SetEneArr(i,0);

                SetMaxE(0);
                SetResetRequiredFlag(kFALSE);
            }
        }

        TEvePointSet* q = new TEvePointSet(GetName(), fDigitList->GetEntriesFast(), TEvePointSelectorConsumer::kTVT_XYZ);
        q->SetOwnIds(kTRUE);

        fEventManager->AddEventElement(q, RecoPointList);

        fq = q;

        gEve->FullRedraw3D();
    }
}

void MpdBmdTowerDraw::DrawTowers()
{
cout<<"MpdBmdTowerDraw::DrawTowers"<<endl;
    gGeoManager->cd("/cave_1");
    TGeoNode* caveNode = gGeoManager->GetCurrentNode();

    for (UInt_t bmdId = 0; bmdId < 2; bmdId++)
    {
        TString bmd_path = Form("%s/BBC_common%d_%d", BMD_PARENT, bmdId+1, bmdId+1);
        if (gGeoManager->cd(bmd_path) == false)
        {
            cout<<"ERROR: MpdBmdTowerDraw::DrawTowers(): Path '"<<bmd_path<<"' not found"<<endl;
            return;
        }
        TGeoNode* bmdNode = gGeoManager->GetCurrentNode();
        TGeoVolume* bmdVolumeClone = bmdNode->GetVolume()->CloneVolume();
        TObjArray* bmdArr= bmdNode->GetVolume()->GetNodes();

        for (UInt_t module = 0; module < (bmdNode->GetVolume()->GetNdaughters()); module++)
        {
            TGeoNode* moduleNode = (TGeoNode*) bmdArr->UncheckedAt(module);

            TGeoNode* moduleNodeCopy;
            if (GetShadowFlag())
            {
                RecursiveChangeNodeTransparent(moduleNode, 0);
                moduleNodeCopy =  moduleNode->MakeCopyNode();
            }

            UInt_t iCur = moduleNode->GetNumber()-1;
        //    cout << "moduleNode->GetNumber() = " << moduleNode->GetNumber() << endl;
            TGeoBBox* box = (TGeoBBox*) moduleNode->GetVolume()->GetShape()->Clone();
            TGeoPgon *pgon= new TGeoPgon(0,360,6,2);
            pgon->DefineSection(0, box->GetDZ(), 0, 2.5);//1.611*cm);
            pgon->DefineSection(1, GetModuleZLen() * GetEneArrValue(iCur) / GetMaxE(), 0, 2.5);//1.611*cm);
            //cout<<"GetEneArrValue(iCur) = "<< GetEneArrValue(iCur)<<"; box->GetName() = "<<box->GetName()<<"; moduleNode->GetMotherVolume()->GetName() = "<<moduleNode->GetMotherVolume()->GetName()<<"; moduleNode->GetName() = "<<moduleNode->GetName()<<"; moduleNode->GetNumber() = "<<moduleNode->GetNumber()<<"; iCur = "<<iCur<<endl;

            TGeoMatrix* mat =moduleNode->GetMatrix()->MakeClone();
            if (GetEneArrValue(iCur) != 0)
            {
                pgon->SetBoxDimensions(box->GetDX(), box->GetDY(), box->GetDZ());//, GetModuleZLen() * GetEneArrValue(iCur) / GetMaxE());
            //    box->SetZ(GetModuleZLen() * GetEneArrValue(iCur) / GetMaxE());
                //cout << box->GetDX() << " " << box->GetDY() << " " << GetModuleZLen() * GetEneArrValue(iCur) / GetMaxE() << endl;
               // box->SetBoxDimensions(250, 250, 250);
            //   ((TGeoTranslation*)mat)->SetDz( -box->GetDZ());///*GetModuleZLen() * */GetEneArrValue(iCur) / GetMaxE() - GetModuleZLen());
               ((TGeoTranslation*)mat)->SetDz( box->GetDZ());///*GetModuleZLen() * */GetEneArrValue(iCur) / GetMaxE() + GetModuleZLen());
            }

            bmdVolumeClone->RemoveNode(moduleNode);
            TGeoVolume* moduleVolumeCopy = moduleNode->GetVolume()->MakeCopyVolume((TGeoShape*)pgon);
            moduleVolumeCopy->SetVisibility(kTRUE);
//            moduleVolumeCopy->SetTransparency(2.5*GetModuleZLen() * GetEneArrValue(iCur));
            if (GetEneArrValue(iCur) == 0)
                moduleVolumeCopy->SetVisibility(kFALSE);

            //the internal structure of Module does note alow to change it's shape, so we delete it for the towers visualization
            TObjArray* arr = moduleVolumeCopy->GetNodes();
            UInt_t count = moduleVolumeCopy->GetNdaughters();
            for (UInt_t sa = 0; sa < count; sa++)
            {
                TGeoNode* node = (TGeoNode*) arr->UncheckedAt(0);
                moduleVolumeCopy->RemoveNode(node);
            }
            if (GetShadowFlag())
                RecursiveChangeNodeTransparent(moduleNodeCopy, 99);

            bmdVolumeClone->AddNode(moduleVolumeCopy, moduleNode->GetNumber(), mat);
            if(GetShadowFlag())
                bmdVolumeClone->AddNode(moduleNodeCopy->GetVolume(), moduleNode->GetNumber(),moduleNode->GetMatrix());
        }

        caveNode->GetVolume()->AddNode(bmdVolumeClone, bmdNode->GetNumber(), bmdNode->GetMatrix());
        caveNode->GetVolume()->RemoveNode(bmdNode);
        //bmdNode->GetVolume()->SetInvisible();//VisibleDaughters(kTRUE);
        //bmdNode->SetVisibility(kFALSE);
        //bmdNode->VisibleDaughters(kTRUE);
        //RecursiveChangeNodeTransparent(bmdNode, 80);
        //RecursiveChangeNodeTransparent(bmdNode, 0);
    }

    SetResetRequiredFlag(kTRUE);
}

void MpdBmdTowerDraw::RecursiveChangeNodeTransparent(TGeoNode* node, int transparency)
{
    for (int i = 0; i < node->GetNdaughters(); i++)
    {
        TGeoNode* child = node->GetDaughter(i);
        TGeoVolume* curVolume = child->GetVolume();

        curVolume->SetTransparency(transparency);

        if (child->GetNdaughters() != 0)
            RecursiveChangeNodeTransparent(child, transparency);
    }
}

// -----   Destructor   ----------------------------------------------------
MpdBmdTowerDraw::~MpdBmdTowerDraw()
{
    fDigitList->Delete();
}

// -------------------------------------------------------------------------

/** Action after each event**/
void MpdBmdTowerDraw::Finish()
{
}

// -------------------------------------------------------------------------
void MpdBmdTowerDraw::Reset()
{
    if (fq != 0)
    {
        fq->Reset();
        gEve->RemoveElement(fq, fEventManager->EveRecoPoints);
    }
}

ClassImp(MpdBmdTowerDraw);
