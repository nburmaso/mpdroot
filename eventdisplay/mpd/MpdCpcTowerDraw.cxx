// -------------------------------------------------------------------------
// -----                        TowerDraw source file               -----
// -------------------------------------------------------------------------

//#define DEBUG_CPC_TOWERS
#include "MpdCpcTowerDraw.h"
#include "CpcDigi.h"
//#include "MpdCpcPoint.h"
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
//#include "TGeoTube.h"
//#include "TGeoCompositeShape.h"
#include <iostream>
using namespace std;

#define CPC_PARENT "/cave_1/TOP2_0"

// -----   Default constructor   -------------------------------------------
MpdCpcTowerDraw::MpdCpcTowerDraw()
  : FairTask("MpdCpcTowerDraw", 0),
    fVerbose(0),
    fShadow(kFALSE),
    fResetRequiredFlag(kFALSE),
    fDigitList(NULL),
    fEventManager(NULL),
    fEneArr(NULL),
    fCpcMinEnergyThreshold(0),
    fq(NULL)
{
}

// -----   Standard constructor   ------------------------------------------
MpdCpcTowerDraw::MpdCpcTowerDraw(const char* name, Double_t cpcMinEnergyThreshold, Bool_t shadow, Int_t verbose)
  : FairTask(name, verbose),
    fVerbose(verbose),
    fShadow(shadow),
    fResetRequiredFlag(kFALSE),
    fDigitList(NULL),
    fEventManager(NULL),
    fCpcMinEnergyThreshold(cpcMinEnergyThreshold),
    fq(NULL)
{
}

// -------------------------------------------------------------------------
InitStatus MpdCpcTowerDraw::Init()
{
    if (fVerbose > 0) cout<<"MpdCpcTowerDraw::Init()"<<endl;

    fEventManager = MpdEventManager::Instance();
    if (fVerbose > 1) cout<<"MpdCpcTowerDraw::Init() get instance of EventManager: "<<fEventManager<<endl;

    fEventManager->fgRedrawRecoPointsReqired = kTRUE;
    fEventManager->fgShowRecoPointsIsShow = kTRUE;

    FairRootManager* fManager = FairRootManager::Instance();
    if (fVerbose > 1) cout<<"MpdCpcTowerDraw::Init() get instance of FairRootManager: "<<fManager<<endl;

    fDigitList = (TClonesArray*) fManager->GetObject("CPCPoint");
    if (fDigitList == 0)
    {
        LOG(ERROR)<<"MpdCpcTowerDraw::Init() branch CPCPoint not found! Task will be deactivated"<<FairLogger::endl;
        SetActive(kFALSE);
    }
    
    SetNumModules(191);
    SetModuleZLen(50);
    
    fEneArr = new Double_t[GetNumModules()];
    for (Int_t i = 0; i < GetNumModules(); i++)
        SetEneArr(i,0);
    
    fq = 0;
    
    return kSUCCESS;
}

void MpdCpcTowerDraw::Exec(Option_t* option)
{   
    if (IsActive())
    {   
        Reset();
        if (fVerbose > 0) cout<<"MpdCpcTowerDraw::Exec() current visibility level = "<<gGeoManager->GetVisLevel()<<endl;

        if (fEventManager->fgShowRecoPointsIsShow)
        {
            UInt_t fNhits = fDigitList->GetEntriesFast();
            if (fVerbose > 0) cout<<"MpdCpcTowerDraw::Exec() Number of CPC hits = " << fNhits << endl;

            for (Int_t i = 0; i < GetNumModules(); i++)
                SetEneArr(i,0);

            for (UInt_t iPnt = 0; iPnt < fNhits; iPnt++)
            {
                MpdCpcPoint* dgt = (MpdCpcPoint*) fDigitList->At(iPnt);
                //cout<<"GetDetectorID() = "<<dgt->GetDetectorID()<<endl;
                //cout<<"GetModuleID() = "<<dgt->GetModuleID()<<endl;      
                //cout<<"GetNumModules() "<<GetNumModules()<<"    "<<"dgt->GetCpcID() "<<dgt->GetCpcID()<<"    "<<"dgt->GetRingID() "<<dgt->GetRingID()<<"  "<<"dgt->GetCellID() "<<dgt->GetCellID()<<endl;    
                //UInt_t iCur = GetNumModules() * (dgt->GetCpcID() + dgt->GetRingID() + dgt->GetCellID()-1);
                UInt_t iCur = dgt->GetDetectorID();

                //cout<<"Hit # "<<iPnt<<"iCur = " <<iCur<<"   "<<"CPC_GetELoss() = "<<dgt->GetEnergyLoss()<<endl;
                SetEneArr(iCur, GetEneArrValue(iCur) + dgt->GetEnergyLoss());
                
            }
            
            //cut off energies under threshold
            //if (GetCpcMinEnergyThreshold()!=0)
            //    for (UInt_t i = 0; i < GetNumModules()*2; ++i) {           
            //        if (GetEneArrValue(i) < GetCpcMinEnergyThreshold()) SetEneArr(i,0);            
            //}
            
            //search for maximum bin
            SetMaxE(0);
            for (UInt_t i = 0; i < GetNumModules(); i++)
            {
                Double_t E = GetEneArrValue(i);
                //cout<<"i =  "<<i<<"     "<<"NumModules = "<<GetNumModules()<<"      "<<"E = "<<E<<endl;
                if (E > GetMaxE()) SetMaxE(E);

            }
            if (fVerbose > 0) cout<<"MpdCpcTowerDraw::Exec() maxE = "<<GetMaxE()<<endl;
                        
            DrawTowers();
            
            //gGeoManager->cd("/cave_1/cpc01Empty_1"); 
            //TGeoNode *cpc01 = gGeoManager->GetCurrentNode();
            //cpc01->GetVolume()->SetTransparency(60);
            //gGeoManager->cd("/cave_1/cpc01Empty_2"); 
            //cpc01 = gGeoManager->GetCurrentNode();
            //cpc01->GetVolume()->SetTransparency(60);
            
            //_test
            /*TEveCalo3D* calo3d = new TEveCalo3D();
            //calo3d->SetBarrelRadius(129.00);
            calo3d->SetBarrelRadius(50.0);
            //calo3d->SetEndCapPos(268.36);
            Double_t cpcZPos = 410.5;
            Double_t cpcZLen = 40.0;
            
            calo3d->SetEndCapPos(cpcZPos+cpcZLen);
            //calo3d->SetEta(-100,100);
            cout<<"calo3d->GetEndCapPos() = "<<calo3d->GetEndCapPos()<<endl;
            cout<<"calo3d->GetBackwardEndCapPos() = "<<calo3d->GetBackwardEndCapPos()<<endl;
            cout<<"calo3d->GetEta() = "<<calo3d->GetEta()<<endl;
            cout<<"calo3d->GetEtaMax() = "<<calo3d->GetEtaMax()<<endl;//6.28319
            cout<<"calo3d->GetEtaMin() = "<<calo3d->GetEtaMin()<<endl;//-6.28319
            cout<<"calo3d->GetEtaRng() = "<<calo3d->GetEtaRng()<<endl;
            cout<<"calo3d->GetPhi() = "<<calo3d->GetPhi()<<endl;
            cout<<"calo3d->GetPhiMax() = "<<calo3d->GetPhiMax()<<endl;
            cout<<"calo3d->GetPhiMin() = "<<calo3d->GetPhiMin()<<endl;
            cout<<"calo3d->GetPhiRng() = "<<calo3d->GetPhiRng()<<endl;
            cout<<"calo3d->GetForwardEndCapPos() = "<<calo3d->GetForwardEndCapPos()<<endl;
            cout<<"calo3d->GetTransitionEta() = "<<calo3d->GetTransitionEta()<<endl;
            cout<<"calo3d->GetTransitionEtaBackward() = "<<calo3d->GetTransitionEtaBackward()<<endl;
            cout<<"calo3d->GetTransitionEtaForward() = "<<calo3d->GetTransitionEtaForward()<<endl;
            cout<<"calo3d->GetTransitionTheta() = "<<calo3d->GetTransitionTheta()<<endl;
            cout<<"calo3d->GetTransitionThetaBackward() = "<<calo3d->GetTransitionThetaBackward()<<endl;
            cout<<"calo3d->GetTransitionThetaForward() = "<<calo3d->GetTransitionThetaForward()<<endl;           
            cout<<"calo3d->GetMaxValAbs() = "<<calo3d->GetMaxValAbs()<<endl;           
            cout<<"calo3d->GetMaxTowerH() = "<<calo3d->GetMaxTowerH()<<endl;
            
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
            TEveScene* s = gEve->SpawnNewScene("Scene CPC");
            v->AddScene(s);
            calo3d->SetData(data);
            s->AddElement(calo3d);

            //gEve->GetBrowser()->GetTabRight()->SetTab(1);
            gEve->Redraw3D(kFALSE);*/
            //_____
        }
        else
        {
            if (GetResetRequiredFlag())
            {
                for (Int_t i = 0; i < 2 * GetNumModules(); i++)
                    SetEneArr(i,1);

                SetMaxE(1);
                DrawTowers();
                for (Int_t i = 0; i < 2 * GetNumModules(); i++)
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

void MpdCpcTowerDraw::DrawTowers()
{
    gGeoManager->cd("/cave_1");
    TGeoNode* caveNode = gGeoManager->GetCurrentNode();
    
    for (UInt_t cpcId = 0; cpcId < 2; cpcId++)
    {
        //UInt_t cpcId = 1;
        TString cpc_path = Form("%s/replica%d_%d", CPC_PARENT, cpcId+1, cpcId+1);
        if (gGeoManager->cd(cpc_path) == false)
        {
            cout<<"ERROR: MpdCpcTowerDraw::DrawTowers(): Path '"<<cpc_path<<"' not found"<<endl;
            return;
        }
        TGeoNode* cpcNode = gGeoManager->GetCurrentNode();
        //cout<<"cpcNode"<<cpcNode<<endl;
        TGeoVolume* cpcVolumeClone = cpcNode->GetVolume()->CloneVolume();
        TObjArray* cpcArr= cpcNode->GetVolume()->GetNodes();
        //cout<<"cpcArr"<<cpcArr<<endl;
        //cout<<"cpcNode->GetVolume()->GetNdaughters() = "<<cpcNode->GetVolume()->GetNdaughters()<<endl;
        for (UInt_t module = 0; module < cpcNode->GetVolume()->GetNdaughters(); module++)
        {
            TGeoNode* moduleNode = (TGeoNode*) cpcArr->UncheckedAt(module);
            //Int_t cpcnum = cpcNode->GetVolume()->GetNdaughters();
            //cout<<"cpcNode->GetVolume()->GetNdaughters() "<<cpcnum<<endl;
            TGeoNode* moduleNodeCopy;
            if (GetShadowFlag())
            {
                RecursiveChangeNodeTransparent(moduleNode, 0);
                moduleNodeCopy =  moduleNode->MakeCopyNode();
            }
            
            UInt_t iCur = moduleNode->GetNumber();
            TGeoBBox* box = (TGeoBBox*) moduleNode->GetVolume()->GetShape()->Clone();

            /*
            Double_t xcoord;
            Double_t ycoord;
            Double_t zcoord=GetModuleZLen() * GetEneArrValue(iCur) / GetMaxE();

            if (module<13 || (module<109 && module>96)) {xcoord = 0.86286; ycoord = 2.172;}
            else if ((module>12 && module<25) || (module<121 && module>108)) {xcoord = 2.02933; ycoord = 22.69925/10;}
            else if ((module>24 && module<37) || (module<133 && module>120)) {xcoord = 33.2343/10; ycoord = 22.69925/10;}
            else if ((module>36 && module<49) || (module<145 && module>132)) {xcoord = 46.1752/10; ycoord = 22.69925/10;}
            else if ((module>48 && module<61) || (module<157 && module>144)) {xcoord = 59.1162/10; ycoord = 22.69925/10;}
            else if ((module>60 && module<73) || (module<169 && module>156)) {xcoord = 72.0571/10; ycoord = 22.69925/10;}
            else if ((module>72 && module<85) || (module<181 && module>168)) {xcoord = 84.9981/10; ycoord = 22.69925/10;}
            else if ((module>84 && module<97) || (module<193 && module>180)) {xcoord = 97.3832/10; ycoord = 22.69925/10;}

             
            TGeoTranslation *cr11 = new TGeoTranslation("cr11", xcoord, 0, 0.);
            TGeoTranslation *cr12 = new TGeoTranslation("cr12", -xcoord, 0, 0.);

            TGeoBBox *box1 = new TGeoBBox("box1", xcoord, ycoord, zcoord);
            TGeoTube *tube1 = new TGeoTubeSeg("tube1", 0., ycoord, zcoord, 0., 180.);
            TGeoTube *tube2 = new TGeoTubeSeg("tube2", 0., ycoord, zcoord, 180., 360.);

              
            cr11->RegisterYourself();
            cr12->RegisterYourself();
            TGeoCompositeShape *segment1 = new TGeoCompositeShape("segment1", "box1+tube1:cr11+tube2:cr12");
            segment1->SetBoxDimensions(box->GetDX(), box->GetDY(), box->GetDZ());
            */
            //cout<<"GetEneArrValue(iCur) = "<< GetEneArrValue(iCur)<<"; box->GetName() = "<<box->GetName()<<"; moduleNode->GetMotherVolume()->GetName() = "<<moduleNode->GetMotherVolume()->GetName()<<"; moduleNode->GetName() = "<<moduleNode->GetName()<<"; moduleNode->GetNumber() = "<<moduleNode->GetNumber()<<"; iCur = "<<iCur<<endl;
                                        
            //TGeoMatrix* mat = moduleNode->GetMatrix()->MakeClone();
            //if (GetEneArrValue(iCur) != 0)
            //{   
                //cout<<"GetModuleZLen() = "<<GetModuleZLen()<<"GetEneArrValue(iCur) = " << GetEneArrValue(iCur) << "GetMaxE() = "<<GetMaxE()<<endl;

                //box->SetBoxDimensions(box->GetDX(), box->GetDY(), GetModuleZLen() * GetEneArrValue(iCur) / GetMaxE());
     
                //cout <<"iCur = "<<iCur<<"   "<< box->GetDX() << " " << box->GetDY() << " " << GetModuleZLen() * GetEneArrValue(iCur) / GetMaxE() << endl;

                //double rad = sqrt(box->GetDX()*box->GetDX() + box->GetDY()*box->GetDY());
                //Double_t xaxid;
                //Double_t yaxid;
                //if (iCur < 12)
                //{
                //    xaxid = rad*TMath::Cos(30*iCur);
                //    yaxid = rad*TMath::Sin(30*iCur);
                //}


               // box->SetBoxDimensions(250, 250, 250);
               //((TGeoTranslation*)mat)->SetDz(GetModuleZLen() * GetEneArrValue(iCur) / GetMaxE() + GetModuleZLen());
                //((TGeoTranslation*)mat)->SetDz(GetModuleZLen() * GetEneArrValue(iCur) / GetMaxE() + GetModuleZLen());
                //TGeoRotation * rot = new TGeoRotation();
                //rot->GetRotationMatrix (moduleNode->GetMatrix()->MakeClone());
                //TGeoTranslation* trans = new TGeoTranslation(box->GetDX(), box->GetDY(), GetModuleZLen() * GetEneArrValue(iCur) / GetMaxE() + GetModuleZLen());
                double r = module % 12*30;
                TGeoRotation * rot = new TGeoRotation("rot", 0, 0, r);
                TGeoCombiTrans *combi1 = new TGeoCombiTrans(0, 0, GetModuleZLen() * GetEneArrValue(iCur) / GetMaxE() + GetModuleZLen(), rot);
                TGeoCombiTrans *combi2 = new TGeoCombiTrans(0, 0, -GetModuleZLen() * GetEneArrValue(iCur) / GetMaxE() - GetModuleZLen(), rot);
                cout<<"Z value = "<<GetModuleZLen() * GetEneArrValue(iCur) / GetMaxE() + GetModuleZLen()<<endl;
            //}
            
            cpcVolumeClone->RemoveNode(moduleNode);
            TGeoVolume* moduleVolumeCopy = moduleNode->GetVolume()->MakeCopyVolume((TGeoShape*)box);
            moduleVolumeCopy->SetVisibility(kTRUE);
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
                RecursiveChangeNodeTransparent(moduleNodeCopy, 0);
            
            if (cpcId==0)cpcVolumeClone->AddNode(moduleVolumeCopy, moduleNode->GetNumber(), combi1);
            if (cpcId==1)cpcVolumeClone->AddNode(moduleVolumeCopy, moduleNode->GetNumber(), combi2);
            //if(GetShadowFlag())
                //cpcVolumeClone->AddNode(moduleNodeCopy->GetVolume(), moduleNode->GetNumber(),moduleNode->GetMatrix());
        }

        caveNode->GetVolume()->AddNode(cpcVolumeClone, cpcNode->GetNumber(), cpcNode->GetMatrix());
        caveNode->GetVolume()->RemoveNode(cpcNode);
        //cpcNode->GetVolume()->SetInvisible();//VisibleDaughters(kTRUE);
        //cpcNode->SetVisibility(kFALSE);
        //cpcNode->VisibleDaughters(kTRUE);
        RecursiveChangeNodeTransparent(cpcNode, 0);
        //RecursiveChangeNodeTransparent(cpcNode, 50);
    }

    SetResetRequiredFlag(kTRUE);
}

void MpdCpcTowerDraw::RecursiveChangeNodeTransparent(TGeoNode* node, int transparency)
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
MpdCpcTowerDraw::~MpdCpcTowerDraw()
{
    fDigitList->Delete();
}

// -------------------------------------------------------------------------

/** Action after each event**/
void MpdCpcTowerDraw::Finish()
{
}

// -------------------------------------------------------------------------
void MpdCpcTowerDraw::Reset()
{     
    if (fq != 0)
    {           
        fq->Reset();
        gEve->RemoveElement(fq, fEventManager->EveRecoPoints);                
    }    
}

ClassImp(MpdCpcTowerDraw);
