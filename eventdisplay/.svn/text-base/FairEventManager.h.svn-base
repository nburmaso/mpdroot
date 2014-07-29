/**
* class for event management and navigation.
* 06.12.07 M.Al-Turany
*/
#ifndef FairEventManager_H
#define FairEventManager_H

#include "TEveEventManager.h"           // for TEveEventManager

#include "FairRunAna.h"                 // for FairRunAna

#include "Rtypes.h"                     // for Float_t, Int_t, Bool_t, etc

//new
#include <TEveViewer.h>
#include <TGLViewer.h>
#include <TEveScene.h>
#include <TGeoNode.h>
#include <TEveProjectionManager.h>

class FairRootManager; //does not work with streamer, reason unclear
class FairTask;
class TGListTreeItem;

class FairEventManager : public TEveEventManager
{
  public:
    static FairEventManager* Instance();
    FairEventManager();
    virtual ~FairEventManager();
    virtual void Open();
    virtual void GotoEvent(Int_t event);    // *MENU*
    virtual void NextEvent();               // *MENU*
    virtual void PrevEvent();               // *MENU*
    virtual void Close();
    virtual void DisplaySettings();         //  *Menu*
    virtual Int_t Color(Int_t pdg);
    void AddTask(FairTask* t) {fRunAna->AddTask(t);}
    virtual void Init(Int_t visopt = 1, Int_t vislvl = 3, Int_t maxvisnds = 10000);
    virtual Int_t GetCurrentEvent() {return fEntry;}
    virtual void SetPriOnly(Bool_t Pri) {fPriOnly=Pri;}
    virtual Bool_t IsPriOnly() {return fPriOnly;}
    virtual void SelectPDG(Int_t PDG) {fCurrentPDG=PDG;}
    virtual Int_t GetCurrentPDG() {return fCurrentPDG;}

    virtual void SetMaxEnergy(Float_t max) {fMaxEnergy = max;}
    virtual void SetMinEnergy(Float_t min) {fMinEnergy = min;}
    virtual void SetEvtMaxEnergy(Float_t max) {fEvtMaxEnergy = max;}
    virtual void SetEvtMinEnergy(Float_t min) {fEvtMinEnergy = min;}
    virtual Float_t GetEvtMaxEnergy() {return fEvtMaxEnergy;}
    virtual Float_t GetEvtMinEnergy() {return fEvtMinEnergy;}
    virtual Float_t GetMaxEnergy() {return fMaxEnergy;}
    virtual Float_t GetMinEnergy() {return fMinEnergy;}

    void UpdateEditor();
    virtual void AddParticlesToPdgDataBase(Int_t pdg=0);

    //MultiView features
    void SetDepth(Float_t d);
    void ImportGeomRPhi(TEveElement* el);
    void ImportGeomRhoZ(TEveElement* el);
    void ImportEventRPhi(TEveElement* el);
    void ImportEventRhoZ(TEveElement* el);

    // viewer for RPhi projection
    TEveViewer* fRPhiView;
    // viewer for RPhoZ projection
    TEveViewer* fRhoZView;
    // 3D view in multi-viewer
    TEveViewer* fMulti3DView;
    // RPhi projection in multi-viewer
    TEveViewer* fMultiRPhiView;
    // RPhoZ projection in multi-viewer
    TEveViewer* fMultiRhoZView;

    ClassDef(FairEventManager,1);
  private:
    FairRootManager* fRootManager; //!
    Int_t fEntry;                 //!
    FairRunAna* fRunAna;          //!
    TGListTreeItem*  fEvent;     //!
    Bool_t fPriOnly;             //!
    Int_t fCurrentPDG;           //!
    // the most minimum particle energy for selected event
    Float_t fMinEnergy;         //!
    // the most maximum particle energy for selected event
    Float_t fMaxEnergy;         //!
    // minimum energy to cut particles by energy in selected event
    Float_t fEvtMinEnergy;      //!
    // maximum energy to cut particles by energy in selected event
    Float_t fEvtMaxEnergy;      //!

    // projection manager for RPhi view
    TEveProjectionManager* fRPhiMng;
    // projection manager for RPho view
    TEveProjectionManager* fRhoZMng;
    // scene for geometry presentation in RPhi plane
    TEveScene* fRPhiGeomScene;
    // scene for geometry presentation in RPhoZ plane
    TEveScene* fRhoZGeomScene;
    // scene for event presenation in RPhi plane
    TEveScene* fRPhiEventScene;
    // scene for event presenation in RPhoZ plane
    TEveScene* fRhoZEventScene;

    static FairEventManager*    fgRinstance; //!

    FairEventManager(const FairEventManager&);
    FairEventManager& operator=(const FairEventManager&);

    // changing color and visibility of geometry nodes
    void ChangeNodeProperty(TGeoNode* node, int level);
    // get color id by color name
    Int_t GetColor(TString colorName);
};

#endif
