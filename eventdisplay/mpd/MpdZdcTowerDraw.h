// -------------------------------------------------------------------------
// -----                      ZdcTowerDraw header file                 -----
// -------------------------------------------------------------------------

#ifndef ZDCTOWERDRAW_H
#define ZDCTOWERDRAW_H

#include "FairTask.h"
#include "FairEventManager.h"
#include "TEvePointSet.h"
#include <TClonesArray.h>


class MpdZdcTowerDraw : public FairTask
{
  public:
    /** Default constructor **/
    MpdZdcTowerDraw();

    /** Standard constructor
    *@param name        Name of task
    *@param verbose    Verbosity level   
    **/
    MpdZdcTowerDraw(const char* name,Double_t zdcMinEnergyThreshold = 0, Bool_t shadow = kFALSE,  Int_t verbose = 0);

    /** Destructor **/
    virtual ~MpdZdcTowerDraw();

    /** Set verbosity level. For this task and all of the subtasks. **/
    void SetVerbose(UInt_t verbose) { fVerbose = verbose; }
    /** Executed task **/
    virtual void Exec(Option_t* option);
    void Reset();

  protected:    
    TClonesArray* fDigitList; //!
    FairEventManager* fEventManager; //! 
    TEvePointSet* fq;    //!
    
    virtual InitStatus Init();
    // Action after each event
    virtual void Finish();

    void RecursiveChangeNodeTransparent(TGeoNode* node, int transparency);
    // adjust towers heights
    void DrawTowers();
    
    /** Accessors **/
    UInt_t GetVerbose() const { return fVerbose; }
    Bool_t GetShadowFlag() const { return fShadow; }
    Bool_t GetResetRequiredFlag() const { return fResetRequiredFlag; }
    Double_t GetEneArrValue(UInt_t i) const { return fEneArr[i]; }
    Double_t GetMaxE() const { return fMaxE; }  
    UInt_t GetNumModules() const { return fNumModules; }  
    Double_t GetModuleZLen() const { return fModuleZLen; }  
    Double_t GetZdcMinEnergyThreshold() const { return fZdcMinEnergyThreshold; }
    
    /** Modifiers **/
    void SetResetRequiredFlag(Bool_t resetRequiredFlag) { fResetRequiredFlag = resetRequiredFlag; }
    // set energy loss array value
    void SetEneArr(UInt_t i, Double_t val) { fEneArr[i] = val; }
    void SetMaxE(Double_t maxE) { fMaxE = maxE; }
    void SetNumModules(UInt_t numModules) { fNumModules = numModules; }
    void SetModuleZLen(Double_t moduleZLen) { fModuleZLen = moduleZLen; }
    
  private:
    // Verbosity level
    Int_t fVerbose;
    // kTRUE to display transparent contur of zdc
    Bool_t fShadow;
    // flag true is box sizes are adjusted
    Bool_t fResetRequiredFlag;
    
    // array of energies in each box of ZDC, GeV
    Double_t* fEneArr;  //!
    // energy loss at the bin with maximum energy loss, GeV
    Double_t fMaxE;
    
    // number of modules in one ZDC detector
    UInt_t fNumModules;
    // z lenght of ZDC module, cm
    Double_t fModuleZLen;
    
    // min energy threshold
    Double_t fZdcMinEnergyThreshold;
    
    MpdZdcTowerDraw(const MpdZdcTowerDraw&);
    MpdZdcTowerDraw& operator=(const MpdZdcTowerDraw&);

    ClassDef(MpdZdcTowerDraw,1);
};

#endif
