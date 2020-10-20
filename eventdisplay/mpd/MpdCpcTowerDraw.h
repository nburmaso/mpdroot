// -------------------------------------------------------------------------
// -----                      CpcTowerDraw header file                 -----
// -------------------------------------------------------------------------

#ifndef MPDCPCTOWERDRAW_H
#define MPDCPCTOWERDRAW_H

#include "FairTask.h"
#include "MpdEventManager.h"
#include "TEvePointSet.h"
#include <TClonesArray.h>


class MpdCpcTowerDraw : public FairTask
{
  public:
    /** Default constructor **/
    MpdCpcTowerDraw();

    /** Standard constructor
    *@param name        Name of task
    *@param verbose    Verbosity level   
    **/
    MpdCpcTowerDraw(const char* name,Double_t cpcMinEnergyThreshold = 0, Bool_t shadow = kFALSE,  Int_t verbose = 0);

    /** Destructor **/
    virtual ~MpdCpcTowerDraw();

    /** Set verbosity level. For this task and all of the subtasks. **/
    void SetVerbose(UInt_t verbose) { fVerbose = verbose; }
    /** Executed task **/
    virtual void Exec(Option_t* option);
    void Reset();

  protected:    
    TClonesArray* fDigitList; //!
    MpdEventManager* fEventManager; //! 
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
    Double_t GetCpcMinEnergyThreshold() const { return fCpcMinEnergyThreshold; }
    
    /** Modifiers **/
    void SetResetRequiredFlag(Bool_t resetRequiredFlag) { fResetRequiredFlag = resetRequiredFlag; }
    // set energy loss array value
    void SetEneArr(UInt_t i, Double_t val) { fEneArr[i] = val; }
    void SetMaxE(Double_t maxE) { fMaxE = maxE; }
    void SetNumModules(UInt_t numModules) { fNumModules = numModules; }
    void SetModuleZLen(Double_t moduleZLen) { fModuleZLen = moduleZLen; }
    
  private:
    Int_t fVerbose; // Verbosity level
    Bool_t fShadow; // kTRUE to display transparent contur of cpc
    Bool_t fResetRequiredFlag; // flag true is box sizes are adjusted
    
    Double_t* fEneArr; //! array of energies in each box of BMD, GeV
    Double_t fMaxE; // energy loss at the bin with maximum energy loss, GeV
    
    UInt_t fNumModules; // number of modules in one CPC detector
    Double_t fModuleZLen; // z lenght of CPC module, cm
    
    Double_t fCpcMinEnergyThreshold;// min energy threshold
    
    MpdCpcTowerDraw(const MpdCpcTowerDraw&);
    MpdCpcTowerDraw& operator=(const MpdCpcTowerDraw&);

    ClassDef(MpdCpcTowerDraw,1);
};

#endif
