// -------------------------------------------------------------------------
// -----                      FairPointSetDraw header file                    -----
// -----          Created 10/12/07  by M. Al-Turany                    -----
// -------------------------------------------------------------------------


/** FairPointSetDraw
 * @author M. Al-Turany
 * @since 03.01.08
 *   Task to display MC points
 **
 **/

#ifndef FAIRPOINTSETDRAW_H
#define FAIRPOINTSETDRAW_H
#include "FairTask.h"
#include "FairEventManager.h"

#include "TClonesArray.h"
#include "TVector3.h"
#include "TEvePointSet.h"


class FairPointSetDraw : public FairTask
{
  public:
    /** Default constructor **/
    FairPointSetDraw();

    /** Standard constructor
    *@param name        Name of task
    *@param iVerbose    Verbosity level
    **/
    FairPointSetDraw(const char* name, Color_t color ,Style_t mstyle, Int_t iVerbose = 1);

    /** Destructor **/
    virtual ~FairPointSetDraw();

    /** Set verbosity level. For this task and all of the subtasks. **/
    void SetVerbose(Int_t iVerbose) {fVerbose = iVerbose;}
    /** Executed task **/
    virtual void Exec(Option_t* option);
    void Reset();

  protected:
    virtual TVector3 GetVector(TObject* obj) = 0;
    virtual void AddEveElementList() = 0;
    virtual void RemoveEveElementList() = 0;
    virtual TObject* GetValue(TObject* obj, Int_t i);

    virtual void SetParContainers();
    virtual InitStatus Init();
    /** Action after each event**/
    virtual void Finish();

    // Verbosity level
    Int_t fVerbose;
    TClonesArray* fPointList;           //!
    FairEventManager* fEventManager;    //!
    TEvePointSet* fq;                   //!
    Color_t fColor;                     //!
    Style_t fStyle;                     //!

  private:
    FairPointSetDraw(const FairPointSetDraw&);
    FairPointSetDraw& operator=(const FairPointSetDraw&);

    ClassDef(FairPointSetDraw,1);
};

#endif
