/*********************************************************************
 *
 *         Class BmdDigiScheme
 *
 *  Author:   RO
 *  e-mail:   rogachevsky@jinr.ru
 *  Version:  16-May-2008
 *
 ************************************************************************/

#ifndef BMDDIGISCHEME_H
#define BMDDIGISCHEME_H

#include <map>
#include <vector>

#include "TObject.h"

#include "FairGeoNode.h"

#include "BmdDigiPar.h"
#include "BmdGeoPar.h"

/* using std::pair; */
/* using std::vector; */
using namespace std;

typedef std::vector<Int_t>
BmdVolId_t; // now - {MotherMotherCopyNo, MotherCopyNo, VolumeId, CopyNo}
typedef std::vector<Int_t>
    BmdDigiId_t; // now - {DetectorID, ModuleID, ChannelID}
typedef std::vector<Double_t>
    BmdVolInfo_t; // now - Center X,Y,Z; and Dx,Dy,Dz from center to the corner

//___________________________________________________________________________
class BmdDigiScheme : public TObject {

 public:
  BmdDigiScheme();
  virtual ~BmdDigiScheme();

  Bool_t Init(BmdGeoPar *geoPar, BmdDigiPar *digiPar, Bool_t pAdd = kFALSE,
              Int_t pVerbose = 0);
  Bool_t Init(BmdGeoPar *geoPar, BmdDigiPar *digiPar, Int_t pVerbose = 0);

  static BmdDigiScheme *Instance();

  Bool_t AddNodes(TObjArray *sensNodes, Int_t pGlobalDetectorNumber,
                  Bool_t pAdd = kFALSE, Int_t pVerbose = 0);
  BmdVolInfo_t *CreateVolInfoElement(FairGeoNode *nod, Int_t pVerbose);
  BmdVolId_t *CreateVolElement(FairGeoNode *nod, Int_t nodeNumber,
                               BmdDigiId_t *right, Int_t pGlobalDetectorNumber,
                               Int_t pVerbose);
  Bool_t CreateVolCopyElements(BmdVolId_t *left, BmdDigiId_t *right);
  Bool_t CreateVolInfoCopyElements(BmdDigiId_t *right, BmdVolInfo_t *volInfo);

  Bool_t IsVolumeExist(BmdVolId_t *pVolId);
  BmdDigiId_t GetDigiId(BmdVolId_t *pVolId);
  Int_t GetDetectorID(BmdVolId_t *pVolId);
  Int_t GetChannelID(BmdVolId_t *pVolId);
  Bool_t GetDetIdModIdChanId(Int_t pMcVolumeNumber, Int_t pMcCopyNumber,
                             Int_t pMotherCopyNumber,
                             Int_t pMotherMotherCopyNumber, Int_t &pDetId,
                             Int_t &pChanId, Int_t &pModId);

  Bool_t GetVolCenterXYZ(BmdDigiId_t *pDigiId, Double_t &x, Double_t &y,
                         Double_t &z);
  Bool_t GetVolDxDyDz(BmdDigiId_t *pDigiId, Double_t &Dx, Double_t &Dy,
                      Double_t &Dz);
  BmdVolInfo_t *GetVolInfo(BmdVolId_t *pVolId);

  void Print();
  void PrintVolume(Int_t volID, Int_t copyNo, Int_t copyNoMother,
                   Int_t copyNoMotherMother = 1);

  Bool_t CalcDimensions(Int_t pGlobalDetectorNumber, Int_t &nx, Int_t &ny,
                        Int_t &nz);
  void GetBmdDimensions(Int_t &nx, Int_t &ny, Int_t &nz);

  BmdDigiId_t GetDigiIdFromCoords(Double_t x, Double_t y, Double_t z);
  BmdDigiId_t GetDigiIdFromVolumeData(Int_t pMcVolumeNumber,
                                      Int_t pMcCopyNumber,
                                      Int_t pMotherCopyNumber,
                                      Int_t pMotherMotherCopyNumber);
  void SplitDigiID(BmdDigiId_t digiID, Int_t &detID, Int_t &modID,
                   Int_t &chanID);

  inline BmdDigiPar *GetBmdDigiPar() { return fBmdDigiPar; };

protected:
  static BmdDigiScheme *fInstance; // Instance of singleton object
  static Int_t fRefcount;          // Counter of references on this
  static Bool_t fInitialized;      // Defines whether was initialized
  //  static Bool_t fYes;              //

private:
  std::map<BmdVolId_t, BmdDigiId_t>
      fVolToDigiIdMap; //! correspondence for all active volumes (Bmd+Bmd)
  std::map<BmdDigiId_t, BmdVolInfo_t *>
      fDigiToVolInfoMap; //! correspondence for active volumes (must exist for
                         //! Bmd, can also exist for Bmd)

  Int_t Nx;
  Int_t Ny;
  Int_t Nz;
 
  BmdDigiPar *fBmdDigiPar; //!
  TObjArray *fPasNodes;    //!

  ClassDef(BmdDigiScheme, 1);
};

#endif // BMDDIGISCHEME_H
