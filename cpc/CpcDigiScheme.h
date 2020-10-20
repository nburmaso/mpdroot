/*********************************************************************
 *
 *         Class BmdDigiScheme
 *
 *  Author:   RO
 *  e-mail:   rogachevsky@jinr.ru
 *  Version:  16-May-2008
 *
 ************************************************************************/

#ifndef CPCDIGISCHEME_H
#define CPCDIGISCHEME_H

#include <map>
#include <vector>

#include "TObject.h"

#include "FairGeoNode.h"

#include "CpcDigiPar.h"
#include "MpdCpcGeoPar.h"

/* using std::pair; */
/* using std::vector; */
using namespace std;

typedef std::vector<Int_t>
CpcVolId_t; // now - {MotherMotherCopyNo, MotherCopyNo, VolumeId, CopyNo}
typedef std::vector<Int_t>
    CpcDigiId_t; // now - {DetectorID, ModuleID, ChannelID}
typedef std::vector<Double_t>
    CpcVolInfo_t; // now - Center X,Y,Z; and Dx,Dy,Dz from center to the corner

//___________________________________________________________________________
class CpcDigiScheme : public TObject {

 public:
  CpcDigiScheme();
  virtual ~CpcDigiScheme();

  Bool_t Init(MpdCpcGeoPar *geoPar, CpcDigiPar *digiPar, Bool_t pAdd = kFALSE,
              Int_t pVerbose = 0);
  Bool_t Init(MpdCpcGeoPar *geoPar, CpcDigiPar *digiPar, Int_t pVerbose = 0);

  static CpcDigiScheme *Instance();

  Bool_t AddNodes(TObjArray *sensNodes, Int_t pGlobalDetectorNumber,
                  Bool_t pAdd = kFALSE, Int_t pVerbose = 0);
  CpcVolInfo_t *CreateVolInfoElement(FairGeoNode *nod, Int_t pVerbose);
  CpcVolId_t *CreateVolElement(FairGeoNode *nod, Int_t nodeNumber,
                               CpcDigiId_t *right, Int_t pGlobalDetectorNumber,
                               Int_t pVerbose);
  Bool_t CreateVolCopyElements(CpcVolId_t *left, CpcDigiId_t *right);
  Bool_t CreateVolInfoCopyElements(CpcDigiId_t *right, CpcVolInfo_t *volInfo);

  Bool_t IsVolumeExist(CpcVolId_t *pVolId);
  CpcDigiId_t GetDigiId(CpcVolId_t *pVolId);
  Int_t GetDetectorID(CpcVolId_t *pVolId);
  Int_t GetChannelID(CpcVolId_t *pVolId);
  Bool_t GetDetIdModIdChanId(Int_t pMcVolumeNumber, Int_t pMcCopyNumber,
                             Int_t pMotherCopyNumber,
                             Int_t pMotherMotherCopyNumber, Int_t &pDetId,
                             Int_t &pChanId, Int_t &pModId);

  Bool_t GetVolCenterXYZ(CpcDigiId_t *pDigiId, Double_t &x, Double_t &y,
                         Double_t &z);
  Bool_t GetVolDxDyDz(CpcDigiId_t *pDigiId, Double_t &Dx, Double_t &Dy,
                      Double_t &Dz);
  CpcVolInfo_t *GetVolInfo(CpcVolId_t *pVolId);

  void Print();
  void PrintVolume(Int_t volID, Int_t copyNo, Int_t copyNoMother,
                   Int_t copyNoMotherMother = 1);

  Bool_t CalcDimensions(Int_t pGlobalDetectorNumber, Int_t &nx, Int_t &ny,
                        Int_t &nz);
  void GetCpcDimensions(Int_t &nx, Int_t &ny, Int_t &nz);

  CpcDigiId_t GetDigiIdFromCoords(Double_t x, Double_t y, Double_t z);
  CpcDigiId_t GetDigiIdFromVolumeData(Int_t pMcVolumeNumber,
                                      Int_t pMcCopyNumber,
                                      Int_t pMotherCopyNumber,
                                      Int_t pMotherMotherCopyNumber);
  void SplitDigiID(CpcDigiId_t digiID, Int_t &detID, Int_t &modID,
                   Int_t &chanID);

  inline CpcDigiPar *GetCpcDigiPar() { return fCpcDigiPar; };

protected:
  static CpcDigiScheme *fInstance; // Instance of singleton object
  static Int_t fRefcount;          // Counter of references on this
  static Bool_t fInitialized;      // Defines whether was initialized
  //  static Bool_t fYes;              //

private:
  std::map<CpcVolId_t, CpcDigiId_t>
      fVolToDigiIdMap; //! correspondence for all active volumes (Bmd+Bmd)
  std::map<CpcDigiId_t, CpcVolInfo_t *>
      fDigiToVolInfoMap; //! correspondence for active volumes (must exist for
                         //! Bmd, can also exist for Bmd)

  Int_t Nx;
  Int_t Ny;
  Int_t Nz;
 
  CpcDigiPar *fCpcDigiPar; //!
  TObjArray *fPasNodes;    //!

  ClassDef(CpcDigiScheme, 1);
};

#endif // CPCDIGISCHEME_H
