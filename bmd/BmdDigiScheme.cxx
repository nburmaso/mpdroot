/*******************************************************************************
 *
 *         Class BmdDigiScheme
 *
 *  Author:   RO
 *  e-mail:   rogachevsky@jinr.ru
 *  Version:  16-May-2008
 *
 ******************************************************************************/

#include <iostream>

#include "TGeoBBox.h"
#include "TGeoManager.h"
#include "TGeoNode.h"

#include "BmdDigiScheme.h"

using namespace std;


BmdDigiScheme *BmdDigiScheme::fInstance = 0;
Bool_t BmdDigiScheme::fInitialized = 0;
Int_t BmdDigiScheme::fRefcount = 0;

static Int_t kBMD = 1; //  // hard-coded basic BMD detector number

//___________________________________________________________

BmdDigiScheme::BmdDigiScheme() : Nx(0), Ny(0), Nz(0) {
  Nx = 0;
  Ny = 0;
  Nz = 0;
  fBmdDigiPar = 0;
}

// -------------------------------------------------------------------------
//___________________________________________________________

BmdDigiScheme::~BmdDigiScheme() {
  fRefcount--;
  if (!fRefcount) {
    delete this;
    fInstance = NULL;
  }
}
// -------------------------------------------------------------------------
//___________________________________________________________

BmdDigiScheme *BmdDigiScheme::Instance() {
  if (!fInstance)
    fInstance = new BmdDigiScheme();
  fRefcount++;
  return fInstance;
}

// -------------------------------------------------------------------------
//___________________________________________________________

Bool_t BmdDigiScheme::Init(BmdGeoPar *geoPar, BmdDigiPar *digiPar, Bool_t pAdd,
                           Int_t pVerbose) {
  if (!fInitialized) {

    if (!geoPar) {
      cout << "-W- BmdDigiScheme::Init: "
           << "No geometry parameters available!" << endl;
      return kFALSE;
    }
    if (!digiPar) {
      cout << "-W-  BmdDigiScheme::Init: "
           << "No digitization parameters available!" << endl;
      //      return kFALSE;
    }
    fBmdDigiPar = digiPar;

    TObjArray *sensNodes = geoPar->GetGeoSensitiveNodes();
    if (!sensNodes) {
      cout << "-W-  BmdDigiScheme::Init: "
           << "No sensitive nodes available!" << endl;
      return kFALSE;
    }

    fPasNodes = geoPar->GetGeoPassiveNodes();
    if (!fPasNodes) {
      cout << "-W-  BmdDigiScheme::Init: "
           << "No passive nodes available!" << endl;
      return kFALSE;
    }

    fInitialized = kTRUE;

    AddNodes(sensNodes, kBMD, pAdd, pVerbose);
    CalcDimensions(kBMD, Nx, Ny, Nz);

    if (pVerbose)
      cout << endl << "-W-  BmdDigiScheme::Init: finished." << endl;
  }
  return kTRUE;
}

// -------------------------------------------------------------------------
//___________________________________________________________

BmdVolInfo_t *BmdDigiScheme::CreateVolInfoElement(FairGeoNode *nod,
                                                  Int_t pVerbose) {
  if (!nod)
    return NULL;

  static TString root_name_copy1 = "";
  static Double_t volData[6] = {0, 0, 0, 0, 0, 0};
  Int_t i, j;

  TString shape_name = nod->getShapePointer()->GetName();
  TString root_name = nod->getRootVolume()->GetName();
  TString tmp = "          ";
  TGeoBBox *shape = 0;

  FairGeoVector pos = nod->getLabTransform()->getTranslation();

  for (i = 0; i < 3; i++)
    volData[i] = pos.getValues(i); // [cm]

  if (root_name != root_name_copy1) {

    root_name_copy1 = root_name;

    shape = (TGeoBBox *)nod->getRootVolume()->GetShape();

    if (shape_name.Contains("BOX")) {
      volData[3] = shape->GetDX(); // [cm]
      volData[4] = shape->GetDY(); // [cm]
      volData[5] = shape->GetDZ(); // [cm]
    } else {
      if (shape_name.Contains("PGON")) {
        volData[3] = nod->getPoint(2)->getZ() / 10.; // [cm]
        volData[4] = volData[3];
        volData[5] = shape->GetDZ(); // [cm]
      }
    }
  }

  BmdVolInfo_t *volInfo = new BmdVolInfo_t;
  for (i = 0; i < 6; i++)
    volInfo->push_back(volData[i]);

  if (pVerbose > 1) {
    if (pVerbose > 2)
      tmp = "  root: " + root_name + ", shape: " + shape_name;
    tmp += "X,Y,Z,   Dx,Dy,Dz: ";
    cout << tmp << volData[0] << "," << volData[1] << "," << volData[2]
         << ",   " << volData[3] << "," << volData[4] << "," << volData[5]
         << endl;
  }
  return volInfo;
}

// -------------------------------------------------------------------------
//___________________________________________________________

BmdVolId_t *BmdDigiScheme::CreateVolElement(FairGeoNode *nod, Int_t nodeNumber,
                                            BmdDigiId_t *right,
                                            Int_t pGlobalDetectorNumber,
                                            Int_t pVerbose) {
  if (!nod)
    return NULL;

  FairGeoNode *nod0, *nod1;
  TString mother_name, tmp;

  if (!fPasNodes)
    return NULL;

  nod0 = (FairGeoNode *)fPasNodes->At(0);
  nod1 = (FairGeoNode *)fPasNodes->At(1);

  if ((!nod0) || (!nod1))
    return NULL;

  mother_name = nod->getMother();

  if (mother_name == nod1->GetName()) {

    (*right).push_back(nod0->getCopyNo() + pGlobalDetectorNumber - 1);
    (*right).push_back(nod1->getCopyNo());
    (*right).push_back(nodeNumber);

    BmdVolId_t *left = new BmdVolId_t;
    (*left).push_back(nod0->getCopyNo());
    (*left).push_back(nod1->getCopyNo());
    (*left).push_back(nod->getMCid());
    (*left).push_back(nod->getCopyNo());

    if (pVerbose > 1) {
      tmp = "  mother: " + mother_name + ", me: " + (nod->getName()) + "    ";
      cout << tmp << (*left)[0] << "," << (*left)[1] << "," << (*left)[2] << ","
           << (*left)[3] << " : " << (*right)[0] << "," << (*right)[1] << ","
           << (*right)[2] << endl;
    }

    return left;

  } else {
    cout << "-E-  BmdDigiScheme::CreateVolInfoElement:  Strange for me node: "
         << nod->GetName() << "  Node number:" << nodeNumber
         << "  Mother:" << mother_name << endl;
    return NULL;
  }
}

// -------------------------------------------------------------------------
//___________________________________________________________

Bool_t BmdDigiScheme::AddNodes(TObjArray *sensNodes,
                               Int_t pGlobalDetectorNumber, Bool_t pAdd,
                               Int_t pVerbose) {
  Int_t nNodes = sensNodes->GetEntriesFast();
  FairGeoNode *nod = 0;
  Int_t nodeNumber, nodeCopyNo, nodeVolumeId, chanId2 = 0, chanId1 = 0;
  BmdVolId_t *left1, *left2;
  BmdDigiId_t *right1, *right2;

  if (pVerbose) {
    cout << "-W-  BmdDigiScheme::AddNodes: started:"
         << "GlobalDetectorNumber:" << pGlobalDetectorNumber
         << "  nNodes:" << nNodes << endl;
  }

  for (nodeNumber = 0; nodeNumber < nNodes; nodeNumber++) {
    nod = (FairGeoNode *)sensNodes->At(nodeNumber);
    if (nod) {

      right1 = new BmdDigiId_t;
      left1 = CreateVolElement(nod, nodeNumber, right1, pGlobalDetectorNumber,
                               pVerbose);

      fVolToDigiIdMap[*left1] = *right1;

      left2 = new BmdDigiId_t((*left1).begin(), (*left1).end());
      (*left2)[0] = (*left1)[0] + 1;
      right2 = new BmdDigiId_t(right1->begin(), right1->end());
      (*right2)[0] = (*right1)[0] + 1;

      fVolToDigiIdMap[*left2] = *right2;

      CreateVolCopyElements(left1, right1);

      if (pAdd) {
        BmdVolInfo_t *volInfo1 = CreateVolInfoElement(nod, pVerbose);
        if (volInfo1) {
          fDigiToVolInfoMap[*right1] = volInfo1;
          BmdVolInfo_t *volInfo2 = new BmdVolInfo_t(*volInfo1);
          (*volInfo2)[2] = -(*volInfo2)[2]; // Z
          fDigiToVolInfoMap[*right2] = volInfo2;

          CreateVolInfoCopyElements(right1, volInfo1);
        }
      }
    } else {
      cout << "-W-  BmdDigiScheme::AddNodes: "
           << "Node number " << nodeNumber << " from " << nNodes
           << " not found!" << endl;
      return kFALSE;
    }
  }
  return kTRUE;
}

// -------------------------------------------------------------------------
//___________________________________________________________

Bool_t BmdDigiScheme::CreateVolCopyElements(BmdVolId_t *left,
                                            BmdDigiId_t *right) {
  BmdVolId_t *left1, *left2;
  BmdDigiId_t *right1, *right2;

  if (!fPasNodes)
    return kFALSE;

  FairGeoNode *nod1 = (FairGeoNode *)fPasNodes->At(fPasNodes->GetEntries() - 2);

  if (!nod1)
    return kFALSE;

  Int_t moduleID, nModules; // {MotherMotherCopyNo, MotherCopyNo, VolumeId,
                            // CopyNo},{DetectorID, ModuleID, ChannelID}

  TString lm_name = nod1->getName();
  TString last_module_number(&(lm_name[lm_name.Last('#') + 1]));
  nModules = last_module_number.Atoi();

  for (moduleID = 1; moduleID < nModules; moduleID++) {

    left1 = new BmdDigiId_t((*left).begin(), (*left).end());
    (*left1)[1] = (*left)[1] + moduleID;
    right1 = new BmdDigiId_t(right->begin(), right->end());
    (*right1)[1] = (*right)[1] + moduleID;

    left2 = new BmdDigiId_t((*left1).begin(), (*left1).end());
    (*left2)[0] = (*left1)[0] + 1;
    right2 = new BmdDigiId_t(right1->begin(), right1->end());
    (*right2)[0] = (*right1)[0] + 1;

    fVolToDigiIdMap[*left1] = *right1;
    fVolToDigiIdMap[*left2] = *right2;
  }

  return kTRUE;
}

// -------------------------------------------------------------------------

//___________________________________________________________
Bool_t BmdDigiScheme::CreateVolInfoCopyElements(BmdDigiId_t *right,
                                                BmdVolInfo_t *volInfo) {

  if (!fPasNodes)
    return kFALSE;

  FairGeoNode *nod1 = (FairGeoNode *)fPasNodes->At(fPasNodes->GetEntries() - 2);

  if (!nod1)
    return kFALSE;

  Int_t moduleID, nModules;

  TString lm_name = nod1->getName();
  TString last_module_number(&(lm_name[lm_name.Last('#') + 1]));
  nModules = last_module_number.Atoi();

  for (moduleID = 1; moduleID < nModules; moduleID++) {

    nod1 = (FairGeoNode *)fPasNodes->At(fPasNodes->GetEntries() - 2 - nModules +
                                        moduleID);

    FairGeoVector pos = nod1->getLabTransform()->getTranslation();

    BmdVolInfo_t *volInfo1 =
        new BmdVolInfo_t((*volInfo).begin(), (*volInfo).end());
    (*volInfo1)[0] = pos.getValues(0); // X [cm]
    (*volInfo1)[1] = pos.getValues(1); // Y [cm]

    BmdDigiId_t *right1 = new BmdDigiId_t(right->begin(), right->end());
    (*right1)[1] = (*right)[1] + moduleID;

    BmdDigiId_t *right2 = new BmdDigiId_t(right1->begin(), right1->end());
    (*right2)[0] = (*right1)[0] + 1;

    BmdVolInfo_t *volInfo2 = new BmdVolInfo_t(*volInfo1);
    (*volInfo2)[2] = -(*volInfo2)[2]; // Z [cm]

    fDigiToVolInfoMap[*right1] = volInfo1;
    fDigiToVolInfoMap[*right2] = volInfo2;
  }

  return kTRUE;
}

// -------------------------------------------------------------------------

//___________________________________________________________
Bool_t BmdDigiScheme::Init(BmdGeoPar *geoPar, BmdDigiPar *digiPar,
                           Int_t pVerbose) {
  //  if (!fYes) {
    if (!fInitialized) {
      cout << "-W- BmdDigiScheme::Init: "
           << "Init basic BMD first!" << endl;
      return kFALSE;
    }

    //    fYes = kTRUE;

    if (!geoPar) {
      cout << "-W- BmdDigiScheme::Init: "
           << "No geometry parameters available!" << endl;
      return kFALSE;
    }
    if (!digiPar) {
      cout << "-W-  BmdDigiScheme::Init: "
           << "No digitization parameters available!" << endl;
      return kFALSE;
    }
    fBmdDigiPar = digiPar;

    TObjArray *sensNodes;

    sensNodes = geoPar->GetGeoSensitiveNodes();
    if (!sensNodes) {
      cout << "-W-  BmdDigiScheme::Init: "
           << "No sensitive nodes available!" << endl;
      return kFALSE;
    }

    AddNodes(sensNodes, kBMD + 2, kTRUE,
             pVerbose); // kBMD+2 - hard-coded global BMD_ detector number
    CalcDimensions(kBMD + 2, Nx, Ny, Nz);
    //  }
  return kTRUE;
}

// -------------------------------------------------------------------------

//___________________________________________________________
Bool_t BmdDigiScheme::GetVolCenterXYZ(BmdDigiId_t *pDigiID, Double_t &x,
                                      Double_t &y, Double_t &z) {
  if (!pDigiID)
    return kFALSE;
  if (fDigiToVolInfoMap.find(*pDigiID) == fDigiToVolInfoMap.end())
    return kFALSE;
  else {
    BmdVolInfo_t *volInfo = fDigiToVolInfoMap[*pDigiID];
    if (volInfo) {
      x = (*volInfo)[0];
      y = (*volInfo)[1];
      z = (*volInfo)[2];
      return kTRUE;
    } else
      return kFALSE;
  }
}

// -------------------------------------------------------------------------

//___________________________________________________________
Bool_t BmdDigiScheme::GetVolDxDyDz(BmdDigiId_t *pDigiID, Double_t &Dx,
                                   Double_t &Dy, Double_t &Dz) {
  if (!pDigiID)
    return kFALSE;
  if (fDigiToVolInfoMap.find(*pDigiID) == fDigiToVolInfoMap.end())
    return kFALSE;
  else {
    BmdVolInfo_t *volInfo = fDigiToVolInfoMap[*pDigiID];
    if (volInfo) {
      Dx = (*volInfo)[3];
      Dy = (*volInfo)[4];
      Dz = (*volInfo)[5];
      return kTRUE;
    } else
      return kFALSE;
  }
}

// -------------------------------------------------------------------------

//___________________________________________________________
Bool_t BmdDigiScheme::IsVolumeExist(BmdVolId_t *pVolId) {
  if (!pVolId)
    return kFALSE;
  else
    return (!(fVolToDigiIdMap.find(*pVolId) == fVolToDigiIdMap.end()));
}

// -------------------------------------------------------------------------

//___________________________________________________________
BmdDigiId_t BmdDigiScheme::GetDigiId(BmdVolId_t *pVolId) {
  static const BmdDigiId_t not_found(4, -1);

  if (IsVolumeExist(pVolId))
    return fVolToDigiIdMap[*pVolId];
  else
    return not_found;
}

// -------------------------------------------------------------------------

//___________________________________________________________
Int_t BmdDigiScheme::GetDetectorID(BmdVolId_t *pVolId) {
  BmdDigiId_t digiID = GetDigiId(pVolId);
  //  return digiID.first;
  return digiID[0];
}

// -------------------------------------------------------------------------

//___________________________________________________________
Int_t BmdDigiScheme::GetChannelID(BmdVolId_t *pVolId) {
  BmdDigiId_t digiID = GetDigiId(pVolId);
  //  return digiID.second;
  return digiID[2];
}

// -------------------------------------------------------------------------

//___________________________________________________________
BmdVolInfo_t *BmdDigiScheme::GetVolInfo(BmdVolId_t *pVolId) {
  if (IsVolumeExist(pVolId)) {

    BmdDigiId_t pDigiID = GetDigiId(pVolId);

    if (fDigiToVolInfoMap.find(pDigiID) == fDigiToVolInfoMap.end())
      return NULL;
    else
      return fDigiToVolInfoMap[pDigiID];
  } else
    return NULL;
}

// -------------------------------------------------------------------------

//___________________________________________________________
void BmdDigiScheme::PrintVolume(Int_t volID, Int_t copyNo, Int_t copyNoMother,
                                Int_t copyNoMotherMother) {
  Int_t content[] = {copyNoMotherMother, copyNoMother, volID, copyNo};
  BmdVolId_t pVolId(content, content + sizeof(content) / sizeof(Int_t));

  BmdDigiId_t pDigiID = GetDigiId(&pVolId);

  cout << " Bmd Volume: " << copyNoMotherMother << "," << copyNoMother << ","
       << volID << "," << copyNo << "   DigiID: " << pDigiID[0] << ","
       << pDigiID[1] << "," << pDigiID[2];

  BmdVolInfo_t *pVolInfo = GetVolInfo(&pVolId);

  if (pVolInfo)
    cout << "    X,Y,Z [cm]: " << (*pVolInfo)[0] << "," << (*pVolInfo)[1] << ","
         << (*pVolInfo)[2] << "    Dx,Dy,Dz [cm]: " << (*pVolInfo)[3] << ","
         << (*pVolInfo)[4] << "," << (*pVolInfo)[5];

  cout << endl;
}

// -------------------------------------------------------------------------

//___________________________________________________________
Bool_t BmdDigiScheme::CalcDimensions(Int_t pGlobalDetectorNumber, Int_t &nx,
                                     Int_t &ny, Int_t &nz) {

  if (fDigiToVolInfoMap.empty())
    return kFALSE;

  Bool_t result = kFALSE;
  std::map<BmdDigiId_t, BmdVolInfo_t *>::iterator it;
  std::map<Double_t, Int_t> xmap, ymap, zmap;
  Double_t x, y, z;
  nx = ny = nz = 0;

  for (it = fDigiToVolInfoMap.begin(); it != fDigiToVolInfoMap.end(); ++it) {

    //    cout << ". ";

    //    if ((*it).first.first==pGlobalDetectorNumber) {
    if ((*it).first[0] == 1) {

      result = kTRUE;

      x = (*((*it).second))[0];
      if (xmap.count(x))
        xmap[x] = xmap[x] + 1;
      else
        xmap[x] = 1;

      y = (*((*it).second))[1];
      if (ymap.count(y))
        ymap[y] = ymap[y] + 1;
      else
        ymap[y] = 1;

      z = (*((*it).second))[2];
      if (zmap.count(z))
        zmap[z] = zmap[z] + 1;
      else
        zmap[z] = 1;
    }
  }

  nx = xmap.size();
  ny = ymap.size();
  nz = zmap.size();

  return result;
}

// -------------------------------------------------------------------------

void BmdDigiScheme::GetBmdDimensions(Int_t &nx, Int_t &ny, Int_t &nz) {
  nx = Nx;
  ny = Ny;
  nz = Nz;
}

// -------------------------------------------------------------------------

BmdDigiId_t BmdDigiScheme::GetDigiIdFromCoords(Double_t x, Double_t y,
                                               Double_t z) {
  Int_t content[] = {-1, -1, -1, -1};
  BmdVolId_t resultmc(content, content + sizeof(content) / sizeof(Int_t));

  BmdDigiId_t result(-1, -1);
  if (gGeoManager) {
    TGeoNode *tgn = gGeoManager->FindNode(x, y, z);
    if (tgn) {
      resultmc[0] = 1 * (z > 0) + 2 * (z < 0);
      resultmc[1] = tgn->GetMotherVolume()->GetNumber();
      resultmc[2] = tgn->GetVolume()->GetNumber();
      resultmc[3] = tgn->GetNumber();
      if (fVolToDigiIdMap.find(resultmc) != fVolToDigiIdMap.end())
        result = fVolToDigiIdMap[resultmc];
    }
  }
  return result;
}

// -------------------------------------------------------------------------

BmdDigiId_t BmdDigiScheme::GetDigiIdFromVolumeData(
    Int_t pMcVolumeNumber, Int_t pMcCopyNumber, Int_t pMotherCopyNumber,
    Int_t pMotherMotherCopyNumber) {
  Int_t content[] = {pMotherMotherCopyNumber, pMotherCopyNumber,
                     pMcVolumeNumber, pMcCopyNumber};
  BmdVolId_t pVolId(content, content + sizeof(content) / sizeof(Int_t));
  BmdDigiId_t digiID = GetDigiId(&pVolId);
  return digiID;
}

// -------------------------------------------------------------------------

void BmdDigiScheme::SplitDigiID(BmdDigiId_t digiID, Int_t &detID, Int_t &modID,
                                Int_t &chanID) {
  detID = digiID[0];
  modID = digiID[1];
  chanID = digiID[2];
}

// -------------------------------------------------------------------------

void BmdDigiScheme::Print() {
  cout << "*********************************************" << endl;

  cout << "***  BmdDigiScheme:" << endl;
  cout << " Bmd  Nx,Ny,Nz:  " << Nx << "," << Ny << "," << Nz
       <<  endl;

  std::map<BmdVolId_t, BmdDigiId_t>::iterator it;

  for (it = fVolToDigiIdMap.begin(); it != fVolToDigiIdMap.end(); ++it)
    PrintVolume((*it).first[2], (*it).first[3], (*it).first[1], (*it).first[0]);

  cout << "*********************************************" << endl;
}

// -------------------------------------------------------------------------
Bool_t BmdDigiScheme::GetDetIdModIdChanId(Int_t pMcVolumeNumber,
                                          Int_t pMcCopyNumber,
                                          Int_t pMotherCopyNumber,
                                          Int_t pMotherMotherCopyNumber,
                                          Int_t &pDetId, Int_t &pChanId,
                                          Int_t &pModId) {
  Int_t content[] = {pMotherMotherCopyNumber, pMotherCopyNumber,
                     pMcVolumeNumber, pMcCopyNumber};
  BmdVolId_t pVolId(content, content + sizeof(content) / sizeof(Int_t));
  BmdDigiId_t digiID = GetDigiId(&pVolId);
  pDetId = digiID[0];
  pModId = digiID[1];
  pChanId = digiID[2];

  return kTRUE;
}

// -------------------------------------------------------------------------
ClassImp(BmdDigiScheme)
