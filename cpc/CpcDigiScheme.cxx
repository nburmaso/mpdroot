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

#include "CpcDigiScheme.h"

using namespace std;


CpcDigiScheme *CpcDigiScheme::fInstance = 0;
Bool_t CpcDigiScheme::fInitialized = 0;
Int_t CpcDigiScheme::fRefcount = 0;

static Int_t kCPC = 1; //  // hard-coded basic CPC detector number

//___________________________________________________________

CpcDigiScheme::CpcDigiScheme() : Nx(0), Ny(0), Nz(0) {
  Nx = 0;
  Ny = 0;
  Nz = 0;
  fCpcDigiPar = 0;
}

// -------------------------------------------------------------------------
//___________________________________________________________

CpcDigiScheme::~CpcDigiScheme() {
  fRefcount--;
  if (!fRefcount) {
    delete this;
    fInstance = NULL;
  }
}
// -------------------------------------------------------------------------
//___________________________________________________________

CpcDigiScheme *CpcDigiScheme::Instance() {
  if (!fInstance)
    fInstance = new CpcDigiScheme();
  fRefcount++;
  return fInstance;
}

// -------------------------------------------------------------------------
//___________________________________________________________

Bool_t CpcDigiScheme::Init(MpdCpcGeoPar *geoPar, CpcDigiPar *digiPar, Bool_t pAdd,
                           Int_t pVerbose) {
  if (!fInitialized) {

    if (!geoPar) {
      cout << "-W- CpcDigiScheme::Init: "
           << "No geometry parameters available!" << endl;
      return kFALSE;
    }
    if (!digiPar) {
      cout << "-W-  CpcDigiScheme::Init: "
           << "No digitization parameters available!" << endl;
      //      return kFALSE;
    }
    fCpcDigiPar = digiPar;

    TObjArray *sensNodes = geoPar->GetGeoSensitiveNodes();
    if (!sensNodes) {
      cout << "-W-  CpcDigiScheme::Init: "
           << "No sensitive nodes available!" << endl;
      return kFALSE;
    }

    fPasNodes = geoPar->GetGeoPassiveNodes();
    if (!fPasNodes) {
      cout << "-W-  CpcDigiScheme::Init: "
           << "No passive nodes available!" << endl;
      return kFALSE;
    }

    fInitialized = kTRUE;

    AddNodes(sensNodes, kCPC, pAdd, pVerbose);
    CalcDimensions(kCPC, Nx, Ny, Nz);

    if (pVerbose)
      cout << endl << "-W-  CpcDigiScheme::Init: finished." << endl;
  }
  return kTRUE;
}

// -------------------------------------------------------------------------
//___________________________________________________________

CpcVolInfo_t *CpcDigiScheme::CreateVolInfoElement(FairGeoNode *nod,
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

  CpcVolInfo_t *volInfo = new CpcVolInfo_t;
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

CpcVolId_t *CpcDigiScheme::CreateVolElement(FairGeoNode *nod, Int_t nodeNumber,
                                            CpcDigiId_t *right,
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

    CpcVolId_t *left = new CpcVolId_t;
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

Bool_t CpcDigiScheme::AddNodes(TObjArray *sensNodes,
                               Int_t pGlobalDetectorNumber, Bool_t pAdd,
                               Int_t pVerbose) {
  Int_t nNodes = sensNodes->GetEntriesFast();
  FairGeoNode *nod = 0;
  Int_t nodeNumber, nodeCopyNo, nodeVolumeId, chanId2 = 0, chanId1 = 0;
  CpcVolId_t *left1, *left2;
  CpcDigiId_t *right1, *right2;

  if (pVerbose) {
    cout << "-W-  BmdDigiScheme::AddNodes: started:"
         << "GlobalDetectorNumber:" << pGlobalDetectorNumber
         << "  nNodes:" << nNodes << endl;
  }

  for (nodeNumber = 0; nodeNumber < nNodes; nodeNumber++) {
    nod = (FairGeoNode *)sensNodes->At(nodeNumber);
    if (nod) {

      right1 = new CpcDigiId_t;
      left1 = CreateVolElement(nod, nodeNumber, right1, pGlobalDetectorNumber,
                               pVerbose);

      fVolToDigiIdMap[*left1] = *right1;

      left2 = new CpcDigiId_t((*left1).begin(), (*left1).end());
      (*left2)[0] = (*left1)[0] + 1;
      right2 = new CpcDigiId_t(right1->begin(), right1->end());
      (*right2)[0] = (*right1)[0] + 1;

      fVolToDigiIdMap[*left2] = *right2;

      CreateVolCopyElements(left1, right1);

      if (pAdd) {
        CpcVolInfo_t *volInfo1 = CreateVolInfoElement(nod, pVerbose);
        if (volInfo1) {
          fDigiToVolInfoMap[*right1] = volInfo1;
          CpcVolInfo_t *volInfo2 = new CpcVolInfo_t(*volInfo1);
          (*volInfo2)[2] = -(*volInfo2)[2]; // Z
          fDigiToVolInfoMap[*right2] = volInfo2;

          CreateVolInfoCopyElements(right1, volInfo1);
        }
      }
    } else {
      cout << "-W-  CpcDigiScheme::AddNodes: "
           << "Node number " << nodeNumber << " from " << nNodes
           << " not found!" << endl;
      return kFALSE;
    }
  }
  return kTRUE;
}

// -------------------------------------------------------------------------
//___________________________________________________________

Bool_t CpcDigiScheme::CreateVolCopyElements(CpcVolId_t *left,
                                            CpcDigiId_t *right) {
  CpcVolId_t *left1, *left2;
  CpcDigiId_t *right1, *right2;

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

    left1 = new CpcDigiId_t((*left).begin(), (*left).end());
    (*left1)[1] = (*left)[1] + moduleID;
    right1 = new CpcDigiId_t(right->begin(), right->end());
    (*right1)[1] = (*right)[1] + moduleID;

    left2 = new CpcDigiId_t((*left1).begin(), (*left1).end());
    (*left2)[0] = (*left1)[0] + 1;
    right2 = new CpcDigiId_t(right1->begin(), right1->end());
    (*right2)[0] = (*right1)[0] + 1;

    fVolToDigiIdMap[*left1] = *right1;
    fVolToDigiIdMap[*left2] = *right2;
  }

  return kTRUE;
}

// -------------------------------------------------------------------------

//___________________________________________________________
Bool_t CpcDigiScheme::CreateVolInfoCopyElements(CpcDigiId_t *right,
                                                CpcVolInfo_t *volInfo) {

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

    CpcVolInfo_t *volInfo1 =
        new CpcVolInfo_t((*volInfo).begin(), (*volInfo).end());
    (*volInfo1)[0] = pos.getValues(0); // X [cm]
    (*volInfo1)[1] = pos.getValues(1); // Y [cm]

    CpcDigiId_t *right1 = new CpcDigiId_t(right->begin(), right->end());
    (*right1)[1] = (*right)[1] + moduleID;

    CpcDigiId_t *right2 = new CpcDigiId_t(right1->begin(), right1->end());
    (*right2)[0] = (*right1)[0] + 1;

    CpcVolInfo_t *volInfo2 = new CpcVolInfo_t(*volInfo1);
    (*volInfo2)[2] = -(*volInfo2)[2]; // Z [cm]

    fDigiToVolInfoMap[*right1] = volInfo1;
    fDigiToVolInfoMap[*right2] = volInfo2;
  }

  return kTRUE;
}

// -------------------------------------------------------------------------

//___________________________________________________________
Bool_t CpcDigiScheme::Init(MpdCpcGeoPar *geoPar, CpcDigiPar *digiPar,
                           Int_t pVerbose) {
  //  if (!fYes) {
    if (!fInitialized) {
      cout << "-W- CpcDigiScheme::Init: "
           << "Init basic CPC first!" << endl;
      return kFALSE;
    }

    //    fYes = kTRUE;

    if (!geoPar) {
      cout << "-W- CpcDigiScheme::Init: "
           << "No geometry parameters available!" << endl;
      return kFALSE;
    }
    if (!digiPar) {
      cout << "-W-  CpcDigiScheme::Init: "
           << "No digitization parameters available!" << endl;
      return kFALSE;
    }
    fCpcDigiPar = digiPar;

    TObjArray *sensNodes;

    sensNodes = geoPar->GetGeoSensitiveNodes();
    if (!sensNodes) {
      cout << "-W-  CpcDigiScheme::Init: "
           << "No sensitive nodes available!" << endl;
      return kFALSE;
    }

    AddNodes(sensNodes, kCPC + 2, kTRUE,
             pVerbose); // kCPC+2 - hard-coded global BMD_ detector number
    CalcDimensions(kCPC + 2, Nx, Ny, Nz);
    //  }
  return kTRUE;
}

// -------------------------------------------------------------------------

//___________________________________________________________
Bool_t CpcDigiScheme::GetVolCenterXYZ(CpcDigiId_t *pDigiID, Double_t &x,
                                      Double_t &y, Double_t &z) {
  if (!pDigiID)
    return kFALSE;
  if (fDigiToVolInfoMap.find(*pDigiID) == fDigiToVolInfoMap.end())
    return kFALSE;
  else {
    CpcVolInfo_t *volInfo = fDigiToVolInfoMap[*pDigiID];
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
Bool_t CpcDigiScheme::GetVolDxDyDz(CpcDigiId_t *pDigiID, Double_t &Dx,
                                   Double_t &Dy, Double_t &Dz) {
  if (!pDigiID)
    return kFALSE;
  if (fDigiToVolInfoMap.find(*pDigiID) == fDigiToVolInfoMap.end())
    return kFALSE;
  else {
    CpcVolInfo_t *volInfo = fDigiToVolInfoMap[*pDigiID];
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
Bool_t CpcDigiScheme::IsVolumeExist(CpcVolId_t *pVolId) {
  if (!pVolId)
    return kFALSE;
  else
    return (!(fVolToDigiIdMap.find(*pVolId) == fVolToDigiIdMap.end()));
}

// -------------------------------------------------------------------------

//___________________________________________________________
CpcDigiId_t CpcDigiScheme::GetDigiId(CpcVolId_t *pVolId) {
  static const CpcDigiId_t not_found(4, -1);

  if (IsVolumeExist(pVolId))
    return fVolToDigiIdMap[*pVolId];
  else
    return not_found;
}

// -------------------------------------------------------------------------

//___________________________________________________________
Int_t CpcDigiScheme::GetDetectorID(CpcVolId_t *pVolId) {
  CpcDigiId_t digiID = GetDigiId(pVolId);
  //  return digiID.first;
  return digiID[0];
}

// -------------------------------------------------------------------------

//___________________________________________________________
Int_t CpcDigiScheme::GetChannelID(CpcVolId_t *pVolId) {
  CpcDigiId_t digiID = GetDigiId(pVolId);
  //  return digiID.second;
  return digiID[2];
}

// -------------------------------------------------------------------------

//___________________________________________________________
CpcVolInfo_t *CpcDigiScheme::GetVolInfo(CpcVolId_t *pVolId) {
  if (IsVolumeExist(pVolId)) {

    CpcDigiId_t pDigiID = GetDigiId(pVolId);

    if (fDigiToVolInfoMap.find(pDigiID) == fDigiToVolInfoMap.end())
      return NULL;
    else
      return fDigiToVolInfoMap[pDigiID];
  } else
    return NULL;
}

// -------------------------------------------------------------------------

//___________________________________________________________
void CpcDigiScheme::PrintVolume(Int_t volID, Int_t copyNo, Int_t copyNoMother,
                                Int_t copyNoMotherMother) {
  Int_t content[] = {copyNoMotherMother, copyNoMother, volID, copyNo};
  CpcVolId_t pVolId(content, content + sizeof(content) / sizeof(Int_t));

  CpcDigiId_t pDigiID = GetDigiId(&pVolId);

  cout << " Cpc Volume: " << copyNoMotherMother << "," << copyNoMother << ","
       << volID << "," << copyNo << "   DigiID: " << pDigiID[0] << ","
       << pDigiID[1] << "," << pDigiID[2];

  CpcVolInfo_t *pVolInfo = GetVolInfo(&pVolId);

  if (pVolInfo)
    cout << "    X,Y,Z [cm]: " << (*pVolInfo)[0] << "," << (*pVolInfo)[1] << ","
         << (*pVolInfo)[2] << "    Dx,Dy,Dz [cm]: " << (*pVolInfo)[3] << ","
         << (*pVolInfo)[4] << "," << (*pVolInfo)[5];

  cout << endl;
}

// -------------------------------------------------------------------------

//___________________________________________________________
Bool_t CpcDigiScheme::CalcDimensions(Int_t pGlobalDetectorNumber, Int_t &nx,
                                     Int_t &ny, Int_t &nz) {

  if (fDigiToVolInfoMap.empty())
    return kFALSE;

  Bool_t result = kFALSE;
  std::map<CpcDigiId_t, CpcVolInfo_t *>::iterator it;
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

void CpcDigiScheme::GetCpcDimensions(Int_t &nx, Int_t &ny, Int_t &nz) {
  nx = Nx;
  ny = Ny;
  nz = Nz;
}

// -------------------------------------------------------------------------

CpcDigiId_t CpcDigiScheme::GetDigiIdFromCoords(Double_t x, Double_t y,
                                               Double_t z) {
  Int_t content[] = {-1, -1, -1, -1};
  CpcVolId_t resultmc(content, content + sizeof(content) / sizeof(Int_t));

  CpcDigiId_t result(-1, -1);
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

CpcDigiId_t CpcDigiScheme::GetDigiIdFromVolumeData(
    Int_t pMcVolumeNumber, Int_t pMcCopyNumber, Int_t pMotherCopyNumber,
    Int_t pMotherMotherCopyNumber) {
  Int_t content[] = {pMotherMotherCopyNumber, pMotherCopyNumber,
                     pMcVolumeNumber, pMcCopyNumber};
  CpcVolId_t pVolId(content, content + sizeof(content) / sizeof(Int_t));
  CpcDigiId_t digiID = GetDigiId(&pVolId);
  return digiID;
}

// -------------------------------------------------------------------------

void CpcDigiScheme::SplitDigiID(CpcDigiId_t digiID, Int_t &detID, Int_t &modID,
                                Int_t &chanID) {
  detID = digiID[0];
  modID = digiID[1];
  chanID = digiID[2];
}

// -------------------------------------------------------------------------

void CpcDigiScheme::Print() {
  cout << "*********************************************" << endl;

  cout << "***  BmdDigiScheme:" << endl;
  cout << " Cpc  Nx,Ny,Nz:  " << Nx << "," << Ny << "," << Nz
       <<  endl;

  std::map<CpcVolId_t, CpcDigiId_t>::iterator it;

  for (it = fVolToDigiIdMap.begin(); it != fVolToDigiIdMap.end(); ++it)
    PrintVolume((*it).first[2], (*it).first[3], (*it).first[1], (*it).first[0]);

  cout << "*********************************************" << endl;
}

// -------------------------------------------------------------------------
Bool_t CpcDigiScheme::GetDetIdModIdChanId(Int_t pMcVolumeNumber,
                                          Int_t pMcCopyNumber,
                                          Int_t pMotherCopyNumber,
                                          Int_t pMotherMotherCopyNumber,
                                          Int_t &pDetId, Int_t &pChanId,
                                          Int_t &pModId) {
  Int_t content[] = {pMotherMotherCopyNumber, pMotherCopyNumber,
                     pMcVolumeNumber, pMcCopyNumber};
  CpcVolId_t pVolId(content, content + sizeof(content) / sizeof(Int_t));
  CpcDigiId_t digiID = GetDigiId(&pVolId);
  pDetId = digiID[0];
  pModId = digiID[1];
  pChanId = digiID[2];

  return kTRUE;
}

// -------------------------------------------------------------------------
ClassImp(CpcDigiScheme)
