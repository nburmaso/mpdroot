//*-- AUTHOR : Denis Bertini
//*-- Modified : 06/10/2005 by Denis Bertini

/////////////////////////////////////////////////////////////
//  CbmSttParRootFileIo
//
//  Class for Start parameter input/output from/into ROOT file
//
//  It is derived from the base class HDetParRootFileIo and
//  inherits from it basic functions e.g. write(...)
//
/////////////////////////////////////////////////////////////
using namespace std;
#include "CbmSttParRootFileIo.h"
#include "FairParRootFileIo.h"
#include "FairRuntimeDb.h"
#include "CbmSttDigiPar.h"
#include <iostream> 
#include <iomanip>

ClassImp(CbmSttParRootFileIo)

CbmSttParRootFileIo::CbmSttParRootFileIo(FairParRootFile* f) : FairDetParRootFileIo(f)
{
  fName="CbmSttParIo";
}


CbmSttParRootFileIo::~CbmSttParRootFileIo() 
{
}

Bool_t CbmSttParRootFileIo::init(FairParSet* pPar,Int_t* set)
{
  const Text_t* name=pPar->GetName();
  if (pFile) 
  {
      if (!strcmp(name,"SttDigiPar")) return read((CbmSttDigiPar*)pPar,set);
  }
  cerr<<"initialization of "<<name<<" not possible from ROOT file!"<<endl;
  return kFALSE;
}


Bool_t CbmSttParRootFileIo::read(CbmSttDigiPar* pPar,Int_t* set) 
{
    cout << "-I- CbmSttParRootFileIo : reading " << endl;

    return kTRUE;
}

