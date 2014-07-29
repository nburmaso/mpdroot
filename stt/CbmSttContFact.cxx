//*-- AUTHOR : Denis Bertini
//*-- Created : 20/06/2005

/////////////////////////////////////////////////////////////
//
//  CbmSttContFact
//
//  Factory for the parameter containers in libStt
//
/////////////////////////////////////////////////////////////

using namespace std;
#include "CbmSttContFact.h"
#include "FairRuntimeDb.h"
#include "CbmGeoSttPar.h"
#include "CbmSttDigiPar.h"
#include "FairParRootFileIo.h"
#include "FairParAsciiFileIo.h"
#include "CbmSttParRootFileIo.h"
#include "CbmSttParAsciiFileIo.h"
#include <iostream>
#include <iomanip>

ClassImp(CbmSttContFact)

static CbmSttContFact gCbmSttContFact;

CbmSttContFact::CbmSttContFact() 
{
  // Constructor (called when the library is loaded)
  fName="CbmSttContFact";
  fTitle="Factory for parameter containers in libStt";
  setAllContainers();
  FairRuntimeDb::instance()->addContFactory(this);
}

void CbmSttContFact::setAllContainers() 
{
    /** Creates the Container objects with all accepted contexts and adds them to
     *  the list of containers for the STT library.*/
    FairContainer* p2= new FairContainer("CbmGeoSttPar",
				       "Stt Geometry Parameters",
				       "TestDefaultContext");
    p2->addContext("TestNonDefaultContext");
    
    containers->Add(p2);
}

FairParSet* CbmSttContFact::createContainer(FairContainer* c)
{
  /** Calls the constructor of the corresponding parameter container.
   * For an actual context, which is not an empty string and not the default context
   * of this container, the name is concatinated with the context. */
  const char* name=c->GetName();
  cout << " -I container name " << name << endl;
  FairParSet* p=0;

  if (strcmp(name,"CbmGeoSttPar")==0) {
    p=new CbmGeoSttPar(c->getConcatName().Data(),c->GetTitle(),c->getContext());
  }
  return p;
}
/*
void  CbmSttContFact::activateParIo(CbmParIo* io) {
  // activates the input/output class for the parameters
  // needed by the Stt
  if (strcmp(io->IsA()->GetName(),"FairParRootFileIo")==0) {
    CbmSttParRootFileIo* p=new CbmSttParRootFileIo(((FairParRootFileIo*)io)->getParRootFile());
    io->setDetParIo(p);
  }
  if (strcmp(io->IsA()->GetName(),"FairParAsciiFileIo")==0) {
    CbmSttParAsciiFileIo* p=new CbmSttParAsciiFileIo(((FairParAsciiFileIo*)io)->getFile());
    io->setDetParIo(p);
  }
}

*/
