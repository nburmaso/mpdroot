//*-- AUTHOR : Ilse Koenig
//*-- Created : 25/10/2004

/////////////////////////////////////////////////////////////
//
//  MpdSftContFact
//
//  Factory for the parameter containers in libTof
//
/////////////////////////////////////////////////////////////

using namespace std;
#include "MpdSftContFact.h"
#include "FairRuntimeDb.h"
#include "MpdSftGeoPar.h"
#include <iostream>
#include <iomanip>

ClassImp(MpdSftContFact)

static MpdSftContFact gMpdSftContFact;

MpdSftContFact::MpdSftContFact() {
  // Constructor (called when the library is loaded)
  fName="MpdSftContFact";
  fTitle="Factory for parameter containers in libSft";
  setAllContainers();
  FairRuntimeDb::instance()->addContFactory(this);
}

void MpdSftContFact::setAllContainers() {
  /** Creates the Container objects with all accepted contexts and adds them to
   *  the list of containers for the Tof library.*/

    FairContainer* p= new FairContainer("MpdSftGeoPar",
				      "Sft Geometry Parameters",
				      "TestDefaultContext");
    p->addContext("TestNonDefaultContext");

    containers->Add(p);
}

FairParSet* MpdSftContFact::createContainer(FairContainer* c) {
  /** Calls the constructor of the corresponding parameter container.
   * For an actual context, which is not an empty string and not the default context
   * of this container, the name is concatinated with the context. */
  const char* name=c->GetName();
  cout << " -I container name " << name << endl;
  FairParSet* p=0;
  if (strcmp(name,"MpdSftGeoPar")==0) {
    p=new MpdSftGeoPar(c->getConcatName().Data(),c->GetTitle(),c->getContext());
  }
  return p;
}

