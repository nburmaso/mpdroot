/////////////////////////////////////////////////////////////
//
//  MpdDchContFact
//
//  Factory for the parameter containers in libDch
//
/////////////////////////////////////////////////////////////

using namespace std;
#include "MpdDchContFact.h"
#include "FairRuntimeDb.h"
#include "MpdDchGeoPar.h"
#include <iostream>
#include <iomanip>

ClassImp(MpdDchContFact)

static MpdDchContFact gMpdDchContFact;

MpdDchContFact::MpdDchContFact() {
  // Constructor (called when the library is loaded)
  fName="MpdDchContFact";
  fTitle="Factory for parameter containers in libDch";
  setAllContainers();
  FairRuntimeDb::instance()->addContFactory(this);
}

void MpdDchContFact::setAllContainers() {
  /** Creates the Container objects with all accepted contexts and adds them to
   *  the list of containers for the Tof library.*/

    FairContainer* p= new FairContainer("MpdDchGeoPar",
                                          "Dch Geometry Parameters",
                                          "TestDefaultContext");
    p->addContext("TestNonDefaultContext");

    containers->Add(p);
}

FairParSet* MpdDchContFact::createContainer(FairContainer* c) {
  /** Calls the constructor of the corresponding parameter container.
   * For an actual context, which is not an empty string and not the default context
   * of this container, the name is concatinated with the context. */
  const char* name=c->GetName();
  cout << " -I container name " << name << endl;
  FairParSet* p=0;
  if (strcmp(name,"MpdDchGeoPar")==0) {
    p=new MpdDchGeoPar(c->getConcatName().Data(),c->GetTitle(),c->getContext());
  }
  return p;
}

