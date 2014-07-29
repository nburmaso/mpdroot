/////////////////////////////////////////////////////////////
//
//  MpdFsaContFact
//
//  Factory for the parameter containers in libFsa
//
/////////////////////////////////////////////////////////////

using namespace std;
#include "MpdFsaContFact.h"
#include "FairRuntimeDb.h"
#include "MpdFsaGeoPar.h"
#include <iostream>
#include <iomanip>

ClassImp(MpdFsaContFact)

static MpdFsaContFact gMpdFsaContFact;

MpdFsaContFact::MpdFsaContFact() {
  // Constructor (called when the library is loaded)
  fName="MpdFsaContFact";
  fTitle="Factory for parameter containers in libFsa";
  setAllContainers();
  FairRuntimeDb::instance()->addContFactory(this);
}

void MpdFsaContFact::setAllContainers() {
  /** Creates the Container objects with all accepted contexts and adds them to
   *  the list of containers for the Tof library.*/

    FairContainer* p= new FairContainer("MpdFsaGeoPar",
                                          "Fsa Geometry Parameters",
                                          "TestDefaultContext");
    p->addContext("TestNonDefaultContext");

    containers->Add(p);
}

FairParSet* MpdFsaContFact::createContainer(FairContainer* c) {
  /** Calls the constructor of the corresponding parameter container.
   * For an actual context, which is not an empty string and not the default context
   * of this container, the name is concatinated with the context. */
  const char* name=c->GetName();
  cout << " -I container name " << name << endl;
  FairParSet* p=0;
  if (strcmp(name,"MpdFsaGeoPar")==0) {
    p=new MpdFsaGeoPar(c->getConcatName().Data(),c->GetTitle(),c->getContext());
  }
  return p;
}

