/////////////////////////////////////////////////////////////
//
//  MpdTgemContFact
//
//  Factory for the parameter containers in libTgem
//
/////////////////////////////////////////////////////////////

using namespace std;
#include "MpdTgemContFact.h"
#include "FairRuntimeDb.h"
#include "MpdTgemGeoPar.h"
#include <iostream>
#include <iomanip>

ClassImp(MpdTgemContFact)

static MpdTgemContFact gMpdTgemContFact;

MpdTgemContFact::MpdTgemContFact() {
  // Constructor (called when the library is loaded)
  fName="MpdTgemContFact";
  fTitle="Factory for parameter containers in libTgem";
  setAllContainers();
  FairRuntimeDb::instance()->addContFactory(this);
}

void MpdTgemContFact::setAllContainers() {
  /** Creates the Container objects with all accepted contexts and adds them to
   *  the list of containers for the Tof library.*/

    FairContainer* p= new FairContainer("MpdTgemGeoPar",
                                          "Tgem Geometry Parameters",
                                          "TestDefaultContext");
    p->addContext("TestNonDefaultContext");

    containers->Add(p);
}

FairParSet* MpdTgemContFact::createContainer(FairContainer* c) {
  /** Calls the constructor of the corresponding parameter container.
   * For an actual context, which is not an empty string and not the default context
   * of this container, the name is concatinated with the context. */
  const char* name=c->GetName();
  cout << " -I container name " << name << endl;
  FairParSet* p=0;
  if (strcmp(name,"MpdTgemGeoPar")==0) {
    p=new MpdTgemGeoPar(c->getConcatName().Data(),c->GetTitle(),c->getContext());
  }
  return p;
}

