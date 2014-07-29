/////////////////////////////////////////////////////////////
//
//  MpdCpcContFact
//
//  Factory for the parameter containers in libCpc
//
/////////////////////////////////////////////////////////////

using namespace std;
#include "MpdCpcContFact.h"
#include "FairRuntimeDb.h"
#include "MpdCpcGeoPar.h"
#include <iostream>
#include <iomanip>

ClassImp(MpdCpcContFact)

static MpdCpcContFact gMpdCpcContFact;

MpdCpcContFact::MpdCpcContFact() {
  // Constructor (called when the library is loaded)
  fName="MpdCpcContFact";
  fTitle="Factory for parameter containers in libCpc";
  setAllContainers();
  FairRuntimeDb::instance()->addContFactory(this);
}

void MpdCpcContFact::setAllContainers() {
  /** Creates the Container objects with all accepted contexts and adds them to
   *  the list of containers for the Tof library.*/

    FairContainer* p= new FairContainer("MpdCpcGeoPar",
                                          "Cpc Geometry Parameters",
                                          "TestDefaultContext");
    p->addContext("TestNonDefaultContext");

    containers->Add(p);
}

FairParSet* MpdCpcContFact::createContainer(FairContainer* c) {
  /** Calls the constructor of the corresponding parameter container.
   * For an actual context, which is not an empty string and not the default context
   * of this container, the name is concatinated with the context. */
  const char* name=c->GetName();
  cout << " -I container name " << name << endl;
  FairParSet* p=0;
  if (strcmp(name,"MpdCpcGeoPar")==0) {
    p=new MpdCpcGeoPar(c->getConcatName().Data(),c->GetTitle(),c->getContext());
  }
  return p;
}

