/////////////////////////////////////////////////////////////
//
//  MpdStrawECTContFact
//
//  Factory for the parameter containers in libStrawECT
//
/////////////////////////////////////////////////////////////

using namespace std;
#include "MpdStrawECTContFact.h"
#include "FairRuntimeDb.h"
#include "MpdStrawECTGeoPar.h"
#include <iostream>
#include <iomanip>

ClassImp(MpdStrawECTContFact)

static MpdStrawECTContFact gMpdStrawECTContFact;

MpdStrawECTContFact::MpdStrawECTContFact() {
  // Constructor (called when the library is loaded)
  fName="MpdStrawECTContFact";
  fTitle="Factory for parameter containers in libStrawECT";
  setAllContainers();
  FairRuntimeDb::instance()->addContFactory(this);
}

void MpdStrawECTContFact::setAllContainers() {
  /** Creates the Container objects with all accepted contexts and adds them to
   *  the list of containers for the Tof library.*/

    FairContainer* p= new FairContainer("MpdStrawECTGeoPar",
                                          "StrawECT Geometry Parameters",
                                          "TestDefaultContext");
    p->addContext("TestNonDefaultContext");

    containers->Add(p);
}

FairParSet* MpdStrawECTContFact::createContainer(FairContainer* c) {
  /** Calls the constructor of the corresponding parameter container.
   * For an actual context, which is not an empty string and not the default context
   * of this container, the name is concatinated with the context. */
  const char* name=c->GetName();
  cout << " -I container name " << name << endl;
  FairParSet* p=0;
  if (strcmp(name,"MpdStrawECTGeoPar")==0) {
    p=new MpdStrawECTGeoPar(c->getConcatName().Data(),c->GetTitle(),c->getContext());
  }
  return p;
}

