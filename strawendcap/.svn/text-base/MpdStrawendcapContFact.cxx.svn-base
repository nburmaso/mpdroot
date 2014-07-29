/////////////////////////////////////////////////////////////
//
//  MpdStrawendcapContFact
//
//  Factory for the parameter containers in libStrawendcap
//
/////////////////////////////////////////////////////////////

using namespace std;
#include "MpdStrawendcapContFact.h"
#include "FairRuntimeDb.h"
#include "MpdStrawendcapGeoPar.h"
#include <iostream>
#include <iomanip>

ClassImp(MpdStrawendcapContFact)

static MpdStrawendcapContFact gMpdStrawendcapContFact;

MpdStrawendcapContFact::MpdStrawendcapContFact() {
  // Constructor (called when the library is loaded)
  fName="MpdStrawendcapContFact";
  fTitle="Factory for parameter containers in libStrawendcap";
  setAllContainers();
  FairRuntimeDb::instance()->addContFactory(this);
}

void MpdStrawendcapContFact::setAllContainers() {
  /** Creates the Container objects with all accepted contexts and adds them to
   *  the list of containers for the Tof library.*/

    FairContainer* p= new FairContainer("MpdStrawendcapGeoPar",
                                          "Strawendcap Geometry Parameters",
                                          "TestDefaultContext");
    p->addContext("TestNonDefaultContext");

    containers->Add(p);
}

FairParSet* MpdStrawendcapContFact::createContainer(FairContainer* c) {
  /** Calls the constructor of the corresponding parameter container.
   * For an actual context, which is not an empty string and not the default context
   * of this container, the name is concatinated with the context. */
  const char* name=c->GetName();
  cout << " -I container name " << name << endl;
  FairParSet* p=0;
  if (strcmp(name,"MpdStrawendcapGeoPar")==0) {
    p=new MpdStrawendcapGeoPar(c->getConcatName().Data(),c->GetTitle(),c->getContext());
  }
  return p;
}

