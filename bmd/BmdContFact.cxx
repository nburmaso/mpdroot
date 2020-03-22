/*************************************************************************************
 *
 *         Class BmdContFact
 *
 *  Adopted for MPD by:   RO
 *  e-mail:   rogachevsky@jinr.ru
 *  Version:  8-Apr-2008
 *
 ************************************************************************************/

using namespace std;
#include "BmdContFact.h"
#include "BmdGeoPar.h"
#include "FairParAsciiFileIo.h"
#include "FairParRootFileIo.h"
#include "FairRuntimeDb.h"
#include <iomanip>
#include <iostream>

ClassImp(BmdContFact)

    static BmdContFact gBmdContFact;

BmdContFact::BmdContFact() {
  // Constructor (called when the library is loaded)
  fName = "BmdContFact";
  fTitle = "Factory for parameter containers in libBmd";
  setAllContainers();
  FairRuntimeDb::instance()->addContFactory(this);
}

void BmdContFact::setAllContainers() {
  /** Creates the Container objects with all accepted contexts and adds them to
   *  the list of containers for the BMD library.*/

  FairContainer *p = new FairContainer("BmdGeoPar", "Bmd Geometry Parameters",
                                       "BmdDefaultContext");
  p->addContext("BmdNonDefaultContext");

  containers->Add(p);

  //    p->print();
}

FairParSet *BmdContFact::createContainer(FairContainer *c) {
  /** Calls the constructor of the corresponding parameter container.
   * For an actual context, which is not an empty string and not the default
   * context of this container, the name is concatinated with the context. */
  const char *name = c->GetName();
  FairParSet *p = NULL;
  if (strcmp(name, "BmdGeoPar") == 0) {
    p = new BmdGeoPar(c->getConcatName().Data(), c->GetTitle(),
                      c->getContext());
  }
  return p;
}
