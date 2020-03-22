// -------------------------------------------------------------------------
// -----                     MpdbmdContFact header file                -----
// -------------------------------------------------------------------------

#ifndef MPDBMDCONTFACT_H
#define MPDBMDCONTFACT_H

#include "FairContFact.h"

class FairContainer;

class BmdContFact : public FairContFact {
private:
  void setAllContainers();

public:
  BmdContFact();
  ~BmdContFact() {}
  FairParSet *createContainer(FairContainer *);

  ClassDef(BmdContFact, 0) // Factory for all HYP parameter containers
};

#endif /* !MPDBMDCONTFACT_H */
