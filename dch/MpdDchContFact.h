#ifndef MPDDCHCONTFACT_H
#define MPDDCHCONTFACT_H

#include "FairContFact.h"

class FairContainer;

class MpdDchContFact : public FairContFact {
private:
  void setAllContainers();
public:
  MpdDchContFact();
  ~MpdDchContFact() {}
  FairParSet* createContainer(FairContainer*);
  ClassDef( MpdDchContFact,0) // Factory for all Dch parameter containers
};

#endif  /* !MPDDCHCONTFACT_H */
