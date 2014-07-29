#ifndef MPDSFTCONTFACT_H
#define MPDSFTCONTFACT_H

#include "FairContFact.h"

class FairContainer;

class MpdSftContFact : public FairContFact {
private:
  void setAllContainers();
public:
  MpdSftContFact();
  ~MpdSftContFact() {}
  FairParSet* createContainer(FairContainer*);
  ClassDef( MpdSftContFact,0) // Factory for all SFT parameter containers
};

#endif  /* !MPDSFTCONTFACT_H */
