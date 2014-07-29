#ifndef MPDFSACONTFACT_H
#define MPDFSACONTFACT_H

#include "FairContFact.h"

class FairContainer;

class MpdFsaContFact : public FairContFact {
private:
  void setAllContainers();
public:
  MpdFsaContFact();
  ~MpdFsaContFact() {}
  FairParSet* createContainer(FairContainer*);
  ClassDef( MpdFsaContFact,0) // Factory for all FSA parameter containers
};

#endif  /* !MPDFSACONTFACT_H */
