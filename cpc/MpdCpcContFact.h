#ifndef MPDCPCCONTFACT_H
#define MPDCPCCONTFACT_H

#include "FairContFact.h"

class FairContainer;

class MpdCpcContFact : public FairContFact {
private:
  void setAllContainers();
public:
  MpdCpcContFact();
  ~MpdCpcContFact() {}
  FairParSet* createContainer(FairContainer*);
  ClassDef( MpdCpcContFact,0) // Factory for all CPC parameter containers
};

#endif  /* !MPDCPCCONTFACT_H */
