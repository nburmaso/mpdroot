#ifndef MPDStrawECTCONTFACT_H
#define MPDStrawECTCONTFACT_H

#include "FairContFact.h"

class FairContainer;

class MpdStrawECTContFact : public FairContFact {
private:
  void setAllContainers();
public:
  MpdStrawECTContFact();
  ~MpdStrawECTContFact() {}
  FairParSet* createContainer(FairContainer*);
  ClassDef( MpdStrawECTContFact,0) // Factory for all StrawECT parameter containers
};

#endif  /* !MPDStrawECTCONTFACT_H */
