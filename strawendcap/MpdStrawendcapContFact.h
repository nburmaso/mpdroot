#ifndef MPDStrawendcapCONTFACT_H
#define MPDStrawendcapCONTFACT_H

#include "FairContFact.h"

class FairContainer;

class MpdStrawendcapContFact : public FairContFact {
private:
  void setAllContainers();
public:
  MpdStrawendcapContFact();
  ~MpdStrawendcapContFact() {}
  FairParSet* createContainer(FairContainer*);
  ClassDef( MpdStrawendcapContFact,0) // Factory for all Strawendcap parameter containers
};

#endif  /* !MPDStrawendcapCONTFACT_H */
