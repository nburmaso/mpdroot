#ifndef MPDTgemCONTFACT_H
#define MPDTgemCONTFACT_H

#include "FairContFact.h"

class FairContainer;

class MpdTgemContFact : public FairContFact {
private:
  void setAllContainers();
public:
  MpdTgemContFact();
  ~MpdTgemContFact() {}
  FairParSet* createContainer(FairContainer*);
  ClassDef( MpdTgemContFact,0) // Factory for all Tgem parameter containers
};

#endif  /* !MPDTgemCONTFACT_H */
