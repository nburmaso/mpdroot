#ifndef CBMSTTCONTFACT_H
#define CBMSTTCONTFACT_H

#include "FairContFact.h"

class FairContainer;

class CbmSttContFact : public FairContFact
{
private:
  void setAllContainers();
public:
  CbmSttContFact();
  ~CbmSttContFact() {}
  FairParSet* createContainer(FairContainer*);
//  void  activateParIo(FairParIo* io);
  ClassDef( CbmSttContFact,0) // Factory for all STT parameter containers
};

#endif  /* !CBMSTTCONTFACT_H */
