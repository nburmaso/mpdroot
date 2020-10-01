/*
 * MpdHbtEvent.h
 *
 *  Created on: 1 paź 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef NICAMPDHBTEVENT_H_
#define NICAMPDHBTEVENT_H_

#include "NicaExpEvent.h"
class NicaPackage;
class NicaMpdHbtEvent : public NicaExpEvent {
  Double_t fR;

 public:
  NicaMpdHbtEvent();
  NicaMpdHbtEvent(const NicaMpdHbtEvent &other);
  NicaMpdHbtEvent &operator=(const NicaMpdHbtEvent &other);
  virtual void CreateSource();
  // set optimal radius
  void SetR(Double_t R) { fR = R; };
  // get optimal value of radius
  void GetR() const { return fR; };
  virtual Bool_t AreCompatible(const NicaEvent *buffered) const;
  virtual NicaPackage *Report() const;
  virtual ~NicaMpdHbtEvent();
  ClassDef(NicaMpdHbtEvent, 1)
};

#endif /* NICAMPDHBTEVENT_H_ */
