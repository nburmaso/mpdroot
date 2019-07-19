/**
 * \class MpdMiniBECalHit
 * \brief Holds information about ECal tower
 *
 * The class holds information about the tower from
 * the Barrel Electromagnetic Calorimeter (ECal)
 *
 * \author Grigory Nigmatkulov (NRNU MEPhI)
 * \email nigmatkulov@gmail.com ; ganigmatkulov@mephi.ru
 * \date July 11, 2019
 */

#ifndef MpdMiniBECalHit_h
#define MpdMiniBECalHit_h

// C++ headers
#include <limits>

// ROOT headers
#include "TObject.h"

//_________________
class MpdMiniBECalHit : public TObject {

 public:
  /// Default constructor
  MpdMiniBECalHit();
  /// Constructor that takes ADC and energy
  MpdMiniBECalHit(Int_t adc, Float_t e);
  /// Copy constructor
  MpdMiniBECalHit(const MpdMiniBECalHit &hit);
  /// Destructor
  virtual ~MpdMiniBECalHit();
  /// Print tower information
  virtual void Print(const Char_t* option = "") const;

  //
  // Getters
  //
  
  /// Return ADC of the tower
  Int_t   adc() const     { return (Int_t)mAdc; }
  /// Return energy of the tower
  Float_t energy() const  { return (Float_t)mE / 1000.f; }
  /// Return if the tower is bad
  Bool_t  isBad() const;
  /// Return softId
  Int_t   numericIndex2SoftId(Int_t idx) const { return (idx+1); }

  //
  // Setters
  //
  
  /// Set tower ADC
  void setAdc(Int_t adc);
  /// Set tower energy
  void setEnergy(Float_t energy);

 protected:

  /// ADC
  UShort_t mAdc;
  /// Energy * 1000
  Short_t mE;

  ClassDef(MpdMiniBECalHit, 1)
};

#endif // #define MpdMiniBECalHit_h
