/**
 * \class MpdMiniBTofHit
 * \brief Stores BTOF hit information
 *
 * The MpdMiniBTofHit class holds inofmation about hits in Barrel Time-Of-Flight detector
 *
 * \author Grigory Nigmatkulov
 * \email nigmatkulov@gmail.com ; ganigmatkulov@mephi.ru
 * \date July 11, 2019
 */

#ifndef MpdMiniBTofHit_h
#define MpdMiniBTofHit_h

// C++ headers
#include <limits>

// ROOT headers
#include "TObject.h"

//_________________
class MpdMiniBTofHit : public TObject {

 public:
  /// Default consturctor
  MpdMiniBTofHit();
  /// Constructor that takes id
  MpdMiniBTofHit(int id);
  /// Copy constructor
  MpdMiniBTofHit(const MpdMiniBTofHit &hit);
  /// Destructor
  virtual ~MpdMiniBTofHit();
  /// Print hit information
  virtual void Print(const Char_t* option = "") const;

  //
  // Getters
  //

  /// Return ID of the hit
  Int_t   id() const       { return mId; }
  /// Return sector number
  Int_t   sector() const   { return ((mId & 0xFF000000) >> 24); }
  /// Return box number
  Int_t   box() const      { return ((mId & 0x00FF0000) >> 16); }
  /// Return detector number
  Int_t   detector() const { return ((mId & 0x0000FF00) >> 8); }
  /// Return strip number
  Int_t   strip() const    { return ( mId & 0x000000FF); }

  //
  // Setters
  //

  /// Set ID of the hit
  void setId(Int_t id)
  { if (id<0) { mId = -1; }
    else { mId = (id > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)id; } }
  /// Set ID of the track using sector, box, detector and strip
  void setId(Int_t sector, Int_t box, Int_t detector, Int_t strip);

 private:

  /// Id encoding (255 max):
  /// strip 	[1,...,24],	0x000000FF
  /// detector 	[1,...,20],	0x0000FF00
  /// box	[1,...,1],	0x00FF0000
  /// sector 	[1,...,14],	0xFF000000

  Short_t mId;     

  ClassDef(MpdMiniBTofHit, 1)
};

#endif // #define MpdMiniBTofHit_h
