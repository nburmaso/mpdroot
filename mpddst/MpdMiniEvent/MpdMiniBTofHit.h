/**
 * \class MpdMiniBTofHit
 * \brief Stores BTOF hit information
 *
 * The MpdMiniBTofHit class holds inofmation about hits in Barrel Time-Of-Flight detector
 *
 * \author Grigory Nigmatkulov (NRNU MEPhI), Pavel Batyuk (JINR)
 * \email nigmatkulov@gmail.com ; ganigmatkulov@mephi.ru ; pavel.batyuk@jinr.ru
 * \date May 01, 2020
 */

#ifndef MpdMiniBTofHit_h
#define MpdMiniBTofHit_h

// C++ headers
#include <limits>

// ROOT headers
#include "TObject.h"
#include <TVector3.h>

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
  Int_t id() const {
    return fDetectorID;
  }
  /// Return sector number
  Int_t sector() const {
    return ((fDetectorID & 0xFF000000) >> 24);
  }
  /// Return detector number
  Int_t detector() const {
    return ((fDetectorID & 0x00FF0000) >> 16);
  }
  /// Return strip number
  Int_t strip() const {
    return ( fDetectorID & 0x000000FF);
  }
  // Return gap number
  Int_t gap() const {
    return ((fDetectorID & 0x0000FF00) >> 8);
  }
  /// Return matching flag (false - no match, true - one-to-one)
  Bool_t btofMatchFlag() const {
    return mBTofMatchFlag;
  }
  /// Return hit position
  TVector3 btofHitPos() const {
    return TVector3(btofHitPosX(), btofHitPosY(), btofHitPosZ());
  }
  /// Return x comonent of hit position
  Float_t btofHitPosX() const {
    return (Float_t) mBTofHitPosX / 100.;
  }
  /// Return y comonent of hit position
  Float_t btofHitPosY() const {
    return (Float_t) mBTofHitPosY / 100.;
  }
  /// Return z comonent of hit position
  Float_t btofHitPosZ() const {
    return (Float_t) mBTofHitPosZ / 100.;
  }
  /// Return time of flight
  Float_t btof() const {
    return mBTof;
  }

  //
  // Setters
  //

  /// Set ID of the hit
  void setId(Int_t id) {
    fDetectorID = id;
  }
  /// Set ID of the track using sector, box, detector and strip
  void setId(Int_t sector, Int_t gap, Int_t detector, Int_t strip);
  /// Set TOF-matching flag
  void setBTofMatchFlag(Bool_t flag) {
    mBTofMatchFlag = flag;
  }
  /// Set hit position (x,y,z)
  void setHitPositionXYZ(Float_t x, Float_t y, Float_t z);
  /// Set hit position x (cm)
  void setHitPositionX(Float_t x);
  /// Set hit position y (cm)
  void setHitPositionY(Float_t y);
  /// Set hit position z (cm)
  void setHitPositionZ(Float_t z);

  /// Set time of flight
  void setTOF(Float_t tof) {
    mBTof = tof;
  }

 private:

  // CAUTION:  MAX_VALUE = 255(0xFF)
  // MpdTof --------------------------------
  // sector 	[1,...,14],	0xFF000000
  // detector	[1,...,20],	0x00FF0000
  // gap	[1,...,3],	0x0000FF00
  // strip 	[1,...,24],	0x000000FF

  // Put here just for information!
  // MpdEtof -------------------------------- 
  // strip 	[1,...,131],??	0x000000FF 
  // detector 	[1,...,1],??	0x0000FF00 	
  // box		[1,...,30],??	0x00FF0000
  // sector 	[1,...,2],??	0xFF000000

  // Det ID (see above how to get det. elements)
  Int_t fDetectorID;
  /// false - no match, true - one-to-one
  Bool_t mBTofMatchFlag;
  /// Hit position projected on X plane (compression = position * 100)
  Short_t mBTofHitPosX;
  /// Hit position projected on Y plane (compression = position * 100)
  Short_t mBTofHitPosY;
  /// Hit position projected on Z plane (compression = position * 100)
  Short_t mBTofHitPosZ;
  /// Time-Of-Flight
  Float_t mBTof;

  ClassDef(MpdMiniBTofHit, 1)
};

#endif // #define MpdMiniBTofHit_h
