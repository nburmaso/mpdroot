/**
 * \class MpdFemtoModelFreezeOutGenerator
 * \brief A base class for freeze-out coordinate generator
 *
 * An abstract base class for freeze-out coordinates generator
 */

#ifndef MpdFemtoBaseModelFreezeOutGenerator_h
#define MpdFemtoBaseModelFreezeOutGenerator_h

// ROOT headers
#include "TRandom3.h"

// MpdFemtoMaker headers
#include "MpdFemtoPair.h"

//_________________
class MpdFemtoBaseModelFreezeOutGenerator {

 public:
  /// Default constructor
  MpdFemtoBaseModelFreezeOutGenerator();
  /// Copy constructor
  MpdFemtoBaseModelFreezeOutGenerator(const MpdFemtoBaseModelFreezeOutGenerator &aModel);
  /// Assignment operator
  MpdFemtoBaseModelFreezeOutGenerator& operator=(const MpdFemtoBaseModelFreezeOutGenerator& aGen);
  /// Destructor
  virtual ~MpdFemtoBaseModelFreezeOutGenerator();

  /// Generate freeze-out parameters (x,y,z,t)
  virtual void generateFreezeOut(MpdFemtoPair *aPair) = 0;
  /// Clone freeze-out generator
  virtual MpdFemtoBaseModelFreezeOutGenerator* clone() const;

 protected:
  // Randomizer
  TRandom3 *mRandom; //!<!

 private:

#ifdef __ROOT__
  ClassDef(MpdFemtoBaseModelFreezeOutGenerator, 1);
#endif
};

#endif // #define MpdFemtoBaseModelFreezeOutGenerator_h
