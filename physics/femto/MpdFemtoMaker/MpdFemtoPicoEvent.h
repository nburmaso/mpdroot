/**
 * \class MpdFemtoPicoEvent
 * \brief Stores collection of particles for processing
 *
 * MpdFemtoPicoEvent stores collections of particles for the further processing
 */

#ifndef MpdFemtoPicoEvent_h
#define MpdFemtoPicoEvent_h

// MpdFemtoMaker headers
#include "MpdFemtoParticleCollection.h"

//_________________

class MpdFemtoPicoEvent {
public:
    /// Default constructor
    MpdFemtoPicoEvent();
    /// Copy constructor
    MpdFemtoPicoEvent(const MpdFemtoPicoEvent& copy);
    /// Copy constructor
    MpdFemtoPicoEvent& operator=(const MpdFemtoPicoEvent& copy);
    /// Default destructor
    ~MpdFemtoPicoEvent();

    //
    // Getters
    //

    /// First particle collection

    MpdFemtoParticleCollection* firstParticleCollection() {
        return mFirstParticleCollection;
    }
    /// Second particle collection

    MpdFemtoParticleCollection* secondParticleCollection() {
        return mSecondParticleCollection;
    }
    /// Third particle collection

    MpdFemtoParticleCollection* thirdParticleCollection() {
        return mThirdParticleCollection;
    }

private:

    /// First particle collection
    MpdFemtoParticleCollection* mFirstParticleCollection;
    /// First particle collection
    MpdFemtoParticleCollection* mSecondParticleCollection;
    /// First particle collection
    MpdFemtoParticleCollection* mThirdParticleCollection;

    ClassDef(MpdFemtoPicoEvent, 0)
};

#endif // #define MpdFemtoPicoEvent_h
