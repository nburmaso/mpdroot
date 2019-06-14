/**
 * \class MpdFemtoEnumeration
 * \brief Enumerations for MpdFemtoMaker
 *
 * The file keeps enumarations for particle types and I/O mode
 */

#ifndef MpdFemtoEnumeration_h
#define MpdFemtoEnumeration_h

/// Particle types

enum MpdFemtoParticleType {
    hbtUndefined, hbtTrack, hbtV0, hbtKink, hbtXi
};
/// I/O mode

enum MpdFemtoIOMode {
    hbtRead, hbtWrite
};

#endif //#define MpdFemtoEnumeration_h
