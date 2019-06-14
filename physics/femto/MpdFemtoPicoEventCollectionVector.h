/**
 * \class MpdFemtoPicoEventCollectionVector
 * \brief Holds collection of PicoEvents for vertex-dependent analysis
 *
 * Holds collection (STL vector) and analysis for vertex-dependent event mixing.
 */

#ifndef MpdFemtoPicoEventCollectionVector_h
#define MpdFemtoPicoEventCollectionVector_h

// C++ headers
#include <vector>
#include <list>

// MpdFemtoMaker header
#include "MpdFemtoPicoEventCollection.h"

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef std::vector<MpdFemtoPicoEventCollection*, std::allocator<MpdFemtoPicoEventCollection*> > MpdFemtoPicoEventCollectionVector; //!
typedef std::vector<MpdFemtoPicoEventCollection*, std::allocator<MpdFemtoPicoEventCollection*> >::iterator MpdFemtoPicoEventCollectionIterator; //!
#else
typedef std::vector<MpdFemtoPicoEventCollection*> MpdFemtoPicoEventCollectionVector; //!
typedef std::vector<MpdFemtoPicoEventCollection*>::iterator MpdFemtoPicoEventCollectionIterator; //!
#endif

#endif // #define MpdFemtoPicoEventCollectionVector_h
