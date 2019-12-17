/**
 * \class MpdFemtoPicoEventCollection
 * \brief A collection of hbt pico events
 *
 * A collection of PicoEvents is what makes up the EventMixingBuffer
 * of each Analysis
 */

#ifndef MpdFemtoPicoEventCollection_h
#define MpdFemtoPicoEventCollection_h

// C++ headers
#include <list>

// MpdFemtoMaker headers
class MpdFemtoPicoEvent;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef std::list<MpdFemtoPicoEvent*, std::allocator<MpdFemtoPicoEvent*> > MpdFemtoPicoEventCollection;
typedef std::list<MpdFemtoPicoEvent*, std::allocator<MpdFemtoPicoEvent*> >::iterator MpdFemtoPicoEventIterator;
#else
typedef std::list<MpdFemtoPicoEvent*> MpdFemtoPicoEventCollection;
typedef std::list<MpdFemtoPicoEvent*>::iterator MpdFemtoPicoEventIterator;
#endif

#endif // #define MpdFemtoPicoEventCollection_h
