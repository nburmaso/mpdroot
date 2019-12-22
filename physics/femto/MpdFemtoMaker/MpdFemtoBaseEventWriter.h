/**
 * \class MpdFemtoBaseEventWriter
 * \brief Typedef for the MpdFemtoBaseEventReader that makes it a Writer
 *
 * The typedef for the MpdFemtoBaseEventReader that makes it a Writer
 */

#ifndef MpdFemtoBaseEventWriter_h
#define MpdFemtoBaseEventWriter_h

// MpdFemtoMaker headers
#include "MpdFemtoBaseEventReader.h"

// The writer is reader-writer
typedef MpdFemtoBaseEventReader MpdFemtoBaseEventWriter; //!

#endif // #define MpdFemtoBaseEventWriter_h
