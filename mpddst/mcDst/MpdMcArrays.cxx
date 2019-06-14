//
// The class holds array names and their sizes
//

#include "MpdMcArrays.h"

//________________
MpdMcArrays::MpdMcArrays() {
  /* emtpy */
}

//________________
MpdMcArrays::~MpdMcArrays() {
  /* emtpy */
}

// Array names
//________________
const char* MpdMcArrays::mcArrayNames[NAllMpdMcArrays] = { "Event", "Particle" };

// Array types
//________________
const char* MpdMcArrays::mcArrayTypes[NAllMpdMcArrays] = { "MpdMcEvent", "MpdMcParticle" };

// Array sizes
// These are intial sizes. Automatically resized if too small.
// Choosing too large initial values gives a performance penalty when reading
// only selected UDst branches
//_________________
int MpdMcArrays::mcArraySizes[NAllMpdMcArrays] = { 1, 1000 };
