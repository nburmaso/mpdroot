//
// The MpdMiniArrays holds names, types and initial sizes of the mini arrays
//

// MiniDst headers
#include "MpdMiniArrays.h"

//    ARRAY NAMES
//_________________
const char* MpdMiniArrays::miniArrayNames[NAllMiniArrays] = { "Event",
    "Track",
    "BTofHit",
    "BTofPidTraits",
    "BECalHit",
    "BECalPidTraits",
    "TrackCovMatrix",
    "McEvent",
    "McTrack"};

//   ARRAY TYPES
//_________________
const char* MpdMiniArrays::miniArrayTypes[NAllMiniArrays] = { "MpdMiniEvent",
    "MpdMiniTrack",
    "MpdMiniBTofHit",
    "MpdMiniBTofPidTraits",
    "MpdMiniBECalHit",
    "MpdMiniBECalPidTraits",
    "MpdMiniTrackCovMatrix",
    "MpdMiniMcEvent",
    "MpdMiniMcTrack"};

//              ARRAY SIZES 
// These are intial sizes. Automatically resized if too small.
// Choosing too large initial values gives a performance penalty when reading
// only selected miniDst branches
//_________________
int MpdMiniArrays::miniArraySizes[NAllMiniArrays] = {1, // MpdMiniEvent
    800,   // MpdMiniTrack
    100,   // MpdMiniBTofHit
    100,   // MpdMiniBTofPidTraits
    60000, // MpdMiniBECalHit (size is not yet defined in TDR)
    100,   // MpdMiniBECalPidTraits
    800,   // MpdMiniTrackCovMatrix
    1,     // MpdMiniMcEvent
    1000   // MpdMiniMcTrack
};

//_________________
MpdMiniArrays::MpdMiniArrays() {
  // Default constructor
  /* empty */
}

//_________________
MpdMiniArrays::~MpdMiniArrays() {
  // Destructor
  /* empty */
}
