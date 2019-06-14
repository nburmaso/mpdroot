//
// The class holds DST structure and access methods
//

// C++ headers
#include <iostream>

// McDst headers
#include "MpdMcEvent.h"
#include "MpdMcParticle.h"
#include "MpdMcDst.h"       // MUST be the last one

#ifdef __ROOT__
ClassImp(McDst)
#endif

TClonesArray** MpdMcDst::mcArrays = nullptr;

//_________________
MpdMcDst::~MpdMcDst() {
  /* empty */
}

//________________
void MpdMcDst::unset() {
  // Unset pointers
  mcArrays = nullptr;
}

//________________
void MpdMcDst::set(TClonesArray** theMcArrays) {
  // Set pointers
  mcArrays = theMcArrays;
}

//________________
void MpdMcDst::print() {
  // Print all information
  std::cout << "\n==================== Full event information ====================\n";
  printEventInfo();
  printParticles();
  std::cout << "\n================================================================\n";
}

//________________
void MpdMcDst::printEventInfo() {
  // Print event information
  event()->print();
}

//________________
void MpdMcDst::printParticles() {
  // Print all particles
  if( numberOfParticles() == 0 ) {
    std::cout << "No particles found!" << std::endl;
    return;
  }

  std::cout << "\n Particle list contains: " << numberOfParticles() << " entries\n\n";
  // Particle loop
  for(UInt_t iPart=0; iPart<numberOfParticles(); iPart++) {
    std::cout << "+++ particles #[" << iPart << "/" << numberOfParticles << "]\n";
    particle(iPart)->print();
  }

  std::cout << std::endl;
}
