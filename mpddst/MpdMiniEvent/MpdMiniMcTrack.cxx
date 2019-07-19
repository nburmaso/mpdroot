//
// MpdMiniMcTrack holds information about Monte Carlo particle
//

// MpdMiniDst headers
#include "MpdMiniMessMgr.h"
#include "MpdMiniMcTrack.h"

ClassImp(MpdMiniMcTrack)

//_________________
MpdMiniMcTrack::MpdMiniMcTrack() : TObject(),
  fId(0), fStatus(0), fPdgId(0), fParentIndex(0), fChild{},
  fPx(0), fPy(0), fPz(0), fEnergy(0),
  fX(0), fY(0), fZ(0), fT(0) {
  // Default constructor
  /* empty */
}

//_________________
MpdMiniMcTrack::MpdMiniMcTrack(const MpdMiniMcTrack& copy) : TObject() {
  // Copy constructor
  fId = copy.fId;
  fStatus = copy.fStatus;
  fPdgId = copy.fPdgId;
  fParentIndex = copy.fParentIndex;
  fChild[0] = copy.fChild[0];
  fChild[1] = copy.fChild[1];
  fPx = copy.fPx;
  fPy = copy.fPy;
  fPz = copy.fPz;
  fEnergy = copy.fEnergy;
  fX = copy.fX;
  fY = copy.fY;
  fZ = copy.fZ;
  fT = copy.fT;
}

//_________________
MpdMiniMcTrack::~MpdMiniMcTrack() {
  // Destructor
  /* empty */
}

//_________________
void MpdMiniMcTrack::Print(const Char_t* option __attribute__((unused)) ) const {
  LOG_INFO << "id: " << fId << " status: " << fStatus << " PDG code: " << fPdgId
	   << " 1st child idx: " << fChild[0] << " 2nd child idx: " << fChild[1] << "\n"
	   << "fourMomentum (px,py,pz,e): (" << fPx << "," << fPy << "," << fPz << "," << fEnergy << ")\n"
	   << "fourCoordinate (x,y,z,t): (" << fX << "," << fY << "," << fZ << "," << fT << ")\n"
	   << endm;
}
