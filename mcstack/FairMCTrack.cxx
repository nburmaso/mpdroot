// -------------------------------------------------------------------------
// -----                      FairMCTrack source file                  -----
// -----                  Created 03/08/04  by V. Friese               -----
// -----              adopted for NICA/MPD 29/03/10  (litvin)          -----
// -------------------------------------------------------------------------
#include "FairMCTrack.h"

#include <iostream>

using std::cout;
using std::endl;


// -----   Default constructor   -------------------------------------------
FairMCTrack::FairMCTrack()// {
//   fPdgCode  = fNPoints = 0;
//   fMotherId = -1;
//   fPx = fPy = fPz = 0.;
//   fStartX = fStartY  = fStartZ = fStartT = 0.;
// }
  : TObject(),
    fPdgCode(0),
    fMotherId(-1),
    fPx(0.),
    fPy(0.),
    fPz(0.),
    fStartX(0.),
    fStartY(0.),
    fStartZ(0.),
    fStartT(0.),
    fNPoints(0)
{
}
// -------------------------------------------------------------------------



// -----   Standard constructor   ------------------------------------------
FairMCTrack::FairMCTrack(Int_t pdgCode, Int_t motherId, Double_t px,
			 Double_t py, Double_t pz, Double_t x, Double_t y,
			 Double_t z, Double_t t, Int_t nPoints = 0) 
: TObject() {
  fPdgCode  = pdgCode;
  fMotherId = motherId;
  fPx = px;
  fPy = py;
  fPz = pz;
  fStartX = x;
  fStartY = y;
  fStartZ = z;
  fStartT = t;
  //   if (nPoints >= 0) fNPoints = nPoints;
  //   else              fNPoints = 0;
  fNPoints = nPoints;
}
// -------------------------------------------------------------------------



// -----   Copy constructor   ----------------------------------------------
FairMCTrack::FairMCTrack(const FairMCTrack& track) // { 
//   *this = track;
// }
  : TObject(track),
    fPdgCode(track.fPdgCode),
    fMotherId(track.fMotherId),
    fPx(track.fPx),
    fPy(track.fPy),
    fPz(track.fPz),
    fStartX(track.fStartX),
    fStartY(track.fStartY),
    fStartZ(track.fStartZ),
    fStartT(track.fStartT),
    fNPoints(track.fNPoints)
{
}
// -------------------------------------------------------------------------



// -----   Constructor from TParticle   ------------------------------------
FairMCTrack::FairMCTrack(TParticle* part) 
: TObject(){
  fPdgCode  = part->GetPdgCode();
  fMotherId = part->GetMother(0);
  fPx       = part->Px();
  fPy       = part->Py();
  fPz       = part->Pz();
  fStartX   = part->Vx();
  fStartY   = part->Vy();
  fStartZ   = part->Vz();
  fStartT   = part->T()*1e09;
  fNPoints  = 0;
}
// -------------------------------------------------------------------------


  
// -----   Destructor   ----------------------------------------------------
FairMCTrack::~FairMCTrack() { }
// -------------------------------------------------------------------------



// -----   Public method Print   -------------------------------------------
void FairMCTrack::Print(Int_t trackId) const {
  cout << "Track " << trackId << ", mother : " << fMotherId << ", Type "
       << fPdgCode << ", momentum (" << fPx << ", " << fPy << ", " << fPz
       << ") GeV" << endl;
  cout << "       STS " << GetNPoints(kSTS) << ", TPC " << GetNPoints(kTPC)
       << ", TOF " << GetNPoints(kTOF) << ", ETOF " << GetNPoints(kETOF)  
       << ", FFD " << GetNPoints(kFFD)  << ", ECT " << GetNPoints(kECT) 
       << ", ECAL " << GetNPoints(kECAL)   << ", NDET " << GetNPoints(kNDET) 
       << ", CPC " << GetNPoints(kCPC) << ", BBC " << GetNPoints(kBBC) 
       << ", ZDC " << GetNPoints(kZDC) << ", FSA " << GetNPoints(kFSA)
       << ", BMD " << GetNPoints(kBMD) << endl;
}
// -------------------------------------------------------------------------



// -----   Public method GetMass   -----------------------------------------
Double_t FairMCTrack::GetMass() const {
  if ( TDatabasePDG::Instance() ) {
    TParticlePDG* particle = TDatabasePDG::Instance()->GetParticle(fPdgCode);
    if ( particle ) return particle->Mass();
    else return 0.;
  }
  return 0.;
}
// -------------------------------------------------------------------------




// -----   Public method GetRapidity   -------------------------------------
Double_t FairMCTrack::GetRapidity() const {
  Double_t e = GetEnergy();
  Double_t y = 0.5 * TMath::Log( (e+fPz) / (e-fPz) );
  return y;
}
// -------------------------------------------------------------------------




// -----   Public method GetNPoints   --------------------------------------
Int_t FairMCTrack::GetNPoints(DetectorId detId) const {

  //kSTS, kTPC, kTOF, kETOF, kFD, kECT, kECAL, kNDET, kCPC, kBBC, kZDC, kFSA, kBMD

  if      (( detId <= kBMD  ) && ( detId >= kSTS  ))
    return (  fNPoints &   (1 << detId));
  else {
    cout << "-E- FairMCTrack::GetNPoints: Unknown detector ID "
	 << detId << endl;
    return 0;
  }
}
// -------------------------------------------------------------------------



// -----   Public method SetNPoints   --------------------------------------
void FairMCTrack::SetNPoints(Int_t iDet, Int_t nPoints) {

  //kSTS, kTPC, kTOF, kETOF, kFD, kECT, kECAL, kNDET, kCPC, kBBC, kZDC, kFSA, kBMD

  Int_t mpd_nPoints = (nPoints>0)*(1 << iDet);

  if (( iDet <= kBMD  ) && ( iDet >= kSTS  )) {
    fNPoints = ( fNPoints & ( ~ (1 << iDet) ) )  |  mpd_nPoints;
  }
  else cout << "-E- FairMCTrack::SetNPoints: Unknown detector ID "
	    << iDet << endl;

}
// -------------------------------------------------------------------------












ClassImp(FairMCTrack)
