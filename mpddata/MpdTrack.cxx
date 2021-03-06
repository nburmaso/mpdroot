// Author: Oleg Rogachevsky
// Update: 2009-09-17 18:43:28+0400
// Copyright: 2009 (C) MPD coll.
//
// Track container 

#ifndef ROOT_MpdTrack
#include "MpdTrack.h"
#endif

#include <TMath.h>

// -----   Default constructor ---------------------------------------
MpdTrack::MpdTrack():
  TObject(), fPidProbElectron(0.), fPidProbPion(0.),fPidProbKaon(0.), fPidProbProton(0.), 
  fPidTPCProbElectron(0.), fPidTPCProbPion(0.),fPidTPCProbKaon(0.), fPidTPCProbProton(0.), 
  fPidTOFProbElectron(0.), fPidTOFProbPion(0.),fPidTOFProbKaon(0.), fPidTOFProbProton(0.),      
  fTofBeta(0.), fTofMass2(0.), fdEdXTPC(0.), fTofFlag(0)  
{}
// -------------------------------------------------------------------
Float_t MpdTrack::GetPx()
{
  return TMath::Abs(fPt)*TMath::Cos(fPhi); 
}
Float_t MpdTrack::GetPy()
{
  return TMath::Abs(fPt)*TMath::Sin(fPhi);
}
Float_t MpdTrack::GetPz()
{
  if ( TMath::Sin(fTheta) == 0.) return TMath::Sqrt(-1); // NaN
  return TMath::Abs(fPt)/TMath::Tan(fTheta);
}
Float_t MpdTrack::GetEta()
{
  return -TMath::Log(TMath::Tan(0.5*fTheta));
}

// -------------------------------------------------------------------
ClassImp(MpdTrack);
