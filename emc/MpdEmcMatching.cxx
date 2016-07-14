//--------------------------------------------------------------------
//
// Description:
//      MPD TPC-EMC Matching
//
//
// Author List:
//      Alexander Zinchenko LHEP, JINR, Dubna - 1-June-2016
//
//--------------------------------------------------------------------

#include "MpdEmcMatching.h"
#include "MpdEmcGeoPar.h"
//#include "MpdEmcDigit.h"
#include "MpdEmcMatch.h"
#include "MpdEmcPoint.h"
#include "MpdKalmanFilter.h"
#include "MpdTpcHit.h"
#include "MpdTpcKalmanTrack.h"

#include "FairMCPoint.h"
#include "FairMCTrack.h"
#include "FairRootManager.h"
#include "FairRunAna.h"

#include <TClonesArray.h>
#include <TGeoManager.h>
#include <TMath.h>

using namespace std;
using namespace TMath;

//FILE *lun = fopen ("file.txt","w"); //AZ

// -----   Default constructor   -------------------------------------------

MpdEmcMatching::MpdEmcMatching() :
FairTask("TPC-EMC matching") {

}

// -----   Destructor   ----------------------------------------------------

MpdEmcMatching::~MpdEmcMatching() {
}
// -------------------------------------------------------------------------

// -----   Public method Init   --------------------------------------------

InitStatus MpdEmcMatching::Init() {

    cout << "******************* EMC Matching INIT *********************" << endl;

    // Get RootManager
    FairRootManager* ioman = FairRootManager::Instance();
    if (!ioman) {
        cout << "-E- MpdEmcMatching::Init: "
                << "RootManager not instantiated!" << endl;
        return kFATAL;
    }

    fGeoPar = new MpdEmcGeoPar();

    // Containers for rec. points from all+2 sectors
    Int_t nSec = fGeoPar->GetNsec();
    multimap<Double_t,Int_t> aaa;
    fRecPoints.assign(nSec+2,aaa);

    // Get input array
    fPointArray = (TClonesArray*) ioman->GetObject("EmcPoint");
    if (!fPointArray) {
        cout << "-W- MpdEmcMatching::Init: " << "No EmcPoint array!" << endl;
        return kERROR;
    }
    
    fMcTrackArray = (TClonesArray*) ioman->GetObject("MCTrack");
    if (!fMcTrackArray) {
        cout << "-W- MpdEmcMatching::Init: " << "No MCTrack array!" << endl;
        return kERROR;
    }

    fRecPointArray = (TClonesArray*) ioman->GetObject("EmcRecPoint");
    if (!fRecPointArray) {
      cout << "-W- MpdEmcMatching::Init: " << "No RecPoint array!" << endl;
      return kERROR;
    }

    fTpcTracks = (TClonesArray*) ioman->GetObject("TpcKalmanTrack");
    if (!fTpcTracks) {
      cout << "-W- MpdEmcMatching::Init: " << "No TPC track array!" << endl;
      return kERROR;
    }

    // Create and register output array
    //*
    fMatchArray = new TClonesArray("MpdEmcMatch", 100);
    ioman->Register("EmcMatch", "EMC", fMatchArray, kTRUE);
    //*/

    cout << "-I- MpdEmcMatching: Intialization successfull" << endl;

    return kSUCCESS;

}

//__________________________________________________________________________

void MpdEmcMatching::Finish() {
    cout << "-I- MpdEmcMatching: Finish" << endl;
}

//__________________________________________________________________________

void MpdEmcMatching::Exec(Option_t* opt) 
{
  // Main processing engine

  cout << "MpdEmcMatching::Exec started" << endl;

  static const Int_t nSec = fGeoPar->GetNsec(); // number of EMC sectors
  static const Int_t nTowSec = fGeoPar->GetNsupMod() * fGeoPar->GetNModInSuperModByPhi();

  // Reset output Array
  //if (!fDigiArray) Fatal("MpdEmcMatching::Exec", "No array of digits");
  fMatchArray->Delete();

  // Clear rec. point containers
  for (Int_t i = 0; i < nSec+2; ++i) fRecPoints[i].clear();
  
  // Fill rec. point containers
  Int_t nRecP = fRecPointArray->GetEntriesFast();
  cout << " Total number of rec. points: " << nRecP << endl;

  for (Int_t i = 0; i < nRecP; ++i) {
    MpdTpcHit *recp = (MpdTpcHit*) fRecPointArray->UncheckedAt(i);
    Int_t isec = recp->GetLocalX() / nTowSec;
    if (recp->GetLocalX() < 0) isec = nSec - 1;
    else if (isec >= nSec) isec = 0;
    fRecPoints[isec].insert(pair<Double_t,Int_t>(recp->GetZ(),i));
  }

  Int_t ntpc = fTpcTracks->GetEntriesFast();
  if (ntpc == 0) return;

  for (Int_t i = 0; i < ntpc; ++i) {
    //if (isec < nSec && (*tr->GetParamAtHit())(1,0) > 20 && (*tr->GetParamAtHit())(3,0) > 0) continue; // different sides
    //else if (isec >= nSec && (*tr->GetParamAtHit())(1,0) < -20 && (*tr->GetParamAtHit())(3,0) < 0) continue; // different sides
    DoMatching(i);
  }
}

//__________________________________________________________________________

void MpdEmcMatching::DoMatching(Int_t itrack) 
{
  // Match track with EMC clusters

  static const Int_t nSec = fGeoPar->GetNsec(); // number of EMC sectors
  static const Double_t rMax = 216.0; // outer radius !!!
  static const Double_t rMin = rMax - 40.0; // inner radius !!!
  static const Double_t zMax = fGeoPar->GetNrows() * fGeoPar->GetNModInSuperModByZ() * 
    fGeoPar->GetLengthOfModuleByZ();
  static const Double_t dzTower = fGeoPar->GetLengthOfModuleByZ();
  //static Double_t dphiTower = TMath::TwoPi() / nSec / fGeoPar->GetNsupMod() / fGeoPar->GetNModInSuperModByPhi();
  static const Double_t dphiSec = TMath::TwoPi() / 360 * fGeoPar->GetAngleOfSector();

  MpdTpcKalmanTrack *tr = (MpdTpcKalmanTrack*) fTpcTracks->UncheckedAt(itrack);
  if (TMath::Abs((*tr->GetParamAtHit())(1,0)) > zMax) return;
  MpdTpcKalmanTrack tr1(*tr);
  tr1.SetParam(*tr1.GetParamAtHit());
  tr1.SetParamNew(*tr1.GetParamAtHit());
  tr1.SetWeight(*tr1.GetWeightAtHit());
  tr1.SetPos(tr1.GetPosAtHit());
  tr1.SetPosNew(tr1.GetPos());
  tr1.SetLength(tr1.GetLengAtHit());
  Double_t eta = TMath::Abs (tr->Momentum3().Eta());
  // For chi2: parameters obtained for box e (pT = 0.2-1.9, eta = -1.1-1.1)
  Double_t depth = 2.3 + 8.8 * eta;
  Double_t sigmaL = 0.82 + 1.50 * eta + 0.91 * eta * eta;
  Double_t sigmaL2 = sigmaL * sigmaL, sigmaT2 = 0.66 * 0.66;
  // For chi2pi: parameters obtained for box pi (pT = 0.1-2.0, eta = -1.1-1.1)
  Double_t sig1 = 7.8 - 10.9 * eta + 8.6 * eta * eta;
  Double_t sig2 = 7.1 - 8.1 * eta + 5.3 * eta * eta;
  Double_t et = TMath::Min (eta, 1.0);
  Double_t sigT = 1.4 - 2.2 * et + 5.6 * et * et - 3.2 * et * et * et;
  
  Int_t secs[2], iok = 0;
  Double_t zminmax[2] = {999,-999}, ztr[2], phitr[2];
  pair<multimap<Double_t,Int_t>::iterator,multimap<Double_t,Int_t>::iterator> pits[2]; // up to 2 sectors to check

  // Loop over TPC tracks
  MpdKalmanFilter *pKF = MpdKalmanFilter::Instance("KF","KF");

  MpdKalmanHit hEnd; 
  hEnd.SetType(MpdKalmanHit::kFixedR);
  
  for (Int_t io = 0; io < 2; ++io) {
    Double_t r = (io == 0) ? rMin : rMax;
    hEnd.SetPos(r);
    iok = pKF->PropagateToHit(&tr1,&hEnd,kTRUE);
    if (!iok) break;
    ztr[io] = tr1.GetParamNew(1);
    Double_t phi = tr1.GetParamNew(0) / tr1.GetPosNew();
    if (phi < 0) phi += TMath::TwoPi();
    else if (phi > TMath::TwoPi()) phi -= TMath::TwoPi();
    phitr[io] = phi;
    zminmax[0] = TMath::Min (zminmax[0],ztr[io]);
    zminmax[1] = TMath::Max (zminmax[1],ztr[io]);
  }
  if (!iok) return;
  zminmax[0] -= 2 * dzTower;
  zminmax[1] += 2 * dzTower;
  
  secs[0] = phitr[0] / dphiSec;
  secs[1] = phitr[1] / dphiSec;
  Int_t dsecs = secs[1] - secs[0];
  if (TMath::Abs(dsecs) > 1 && TMath::Abs(dsecs) < nSec / 2) secs[1] = secs[0] + TMath::Sign(1,dsecs);
  else if (TMath::Abs(dsecs) > nSec / 2 && TMath::Abs(dsecs) < nSec - 1) secs[1] = secs[0] - TMath::Sign(1,dsecs);
  if (secs[1] < 0) secs[1] += nSec;
  
  // Select rec. points (road) to compute distance to the track
  for (Int_t io = 0; io < 2; ++io) {
    if (fRecPoints[secs[io]].count(zminmax[0]) > 1 || fRecPoints[secs[io]].count(zminmax[1]) > 1)
      { cout << " !!! The same coordinate !!! " << endl; exit(0); }
    pits[io].first = fRecPoints[secs[io]].lower_bound(zminmax[0]);
    pits[io].second = fRecPoints[secs[io]].upper_bound(zminmax[1]);
  }
   
  phitr[1] = MpdKalmanFilter::Instance()->Proxim(phitr[0],phitr[1]);
  TVector2 track(ztr[1]-ztr[0],(phitr[1]-phitr[0])*rMax);
  Double_t trLeng = track.Mod();
  Int_t imatch = fMatchArray->GetEntriesFast();
  
  for (Int_t io = 0; io < 2; ++io) {
    if (io && secs[io] == secs[0]) continue; // the same sector
    
    for (multimap<Double_t,Int_t>::iterator it = pits[io].first; it != pits[io].second; ++it) {
      MpdTpcHit *recp = (MpdTpcHit*) fRecPointArray->UncheckedAt(it->second);
      Double_t phiRecp = recp->GetX();
      phiRecp = MpdKalmanFilter::Instance()->Proxim(phitr[0],phiRecp);
      TVector2 vect(it->first-ztr[0],(phiRecp-phitr[0])*rMax);
      Double_t distT = TMath::Sin(vect.DeltaPhi(track)) * vect.Mod();
      Double_t distL = TMath::Cos(vect.DeltaPhi(track)) * vect.Mod();
      MpdEmcMatch *match = new ((*fMatchArray)[imatch++]) MpdEmcMatch(itrack, it->second, distT, distL, 
								      recp->GetQ(), trLeng); 
      Double_t chi2 = distT * distT / sigmaT2 + (distL - depth) * (distL - depth) / sigmaL2;
      match->SetChi2(chi2);
      // Pions
      if (distL >=0 && distL <= trLeng) distL = 0.0;
      else if (distL > trLeng) distL -= trLeng;
      chi2 = distT * distT / sigT / sigT;
      if (distL < 0) chi2 += (distL * distL / sig1 / sig1);
      else chi2 += (distL * distL / sig2 / sig2);
      match->SetChi2pi(chi2);
    }
  }
}

//__________________________________________________________________________

ClassImp(MpdEmcMatching)
