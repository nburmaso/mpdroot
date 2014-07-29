// -------------------------------------------------------------------------
// -----                  MpdEctTrackFinderTofTpc source file          -----
// -----                 Created 17/06/08  by A. Zinchenko             -----
// -------------------------------------------------------------------------

/**  MpdEctTrackFinderTof.h
 *@author A.Zinchenko <Alexander.Zinchenko@jinr.ru>
 **
 ** Track finder in MPD End-Cap Tracker (ECT) using seeds built from TOF, TPC 
 ** and vertex points
 **/

#include "MpdEctTrackFinderTofTpc.h"
#include "MpdKalmanFilter.h"
#include "MpdKalmanHitZ.h"
#include "MpdEctKalmanTrack.h"
#include "MpdStrawendcapPoint.h"
#include "MpdEtofPoint.h"
#include "TpcLheHit.h"
#include "TpcLheKalmanTrack.h"

#include "FairMCTrack.h"
#include "FairRootManager.h"
#include "TMath.h"
//#include "TFile.h"
//#include "TLorentzVector.h"
#include "TVector2.h"
//#include "TClonesArray.h"
#include <TRandom.h>

#include <iostream>
//#include <vector>

using std::cout;
using std::endl;
//using std::vector;

const Double_t MpdEctTrackFinderTofTpc::fgkChi2Cut = 20; //20; //100;
FILE *lun = 0x0; //fopen("error.dat","w");

//__________________________________________________________________________
MpdEctTrackFinderTofTpc::MpdEctTrackFinderTofTpc(const char *name, Int_t iVerbose )
  :FairTask(name, iVerbose)
{
  fKHits = new TClonesArray("MpdKalmanHitZ", 100);
  fTracks = new TClonesArray("MpdEctKalmanTrack", 100);
  fHistoDir = 0x0;
  fhLays = new TH1F("hLays","ECT layers",100,0,100);
  fLayPointers = 0x0;
  fTPC = new TObjArray(300);
}


//__________________________________________________________________________
MpdEctTrackFinderTofTpc::~MpdEctTrackFinderTofTpc()
{
  //delete fKHits;
  //delete fTracks;
  //delete fTrackCand;
  delete [] fLayPointers;
  delete fhLays;
  delete fTPC;
}

//__________________________________________________________________________
InitStatus MpdEctTrackFinderTofTpc::Init()
{
  return ReInit();
}

//__________________________________________________________________________
InitStatus MpdEctTrackFinderTofTpc::ReInit()
{
  fTpcHits = (TClonesArray *) FairRootManager::Instance()->GetObject("LheHit");
  fEctHits =(TClonesArray *) FairRootManager::Instance()->GetObject("STRAWPoint");
  fTofHits =(TClonesArray *) FairRootManager::Instance()->GetObject("ETOFPoint");
  //fTpcTracks = 0x0; //(TClonesArray *) FairRootManager::Instance()->GetObject("TpcKalmanTrack");
  fTpcTracks = (TClonesArray *) FairRootManager::Instance()->GetObject("TpcPoint"); // just for testing
  fMCTracks =(TClonesArray *) FairRootManager::Instance()->GetObject("MCTrack");
  //fSTSTrackMatch = (TClonesArray*) FairRootManager::Instance()->GetObject("STSTrackMatch");
  //fPrimVtx =  (FairVertex *) FairRootManager::Instance() ->GetObject("PrimaryVertex");

  FairRootManager::Instance()->Register("EctTrack", "Ect", fTracks, kTRUE);

  fNPass = 1;
  return kSUCCESS;
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::Reset() 
{
  ///
  cout << " MpdEctTrackFinderTof::Reset  " << endl;

  fKHits->Clear();
  fTracks->Clear();
  fTPC->Clear();
  delete [] fLayPointers;
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::SetParContainers()
{
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::Finish()
{
  //Write();
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::Exec(Option_t * option)
{

  static int eventCounter = 0;    
  cout << " EctRec event " << ++eventCounter << endl;

  Reset();

  // Create Kalman hits
  MakeKalmanHits();

  for (Int_t i = 0; i < fNPass; ++i) {
    fTracks->Clear();
    GetTrackSeeds(i);

    cout << "  Total number of hits for tracking: " << fKHits->GetEntriesFast() << endl;
    cout << "  Total number of track seeds: " << fTracks->GetEntriesFast() << endl;

    DoTracking(i);
    //StoreTracks();
    cout << "  Total number of found tracks: " << fTracks->GetEntriesFast() << endl;
    if (i != fNPass - 1) ExcludeHits(); // exclude used hits
  }
  RemoveDoubles(); // remove double tracks
  AddHits(); // add hit objects to tracks
  cout << "  Total number of found tracks: " << fTracks->GetEntriesFast() << endl;
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::MakeKalmanHits()
{
  /// Create Kalman hits from ECT points.

  fhLays->Reset();
  Int_t nHits = fEctHits->GetEntriesFast(), layMax = 0, lay = 0, nKH = 0;
  Double_t phi, r, coord, errR = 0.2, errRphi = 0.02; // 2mm in R, 200um in R-Phi
  //Double_t phi, r, coord, errR = 0.2, errRphi = 0.005; // 2mm in R, 50um in R-Phi

  for (Int_t ih = 0; ih < nHits; ++ih) {
    MpdStrawendcapPoint *h = (MpdStrawendcapPoint*) fEctHits->UncheckedAt(ih);
    //if (h->GetTrackID() != 1265) continue; // !!!
    //if (h->GetTrackID() != 64) continue; // !!!
    //if (h->GetTrackID() != 1574) continue; // !!!
    //if (h->GetTrackID() != 1427) continue; // !!!
    //if (h->GetTrackID() != 64 && h->GetTrackID() != 92) continue; // !!!
    phi = TMath::ATan2 (h->GetY(), h->GetX()); // tube Phi - radial case
    phi = h->GetPhi(); // tube Phi
    lay = h->GetDetectorID() / 1000;
    // Extrapolate track to Z = Ztube
    Double_t dZ = h->GetZ() - h->GetTrackZ();
    Double_t dt = 0.; // dZ / h->GetPz();
    //if (TMath::Abs(h->GetPz()) > 1.e-6 && h->GetPz() * h->GetZ() > 0) dt = dZ / h->GetPz();
    Double_t xNew = h->GetTrackX() + dt * h->GetPx();
    Double_t yNew = h->GetTrackY() + dt * h->GetPy();
    //cout << dZ << " " << h->GetTrackX() << " " << xNew << " " << h->GetTrackY() << " " << yNew << " " << lay << endl;
    //Double_t zNew = h->GetTrackZ() + dt * h->GetPz(); // just for cross-check
    // Transform to the tube local coordinate system
    Double_t cosPhi = TMath::Cos(phi);
    Double_t sinPhi = TMath::Sin(phi);
    //Double_t xLoc = h->GetX() * cosPhi + h->GetY() * sinPhi; // cross-check
    //Double_t yLoc = -h->GetX() * sinPhi + h->GetY() * cosPhi;
    Double_t xLoc = xNew * cosPhi + yNew * sinPhi;
    Double_t yLoc = -xNew * sinPhi + yNew * cosPhi;
    //Double_t xLoc = (xNew - h->GetX()) * cosPhi + (yNew - h->GetY()) * sinPhi;
    //Double_t yLoc = -(xNew - h->GetX()) * sinPhi + (yNew - h->GetY())  * cosPhi;
    //r = xNew * xNew + yNew * yNew;
    //r = TMath::Sqrt (r);
    //r = TMath::Abs(xLoc);
    r = xLoc;
    //cout << xLoc << " " << yLoc << " " << r << " " << h->GetPz() << endl;
    coord = yLoc;

    // Add error                                            
    Double_t dRphi = 0, dR = 0;
    gRandom->Rannor(dRphi,dR); // add errors
    MpdKalmanHitZ *hit = new ((*fKHits)[nKH++]) MpdKalmanHitZ(r+dR*errR, phi, coord+dRphi*errRphi, 
							      h->GetTrackZ(), errR, errRphi, lay, ih);
    hit->SetXY(h->GetX(), h->GetY()); 
    hit->SetSin(sinPhi);
    hit->SetCos(cosPhi);
    // Add second measurement - just for test at the moment
    //!!!
    //hit->SetNofDim(2);
    //!!!
    //lay = hit.GetLayer();
    layMax = TMath::Max (lay, layMax);
    fhLays->Fill(lay+0.1);
  }
  cout << " Max layer = " << layMax << " " << fKHits->GetEntriesFast() << endl;
  fKHits->Sort(); // in ascending order in abs(Z)

  fLayPointers = new Int_t [layMax+1];
  Int_t ipos = 0;
  for (Int_t i = 0; i <= layMax; ++i) {
    //cout << i << " " << fhLays->GetCellContent(i+1,0) << endl;
    //if (ipos) cout << ((TpcLheHit*)fHits->UncheckedAt(ipos))->GetLayer() << " "
    //     << ((TpcLheHit*)fHits->UncheckedAt(ipos-1))->GetLayer() << endl;
    Int_t cont = TMath::Nint (fhLays->GetCellContent(i+1,0));
    if (cont == 0) {
      fLayPointers[i] = -1;
      continue;
    }
    fLayPointers[i] = ipos;
    ipos += cont;
  }
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::GetTrackSeeds(Int_t iPass)
{
  /// Build track seeds from TOF hits, TPC hits at max Z and vertex

  Int_t tmp[99999] = {0}, idMax = 0, nID = 0;
  GetTpcPoints(); // select TPC points near zMax
  //GetTofPoints(); //
  Int_t nTpc = fTPC->GetEntriesFast();
  Int_t nTof = fTofHits->GetEntriesFast();
 
  Int_t nCand = 0;
  //const Double_t dphiMax = 0.2, dslMax = 0.1; // cuts dPhi and dSlope
  //const Double_t dphiMax = 0.25, dslMax = 0.1; // cuts dPhi and dSlope
  const Double_t dphiMax = 0.5, dslMax = 0.1; // cuts dPhi and dSlope
  TVector3 vert(0.,0.,0.);

  for (Int_t itof = 0; itof < nTof; ++itof) {
    MpdEtofPoint *tof = (MpdEtofPoint*) fTofHits->UncheckedAt(itof);
    //if (tof->GetTrackID() != 64) continue; // !!!
    //if (tof->GetTrackID() != 1265) continue; // !!!
    Double_t rTof = TMath::Sqrt (tof->GetX() * tof->GetX() + tof->GetY() * tof->GetY());
    Double_t slope0 = rTof / tof->GetZ();
    Double_t phTof = TMath::ATan2 (tof->GetY(), tof->GetX());

    for (Int_t itpc = 0; itpc < nTpc; ++itpc) {
      TpcLheHit *tpc = (TpcLheHit*) fTPC->UncheckedAt(itpc);
      // For testing
      TpcPoint *p = 0x0;
      Int_t idTpc = -1;
      if (fTpcTracks) p = (TpcPoint*) fTpcTracks->UncheckedAt(tpc->GetRefIndex());
      if (p) idTpc = p->GetTrackID();
      //if (idTpc != tof->GetTrackID()) continue; // !!! for test - keep only correct combinations

      Double_t rTpc = tpc->GetR();
      if (rTpc > rTof) continue;
      Double_t phTpc = tpc->GetRphi() / rTpc;
      Double_t dphi = MpdKalmanFilter::Instance()->Proxim(phTof, phTpc) - phTof;
      if (TMath::Abs(dphi) > dphiMax) continue;
      Double_t slope = tpc->GetR() / tpc->GetZ();
      if (TMath::Abs(slope-slope0) > dslMax) continue;
      if (lun && idTpc == tof->GetTrackID()) fprintf(lun,"%10.3e %10.3e %10.3e %10.3e\n",rTpc,rTof,dphi,slope-slope0);
      if (tof->GetTrackID() < 99999) { tmp[tof->GetTrackID()]++; idMax = TMath::Max(idMax,tof->GetTrackID()); }

      MpdEctKalmanTrack *track = new ((*fTracks)[nCand++]) MpdEctKalmanTrack(itof, itpc, tof, tpc, vert);
      track->SetDirection(MpdKalmanTrack::kOutward);
      track->SetPos(tpc->GetZ());
      track->SetPosNew(tpc->GetZ());
      EvalParams(tof, tpc, track);
      // Eval. track Phi - linear extrapolation of 2 estimates
      //track->SetParam(2, phTof); // phi
      Double_t dx = tof->GetX() - rTpc*TMath::Cos(phTpc);
      Double_t dy = tof->GetY() - rTpc*TMath::Sin(phTpc);
      Double_t dr = TMath::Sqrt (dx * dx + dy * dy);
      Double_t phTofTpc = TMath::ATan2 (dy, dx);
      Double_t ph = phTpc - 1.0 * (MpdKalmanFilter::Instance()->Proxim(phTofTpc,phTof)-phTofTpc)/(rTof-dr) * rTpc;
      track->SetParam(2, ph); // phi
      //track->SetParam(2, TMath::ATan2(p->GetPy(),p->GetPx())); // exact value - for test
      Double_t th = TMath::ATan2(tof->GetZ()-tpc->GetZ(),dr);
      track->SetParam(3, th); // dip angle
      //track->SetParam(3, TMath::ATan2(p->GetPz(),TMath::Sqrt(p->GetPx()*p->GetPx()+p->GetPy()*p->GetPy()))); // exact value - for test
      track->SetParamNew(*track->GetParam());
      track->GetParam()->Print();
      EvalCovar(tof, tpc, track);
      //if (lun && idTpc == tof->GetTrackID()) fprintf(lun,"%10.3e %10.3e %10.3e %10.3e %10.3e %10.3e %10.3e\n",track->GetParam(2),ph,track->GetParam(3),th,dphi,TMath::Sqrt((*track->GetWeight())(2,2)),TMath::Sqrt((*track->GetWeight())(3,3)));

      cout << nCand-1 << " " << track->GetTrackID() << " " << idTpc << endl;
    }
  }
  for (Int_t j = 0; j <= idMax; ++j) if (tmp[j] > 0) ++nID;
  cout << " Number of ECT track candidates: " << nCand << " " << nID << endl;
  if (lun) fclose(lun);
  //exit(0);
}  
  
//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::GetTpcPoints()
{
  /// Get TPC hits near zMax

  Int_t ids[99999] = {0}, idMax = 0;
  Double_t zMax = 134.5, rMin = 20.5, rMax = 109.5, dR = (rMax-rMin)/50; // dR = 1.78; // old version 
  Int_t nTpc = fTpcHits->GetEntriesFast(), lay0 = -1;
  cout << " TPC hits: " << nTpc << endl;

  Double_t dz = 0.;
  //fTpcHits->Sort();
  for (Int_t i = 0; i < nTpc; ++i) {
    TpcLheHit *hit = (TpcLheHit*) fTpcHits->UncheckedAt(i);
    TpcPoint *p = 0x0;
    if (fTpcTracks) p = (TpcPoint*) fTpcTracks->UncheckedAt(hit->GetRefIndex());
    //if (p && p->GetTrackID() != 64) continue; // !!!
    if (hit->GetZ() < 0) continue;
    if (hit->GetLayer() != lay0) {
      lay0 = hit->GetLayer();
      dz = zMax / hit->GetR() * dR / 2.;
    }
    if (hit->GetZ() < zMax - dz) continue;
    //Int_t id = ((TpcPoint*)fTpcTracks->UncheckedAt(hit->GetRefIndex()))->GetTrackID();
    Int_t id = hit->GetTrackID();
    cout << i << " " << hit->GetLayer() << " " << hit->GetZ() << " " << dz << " " << id << endl;
    fTPC->Add(hit);
    if (id > 99998) continue;
    idMax = TMath::Max (id, idMax);
    ids[id]++;
  }
  Int_t nid = 0;
  for (Int_t i = 0; i <= idMax; ++i) if (ids[i]) ++nid;

  cout << " TPC points for seeding: " << fTPC->GetEntriesFast() << " " << nid << endl;
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::EvalParams(const MpdEtofPoint *tof, const TpcLheHit *tpc, 
				      MpdEctKalmanTrack *track) 
{
  /// Evaluate track parameters

  // Evaluate signed track Pt (curvature) assuming the track coming from the 
  // primary vertex
  Double_t rTof = TMath::Sqrt (tof->GetX() * tof->GetX() + tof->GetY() * tof->GetY());
  Double_t phTof = TMath::ATan2 (tof->GetY(), tof->GetX());
  Double_t phTpc = tpc->GetRphi() / tpc->GetR();
  TVector2 vecTof(rTof*TMath::Cos(phTof)-0.,rTof*TMath::Sin(phTof)-0.);
  TVector2 vecTpc(tpc->GetR()*TMath::Cos(phTpc)-0.,tpc->GetR()*TMath::Sin(phTpc)-0.);
  TVector2 dvec = vecTof - vecTpc;
  Double_t cosAlpha = vecTpc * dvec / vecTpc.Mod() / dvec.Mod();
  Double_t rad = vecTof.Mod() / 2. / TMath::Sin(TMath::ACos(cosAlpha));
  Double_t factor = 0.0015; // 0.3 * 0.5T * 0.01
  //Double_t charge = phTof - MpdKalmanFilter::Instance()->Proxim(phTof,phTpc);
  Double_t charge = MpdKalmanFilter::Instance()->Proxim(phTof,phTpc) - phTof;
  if (rTof < tpc->GetR()) charge = -charge;
  Double_t pt = factor * TMath::Abs(rad) * TMath::Sign(1., -charge);

  track->SetParam(0, vecTpc.X()); // X
  track->SetParam(1, vecTpc.Y()); // Y
  track->SetParam(4, 1./pt); // 1/pt
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::EvalCovar(const MpdEtofPoint *tof, const TpcLheHit *tpc, 
				     MpdEctKalmanTrack *track)
{
  /// Evaluate covariance matrix for the track seed

  static const Double_t xErrTof = 2. / TMath::Sqrt(12.),  yErrTof = xErrTof;
  //static const Double_t xErrTof = 0.02, yErrTof = xErrTof;
  TMatrixDSym weight(5);
  //weight(0,0) = xErrTof * xErrTof; // <xx>
  weight(0,0) = tpc->GetRphiErr() * tpc->GetRphiErr(); // <xx>
  weight(0,0) *= 4.; // extra factor of 2

  //weight(1,1) = yErrTof * yErrTof; // <yy>
  weight(1,1) = tpc->GetRphiErr() * tpc->GetRphiErr(); // <yy>
  weight(1,1) *= 4.; // extra factor of 2

  Double_t phTpc = tpc->GetRphi() / tpc->GetR();
  Double_t xTpc = tpc->GetR() * TMath::Cos(phTpc);
  Double_t yTpc = tpc->GetR() * TMath::Sin(phTpc);
  Double_t xTof = tof->GetX(), yTof = tof->GetY();
  Double_t dx = xTof - xTpc, dy = yTof - yTpc;
  Double_t dist2 = dx * dx + dy * dy;
  Double_t sinPhi = TMath::Sin (track->GetParam(2));
  Double_t cosPhi = TMath::Cos (track->GetParam(2));

  Double_t pTpc = TMath::Cos(phTpc) * cosPhi + TMath::Sin(phTpc) * sinPhi;
  weight(2,2) = pTpc * pTpc * tpc->GetRphiErr() * tpc->GetRphiErr();
  weight(2,2) += xErrTof * xErrTof;
  weight(2,2) /= dist2; // <PhiPhi>
  //weight(2,2) *= 4.; // extra factor of 2
  weight(2,2) *= 2.; // extra factor of sqrt(2)

  Double_t tanThe = TMath::Tan (track->GetParam(3));
  Double_t rTof = TMath::Sqrt (tof->GetX() * tof->GetX() + tof->GetY() * tof->GetY());
  Double_t dR = rTof - tpc->GetR();
  Double_t denom = dR * (1. + tanThe * tanThe);
  weight(3,3) = tpc->GetZerr() * tpc->GetZerr() / denom / denom;
  Double_t dz = tof->GetZ() - tpc->GetZ();
  Double_t dThdR = dz / (dR * dR + dz * dz);
  weight(3,3) += dThdR * dThdR * xErrTof * xErrTof; // <TheThe>
  //weight(3,3) *= 4.; // extra factor of 2
  weight(3,3) *= 8.; // extra factor of 2

  //weight(4,4) = (track->GetParam(4)*0.5) * (track->GetParam(4)*0.5); // error 50%
  //(*fWeight)(4,4) = ((*fParam)(4,0)*0.75) * ((*fParam)(4,0)*0.75); // error 75%
  weight(4,4) = (track->GetParam(4)*1.) * (track->GetParam(4)*1.); // error 100%
  //weight(4,4) = (track->GetParam(4)*2.) * (track->GetParam(4)*2.); // error 200%
  //weight(4,4) = (track->GetParam(4)*4.) * (track->GetParam(4)*4.); // error 400%

  weight.Print();
  //fWeight->Invert(); // weight matrix
  Int_t iok = 0;
  MpdKalmanFilter::Instance()->MnvertLocal(weight.GetMatrixArray(), 5, 5, 5, iok);
  //fWeight->Print();
  track->SetWeight(weight);
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::DoTracking(Int_t iPass)
{
  /// Run Kalman tracking
  
  Int_t nCand = fTracks->GetEntriesFast(), iok = 0;
 
  for (Int_t i = 0; i < nCand; ++i) {
    cout << " Track seed No. " << i << endl;
    MpdEctKalmanTrack *track = (MpdEctKalmanTrack*) fTracks->UncheckedAt(i);
    //if (track->GetTrackID() != 117) continue;
    //if (track->GetTrackID() != 10) continue;
    //if (track->GetTrackID() != 1105) continue;
    //if (track->GetTrackID() != 77) continue;
    //(*(track->GetParamNew()))(4,0) = -0.5; // just for check
    /*
    for (Int_t k = 0; k < 5; ++k) {
      for (Int_t j = i; j < 5; ++j) {
	if (j == i) (*track->GetWeight())(i,j) /= 100.;
	else (*track->GetWeight())(i,j) = (*track->GetWeight())(j,i) = 0.;
      }
    }
    */

    // Start ECT tracking from different layers to account for missing hits
    const Int_t laySkip = 10; //10;
    Int_t layMax = ((MpdKalmanHit*)fKHits->Last())->GetLayer();
    MpdEctKalmanTrack tracks[laySkip];

    for (Int_t j = 0; j < laySkip; ++j) {
      tracks[j] = *track;
      //cout << track->GetParamNew(0) << endl;
      //cout << i << " " << lay << " " << tracks[lay].GetNofHits() << " " << tracks[lay].GetChi2() << " " << tracks[lay].GetParam(0) << endl;
      Int_t lay = 1;
      if (j > 0 && tracks[j-1].GetNofHits() > 0) 
	lay = ((MpdKalmanHit*)tracks[j-1].GetHits()->First())->GetLayer() + 1;
      iok = RunKalmanFilter(&tracks[j], lay);
      //iok = RunKalmanFilter(&tracks[lay], 0);
      //cout << i << " " << lay << " " << tracks[lay].GetNofHits() << " " << tracks[lay].GetChi2() << endl;
    }

    // Select the best track (with max number of hits)
    Int_t nHitsMax = tracks[0].GetNofHits(), iMax = 0;
    for (Int_t j = 1; j < laySkip; ++j) {
      Int_t nhits = tracks[j].GetNofHits();
      if (nhits > nHitsMax) {
	nHitsMax = nhits;
	iMax = j;
      } else if (nhits == nHitsMax) {
	if (tracks[j].GetChi2() < tracks[iMax].GetChi2()) {
	  iMax = j;
	  nHitsMax = nhits;
	}
      }
    }
    *track = tracks[iMax];
    track->GetParamNew()->Print();

    if (0) {
      track->SetParam(*track->GetParamNew());
      track->SetParam(4,track->GetParamNew(4)*1.5);
      track->SetParamNew(*track->GetParam());
      MpdKalmanFilter::Instance()->Refit(track, -1);
      MpdKalmanFilter::Instance()->Refit(track, 1);
    }
    //iok = RunKalmanFilter(track);
    cout << i << " " << track->GetNofHits() << endl;
    if (0 && iok == 0 && track->GetNofHits() < 0) {
      //*
      MpdKalmanHitZ *hit = (MpdKalmanHitZ*) track->GetHits()->UncheckedAt(0);
      Double_t dir = TMath::Sign(1.,track->GetPosNew()); 
      track->SetPos(hit->GetPos()-dir);
      track->SetPosNew(track->GetPos());
      //for (Int_t j = 0; j < 5; ++j) track->SetParam(j,track->GetParam1()[j]);
      //track->SetParamNew(*track->GetParam());
      //MpdKalmanHitZ hitTmp(*hit);
      //hitTmp.SetZ(track->GetPosNew()-dir);
      //MpdKalmanFilter::Instance()->PropagateToHit(track,&hitTmp);
      MpdKalmanFilter::Instance()->Refit(track,1);
      //*/
      /*
      track->Print("");
      Double_t dir = TMath::Sign(1.,track->GetPosNew()); 
      MpdKalmanHitZ *hit = (MpdKalmanHitZ*) track->GetHits()->UncheckedAt(0);
      MpdKalmanHitZ hitTmp(*hit);
      hitTmp.SetZ(track->GetPosNew()+dir);
      MpdKalmanFilter::Instance()->PropagateToHit(track,&hitTmp);
      track->Print("");
      MpdKalmanFilter::Instance()->Refit(track,-1);
      track->Print("");
      hitTmp.SetZ(track->GetPosNew()-dir);
      MpdKalmanFilter::Instance()->PropagateToHit(track,&hitTmp);
      track->Print("");
      MpdKalmanFilter::Instance()->Refit(track,1);
      track->Print("");
      */
    }
  }
}
    
//__________________________________________________________________________
Int_t MpdEctTrackFinderTofTpc::RunKalmanFilter(MpdEctKalmanTrack *track, Int_t layBeg)
{
  /// Run Kalman filter

  const Double_t rMin = 28., rMax = 123.; // min and max radial size of ECT - to be taken elsewhere
  //cout << fHits->GetEntriesFast() << endl;
  Int_t layMax = ((MpdKalmanHit*)fKHits->Last())->GetLayer();
  MpdKalmanHitZ *hitOK = 0x0;
  MpdKalmanTrack::TrackDir trackDir = track->GetDirection();
  //Int_t layBeg = layMax, layEnd = -1, dLay = -1, layOK = -1;
  Int_t layEnd = -1, dLay = -1, layOK = -1;
  if (trackDir == MpdKalmanTrack::kOutward) {
    layEnd = layMax + 1;
    dLay = 1;
  }
  
  //Int_t indxOK = hits->IndexOf(hitOK);
  //Int_t nHits = hits->GetEntriesFast();
  Int_t miss = 0;
  TMatrixDSym pointWeight(5), pointWeightTmp(5), saveWeight(5);
  TMatrixD param(5,1), paramTmp(5,1);

  Double_t saveZ = 0;
  //cout << " Starting hit: " << hitOK->GetLayer() << " " << hitOK->GetTrackID() << " " << hitOK->GetUsage() << endl;
  
  for (Int_t lay = layBeg; lay != layEnd; lay+=dLay) {
    Int_t nLay = GetNofHitsInLayer(lay);
    Int_t indx0 = GetHitsInLayer(lay);
    Double_t dChi2Min = 1.e+6;
    MpdKalmanHitZ *hitMin = 0x0;
    //cout << " lay, nLay: " << lay << " " << nLay << " " << indx0 << endl;
    Int_t indxBeg = 0, indxEnd = nLay, dIndx = 1;
    /*
    if (trackDir == MpdKalmanTrack::kOutward) {
      // !!! This part is due to the simplified hit merging (digitization) -
      // different hit position inside one layer - should be removed later 
      indxBeg = nLay - 1;
      indxEnd = -1;
      dIndx = -1;
    }
    */
    //for (Int_t indx = 0; indx < nLay; ++indx) {
    for (Int_t indx = indxBeg; indx != indxEnd; indx+=dIndx) {
      MpdKalmanHitZ *hit = (MpdKalmanHitZ*) fKHits->UncheckedAt(indx0+indx);
      // !!! Exact ID match
      //if (((FairMCPoint*)fEctHits->UncheckedAt(hit->GetIndex()))->GetTrackID() != track->GetTrackID()) continue;

      // Exclude used hits
      if (hit->GetFlag() != 1) continue;
      //cout << " window:" << /*hit->GetTrackID() <<*/ " " << hit->GetRphi() << " " << track->GetParamNew(0) << " " << hit->GetZ() << " " << track->GetParamNew(1) << endl;
      // Check if the hit within some window (15x15cm for the moment - check!!!)
      //if (TMath::Abs(hit->GetRphi()-track->GetParamNew(0)) > 9) continue;
      //if (TMath::Abs(Proxim(track,hit)-track->GetParamNew(0)) > 15) continue;
      TVector2 dist = GetDistance(track, hit);
      if (dist.X() > 15.) continue; // distance in transverse to the tube direction
      if (hit->GetNofDim() > 1 && dist.Y() > 15.) continue; // distance in R 
      //*if (hit->GetTrackID() == 186)*/ cout << " Hit: " << ((FairMCPoint*)fEctHits->UncheckedAt(hit->GetIndex()))->GetTrackID() << " " << hit->GetLayer() << " " << ((FairMCPoint*)fEctHits->UncheckedAt(hit->GetIndex()))->GetDetectorID() << " " << hit->GetRphi() << " " << hit->GetR() << " " << hit->GetZ() << " " << dist.X() << " " << dist.Y() << " " << track->GetParamNew(0) << " " << track->GetParamNew(1) << endl;
      //track->Print("");
      //hit->Print("");
      if (!MpdKalmanFilter::Instance()->PropagateToHit(track,hit)) return -1;
      //*if (hit->GetTrackID() == -607)*/ cout << /*hit->GetTrackID() <<*/ " " << hit->GetRphi() << " " << track->GetParamNew(0) << " " << track->GetParamNew(1) << " " << hit->GetZ() << " " << track->GetPosNew() << endl;
      //
      // For debugging
      /*
      TVector2 dist0 = GetDistance(track, hit);
      cout << dist0.X() << " ";
      MpdKalmanHitZ hitDbg = *hit;
      Double_t xDbg = hit->GetXY(0) * TMath::Cos(hit->GetPhi()) + hit->GetXY(1) * TMath::Sin(hit->GetPhi());
      Double_t yDbg = -hit->GetXY(0) * TMath::Sin(hit->GetPhi()) + hit->GetXY(1) * TMath::Cos(hit->GetPhi());
      hitDbg.SetRphi(yDbg);
      hitDbg.SetR(xDbg);
      dist = GetDistance(track, &hitDbg);
      cout << dist.X() << endl;
      */
      Double_t radNew = track->GetRadNew();
      if (radNew < rMin || radNew > rMax) return -1;

      //Double_t err = hit->GetRphiErr();
      //if (track->GetNofHits() == 0) hit->SetRphiErr(0.04);

      Double_t dChi2 = MpdKalmanFilter::Instance()->CheckHitZ(track,hit,pointWeight,param);
      //if (track->GetNofHits() == 0) hit->SetRphiErr(err);
      //if (param(3,0) < 0) { cout << " Check: " << param(3,0) << " " << dChi2 << " " << (*fParamNew)(3,0) << " " << hit->GetRphi() << " " << hit->GetZ() << endl; fParamNew->Print();}
      if (dChi2 < dChi2Min) {
      //if (dChi2 < dChi2Min && dist0.X() <= dist.X()) {
      //if (dChi2 < dChi2Min && dist.X() <= 0.2) {
        dChi2Min = dChi2;
        hitMin = hit;
        saveWeight = *track->GetWeight();
        saveZ = track->GetPosNew();
        // temporary storage for the current track
        paramTmp = param;
        pointWeightTmp = pointWeight;
        //cout << " New min dChi2 = " << dChi2 << " " << hitMin->GetRphi() << " " << hitMin->GetR() << endl;
	//cout << track->GetParamNew(0) << " " << track->GetParamNew(1) << endl;
	//cout << hit->GetRphi() << " " << hit->GetZ() << endl;
	//cout << param(0,0) << " " << param(1,0) << endl;
        //paramTmp.Print();
      }
    }
    Double_t cut = fgkChi2Cut;
    //if (track->GetNofHits() == 0) cut /= 2.;
    //if (dChi2Min < fgkChi2Cut) {
    if (dChi2Min < cut) {
      //layOK = lay;
      hitOK = hitMin;
      track->GetHits()->Add(hitOK);
      miss = 0;
      // Restore track params at the best hit
      track->SetChi2(track->GetChi2()+dChi2Min);
      saveWeight += pointWeightTmp;
      track->SetWeight(saveWeight);
      track->SetPosNew(saveZ);
      track->SetParamNew(paramTmp);
      //cout << " *** Adding hit: " << hitOK->GetLayer() << " " << ((FairMCPoint*)fEctHits->UncheckedAt(hitOK->GetIndex()))->GetTrackID() << " " << ((FairMCPoint*)fEctHits->UncheckedAt(hitOK->GetIndex()))->GetDetectorID() << " " << dChi2Min << " " << track->GetChi2() << endl;
      //paramTmp.Print();
      // Check if the accepted hit is the same as the seeded hit
      //if (hitOK->GetLayer() == f2ndHit->GetLayer() && hitOK != f2ndHit) return -1; // abandon track
      if (track->GetNofHits() == 1) track->SetParam1();
    } else {
      ++miss;
      //if (miss > 1) return -1;
    }
    //cout << " lay, miss: " << lay << " " << miss << " " << dChi2Min << " " << fChi2 << endl;
  } // for (Int_t lay = layOK-1; lay >= 0;
  return 0;
}

//__________________________________________________________________________
TVector2 MpdEctTrackFinderTofTpc::GetDistance(MpdEctKalmanTrack *track, MpdKalmanHitZ *hit)
{
  /// Compute distance between track and hit

  Double_t xTr, yTr;
  if (track->GetType() == MpdKalmanTrack::kFixedZ) {
    xTr = track->GetParamNew(0);
    yTr = track->GetParamNew(1);
  } else {
    Double_t rPhi = track->GetParamNew(0);
    Double_t r = track->GetPosNew();
    Double_t ph = rPhi / r;
    xTr = r * TMath::Cos(ph);
    yTr = r * TMath::Sin(ph);
  }
  Double_t phi = hit->GetPhi();
  Double_t cosPhi = TMath::Cos(phi);
  Double_t sinPhi = TMath::Sin(phi);
  // Transform track coordinates to local tube coordinates
  Double_t xLoc = xTr * cosPhi + yTr * sinPhi;
  Double_t yLoc = -xTr * sinPhi + yTr * cosPhi;
  //Double_t xLoc = (xTr - hit->GetXY(0)) * cosPhi + (yTr - hit->GetXY(1)) * sinPhi;
  //Double_t yLoc = -(xTr - hit->GetXY(0)) * sinPhi + (yTr - hit->GetXY(1)) * cosPhi;
  TVector2 dist(TMath::Abs(yLoc-hit->GetRphi()), TMath::Abs(xLoc-hit->GetR()));
  return dist;
}

//__________________________________________________________________________
Double_t MpdEctTrackFinderTofTpc::Proxim(Double_t phi0, Double_t phi)
{
  /// Adjust angle phi to be "around" phi0 - to avoid discontinuity around +- Pi

  Double_t dPhi = phi0 - phi;
  if (TMath::Abs(dPhi) > TMath::Pi()) phi += TMath::Pi() * 2 * TMath::Sign(1.,dPhi);
  return phi;
}

//__________________________________________________________________________
Double_t MpdEctTrackFinderTofTpc::Proxim(MpdKalmanTrack *track, const MpdKalmanHit *hit)
{
  /// Adjust hit coord. R-Phi to be "around" track R-Phi - to avoid 
  /// discontinuity around +- Pi

  Double_t hitPhi = hit->GetRphi() / hit->GetR();
  Double_t phi0 = track->GetParamNew(0) / track->GetPosNew();
  return hit->GetR() * Proxim(phi0, hitPhi);
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::Write()
{
  /// Write

  TFile histoFile("EctRec.root","RECREATE");
  Writedir2current(fHistoDir);
  histoFile.Close();
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::Writedir2current( TObject *obj )
{
  /// Write

  if( !obj->IsFolder() ) obj->Write();
  else{
    TDirectory *cur = gDirectory;
    TDirectory *sub = cur->mkdir(obj->GetName());
    sub->cd();
    TList *listSub = ((TDirectory*)obj)->GetList();
    TIter it(listSub);
    while( TObject *obj1=it() ) Writedir2current(obj1);
    cur->cd();
  }
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::RemoveDoubles()
{
  /// Remove double tracks (if number of common hits greater than 50% of hits on track)

  Int_t nFound = fTracks->GetEntriesFast(), nOK = 0;
  for (Int_t i = 0; i < nFound; ++i) {
    MpdEctKalmanTrack *track = (MpdEctKalmanTrack*) fTracks->UncheckedAt(i);
    if (track == 0x0) continue;
    Int_t nh = track->GetNofHits();
    cout << i << " " << nh << " " << ++nOK << endl;
    for (Int_t j = i + 1; j < nFound; ++j) {
      MpdEctKalmanTrack *track1 = (MpdEctKalmanTrack*) fTracks->UncheckedAt(j);
      if (track1 == 0x0) continue;
      Int_t nh1 = track1->GetNofHits();
      Int_t nc = NofCommonHits(track, track1);
      if (1.*nc/TMath::Min(nh,nh1) < 0.5) continue;
      if (nh > nh1) fTracks->RemoveAt(j);
      else if (nh < nh1) {
	fTracks->RemoveAt(i);
	--nOK;
	break;
      } else {
	if (track->GetChi2() > track1->GetChi2()) {
	  fTracks->RemoveAt(i);
	  --nOK;
	  break;
	}
	fTracks->RemoveAt(j);
      }
    }
  }
  fTracks->Compress();

  // Remove double tracks (originated from the same TPC hit)
  //*
  nFound = fTracks->GetEntriesFast();
  for (Int_t i = 0; i < nFound; ++i) {
    MpdEctKalmanTrack *track = (MpdEctKalmanTrack*) fTracks->UncheckedAt(i);
    if (track == 0x0) continue;
    Int_t iTof = track->GetTpcIndex();
    Int_t nh = track->GetNofHits();
    for (Int_t j = i + 1; j < nFound; ++j) {
      MpdEctKalmanTrack *track1 = (MpdEctKalmanTrack*) fTracks->UncheckedAt(j);
      if (track1 == 0x0) continue;
      Int_t iTof1 = track1->GetTpcIndex();
      if (iTof1 != iTof) continue;
      Int_t nh1 = track1->GetNofHits();
      if (nh > nh1) fTracks->RemoveAt(j);
      else if (nh < nh1) {
	fTracks->RemoveAt(i);
	break;
      } else {
	if (track->GetChi2() > track1->GetChi2()) {
	  fTracks->RemoveAt(i);
	  break;
	}
	fTracks->RemoveAt(j);
      }
    }
  }
  fTracks->Compress();
  //*/
}
	  
//__________________________________________________________________________
Int_t MpdEctTrackFinderTofTpc::NofCommonHits(MpdEctKalmanTrack* track, MpdEctKalmanTrack* track1)
{
  /// Compute number of common hits on 2 tracks

  TObjArray *hits = track->GetHits(), *hits1 = track1->GetHits();
  
  MpdKalmanHit *hit = (MpdKalmanHit*) hits->First();
  MpdKalmanHit *hit1 = (MpdKalmanHit*) hits1->First();

  Int_t nCom = 0;
  while (hit && hit1) {
    if (hit->GetLayer() > hit1->GetLayer()) {
      hit1 = (MpdKalmanHit*) hits1->After(hit1);
      continue;
    }
    if (hit == hit1) ++nCom;
    hit = (MpdKalmanHit*) hits->After(hit);
  }
  return nCom;
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::AddHits()
{
  /// Add hit objects to tracks and compute number of wrongly assigned hits
  /// (hits with ID different from ID of starting track)

  Int_t nFound = fTracks->GetEntriesFast();
  for (Int_t i = 0; i < nFound; ++i) {
    MpdEctKalmanTrack *track = (MpdEctKalmanTrack*) fTracks->UncheckedAt(i);
    Int_t nHits = track->GetNofHits();
    if (nHits == 0) { fTracks->RemoveAt(i); continue; }
    track->SetNofHits(nHits);
    TClonesArray &trHits = *track->GetTrHits();
    SetTrackID(track); // set track ID as ID of majority of its hits
    TObjArray *hits = track->GetHits();
    Int_t nWrong = 0, motherID = track->GetTrackID();
    cout << i << " " << nHits << " " << motherID << endl;
    // Get track mother ID 
    FairMCTrack *mctrack = (FairMCTrack*) fMCTracks->UncheckedAt(motherID);
    while (mctrack->GetMotherId() > 0) {
      motherID = mctrack->GetMotherId();
      mctrack = (FairMCTrack*) fMCTracks->UncheckedAt(mctrack->GetMotherId());
    }

    for (Int_t j = 0; j < nHits; ++j) {
      MpdKalmanHitZ *hit = (MpdKalmanHitZ*) hits->UncheckedAt(j);
      new (trHits[j]) MpdKalmanHitZ(*hit);
      Int_t iproj = (hit->GetLayer() - 1) % 3;
      if (iproj == 0) cout << " U";
      else if (iproj == 1) cout << " R";
      else cout << " V";
      cout << hit->GetLayer();
      MpdStrawendcapPoint *h = (MpdStrawendcapPoint*) fEctHits->UncheckedAt(hit->GetIndex());
      Int_t motherID1 = h->GetTrackID();
      cout << "-" << motherID1;
      // Get point mother ID 
      FairMCTrack *mctrack = (FairMCTrack*) fMCTracks->UncheckedAt(motherID1);
      while (mctrack->GetMotherId() > 0) {
        motherID1 = mctrack->GetMotherId();
        mctrack = (FairMCTrack*) fMCTracks->UncheckedAt(mctrack->GetMotherId());
      }
      if (motherID1 != motherID) nWrong++;
    }
    cout << "\n" << nWrong << " " << track->GetTrackID() << " " << motherID << endl;
    track->SetNofWrong(nWrong);
    track->SetLastLay(((MpdKalmanHit*)track->GetHits()->First())->GetLayer());
  }
  fTracks->Compress();
}

//__________________________________________________________________________
void MpdEctTrackFinderTofTpc::SetTrackID(MpdEctKalmanTrack* track)
{
  /// Set track ID as ID of majority of its hits

  const Int_t idMax = 99999;
  Int_t ids[idMax+1] = {0}, nHits = track->GetNofHits(), locMax = 0;
  TObjArray *hits = track->GetHits();

  for (Int_t i = 0; i < nHits; ++i) {
    MpdKalmanHitZ *hit = (MpdKalmanHitZ*) hits->UncheckedAt(i);
    MpdStrawendcapPoint *p = (MpdStrawendcapPoint*) fEctHits->UncheckedAt(hit->GetIndex());
    Int_t id = p->GetTrackID();
    if (id > idMax) exit(0);
    ++ids[id];
    if (ids[id] > ids[locMax]) locMax = id;
  }
  if (ids[locMax] > 1) track->SetTrackID(locMax);
}

ClassImp(MpdEctTrackFinderTofTpc);
