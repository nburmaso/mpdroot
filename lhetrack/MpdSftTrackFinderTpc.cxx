// -------------------------------------------------------------------------
// -----                  MpdSftTrackFinderTpc source file             -----
// -----                 Created 03/12/08  by A. Zinchenko             -----
// -------------------------------------------------------------------------

/**  MpdSftTrackFinderTpc.h
 *@author A.Zinchenko <Alexander.Zinchenko@jinr.ru>
 **
 ** Track finder in MPD "Silicon" Forward Tracker (SFT) and TPC
 **/

#include "MpdSftTrackFinderTpc.h"
#include "MpdKalmanFilter.h"
#include "MpdKalmanHit.h"
#include "MpdEctKalmanTrack.h"
#include "MpdTpcKalmanFilter.h"
#include "FairHit.h"
#include "TpcLheHit.h"

#include "FairMCPoint.h"
#include "MpdMCTrack.h"
#include "FairRootManager.h"
#include "FairRun.h"
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

// !!! 100 should be used only for track fitting !!!
const Double_t MpdSftTrackFinderTpc::fgkChi2Cut = 100; //10; //100;
FILE *lunSft1 = 0x0; //fopen("error1.dat","w");

//__________________________________________________________________________
MpdSftTrackFinderTpc::MpdSftTrackFinderTpc(const char *name, Int_t iVerbose )
  :FairTask(name, iVerbose)
{
  fKHits = new TClonesArray("MpdKalmanHit", 100);
  fTracks = new TClonesArray("MpdEctKalmanTrack", 100);
  fHistoDir = 0x0;
  fhLays = new TH1F("hLaysSft","ECT layers",100,0,100);
  fLayMinMax[0][0] = fLayMinMax[1][0] = 999;
  fLayMinMax[0][1] = fLayMinMax[1][1] = 0;
}


//__________________________________________________________________________
MpdSftTrackFinderTpc::~MpdSftTrackFinderTpc()
{
  //delete fKHits;
  //delete fTracks;
  //delete fTrackCand;
  delete [] fLayPointers;
  delete fhLays;
}

//__________________________________________________________________________
InitStatus MpdSftTrackFinderTpc::Init()
{
  return ReInit();
}

//__________________________________________________________________________
InitStatus MpdSftTrackFinderTpc::ReInit()
{
  fSftPoints =(TClonesArray *) FairRootManager::Instance()->GetObject("SftPoint");
  fTpcPoints =(TClonesArray *) FairRootManager::Instance()->GetObject("TpcPoint");
  fTpcHits =(TClonesArray *) FairRootManager::Instance()->GetObject("TpcKalmanHit");
  //fTpcTracks = 0x0; //(TClonesArray *) FairRootManager::Instance()->GetObject("TpcKalmanTrack");
  fMCTracks =(TClonesArray *) FairRootManager::Instance()->GetObject("MCTrack");
  //fSTSTrackMatch = (TClonesArray*) FairRootManager::Instance()->GetObject("STSTrackMatch");
  //fPrimVtx =  (FairVertex *) FairRootManager::Instance() ->GetObject("PrimaryVertex");

  FairRootManager::Instance()->Register("EctTrack", "Ect", fTracks, kTRUE);

  fNPass = 1;
  return kSUCCESS;
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::Reset() 
{
  ///
  cout << " MpdSftTrackFinderTpc::Reset  " << endl;

  //fKHits->Clear();
  //fTracks->Clear("C");
  fKHits->Delete();
  fTracks->Delete();
  delete [] fLayPointers;
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::SetParContainers()
{
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::Finish()
{
  //Write();
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::Exec(Option_t * option)
{

  static int eventCounter = 0;    
  cout << " EctRec event " << ++eventCounter << endl;

  Reset();

  // Create Kalman hits
  MakeKalmanHitsSft();

  for (Int_t i = 0; i < fNPass; ++i) {
    fTracks->Clear();
    GetTrackSeeds(0); // Z > 0
    GetTrackSeeds(1); // Z < 0

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
  // Remove TPC tracks which are parts of the combined ones
  RemoveTpcTracks();
  // Propagate to the vertex
  //GoToVertex();
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::MakeKalmanHitsSft()
{
  /// Create Kalman hits from SFT points.
  Int_t lay, nKH = 0, layMax = 0;
  //Double_t phi, r, coord, errR = 0.008, errRphi = 0.008; // 80um in R, 80um in R-Phi
  Double_t phi, r, coord, errR = 0.05, errRphi = 0.0170; // 500um in R, 170um in R-Phi

  fhLays->Reset();
  Int_t nSft = fSftPoints->GetEntriesFast(), iz = 0;
  cout << " Number of SFT points: " << nSft << endl;

  TVector3 pos;
  for (Int_t i = 0; i < nSft; ++i) {
    FairMCPoint *p = (FairMCPoint*) fSftPoints->UncheckedAt(i);
    p->Position(pos);
    Int_t mod = p->GetDetectorID() >> 5;
    lay = (p->GetDetectorID() & 15) - 1;
    lay += (mod-1) * 50;
    //if (lay > 2 && lay < 6) continue;
    layMax = TMath::Max (lay, layMax);
    iz = (p->GetZ() > 0) ? 0 : 1;
    fLayMinMax[iz][0] = TMath::Min (lay, fLayMinMax[iz][0]);
    fLayMinMax[iz][1] = TMath::Max (lay, fLayMinMax[iz][1]);
    fhLays->Fill(lay+0.1);
    // Add error                                            
    Double_t dRphi = 0, dR = 0;
    gRandom->Rannor(dRphi,dR); // add errors
    r = pos.Pt();
    phi = pos.Phi();
    //MpdKalmanHitZ *hit = new ((*fKHits)[nKH++]) MpdKalmanHitZ(r+dR*errR, phi, 0+dRphi*errRphi, 
    //						p->GetZ(), errR, errRphi, lay, i);
    Double_t meas[2] = {0+dRphi*errRphi, r+dR*errR};
    Double_t err[2] = {errRphi, errR};
    Double_t rphi = meas[1] * phi + meas[0];
    phi = rphi / meas[1];
    meas[0] = 0.0; //
    Double_t cosSin[2] = {TMath::Cos(phi), TMath::Sin(phi)};
    MpdKalmanHit *hit = new ( (*fKHits)[nKH++]) MpdKalmanHit(lay*1000000, 2, MpdKalmanHit::kFixedZ,
							     meas, err, cosSin, 9.0, p->GetZ(), i);
  }
  fKHits->Sort(); // in descending order in layer No. (in abs(Z))
  cout << " First and last layers: " << ((MpdKalmanHit*)fKHits->First())->GetLayer() << " "
       << ((MpdKalmanHit*)fKHits->Last())->GetLayer() << endl; 

  fLayPointers = new Int_t [layMax+1];
  Int_t ipos = 0;
  //for (Int_t i = 0; i <= layMax; ++i) {
  for (Int_t i = layMax; i >= 0; --i) {
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
void MpdSftTrackFinderTpc::GetTrackSeeds(Int_t iPass)
{
  /// Build track seeds from SFT hits and vertex

  Int_t nCand = fTracks->GetEntriesFast(), layMax = ((MpdKalmanHit*)fKHits->First())->GetLayer();
  TVector3 vert(0.,0.,0.), pmom;

  // Build track candidates from 2 SFT detectors (closest to the vertex)
  Int_t nLay[2], indx0[2];
  for (Int_t i = 0; i < 2; ++i) {
    Int_t lay = (iPass == 0) ? 1 - i : fLayMinMax[iPass][0] + 1 - i;
    nLay[i] = GetNofHitsInLayer(lay);
    indx0[i] = GetHitsInLayer(lay);
    //cout << lay << " " << nLay[i] << endl;
  }
  /*
  // Build track candidates from 2 SFT detectors (farthest from the vertex)
  Int_t nLay[2], indx0[2];
  for (Int_t lay = 0; lay < 2; ++lay) {
    nLay[lay] = GetNofHitsInLayer(layMax-lay);
    indx0[lay] = GetHitsInLayer(layMax-lay);
  }
  */
  FairHit hitTmp;
  TpcLheHit hit1Tmp;
  for (Int_t i = 0; i < nLay[0]; ++i) {
    // Loop over hits in the downstream SFT
    MpdKalmanHit *hit1 = (MpdKalmanHit*) fKHits->UncheckedAt(indx0[0]+i);
    Int_t id1 = ((FairMCPoint*)fSftPoints->UncheckedAt(hit1->GetIndex()))->GetTrackID();
    MpdMCTrack *mctrack = (MpdMCTrack*) fMCTracks->UncheckedAt(id1);
    if (mctrack->GetMotherId() >= 0) continue; // skip secondary tracks - for test
    hitTmp.SetZ(hit1->GetDist());
    
    for (Int_t j = 0; j < nLay[1]; ++j) {
      // Loop over hits in the upstream SFT
      MpdKalmanHit *hit2 = (MpdKalmanHit*) fKHits->UncheckedAt(indx0[1]+j);
      Int_t id2 = ((FairMCPoint*)fSftPoints->UncheckedAt(hit2->GetIndex()))->GetTrackID();
      if (id2 != id1) continue; // !!! exact match of 2 SFTs
      MpdEctKalmanTrack *track = 
	new ((*fTracks)[nCand++]) MpdEctKalmanTrack(-1, -1, &hitTmp, &hit1Tmp, vert);
      track->SetDirection(MpdKalmanTrack::kOutward);
      Double_t pt = EvalPt(track, hit1, hit2);
      ((FairMCPoint*) fSftPoints->UncheckedAt(hit2->GetIndex()))->Momentum(pmom);
      track->SetParam (4, 1./pt); // q/Pt
      //track->SetParam (4, 1./pmom.Pt()*TMath::Sign(1.,pt)); // q/Pt - exact value
      track->SetTrackID(id1);
      EvalCovar(hit1, hit2, track);
      cout << nCand << " " << id1 << " " << pmom.Pt() << " " << 1./track->GetParam(4) << " " << track->GetParam(0) << " " << track->GetParam(1) << " " << track->GetParam(2) << " " << track->GetParam(3) << " " << hit2->GetPhi() << endl;
      cout << pmom.Phi() << " " << TMath::PiOver2()-pmom.Theta() << endl;
      //track->SetParam(2,pmom.Phi()); // exact value
      //track->SetParam(3,TMath::PiOver2()-pmom.Theta()); // exact value
      track->SetParamNew (*track->GetParam());
      track->SetPos(hit2->GetDist());
      track->SetPosNew(hit2->GetDist());
      if (lunSft1) fprintf(lunSft1,"%10.3e %10.3e %10.3e %10.3e %10.3e %10.3e %10.3e %10.3e \n",track->GetParam(2),MpdKalmanFilter::Instance()->Proxim(track->GetParam(2),pmom.Phi()),track->GetParam(3),MpdKalmanFilter::Instance()->Proxim(track->GetParam(3),TMath::PiOver2()-pmom.Theta()),TMath::Sqrt((*track->Weight2Cov())(2,2)),TMath::Sqrt((*track->Weight2Cov())(3,3)),1./track->GetParam(4),pmom.Pt());
      //track->GetHits()->RemoveAt(0);
      track->GetHits()->Add(hit2);
      track->GetHits()->Compress();
    }
  }
  cout << " Number of SFT track candidates: " << nCand << " " << endl;
  //if (lunCpc) fclose(lunCpc);
}
  
//__________________________________________________________________________
Double_t MpdSftTrackFinderTpc::EvalPt(MpdKalmanTrack *track, const MpdKalmanHit *hit1, const MpdKalmanHit *hit2)
{
  /// Evaluate signed track Pt (curvature) assuming the track coming from the
  /// primary vertex and fill track parameters

  Double_t ph1 = hit1->GetMeas(0) / hit1->GetMeas(1) + hit1->GetPhi();
  //Double_t ph2 = hit2->GetRphi() / hit2->GetR() + hit2->GetPhi();
  Double_t ph2 = hit2->GetMeas(0) / hit2->GetMeas(1) + hit2->GetPhi();
  //Double_t x1 = hit1->GetR() * TMath::Cos(ph1);
  Double_t x1 = hit1->GetMeas(1) * TMath::Cos(ph1);
  Double_t y1 = hit1->GetMeas(1) * TMath::Sin(ph1);
  Double_t x2 = hit2->GetMeas(1) * TMath::Cos(ph2);
  Double_t y2 = hit2->GetMeas(1) * TMath::Sin(ph2);
  Double_t z1 = hit1->GetDist();
  Double_t z2 = hit2->GetDist();
  TVector2 vec1(x1-0.,y1-0.);
  TVector2 vec2(x2-0.,y2-0.);
  TVector2 vec21 = vec1 - vec2;
  Double_t cosAlpha = vec2 * vec21 / vec2.Mod() / vec21.Mod();
  Double_t rad = vec1.Mod() / 2. / TMath::Sin(TMath::ACos(cosAlpha));
  Double_t factor = 0.0015; // 0.3 * 0.5T * 0.01
  //factor *= 3; // 1.5T
  Double_t charge = ph1 - MpdKalmanFilter::Instance()->Proxim(ph1,ph2);
  if (hit1->GetLayer() < hit2->GetLayer()) charge = -charge;
  Double_t pt = factor * TMath::Abs(rad);
  track->SetParam(0,x2);
  track->SetParam(1,y2);
  track->SetParam(2,vec21.Phi());
  track->SetParam(3,TMath::ATan2(z1-z2,vec21.Mod()));
  //return TMath::Min(pt, 25.0) * TMath::Sign(1., -charge);
  return TMath::Min(pt, 25.0) * TMath::Sign(1., charge);
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::EvalCovar(const MpdKalmanHit *hitOut, const MpdKalmanHit *hitIn,
				     MpdEctKalmanTrack *track)
{
  /// Evaluate covariance matrix for track seed

  TMatrixD ww(5,5);
  //ww(0,0) = hitOut->GetRphiErr() * hitOut->GetRphiErr(); // <RphiRphi>
  ww(0,0) = hitOut->GetErr(0) * hitOut->GetErr(0); // <RphiRphi>
  //ww(0,0) *= 4.; // extra factor of 2

  ww(1,1) = hitOut->GetErr(1) * hitOut->GetErr(1); // <RR>

  //Double_t phiOut = hitOut->GetPhi();
  //Double_t phiIn = hitIn->GetPhi();
  Double_t phiOut = hitOut->GetMeas(0) / hitOut->GetMeas(1) + hitOut->GetPhi();
  Double_t phiIn = hitIn->GetMeas(0) / hitIn->GetMeas(1) + hitIn->GetPhi();
  Double_t xIn = hitIn->GetMeas(1) * TMath::Cos(phiIn);
  Double_t yIn = hitIn->GetMeas(1) * TMath::Sin(phiIn);
  Double_t xOut = hitOut->GetMeas(1) * TMath::Cos(phiOut);
  Double_t yOut = hitOut->GetMeas(1) * TMath::Sin(phiOut);
  Double_t dx = xOut - xIn, dy = yOut - yIn;
  Double_t dist2 = dx * dx + dy * dy;
  Double_t sinPhi = TMath::Sin (track->GetParam(2));
  Double_t cosPhi = TMath::Cos (track->GetParam(2));
  Double_t pOut = TMath::Cos(phiOut) * cosPhi + TMath::Sin(phiOut) * sinPhi;
  Double_t pIn = TMath::Cos(phiIn) * cosPhi + TMath::Sin(phiIn) * sinPhi;
  ww(2,2) = (pOut * pOut + pIn * pIn) / dist2 * ww(0,0); // <PhiPhi>
  //ww(2,2) /= 50.; // extra factor of 2

  Double_t tanThe = TMath::Tan(track->GetParam(3));
  Double_t dRad = hitOut->GetMeas(1) - hitIn->GetMeas(1);
  Double_t denom = dRad * (1.+tanThe*tanThe);
  ww(3,3) = ww(1,1) * 2. / denom / denom; // <TheThe>
  ww(3,3) *= 4;

  //ww(4,4) = (track->GetParam(4)*0.5) * (track->GetParam(4)*0.5); // error 50%
  //(*fWeight)(4,4) = ((*fParam)(4,0)*0.75) * ((*fParam)(4,0)*0.75); // error 75%
  ww(4,4) = (track->GetParam(4)*1.) * (track->GetParam(4)*1.); // error 100%

  // Error transformation from R-Rphi to X-Y
  /*
  Double_t cosp = TMath::Cos(phiIn);
  Double_t sinp = TMath::Sin(phiIn);
  Double_t xx = cosp * cosp * ww(1,1) + sinp * sinp * ww(0,0);
  Double_t yy = sinp * sinp * ww(1,1) + cosp * cosp * ww(0,0);
  Double_t xy = 2 * sinp * cosp * ww(1,1) * ww(0,0);
  ww(0,0) = xx * 1;
  ww(1,1) = yy;
  ww(0,1) = ww(1,0) = xy;
  */

  //fWeight->Print();
  //fWeight->Invert(); // weight matrix
  Int_t iok = 0;
  MpdKalmanFilter::Instance()->MnvertLocal(ww.GetMatrixArray(), 5, 5, 5, iok);
  track->SetWeight(ww);
  //fWeight->Print();
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::DoTracking(Int_t iPass)
{
  /// Run Kalman tracking
  
  Int_t nCand = fTracks->GetEntriesFast(), iok = 0;
 
  for (Int_t i = 0; i < nCand; ++i) {
  //for (Int_t i = 20; i < 21; ++i) {
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
    const Int_t layMax = 1; //10;
    MpdEctKalmanTrack tracks[layMax];

    Int_t lay = ((MpdKalmanHit*)track->GetHits()->UncheckedAt(0))->GetLayer() + 1;
    for (Int_t j = 0; j < layMax; ++j) {
      tracks[j] = *track;
      //cout << track->GetParamNew(0) << endl;
      //cout << i << " " << lay << " " << tracks[lay].GetNofHits() << " " << tracks[lay].GetChi2() << " " << tracks[lay].GetParam(0) << endl;
      if (j > 0) {
	if (tracks[j-1].GetNofHits() > 2) // 2 hits from CPC there
	  lay = ((MpdKalmanHit*)tracks[j-1].GetHits()->UncheckedAt(2))->GetLayer() - 1;
	else break; // no more hits found for track
      }
      iok = RunKalmanFilter(&tracks[j], lay);
      //iok = RunKalmanFilter(&tracks[lay], 0);
      cout << j << " " << lay << " " << tracks[j].GetNofHits() << " " << tracks[j].GetChi2() << endl;
    }

    // Select the best track (with max number of hits)
    Int_t nHitsMax = tracks[0].GetNofHits(), iMax = 0;
    for (Int_t j = 1; j < layMax; ++j) {
      Int_t nhits = tracks[j].GetNofHits();
      if (nhits == 0) break;
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
    fTracks->RemoveAt(i);
    new ((*fTracks)[i]) MpdEctKalmanTrack(tracks[iMax]);

    // Refit
    if (1) {
      //track->Weight2Cov()->Print();
      //track->GetParamNew()->Print();
      MpdKalmanHit hitTmp;
      hitTmp.SetType(MpdKalmanHit::kFixedZ);
      hitTmp.SetDist(track->GetPosNew()+TMath::Sign(0.2,track->GetPosNew()));
      MpdKalmanFilter::Instance()->PropagateToHit(track, &hitTmp);
      track->SetDirection(MpdKalmanTrack::kInward);
      //cout << " Chi2 = " << track->GetChi2() << endl;
      MpdKalmanFilter::Instance()->Refit(track, 1);
      //track->GetParamNew()->Print();
      //track->SetDirection(MpdKalmanTrack::kOutward);
      //MpdKalmanFilter::Instance()->Refit(track, 1);
    }

    cout << i << " " << track->GetNofHits() << " " << 1 / track->GetParamNew(4) << endl;
    
  } //for (Int_t i = 0; i < nCand;

  FollowInTPC();
}
    
//__________________________________________________________________________
void MpdSftTrackFinderTpc::FollowInTPC()
{
  /// Follow tracks in TPC
  
  // Manage TPC hits
  if (fTpcHits->GetEntriesFast() == 0) { cout << " !!! No TPC hits" << endl; return; }
  fTpcHits->Sort();
  fhLays->Reset();
  Int_t nHits = fTpcHits->GetEntriesFast(), layMax = 0;
  for (Int_t i = 0; i < nHits; ++i) {
    MpdKalmanHit *hit = (MpdKalmanHit*) fTpcHits->UncheckedAt(i);
    Int_t lay = hit->GetLayer();
    fhLays->Fill(lay+0.1);
    layMax = TMath::Max (layMax, lay);
  }
  delete [] fLayPointers;
  fLayPointers = new Int_t [layMax+1];
  Int_t ipos = 0;
  for (Int_t i = layMax; i > -1; --i) {
    Int_t cont = TMath::Nint (fhLays->GetCellContent(i+1,0));
    if (cont == 0) {
      fLayPointers[i] = -1;
      continue;
    }
    fLayPointers[i] = ipos;
    ipos += cont;
  }

  Int_t nCand = fTracks->GetEntriesFast(), iok = 0;
  for (Int_t i = 0; i < nCand; ++i) {
    MpdEctKalmanTrack *track = (MpdEctKalmanTrack*) fTracks->UncheckedAt(i);
    // Propagate track to TPC first hit (-0.5 cm)
    /*
    MpdKalmanHit *hit = (MpdKalmanHit*) fTpcHits->Last();
    MpdKalmanHitZ hitZ;
    hitZ.SetZ (hit->GetZ() - 0.5);
    MpdKalmanFilter::Instance()->PropagateToHit(track,&hitZ,kTRUE);
    */
    /*
    // Propagate track to TPC inner shell
    MpdKalmanHit hitR;
    hitR.SetType(MpdKalmanHit::kFixedR);
    hitR.SetDist (20.);
    MpdKalmanFilter::Instance()->PropagateToHit(track,&hitR,kTRUE);
    // Propagate thru TPC inner shell
    PassInnerShell(track);
    iok = RunKalmanFilterTPC(track, 0);
    */
    Int_t nh1 = track->GetNofHits();
    iok = RunKalmanFilterTPC(track, layMax);
    if (iok < 0 || track->GetNofHits() == nh1) {
      fTracks->RemoveAt(i);
      continue; // failed propagation or no TPC hits added
    }

    track->Weight2Cov();
    MpdKalmanHit hitR;
    hitR.SetType(MpdKalmanHit::kFixedR);
    hitR.SetDist(10.);
    MpdKalmanFilter::Instance()->PropagateToHit(track,&hitR,kTRUE);
    track->SetParam(*track->GetParamNew());
  }
  fTracks->Compress();
}    

//__________________________________________________________________________
Int_t MpdSftTrackFinderTpc::RunKalmanFilter(MpdEctKalmanTrack *track, Int_t layBeg)
{
  /// Run Kalman filter

  //const Double_t rMin = -28., rMax = 123.; // min and max radial size of ECT - to be taken elsewhere
  const Double_t rMin = -28., rMax = 170.; // min and max radial size of ECT - to be taken elsewhere
  //cout << fHits->GetEntriesFast() << endl;
  //Int_t layMax = ((MpdKalmanHit*)fKHits->Last())->GetLayer();
  Int_t layMax = ((MpdKalmanHit*)fKHits->First())->GetLayer();
  MpdKalmanHit *hitOK = 0x0;
  MpdKalmanTrack::TrackDir trackDir = track->GetDirection();
  //Int_t layBeg = layMax, layEnd = -1, dLay = -1, layOK = -1;
  Int_t layEnd = -1, dLay = -1, layOK = -1;
  if (trackDir == MpdKalmanTrack::kOutward) {
    layEnd = layMax + 1;
    dLay = 1;
  }
  layEnd = (track->GetPos() > 0) ? fLayMinMax[0][1] + 1 : fLayMinMax[1][1] + 1;
  
  //Int_t indxOK = hits->IndexOf(hitOK);
  //Int_t nHits = hits->GetEntriesFast();
  Int_t miss = 0, istop = 0;
  TMatrixDSym pointWeight(5), pointWeightTmp(5), saveWeight(5);
  TMatrixD param(5,1), paramTmp(5,1);

  Double_t saveZ = 0;
  //cout << " Starting hit: " << hitOK->GetLayer() << " " << hitOK->GetTrackID() << " " << hitOK->GetUsage() << endl;
  
  for (Int_t lay = layBeg; lay != layEnd; lay+=dLay) {
    Int_t nLay = GetNofHitsInLayer(lay);
    Int_t indx0 = GetHitsInLayer(lay);
    Double_t dChi2Min = 1.e+6;
    MpdKalmanHit *hitMin = 0x0;
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
      MpdKalmanHit *hit = (MpdKalmanHit*) fKHits->UncheckedAt(indx0+indx);
      if (track->GetPosNew() * hit->GetDist() < 0) { istop = 1; break; } // hit with opposite Z - finish
      // !!! Exact ID match
      if (((FairMCPoint*)fSftPoints->UncheckedAt(hit->GetIndex()))->GetTrackID() != track->GetTrackID()) continue;

      // Exclude used hits
      if (hit->GetFlag() != 1) continue;
      //cout << " window:" << /*hit->GetTrackID() <<*/ " " << hit->GetRphi() << " " << track->GetParamNew(0) << " " << hit->GetZ() << " " << track->GetParamNew(1) << endl;
      // Check if the hit within some window (15x15cm for the moment - check!!!)
      //if (TMath::Abs(hit->GetRphi()-track->GetParamNew(0)) > 9) continue;
      //if (TMath::Abs(Proxim(track,hit)-track->GetParamNew(0)) > 15) continue;
      TVector2 dist = GetDistance(track, hit);
      if (dist.X() > 15.) continue; // distance in transverse to the tube direction
      if (hit->GetNofDim() > 1 && dist.Y() > 25.) continue; // distance in R 
      // For tests
      //FairMCPoint *mc = (FairMCPoint*) fEctHits->UncheckedAt(hit->GetIndex());
      //*if (hit->GetTrackID() == 186)*/ cout << " Hit: " << mc->GetTrackID() << " " << hit->GetLayer() << " " << hit->GetR()*TMath::Cos(hit->GetRphi()/hit->GetR()) << " " << hit->GetR()*TMath::Sin(hit->GetRphi()/hit->GetR()) << " " << hit->GetR() << " " << hit->GetZ() << "; Track: " << track->GetParamNew(1)*TMath::Cos(track->GetParamNew(0)/track->GetParamNew(1)) << " " << track->GetParamNew(1)*TMath::Sin(track->GetParamNew(0)/track->GetParamNew(1)) << " " << track->GetParamNew(1) << " " << track->GetPosNew() << "; Dist: " << dist.X() << " " << dist.Y() << endl;
      //track->Print("");
      //hit->Print("");
      if (!MpdKalmanFilter::Instance()->PropagateToHit(track,hit)) return -1;
      // For tests
      //FairMCPoint *mc = (FairMCPoint*) fEctHits->UncheckedAt(hit->GetIndex());
      FairMCPoint *mc = (FairMCPoint*) fSftPoints->UncheckedAt(hit->GetIndex());
      //*if (hit->GetTrackID() == 186)*/ cout << " Hit: " << mc->GetTrackID() << " " << hit->GetLayer() << " " << mc->GetX() << " " << mc->GetY() << " " << hit->GetDist() << " " << hit->GetPhi() << "; Track: " << track->GetParamNew(0)  << " " << track->GetParamNew(1) << " " << track->GetPosNew() << "; Dist: " << dist.X() << " " << dist.Y() << endl;
      //
      // For debugging
      /*
      TVector2 dist0 = GetDistance(track, hit);
      cout << dist0.X() << " " << dist0.Y() << endl;
      *.
      /*
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

      Double_t dChi2 = MpdKalmanFilter::Instance()->FilterHitZ(track,hit,pointWeight,param);
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
        /*cout << " New min dChi2 = " << dChi2 << " " << hitMin->GetMeas(0) << " " << hitMin->GetMeas(1) << " " << ((FairMCPoint*)fSftPoints->UncheckedAt(hit->GetIndex()))->GetTrackID() << endl;
	cout << 1/param(4,0) << " " << endl;*/
      }
    } // for (Int_t indx = indxBeg; indx != indxEnd;
    if (istop) break;

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
    //track->GetParamNew()->Print();
    //cout << " lay, miss: " << lay << " " << miss << " " << dChi2Min << " " << fChi2 << endl;
  } // for (Int_t lay = layBeg; lay != layEnd;
  // Update track params to those at the last accepted hit
  if (miss == 0) {
    track->SetWeight(saveWeight);
    track->SetPos(saveZ);
    track->SetParam(paramTmp);
  } else {
    track->SetPos(track->GetPosNew());
    track->SetParam(*track->GetParamNew());
  }    
  return 0;
}

//__________________________________________________________________________
Int_t MpdSftTrackFinderTpc::RunKalmanFilterTPC(MpdEctKalmanTrack *track, Int_t layBeg)
{
  /// Run Kalman filter in TPC

  // Get TpcKalmanFilter task
  MpdTpcKalmanFilter *tpcKF = (MpdTpcKalmanFilter*) FairRun::Instance()->GetTask("TPC Kalman filter");
  if (tpcKF == NULL) Fatal("RunKalmanFilterTPC", "!!! No TPC kalman filter task is found !!! ");

  MpdKalmanHit *hitOK = 0x0;
  Int_t layEnd = -1, dLay = -1, layOK = -1;
  Int_t layMax = ((MpdKalmanHit*)fTpcHits->UncheckedAt(0))->GetLayer();
  if (track->GetDirection() == MpdKalmanTrack::kOutward) {
    layEnd = layMax + 1;
    dLay = 1;
  }
  
  Int_t miss = 0;
  TMatrixDSym pointWeight(5), pointWeightTmp(5), saveWeight(5);
  TMatrixD param(5,1), paramTmp(5,1);

  Double_t saveR = 0;
  //cout << " Starting hit: " << hitOK->GetLayer() << " " << hitOK->GetTrackID() << " " << hitOK->GetUsage() << endl;
  
  for (Int_t lay = layBeg; lay != layEnd; lay+=dLay) {
    Int_t nLay = GetNofHitsInLayer(lay);
    if (nLay == 0) continue;
    Int_t indx0 = GetHitsInLayer(lay);
    Double_t dChi2Min = 1.e+6;
    MpdKalmanHit *hitMin = 0x0;
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
    for (Int_t indx = indxBeg; indx != indxEnd; indx+=dIndx) {
      MpdKalmanHit *hit = (MpdKalmanHit*) fTpcHits->UncheckedAt(indx0+indx);
      // !!! Exact ID match
      //if (((FairMCPoint*)fTpcPoints->UncheckedAt(hit->GetIndex()))->GetTrackID() != track->GetTrackID()) continue;
      if (tpcKF->GetHitID(hit) != track->GetTrackID()) continue;

      // Exclude used hits
      if (hit->GetFlag() != 1) continue;
      if (track->GetType() == MpdKalmanTrack::kBarrel) {
	// Check if the hit within some window
	//if (TMath::Abs(hit->GetMeas(0)-track->GetParamNew(0)) > 9) continue;
	//if (TMath::Abs(Proxim(track,hit)-track->GetParamNew(0)) > 15) continue;
	if (TMath::Abs(Proxim(track,hit)-track->GetParamNew(0)) / hit->GetPos() > TMath::PiOver4()) continue;
      }
      //*if (hit->GetTrackID() == 186)*/ cout << " Hit: " << ((FairMCPoint*)fEctHits->UncheckedAt(hit->GetIndex()))->GetTrackID() << " " << hit->GetLayer() << " " << ((FairMCPoint*)fEctHits->UncheckedAt(hit->GetIndex()))->GetDetectorID() << " " << hit->GetRphi() << " " << hit->GetR() << " " << hit->GetZ() << " " << dist.X() << " " << dist.Y() << " " << track->GetParamNew(0) << " " << track->GetParamNew(1) << endl;
      //track->Print("");
      //hit->Print("");
      if (!MpdKalmanFilter::Instance()->PropagateToHit(track,hit)) return -1;
      /*Debug
      cout << " Hit: " << hit->GetLayer() << " " << hit->GetMeas(0) << " " << hit->GetMeas(1) << " " 
	   << hit->GetDist() << ", Track: " << track->GetParamNew(0) << " " << track->GetParamNew(1) 
	   << " " << track->GetPosNew() << " " << track->GetPos() << endl; 
      */

      Double_t dChi2 = MpdKalmanFilter::Instance()->FilterHitR(track,hit,pointWeight,param);
      //if (track->GetNofHits() == 0) hit->SetRphiErr(err);
      //if (param(3,0) < 0) { cout << " Check: " << param(3,0) << " " << dChi2 << " " << (*fParamNew)(3,0) << " " << hit->GetRphi() << " " << hit->GetZ() << endl; fParamNew->Print();}
      if (dChi2 < dChi2Min) {
        dChi2Min = dChi2;
        hitMin = hit;
        saveWeight = *track->GetWeight();
        saveR = track->GetPosNew();
        // temporary storage for the current track
        paramTmp = param;
        pointWeightTmp = pointWeight;
        /*cout << " New min dChi2 = " << dChi2 << " " << hitMin->GetMeas(0) << " " << hitMin->GetMeas(1) 
	     << " " << tpcKF->GetHitID(hit) << endl;
	cout << 1/param(4,0) << " " << endl;*/
      }
    }
    if (dChi2Min < fgkChi2Cut) {
      //layOK = lay;
      hitOK = hitMin;
      track->GetHits()->Add(hitOK);
      track->SetTpcIndex(track->GetTrackID()); // 
      miss = 0;
      // Restore track params at the best hit
      track->SetChi2(track->GetChi2()+dChi2Min);
      saveWeight += pointWeightTmp;
      track->SetWeight(saveWeight);
      track->SetPosNew(saveR);
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
  } // for (Int_t lay = layBeg; lay != layEnd;
  // Update track params to those at the last accepted hit
  if (miss == 0) {
    track->SetWeight(saveWeight);
    track->SetPos(saveR);
    track->SetParam(paramTmp);
  } else {
    track->SetPos(track->GetPosNew());
    track->SetParam(*track->GetParamNew());
  }    
  return 0;
}

//__________________________________________________________________________
TVector2 MpdSftTrackFinderTpc::GetDistance(MpdEctKalmanTrack *track, MpdKalmanHit *hit)
{
  /// Compute distance between track and hit

  Double_t xTr, yTr;
  if (track->GetType() == MpdKalmanTrack::kEndcap) {
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
  //TVector2 dist(yLoc-hit->GetRphi(), xLoc-hit->GetR());
  TVector2 dist(TMath::Abs(yLoc-hit->GetMeas(0)), TMath::Abs(xLoc-hit->GetMeas(1)));
  return dist;
}

//__________________________________________________________________________
Double_t MpdSftTrackFinderTpc::Proxim(MpdKalmanTrack *track, const MpdKalmanHit *hit)
{
  /// Adjust hit coord. R-Phi to be "around" track R-Phi - to avoid 
  /// discontinuity around +- Pi

  Double_t hitPhi = hit->GetMeas(0) / hit->GetDist();
  Double_t phi0 = track->GetParamNew(0) / track->GetPosNew();
  return hit->GetDist() * MpdKalmanFilter::Instance()->Proxim(phi0, hitPhi);
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::Write()
{
  /// Write

  TFile histoFile("EctRec.root","RECREATE");
  Writedir2current(fHistoDir);
  histoFile.Close();
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::Writedir2current( TObject *obj )
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
void MpdSftTrackFinderTpc::RemoveDoubles()
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

  // Remove double tracks (originated from the same ETOF hit)
  /*
  nFound = fTracks->GetEntriesFast();
  for (Int_t i = 0; i < nFound; ++i) {
    MpdEctKalmanTrack *track = (MpdEctKalmanTrack*) fTracks->UncheckedAt(i);
    if (track == 0x0) continue;
    Int_t iTof = track->GetTofIndex();
    Int_t nh = track->GetNofHits();
    for (Int_t j = i + 1; j < nFound; ++j) {
      MpdEctKalmanTrack *track1 = (MpdEctKalmanTrack*) fTracks->UncheckedAt(j);
      if (track1 == 0x0) continue;
      Int_t iTof1 = track1->GetTofIndex();
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
  */
}
	  
//__________________________________________________________________________
Int_t MpdSftTrackFinderTpc::NofCommonHits(MpdEctKalmanTrack* track, MpdEctKalmanTrack* track1)
{
  /// Compute number of common hits on 2 tracks

  TObjArray *hits = track->GetHits(), *hits1 = track1->GetHits();
  
  MpdKalmanHit *hit = (MpdKalmanHit*) hits->First();
  MpdKalmanHit *hit1 = (MpdKalmanHit*) hits1->First();
  Int_t dir = 1;
  if (hit->GetLayer() < ((MpdKalmanHit*) hits->Last())->GetLayer()) dir = -1;

  Int_t nCom = 0;
  while (hit && hit1) {
    if (dir * hit->GetLayer() < dir * hit1->GetLayer()) {
      hit1 = (MpdKalmanHit*) hits1->After(hit1);
      continue;
    }
    if (hit == hit1) ++nCom;
    hit = (MpdKalmanHit*) hits->After(hit);
  }
  return nCom;
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::AddHits()
{
  /// Add hit objects to tracks and compute number of wrongly assigned hits
  /// (hits with ID different from ID of starting track)

  MpdTpcKalmanFilter *tpcKF = (MpdTpcKalmanFilter*) FairRun::Instance()->GetTask("TPC Kalman filter");

  Int_t nFound = fTracks->GetEntriesFast();
  //FairMCPoint *p;
  for (Int_t i = 0; i < nFound; ++i) {
    MpdEctKalmanTrack *track = (MpdEctKalmanTrack*) fTracks->UncheckedAt(i);
    Int_t nHits = track->GetNofHits();
    if (nHits == 0) { fTracks->RemoveAt(i); continue; }
    track->SetNofHits(nHits);
    TClonesArray &trHits = *track->GetTrHits();
    //SetTrackID(track); // set track ID as ID of majority of its hits
    TObjArray *hits = track->GetHits();
    Int_t nWrong = 0, motherID = track->GetTrackID();
    cout << i << " " << nHits << " " << motherID << endl;
    // Get track mother ID 
    MpdMCTrack *mctrack = (MpdMCTrack*) fMCTracks->UncheckedAt(motherID);
    while (mctrack->GetMotherId() > 0) {
      motherID = mctrack->GetMotherId();
      mctrack = (MpdMCTrack*) fMCTracks->UncheckedAt(mctrack->GetMotherId());
    }

    for (Int_t j = 0; j < nHits; ++j) {
      MpdKalmanHit *hit = (MpdKalmanHit*) hits->UncheckedAt(j);
      new (trHits[j]) MpdKalmanHit(*hit);
      //MpdKalmanHit *hit = (MpdKalmanHit*) hits->UncheckedAt(j);
      //new (trHits[j]) MpdKalmanHit(*hit);
      cout << " " << hit->GetLayer();
      //Int_t iproj = (hit->GetLayer() - 1) % 3;
      Int_t iproj = -1, motherID1 = -1;
      //if (hit->GetType() == MpdKalmanHit::kFixedZ) p = (FairMCPoint*) fSftPoints->UncheckedAt(hit->GetIndex()); // SFT
      if (hit->GetType() == MpdKalmanHit::kFixedZ) 
	motherID1 = ((FairMCPoint*) fSftPoints->UncheckedAt(hit->GetIndex()))->GetTrackID(); // SFT
      else {
        //p = (FairMCPoint*) fTpcPoints->UncheckedAt(hit->GetIndex()); // TPC
	motherID1 = tpcKF->GetHitID(hit);
	iproj = -1;
      }
      if (iproj == 0) cout << "U";
      else if (iproj == 1) cout << "R";
      else if (iproj > 0) cout << "V";
      //Int_t motherID1 = p->GetTrackID();
      cout << "-" << motherID1;
      // Get point mother ID 
      MpdMCTrack *mctrack = (MpdMCTrack*) fMCTracks->UncheckedAt(motherID1);
      while (mctrack->GetMotherId() > 0) {
        motherID1 = mctrack->GetMotherId();
        mctrack = (MpdMCTrack*) fMCTracks->UncheckedAt(mctrack->GetMotherId());
      }
      //if (motherID1 != motherID && hit->GetMult() == 1) nWrong++;
      if (motherID1 != motherID) nWrong++;
    }
    cout << "\nWrongs: " << nWrong << " " << track->GetTrackID() << " " << motherID << endl;
    track->SetNofWrong(nWrong);
    track->SetLastLay(((MpdKalmanHit*)track->GetHits()->First())->GetLayer());
  }
  fTracks->Compress();
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::SetTrackID(MpdEctKalmanTrack* track)
{
  /// Set track ID as ID of majority of its hits

  const Int_t idMax = 99999;
  Int_t ids[idMax+1] = {0}, nHits = track->GetNofHits(), locMax = 0;
  TObjArray *hits = track->GetHits();

  for (Int_t i = 0; i < nHits; ++i) {
    MpdKalmanHit *hit = (MpdKalmanHit*) hits->UncheckedAt(i);
    FairMCPoint *p = (FairMCPoint*) fSftPoints->UncheckedAt(hit->GetIndex());
    Int_t id = p->GetTrackID();
    if (id > idMax) exit(0);
    ++ids[id];
    if (ids[id] > ids[locMax]) locMax = id;
  }
  if (ids[locMax] > 1) track->SetTrackID(locMax);
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::RemoveTpcTracks()
{
  /// Remove TPC tracks which are parts of the combined ones
  
  MpdTpcKalmanFilter *tpcKF = (MpdTpcKalmanFilter*) FairRun::Instance()->GetTask("TPC Kalman filter");

  TClonesArray *tpcTracks = tpcKF->GetTracks();
  Int_t nTr = fTracks->GetEntriesFast(), nTpc = tpcTracks->GetEntriesFast();

  for (Int_t i = 0; i < nTr; ++i) {
    MpdEctKalmanTrack *tr = (MpdEctKalmanTrack*) fTracks->UncheckedAt(i);
    if (tr == NULL) continue;
    Int_t id = tr->GetTrackID();
    if (tr->GetTpcIndex() >= 0) {
      // There are TPC hits
      for (Int_t j = 0; j < nTpc; ++j) {
	MpdTpcKalmanTrack *tpc = (MpdTpcKalmanTrack*) tpcTracks->UncheckedAt(j);
	if (tpc == NULL) continue;
	if (tpc->GetTrackID() != tr->GetTrackID()) continue;
	if (tpc->GetNofTrHits() < tr->GetNofTrHits()) { 
	  tpcTracks->RemoveAt(j);
	} else {
	  cout << tpc->GetNofTrHits() << " " << tr->GetNofTrHits() << " " << tr->GetTrackID() 
	       << " " << tpc->Momentum3().Eta() << endl;
	  //Fatal("RemoveTpcTrack", "Strange ???");
	  Warning("RemoveTpcTrack", "Strange ???");
	  fTracks->RemoveAt(i);
	}
      }
    }
  }
  tpcTracks->Compress();
  fTracks->Compress();
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::GoToVertex()
{
  /// Propagate tracks to the vertex
  
  //return;
  Int_t nTr = fTracks->GetEntriesFast();
  // Save last parameters
  /*
  for (Int_t i = 0; i < nTr; ++i) {
    MpdEctKalmanTrack *track = (MpdEctKalmanTrack*) fTracks->UncheckedAt(i);
    //cout << track->GetPos() << " " << track->GetPosNew() << endl;
    track->SetPos(track->GetPosNew());
    track->SetParam(*track->GetParamNew());
  }
  return;
  */
  
  TMatrixD param(5,1);
  TMatrixDSym weight(5), pointWeight(5);
  MpdKalmanHit hitv;
  hitv.SetType(MpdKalmanHit::kFixedZ);
  hitv.SetErr(1,0.0005); // 50um
  hitv.SetErr(0,0.0005); // 50um
  //hitv.SetRerr(0.05); // 1mm
  //hitv.SetRphiErr(0.05); // 1mm
  //hitv.SetPhi(TMath::PiOver2());
  hitv.SetCosSin(0,0.0);
  hitv.SetCosSin(1,1.0);
  hitv.SetNofDim(2);
 
  for (Int_t i = 0; i < nTr; ++i) {
    MpdEctKalmanTrack *track = (MpdEctKalmanTrack*) fTracks->UncheckedAt(i);
    cout << " Refit0: " << i << " " << track->GetNofTrHits() << " " << track->GetChi2() << " " 
	 << 1./track->GetParam(4) << " " << 1./track->GetParamNew(4) << endl;
    //MpdKalmanFilter::Instance()->Refit(track, -1);
    cout << " Refit1: " << track->GetNofTrHits() << " " << track->GetChi2() << " " 
	 << 1./track->GetParam(4) << " " << 1./track->GetParamNew(4) << endl;
    MpdKalmanFilter::Instance()->PropagateToHit(track,&hitv);
    //TVector2 dist = GetDistance(track,&hitv);
    //cout << dist.X() << " " << dist.Y() << endl;
    cout << track->GetParamNew(0) << " " << track->GetParamNew(1) << endl;
    /*
    Double_t dChi2 = MpdKalmanFilter::Instance()->FilterHitZ(track,&hitv,pointWeight,param);
    track->SetChi2(track->GetChi2()+dChi2);
    weight = *track->GetWeight();
    weight += pointWeight;
    track->SetWeight(weight);
    track->SetParam(param);
    cout << " Vertex: " << 1./track->GetParam(4) << endl;
    */
  }
}

//__________________________________________________________________________
void MpdSftTrackFinderTpc::PassInnerShell(MpdKalmanTrack* track)
{
  /// Propagate track thru TPC inner shell (include MS)

  const Int_t nR = 7;
  const Double_t rad[nR] = {20.035, 21.07, 22.105, 24.64, 27.15, 28.165, 29.180};
  const Double_t dx[nR] = {0.0031, 0.0017, 0.0031, 0.0003, 0.001, 0.0017, 0.0013};

  MpdKalmanHit hit;
  hit.SetType(MpdKalmanHit::kFixedR);
  for (Int_t i = 0; i < nR; ++i) {
    hit.SetDist(rad[i]);
    MpdKalmanFilter::Instance()->PropagateToHit(track,&hit,kTRUE);
    // Add multiple scattering
    TMatrixDSym *cov = track->Weight2Cov();
    Double_t th = track->GetParamNew(3);
    Double_t cosTh = TMath::Cos(th);
    Double_t angle2 = MpdKalmanFilter::Instance()->Scattering(track, dx[i]);
    //cov->Print();
    (*cov)(2,2) += (angle2 / cosTh / cosTh );
    (*cov)(3,3) += angle2;
    //cov->Print();
    Int_t iok = 0;
    MpdKalmanFilter::Instance()->MnvertLocal(cov->GetMatrixArray(), 5, 5, 5, iok);
    track->SetWeight(*cov);
  }
}

ClassImp(MpdSftTrackFinderTpc);
