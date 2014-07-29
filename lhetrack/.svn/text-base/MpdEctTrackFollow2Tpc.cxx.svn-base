// -------------------------------------------------------------------------
// -----                  MpdEctTrackFollow2Tpc source file             -----
// -----                 Created 25/08/08  by A. Zinchenko             -----
// -------------------------------------------------------------------------

/**  MpdEctTrackFollow2Tpc.h
 *@author A.Zinchenko <Alexander.Zinchenko@jinr.ru>
 **
 ** Track propagator from MPD End-Cap Tracker (ECT) into TPC
 **/

#include "MpdEctTrackFollow2Tpc.h"
#include "MpdKalmanFilter.h"
#include "MpdKalmanHitR.h"
#include "MpdKalmanHitZ.h"
#include "MpdEctKalmanTrack.h"
#include "MpdStrawendcapPoint.h"
#include "MpdEtofPoint.h"
#include "TpcPoint.h"
#include "TpcLheHit.h"

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

const Double_t MpdEctTrackFollow2Tpc::fgkChi2Cut = 25; //10; //20; //100;
const Double_t MpdEctTrackFollow2Tpc::fgkLayCut = 10;

//__________________________________________________________________________
MpdEctTrackFollow2Tpc::MpdEctTrackFollow2Tpc(const char *name, Int_t iVerbose )
  :FairTask(name, iVerbose),
   fKHits(new TClonesArray("MpdKalmanHitR")),
   fTracks(new TClonesArray("MpdEctKalmanTrack", 100)),
   fHistoDir(0x0),
   fhLays(new TH1F("hLays","TPC layers",100,0,100)),
   fLayPointers(0x0)
{
}


//__________________________________________________________________________
MpdEctTrackFollow2Tpc::~MpdEctTrackFollow2Tpc()
{
  delete fKHits;
  //delete fTracks;
  //delete fTrackCand;
  delete [] fLayPointers;
  delete fhLays;
}

//__________________________________________________________________________
InitStatus MpdEctTrackFollow2Tpc::Init()
{
  return ReInit();
}

//__________________________________________________________________________
InitStatus MpdEctTrackFollow2Tpc::ReInit()
{
  fEctPoints =(TClonesArray *) FairRootManager::Instance()->GetObject("STRAWPoint");
  fTpcPoints =(TClonesArray *) FairRootManager::Instance()->GetObject("TpcPoint");
  fTpcHits = (TClonesArray *) FairRootManager::Instance()->GetObject("LheHit");
  fEctTracks = (TClonesArray *) FairRootManager::Instance()->GetObject("EctTrack");
  fMCTracks =(TClonesArray *) FairRootManager::Instance()->GetObject("MCTrack");
  fTpcTracks = 0x0; //(TClonesArray *) FairRootManager::Instance()->GetObject("TpcPoint"); // just for testing
  //fSTSTrackMatch = (TClonesArray*) FairRootManager::Instance()->GetObject("STSTrackMatch");
  //fPrimVtx =  (FairVertex *) FairRootManager::Instance() ->GetObject("PrimaryVertex");

  FairRootManager::Instance()->Register("EctTpcTrack", "Ect", fTracks, kTRUE);

  fNPass = 1;
  return kSUCCESS;
}

//__________________________________________________________________________
void MpdEctTrackFollow2Tpc::Reset() 
{
  ///
  cout << " MpdEctTrackFollow2Tpc::Reset  " << endl;

  fKHits->Clear();
  fTracks->Clear("C");
  delete [] fLayPointers;
}

//__________________________________________________________________________
void MpdEctTrackFollow2Tpc::SetParContainers()
{
}

//__________________________________________________________________________
void MpdEctTrackFollow2Tpc::Finish()
{
  //Write();
}

//__________________________________________________________________________
void MpdEctTrackFollow2Tpc::Exec(Option_t * option)
{

  static int eventCounter = 0;    
  cout << " EctRec event " << ++eventCounter << endl;

  Reset();

  // Get TPC Kalman hits
  GetTpcHits();

  for (Int_t i = 0; i < fNPass; ++i) {
    fTracks->Clear();
    cout << "  Total number of ECT tracks: " << fEctTracks->GetEntriesFast() << endl;

    FollowInTPC(); // propagate tracks into TPC
    //StoreTracks();
    cout << "  Total number of found tracks: " << fTracks->GetEntriesFast() << endl;
    if (i != fNPass - 1) ExcludeHits(); // exclude used hits
  }
  RemoveDoubles(); // remove double tracks
  AddHits(); // add hit objects to tracks
  cout << "  Total number of found tracks: " << fTracks->GetEntriesFast() << endl;
}

//__________________________________________________________________________
void MpdEctTrackFollow2Tpc::GetTpcHits()
{
  /// Get TPC hits near zMax

  fhLays->Reset();
  Int_t ids[99999] = {0}, idMax = 0;
  // !!!!!!!!! Geometry dependent values
  //Double_t zMax = 134.5, rMin = 20.5, rMax = 109.5, dR = (rMax-rMin)/50; // dR = 1.78; // old version 
  Double_t zMax = 124.5, rMin = 29.195, rMax = 99.81, dR = (rMax-rMin)/50; // 1.4123; // new version (with dead material)
  Int_t nTpc = fTpcHits->GetEntriesFast(), lay0 = -1, layMax = -1;
  cout << " TPC hits: " << nTpc << endl;
  Double_t tanMax = (rMin + fgkLayCut * dR) / zMax;  

  for (Int_t i = 0; i < nTpc; ++i) {
    TpcLheHit *hit = (TpcLheHit*) fTpcHits->UncheckedAt(i);
    //TpcPoint *p = 0x0;
    //if (fTpcTracks) p = (TpcPoint*) fTpcTracks->UncheckedAt(hit->GetRefIndex());
    //if (p && p->GetTrackID() != 64) continue; // !!!
    if (hit->GetZ() < 0 || hit->GetLayer() > fgkLayCut) {
      fTpcHits->RemoveAt(i);
      continue;
    }
    if (hit->GetR() / hit->GetZ() > tanMax) { 
      fTpcHits->RemoveAt(i);
      continue;
    }
    lay0 = hit->GetLayer();
    //Int_t id = ((TpcPoint*)fTpcTracks->UncheckedAt(hit->GetRefIndex()))->GetTrackID();
    Int_t id = hit->GetTrackID();
    cout << i << " " << hit->GetLayer() << " " << hit->GetZ() << " " << id << endl;
    if (id > 99998) { fTpcHits->RemoveAt(i); continue; }
    idMax = TMath::Max (id, idMax);
    ids[id]++;
    layMax = TMath::Max (lay0, layMax);
    fhLays->Fill(lay0+0.1);
    //hit->SetFlag(1); 
  }
  Int_t nid = 0;
  for (Int_t i = 0; i <= idMax; ++i) if (ids[i]) ++nid;

  fTpcHits->Compress();
  cout << " TPC points near Zmax: " << fTpcHits->GetEntriesFast() << " " << nid << " " << fhLays->GetSum() << endl;

  fLayPointers = new Int_t [layMax+1];
  Int_t ipos = 0;
  for (Int_t i = layMax; i > -1; --i) {
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

  // Create TPC Kalman hits
  Int_t nHits = fTpcHits->GetEntriesFast();
  for (Int_t i = 0; i < nHits; ++i) {
    TpcLheHit *hit = (TpcLheHit*) fTpcHits->UncheckedAt(i);
    MpdKalmanHitR *hitK = new ((*fKHits)[i]) MpdKalmanHitR(hit->GetR(), hit->GetRphi(), hit->GetZ(), 
			  //hit->GetRphiErr(), hit->GetZerr(), lay, j);
			  hit->GetRphiErr(), hit->GetZerr(), hit->GetLayer(), hit->GetRefIndex());
    if (hit->GetTrackID() == 1967) cout << hit->GetTrackID() << " " << hit->GetLayer() << endl;
  }
}

//__________________________________________________________________________
void MpdEctTrackFollow2Tpc::FollowInTPC()
{
  /// Propagate tracks into TPC
  
  Int_t nCand = fEctTracks->GetEntriesFast(), iok = 0;
  Double_t zMax = 134.5, rMin = 20.5, rMax = 109.5, dR = (rMax-rMin)/50; // dR = 1.78; // old version 
  //Double_t zMax = 124.5, rMin = 29.195, rMax = 99.81, dR = (rMax-rMin)/50; // 1.4123; // new version (with dead material)
  MpdEctKalmanTrack track;

  for (Int_t i = 0; i < nCand; ++i) {
    MpdEctKalmanTrack *tr = (MpdEctKalmanTrack*) fEctTracks->UncheckedAt(i);
    cout << " Track seed No. " << i << " " << tr->GetNofTrHits() << " " << tr->GetTrackID() << endl;
    //if (track->GetTrackID() != 117) continue;
    //if (track->GetTrackID() != 10) continue;
    //if (track->GetTrackID() != 1105) continue;
    //if (track->GetTrackID() != 77) continue;
    track = *tr;
    track.SetParamNew(*track.GetParam());
    track.ReSetWeight();
    Int_t nHits = tr->GetNofTrHits();
    for (Int_t ih = 0; ih < nHits; ++ih) {
      TObject *hit = tr->GetTrHits()->UncheckedAt(ih);
      track.GetHits()->Add(hit);
    }

    MpdKalmanHitZ hit;
    hit.SetType(MpdKalmanHit::kFixedZ);
    hit.SetZ(zMax);
    MpdKalmanFilter::Instance()->PropagateToHit(&track, &hit);

    Int_t lay0 = (Int_t) ((track.GetRadNew() - rMin) / dR);
    lay0 = TMath::Max (lay0, 0);
    cout << lay0 << endl;

    if (lay0 <= fgkLayCut) iok = RunKalmanFilter(&track, lay0);
    if (iok == -1) cout<<"RunKalmanFilter with error!!!"<<endl;

    hit = *((MpdKalmanHitZ*) track.GetHits()->Last());
    // Keep only tracks with TPC points
    if (hit.GetType() == MpdKalmanHit::kFixedR) new ((*fTracks)[i]) MpdEctKalmanTrack(track);
    //new ((*fTracks)[i]) MpdEctKalmanTrack(track);
    cout << track.GetNofHits() << endl;
  }
}

//__________________________________________________________________________
Int_t MpdEctTrackFollow2Tpc::RunKalmanFilter(MpdEctKalmanTrack *track, Int_t layBeg)
{
  /// Run Kalman filter

  MpdKalmanHitR *hitOK = 0x0;
  Int_t layEnd = -1, dLay = -1, layOK = -1;
  
  Int_t miss = 0;
  TMatrixDSym pointWeight(5), pointWeightTmp(5), saveWeight(5);
  TMatrixD param(5,1), paramTmp(5,1);

  Double_t saveR = 0;
  //cout << " Starting hit: " << hitOK->GetLayer() << " " << hitOK->GetTrackID() << " " << hitOK->GetUsage() << endl;
  
  for (Int_t lay = layBeg; lay != layEnd; lay+=dLay) {
    Int_t nLay = GetNofHitsInLayer(lay);
    Int_t indx0 = GetHitsInLayer(lay);
    Double_t dChi2Min = 1.e+6;
    MpdKalmanHitR *hitMin = 0x0;
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
      MpdKalmanHitR *hit = (MpdKalmanHitR*) fKHits->UncheckedAt(indx0+indx);
      // !!! Exact ID match
      //if (((FairMCPoint*)fEctHits->UncheckedAt(hit->GetIndex()))->GetTrackID() != track->GetTrackID()) continue;

      // Exclude used hits
      if (hit->GetFlag() != 1) continue;
      if (track->GetType() == MpdKalmanTrack::kFixedR) {
	// Check if the hit within some window
	if (TMath::Abs(hit->GetRphi()-track->GetParamNew(0)) > 9) continue;
	if (TMath::Abs(Proxim(track,hit)-track->GetParamNew(0)) > 15) continue;
      }
      //*if (hit->GetTrackID() == 186)*/ cout << " Hit: " << ((FairMCPoint*)fEctHits->UncheckedAt(hit->GetIndex()))->GetTrackID() << " " << hit->GetLayer() << " " << ((FairMCPoint*)fEctHits->UncheckedAt(hit->GetIndex()))->GetDetectorID() << " " << hit->GetRphi() << " " << hit->GetR() << " " << hit->GetZ() << " " << dist.X() << " " << dist.Y() << " " << track->GetParamNew(0) << " " << track->GetParamNew(1) << endl;
      //track->Print("");
      //hit->Print("");
      if (!MpdKalmanFilter::Instance()->PropagateToHit(track,hit)) return -1;
      //*if (hit->GetTrackID() == -607)*/ cout << /*hit->GetTrackID() <<*/ " " << hit->GetRphi() << " " << track->GetParamNew(0) << " " << track->GetParamNew(1) << " " << hit->GetZ() << " " << track->GetPosNew() << endl;

      Double_t dChi2 = MpdKalmanFilter::Instance()->CheckHitR(track,hit,pointWeight,param);
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
        //cout << " New min dChi2 = " << dChi2 << " " << hitMin->GetRphi() << " " << hitMin->GetR() << " " << ((FairMCPoint*)fEctHits->UncheckedAt(hit->GetIndex()))->GetTrackID() << endl;
	//cout << track->GetParamNew(0) << " " << track->GetParamNew(1) << endl;
	//cout << hit->GetRphi() << " " << hit->GetZ() << endl;
	//cout << param(0,0) << " " << param(1,0) << endl;
        //paramTmp.Print();
      }
    }
    if (dChi2Min < fgkChi2Cut) {
      //layOK = lay;
      hitOK = hitMin;
      track->GetHits()->Add(hitOK);
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
  } // for (Int_t lay = layOK-1; lay >= 0;
  return 0;
}

//__________________________________________________________________________
Double_t MpdEctTrackFollow2Tpc::Proxim(MpdKalmanTrack *track, const MpdKalmanHit *hit)
{
  /// Adjust hit coord. R-Phi to be "around" track R-Phi - to avoid 
  /// discontinuity around +- Pi

  Double_t hitPhi = hit->GetRphi() / hit->GetR();
  Double_t phi0 = track->GetParamNew(0) / track->GetPosNew();
  return hit->GetR() * MpdKalmanFilter::Instance()->Proxim(phi0, hitPhi);
}

//__________________________________________________________________________
void MpdEctTrackFollow2Tpc::Write()
{
  /// Write

  TFile histoFile("EctRec.root","RECREATE");
  Writedir2current(fHistoDir);
  histoFile.Close();
}

//__________________________________________________________________________
void MpdEctTrackFollow2Tpc::Writedir2current( TObject *obj )
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
void MpdEctTrackFollow2Tpc::RemoveDoubles()
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
  //*
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
  //*/
}
	  
//__________________________________________________________________________
Int_t MpdEctTrackFollow2Tpc::NofCommonHits(MpdEctKalmanTrack* track, MpdEctKalmanTrack* track1)
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
void MpdEctTrackFollow2Tpc::AddHits()
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
    Int_t nWrong = 0, motherID = track->GetTrackID(), motherID1 = 0;
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
      if (hit->GetType() == MpdKalmanHit::kFixedZ) {
	// ECT point
	Int_t iproj = (hit->GetLayer() - 1) % 3;
	if (iproj == 0) cout << " U";
	else if (iproj == 1) cout << " R";
	else cout << " V";
	cout << hit->GetLayer();
	MpdStrawendcapPoint *h = (MpdStrawendcapPoint*) fEctPoints->UncheckedAt(hit->GetIndex());
	motherID1 = h->GetTrackID();
	cout << "-" << motherID1;
      } else {
	// TPC point
	cout << " " << hit->GetLayer();
	TpcPoint *h = (TpcPoint*) fTpcPoints->UncheckedAt(hit->GetIndex());
	motherID1 = h->GetTrackID();
	cout << "-" << motherID1;
	((MpdKalmanHit*)trHits[j])->SetType(MpdKalmanHit::kFixedR);
      }
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
void MpdEctTrackFollow2Tpc::SetTrackID(MpdEctKalmanTrack* track)
{
  /// Set track ID as ID of majority of its hits

  const Int_t idMax = 99999;
  Int_t ids[idMax+1] = {0}, nHits = track->GetNofHits(), locMax = 0;
  TObjArray *hits = track->GetHits();
  FairMCPoint *p = 0x0;

  for (Int_t i = 0; i < nHits; ++i) {
    MpdKalmanHit *hit = (MpdKalmanHit*) hits->UncheckedAt(i);
    if (hit->GetType() == MpdKalmanHit::kFixedZ) p = (FairMCPoint*) fEctPoints->UncheckedAt(hit->GetIndex());
    else p = (FairMCPoint*) fTpcPoints->UncheckedAt(hit->GetIndex());
    Int_t id = p->GetTrackID();
    if (id > idMax) exit(0);
    ++ids[id];
    if (ids[id] > ids[locMax]) locMax = id;
  }
  if (ids[locMax] > 1) track->SetTrackID(locMax);
}

ClassImp(MpdEctTrackFollow2Tpc);
