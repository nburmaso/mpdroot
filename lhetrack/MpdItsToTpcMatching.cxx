#include "MpdItsToTpcMatching.h"
#include "MpdKalmanFilter.h"
#include "MpdKalmanTrack.h"
#include "MpdMCTrack.h"

#include "MpdItsHit5spd.h"
#include "MpdStsPoint.h"
#include "MpdItsKalmanTrack.h"
#include "MpdTpcKalmanTrack.h"
#include "MpdVector.h"

#include "FairRun.h"
#include "FairRunAna.h"
#include "FairRuntimeDb.h"
#include "FairRootManager.h"

#include <iostream>
#include <map>
#include <set>
#include <string>

using std::cout;
using std::endl;

std::map<Int_t,MpdKalmanTrack*> fmap;
std::ofstream fout("match.dat");
std::ofstream fout_v_match("log_match.txt");
//__________________________________________________________________________

MpdItsToTpcMatching::MpdItsToTpcMatching(const char *name, Int_t iVerbose ) 
  :FairTask(name, iVerbose)
  //fExact(0),
  //fNTracks(0),
  //fNPass(2),
  //fGeo(0)
{

  // AZ fItsTracks = new TClonesArray("MpdVector", 100); /// not needed?
  fTracks = new TClonesArray("MpdItsKalmanTrack", 100); /// TODO where is this used?
  fTracks1 = new TClonesArray("MpdTpcKalmanTrack", 100);
  fTracksRefit = new TClonesArray("MpdItsKalmanTrack", 100);
  fTpcTracksRefit = new TClonesArray("MpdItsKalmanTrack", 100); /// was tpc
}

//__________________________________________________________________________
MpdItsToTpcMatching::~MpdItsToTpcMatching()
{
  //delete fKHits;
  //delete fTracks;
  //delete fTrackCand;
}

//__________________________________________________________________________
InitStatus MpdItsToTpcMatching::Init() 
{
  //return ReInit();
  if (ReInit() != kSUCCESS) return kERROR;
  FairRuntimeDb* rtdb = FairRun::Instance()->GetRuntimeDb();
  FillGeoScheme(); /// TODO

  return kSUCCESS;
}

//__________________________________________________________________________

void MpdItsToTpcMatching::FillGeoScheme()
{

}

//__________________________________________________________________________
InitStatus MpdItsToTpcMatching::ReInit() 
{
  fItsPoints = (TClonesArray *) FairRootManager::Instance()->GetObject("StsPoint"); 
  fItsTracks = (TClonesArray *) FairRootManager::Instance()->GetObject("ItsTrack"); 
  //fItsHits = (TClonesArray *) FairRootManager::Instance()->GetObject("StsHit");
  //if (fItsPoints == 0x0 || fItsHits == 0x0) return kERROR;
  fTpcTracks =(TClonesArray *) FairRootManager::Instance()->GetObject("TpcKalmanTrack");
  fMCTracks =(TClonesArray *) FairRootManager::Instance()->GetObject("MCTrack");

  ///FairRootManager::Instance()->Register("ItsTrackTo27", "ItsCopy", fTracks1, kTRUE); /// originally fTracks
  FairRootManager::Instance()->Register("ItsTrackRefit", "ItsRefit", fTracksRefit, kTRUE); /// originally fTracks
  FairRootManager::Instance()->Register("TpcTrackRefit", "TpcRefit", fTpcTracksRefit, kTRUE); /// originally fTracks
  return kSUCCESS;
}

//__________________________________________________________________________
void MpdItsToTpcMatching::SetParContainers() 
{
  // Get run and runtime database
  FairRunAna* run = FairRunAna::Instance();
  if ( ! run ) Fatal("SetParContainers", "No analysis run");

  FairRuntimeDb* db = run->GetRuntimeDb();
  if ( ! db ) Fatal("SetParContainers", "No runtime database");

  // Get STS geometry parameter container
  db->getContainer("MpdStsGeoPar");
}

//__________________________________________________________________________
void MpdItsToTpcMatching::Finish() 
{
  //Write();
  //if (lunErr) fclose(lunErr); 
}

//__________________________________________________________________________
void MpdItsToTpcMatching::Reset() 
{
  
  cout << " MpdItsToTpcMatching::Reset  " << endl; 
  fout_v_match << " MpdItsToTpcMatching::Reset  " << endl; 
  //fKHits->Clear();
  
  fTracks1->Delete(); // AZ
  fTpcTracksRefit->Delete(); // AZ
  fTracks->Delete();
  fTracksRefit->Delete();
}

//__________________________________________________________________________
void MpdItsToTpcMatching::Exec(Option_t * option) 
{
  fout_v_match << "- track matching -" << endl;
  fmap.clear();
  //if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),__FUNCTION__);
  Reset();
  // MpdKalmanFilter function Refit
  //Bool_t MpdKalmanFilter::Refit(MpdKalmanTrack *track, Int_t iDir, Bool_t updLeng)
  
  multimap <Float_t, MpdItsKalmanTrack*> multimapIts, multimapItsPhi;
  multimap <Float_t, MpdTpcKalmanTrack*> multimapTpc, multimapTpcPhi;

  /// refit uses MpdKalmanTrack while fItsTracks has MpdItsKalmanTrack
  Int_t n = fItsTracks->GetEntriesFast();
  Float_t phi;
  cout << "fItsTracks " << n << endl;
  fout_v_match << "fItsTracks " << n << endl;
  cout << "refit ITS" << endl;
  fout_v_match << "refit ITS" << endl;
  for (Int_t i = 0; i < n; i++) {
    MpdItsKalmanTrack *temp = (MpdItsKalmanTrack*)fItsTracks->UncheckedAt(i);
    MpdItsKalmanTrack *track = (MpdItsKalmanTrack*) new ((*fTracks)[i]) MpdItsKalmanTrack(*temp);
    
    ///cout << "Track id's " << temp->GetTrackID() << " " << track->GetTrackID() << endl;
    ///cout << "refitting nofhits noftrhits getentries nofits" << track->GetNofHits() << " " << track->GetNofTrHits() << " " << track->GetTrHits()->GetEntriesFast() << " " << track->GetNofIts() << endl; 
    track->SetDirection(MpdKalmanTrack::kOutward);
    MpdKalmanFilter::Instance()->Refit(track, -1, 0); // TODO check params /// -1
    
    /// Propagate ITS to 27 Outward
    MpdKalmanHit hitTmp;
    hitTmp.SetType(MpdKalmanHit::kFixedR);
    hitTmp.SetDist(27.0);
    Bool_t ok = MpdKalmanFilter::Instance()->PropagateToHit(track, &hitTmp, kTRUE, kTRUE); // doesn't create GetMeas() coordinates
    
    /// writing to ItsTracksRefit
    new ((*fTracksRefit)[i]) MpdItsKalmanTrack(*track);

    TMatrixD *parNew = track->GetParamNew();
    /// (*parNew)(1,0) -- theta, (*parNew)(0,0) / track->GetPosNew() -- phi
    phi = (*parNew)(0,0) / track->GetPosNew();
    //AZ - save ITS track
    fmap[track->GetTrackID()] = track;
    //AZ

    multimapIts.insert(pair<Float_t, MpdItsKalmanTrack*>((*parNew)(1,0) , track));
    ///multimapItsPhi.insert(pair<Float_t, MpdItsKalmanTrack*>(phi, track));
    multimapItsPhi.insert(pair<Float_t, MpdItsKalmanTrack*>((*parNew)(0,0), track));
    if (TMath::Abs(TMath::Pi() - TMath::Abs(phi)) < TMath::Pi() / 9.0) {
      multimapItsPhi.insert(pair<Float_t, MpdItsKalmanTrack*>((*parNew)(0,0) - TMath::Sign(1, phi) * 2 * TMath::Pi() * track->GetPosNew(), track)); 
      ///multimapItsPhi.insert(pair<Float_t, MpdItsKalmanTrack*>(phi - TMath::Sign(1, phi) * 2 * TMath::Pi(), track)); /// transverse, phi, duplicate hit
    }
  }
  
  n = fTpcTracks->GetEntriesFast();
  cout << "fTpcTracks " << n << endl;
  cout << "refit TPC" << endl;
  fout_v_match << "fTpcTracks " << n << endl;
  fout_v_match << "refit TPC" << endl;

  fout_v_match << "id, dphi, drphi, dz, pt, tpc-its" << endl;
  for (Int_t i = 0; i < n; i++) {
    MpdTpcKalmanTrack *temp = (MpdTpcKalmanTrack*) fTpcTracks->UncheckedAt(i);
    MpdTpcKalmanTrack *track = (MpdTpcKalmanTrack*) new ((*fTracks1)[i]) MpdTpcKalmanTrack(*temp); /// TODO explain why this is needed

    //AZ temp->SetUniqueID(i);
    ///track->SetUniqueID(i); /// removed 16.12.2019
    /// TObject.UniqueID is used to identify track after it is refitted
    /// @temp is original track, @track is to be refitted

    ///cout << track->GetNofHits() << " " << track->GetNofTrHits() << " " << track->GetTrHits()->GetEntriesFast() << " " << track->GetHits() << endl;
    
    track->SetUniqueID(i+1);
    track->SetDirection(MpdKalmanTrack::kInward);

    /// Propagate TPC track to 27 inward
    MpdKalmanHit hitTmp;
    hitTmp.SetType(MpdKalmanHit::kFixedR);
    hitTmp.SetPos(27.0);
    
    /// see MpdTrackFinderIts5spd::GetTrackSeeds
    track->SetParamNew(*track->GetParam());
    track->SetPos(track->GetPosNew());
    track->ReSetWeight();  
    TMatrixDSym w = *track->GetWeight(); // save current weight matrix
    
    Bool_t ok = MpdKalmanFilter::Instance()->PropagateToHit(track, &hitTmp, kFALSE, kFALSE);
    track->SetWeight(w); // restore original weight matrix (near TPC inner shell)

    track->GetHits()->Clear();
    //track->SetChi2Its(track->GetChi2()); // temporary storage
    //track->SetChi2(0.); /// commented 27.6.2019
    //Int_t j = track->GetTrHits()->GetEntriesFast() - 1;

    TMatrixD *parNew = track->GetParamNew();
    //cout << (*parNew)(0,0) << " " << (*parNew)(1,0) << " " << (*parNew)(2,0) << " " << (*parNew)(3,0) << " " << (*parNew)(4,0) << " p " << track->GetPosNew() << endl;

    /// (*parNew)(1,0) -- theta, (*parNew)(0,0) / track->GetPosNew() -- phi

    phi = (*parNew)(0,0) / track->GetPosNew();
    
    // AZ - write parameters
    if (fmap.find(track->GetTrackID()) != fmap.end()) {
      Int_t id = track->GetTrackID();
      fout << id << " " << phi-fmap[id]->GetParamNew(0)/track->GetPosNew() << " " << (*parNew)(0,0) - fmap[id]->GetParamNew(0) << " " 
    	   << (*parNew)(1,0)-fmap[id]->GetParamNew(1) << " " << track->Pt() << endl;
      fout_v_match << id << " " << phi-fmap[id]->GetParamNew(0)/track->GetPosNew() << " " << (*parNew)(0,0) - fmap[id]->GetParamNew(0) << " " 
    	   << (*parNew)(1,0)-fmap[id]->GetParamNew(1) << " " << track->Pt() << endl;
    }
    //AZ
    
    multimapTpc.insert(pair<Float_t, MpdTpcKalmanTrack*>((*parNew)(1,0) , track));
    multimapTpcPhi.insert(pair<Float_t, MpdTpcKalmanTrack*>((*parNew)(0,0), track)); /// was (phi, track)
    ///multimapTpcPhi.insert(pair<Float_t, MpdTpcKalmanTrack*>(phi, track));
    if (TMath::Abs(TMath::Pi() - phi) < TMath::Pi() / 9.0)
      ///multimapTpcPhi.insert(pair<Float_t, MpdTpcKalmanTrack*>(phi - TMath::Sign(1, phi) * 2 * TMath::Pi(), track));
      multimapTpcPhi.insert(pair<Float_t, MpdTpcKalmanTrack*>((*parNew)(0,0) - TMath::Sign(1, phi) * 2 * TMath::Pi() * track->GetPosNew(), track));
  }

  n = fItsTracks->GetEntriesFast();
  
  multimap<Float_t, MpdItsKalmanTrack*>::iterator itits;
  multimap<Float_t, MpdTpcKalmanTrack*>::iterator ittpc;

  Float_t epsz = 0.5;///5e-1;//1.0 /// TODO should it depend on pt as well?
  Float_t epsphi = 0.5;//0.2 for phi, 0.5 for rphi;

  Int_t k = 0, tpck = 0;

  multimap <Float_t, std::tuple<MpdItsKalmanTrack*, MpdItsKalmanTrack*, MpdTpcKalmanTrack*>> multimapMatch; /// 1 was tpc
  /// added 9.10.2019
  set<MpdItsKalmanTrack*> tracksWithoutMatch;

  
  //________________________________________________MAIN MATCHING LOOP________________________________________________________________________
  for (itits = multimapIts.begin(); itits != multimapIts.end(); ++itits) {
    TMatrixD *parNew = (itits->second)->GetParamNew(); 
    //cout << (*parNew)(0,0) << " " << itits->second;
    //cout << "its track noftrhits, nofits, nofhits" << (itits->second)->GetNofTrHits() << " " 
    //	 << (itits->second)->GetNofIts() << " " << (itits->second)->GetNofHits() << endl;
    /// 2.12.2019 phi->rphi
    ///phi = (*parNew)(0,0) / (itits->second)->GetPosNew();
    phi = (*parNew)(0,0);/// phi became rphi here
    
    //cout << "par " << itits->first << " " << phi << endl;
    multimap<Float_t, MpdTpcKalmanTrack*>::iterator itlow = multimapTpc.lower_bound(itits->first - epsz);
    multimap<Float_t, MpdTpcKalmanTrack*>::iterator ittop = multimapTpc.upper_bound(itits->first + epsz);

    multimap<Float_t, MpdTpcKalmanTrack*>::iterator itlowphi = multimapTpcPhi.lower_bound(phi - epsphi);
    multimap<Float_t, MpdTpcKalmanTrack*>::iterator ittopphi = multimapTpcPhi.upper_bound(phi + epsphi);

    /// getting window for possible tpc track matches
    set<MpdTpcKalmanTrack*> setz, setphi, intersect;    
    for (multimap<Float_t, MpdTpcKalmanTrack*>::iterator itr = itlow; itr != ittop; ++itr) {
      setz.insert((*itr).second);
    }
    for (multimap<Float_t, MpdTpcKalmanTrack*>::iterator itr = itlowphi; itr != ittopphi; ++itr) {
      setphi.insert((*itr).second);
    }
    set_intersection(setz.begin(), setz.end(), setphi.begin(), setphi.end(), std::inserter(intersect, intersect.begin()));

    for (set<MpdTpcKalmanTrack*>::iterator it = intersect.begin(); it != intersect.end(); ++it) {
      MpdItsKalmanTrack* temp = new MpdItsKalmanTrack(*(*it)); /// TpcKalmanTrack is converted to ItsKalmanTrack
      TMatrixD *parNew1 = temp->GetParamNew();
      
      // ITS hits are sorted "inward"
      TMatrixD param(5,1);
      TMatrixDSym weight(5), pointWeight(5);
    
      ///cout << "its " << (itits->second)->GetChi2() << " " << (itits->second)->GetChi2Its() << " tpc" << " " << temp->GetChi2() << " " << temp->GetChi2Its() << endl;
      /// from MpvVectorFinder::AddHits()
      TClonesArray &trHits = *temp->GetTrHits();
      Int_t lastIndx = trHits.GetEntriesFast();
      //temp->SetChi2Its((itits->second)->GetChi2Its());
      temp->SetChi2Its(temp->GetChi2()); // tpc chi2 for now
      temp->SetChi2(0);
      
      tpck++;
      // AZ
      TVector3 mom3 = temp->Momentum3(), norm;
      mom3.SetMag(1.0);

      TString mass2 = "0.0194797849"; // pion mass squared
      if (fMCTracks) {
	// Get particle mass - ideal PID
	MpdMCTrack *mctrack = (MpdMCTrack*) fMCTracks->UncheckedAt(temp->GetTrackID());
	TParticlePDG *pdgP = TDatabasePDG::Instance()->GetParticle(mctrack->GetPdgCode());
	if (pdgP) {
	  Double_t mass = pdgP->Mass();
	  if (mass < 0.1 || mass > 0.25) {
	    // Electrons or heavier than pions
	    mass2 = "";
	    mass2 += mass*mass;
	  }
	}
      }
      //AZ

      Int_t hitindex = 0;
      /// loop over each hit from its track
      for (Int_t i = 0; i < (itits->second)->GetHits()->GetEntriesFast(); i++) {
        MpdKalmanHit* hitTmp = ((MpdKalmanHit*) (itits->second)->GetHits()->UncheckedAt(i));
	///cout << "temp " << temp->GetNofHits() << " " << temp->GetNofIts() << " " << temp->GetNofTrHits()<< endl;

	Bool_t ok = MpdKalmanFilter::Instance()->PropagateToHit(temp, hitTmp, kFALSE, kTRUE); // propagate tpc track to its hits
	Double_t dChi2 = MpdKalmanFilter::Instance()->FilterHit(temp, hitTmp, pointWeight, param); 

	/// this is correct. you still propagate track to a given hit, even if hit is not added to the track afterwards because of Chi2
	if (dChi2 <= 10) { /// TODO for 0.1GeV around 16 on 2d hit and further, so no hits added
	  /// from MpdItsKalmanTrack::Refit() ???
	  /// if hit is not included in track then track parameters and weight matrix are not updated
	  temp->SetChi2(temp->GetChi2()+dChi2); // (*it)
	  weight = *temp->GetWeight(); // (*it)
	  weight += pointWeight;
	  temp->SetWeight(weight); // (*it)
	  temp->SetParamNew(param); // (*it)
	  /// from AddHits()
	  temp->GetHits()->Add(hitTmp); // this changes GetNofHits() result 
	  new (trHits[lastIndx+hitindex]) MpdKalmanHit(*hitTmp);
	  hitindex++;
	} else {
	  ///temp->SetChi2(temp->GetChi2() - dChi2); /// if hit is not added then Chi2 does not increase
	}
	//cout << ok /*<< " chi2 " << (*it)->GetChi2() */<< " ";
	//cout << " " << param(1,0) << endl;

	// AZ - Add multiple scattering in the sensor
	norm = MpdKalmanFilter::Instance()->GetGeo()->Normal(hitTmp);
	Double_t step = 0.005 / TMath::Abs(norm * mom3) * 2.0; // extra factor 2. - possible overlaps 
	Double_t x0 = 9.36; // rad. length
	TMatrixDSym *cov = temp->Weight2Cov();
	Double_t th = temp->GetParamNew(3);
	Double_t cosTh = TMath::Cos(th);
	Double_t angle2 = MpdKalmanFilter::Instance()->Scattering(temp, x0, step, mass2);
	//cout << " Scat: " << hit->GetLayer() << " " << step << " " << TMath::Sqrt(angle2) << endl;
	(*cov)(2,2) += (angle2 / cosTh / cosTh);
	(*cov)(3,3) += angle2;
	Int_t iok = 0;
	MpdKalmanFilter::Instance()->MnvertLocal(cov->GetMatrixArray(), 5, 5, 5, iok);
	temp->SetWeight(*cov);
      }

      /// swap GetChi2 - tpc, GetChi2Its - its
      Float_t c = temp->GetChi2Its();
      temp->SetChi2Its(temp->GetChi2());
      temp->SetChi2(c);

      //cout << temp->GetNofTrHits() << " " << temp->GetNofHits() << " " << temp->GetChi2() << " " << temp->GetChi2Its() << endl;
      ///quality instead of Chi2 as map key 1.7.2019
      Float_t qual = - (temp->GetNofHits() + (100.0 - TMath::Min(temp->GetChi2Its(), 100.0)) / 101.0); /// was temp->GetChi2() + temp->GetChi2Its()

      /// !!!! IMPORTANT - adding tuple to multimap
      /// tuple: 0 - propagated track (TPC to ITS), 1 - its track matched, 2 - tpc track matched
      multimapMatch.insert(pair<Float_t, std::tuple<MpdItsKalmanTrack*, MpdItsKalmanTrack*, MpdTpcKalmanTrack*> >(qual , std::make_tuple(temp, itits->second, *it))); 
    } // for (set<MpdTpcKalmanTrack*>::iterator it

    if (intersect.size() == 0) {
      /// add its track standalone to final structure anyway through additional set
      k++;
      //(itits->second)->SetNofIts(5);
      tracksWithoutMatch.insert(itits->second);
    }
  } // for (itits = multimapIts.begin();
  ///sets for its and tpc tracks

  std::set<MpdTpcKalmanTrack*> tpcUnique;
  std::set<MpdItsKalmanTrack*> itsUnique;
  
  /// tuple: 0 - propagated track (TPC to ITS), 1 - its track matched, 2 - tpc track matched

  Int_t matchedCount = 0, qualTr = 0;
  Int_t usedItsHits = 0; // overall amount of ITS hits used when matching
  Int_t wrongMatch = 0;

  cout << "multimapMatch.size " << multimapMatch.size() << endl;
  for (multimap<Float_t, std::tuple<MpdItsKalmanTrack*, MpdItsKalmanTrack*, MpdTpcKalmanTrack*>>::iterator itr = multimapMatch.begin(); itr != multimapMatch.end(); ++itr) { /// tuple 1 was tpc
    ///cout << itr->first << " " << std::get<0>(itr->second) << " " << std::get<1>(itr->second) << " "<< std::get<2>(itr->second) << endl;
    if (!tpcUnique.count(std::get<2>(itr->second)) && !itsUnique.count(std::get<1>(itr->second))) {
      tpcUnique.insert(std::get<2>(itr->second));
      itsUnique.insert(std::get<1>(itr->second));
      
      /*AZ
      /// 16.12.2019
      if (std::get<2>(itr->second)->GetTrackID() != std::get<1>(itr->second)->GetTrackID())
	/// track match for tracks with different id's
	wrongMatch++;
      ///
      */
      // cout << endl << "its: GetNofHits, GetNofTrHits, GetNofIts, GetTrackID " << std::get<0>(itr->second)->GetNofHits() << " " 
      // 	   << std::get<0>(itr->second)->GetNofTrHits() << " " << std::get<0>(itr->second)->GetNofIts() << " " 
      // 	   << std::get<0>(itr->second)->GetTrackID() << endl;

      if (std::get<0>(itr->second)->GetNofHits() > 2) { ///good quality
	/// tuple: 0 - propagated track, 1 - its track matched, 2 - tpc track matched
	qualTr++;
	if (std::get<2>(itr->second)->GetTrackID() != std::get<1>(itr->second)->GetTrackID())
	  wrongMatch++; //AZ - track match for tracks with different id's
      }
      
      Double_t vert[3] = {0.0,0.0,0.0};
      MpdItsKalmanTrack* track = std::get<0>(itr->second); /// was tpc /// matched tpc+its track

      track->SetParam(*track->GetParamNew());
      track->SetPos(track->GetPosNew());

      Double_t pos = track->GetPos();
      TMatrixD par = *track->GetParam();
      TMatrixDSym cov = *track->Weight2Cov();
      Double_t leng = track->GetLength();
      TString nodeNew = track->GetNodeNew();
      //cout << " 1: " << nodeNew << ", " << track->GetNode() << endl;

      // Go to beam pipe
      MpdKalmanHit hit;
      hit.SetType(MpdKalmanHit::kFixedR);
      hit.SetPos(2.9); // fPipeR is calculated with geometry, which is not present in matching class
      Bool_t iok = MpdKalmanFilter::Instance()->PropagateToHit(track, &hit, kTRUE);
      if (iok != 1) {
	// Restore track
	track->SetParam(par);
	track->SetParamNew(par);
	track->SetCovariance(cov);
	track->ReSetWeight();
	track->SetPos(pos);
	track->SetPosNew(pos);
	track->SetLength(leng);
	//track->SetNode(node);
	//cout << " 2: " << nodeNew << ", " << track->GetNode() << endl;
	track->SetNodeNew(nodeNew);
      } else {
	// Add multiple scattering
	//Double_t dX = 0.05 / 8.9; // 0.5 mm of Al
	Double_t dX = 0.1 / 35.28; // 1. mm of Be
	TMatrixDSym* pcov = track->Weight2Cov();
	Double_t th = track->GetParamNew(3);
	Double_t cosTh = TMath::Cos(th);
	Double_t angle2 = MpdKalmanFilter::Instance()->Scattering(track, dX);
	(*pcov)(2,2) += (angle2 / cosTh / cosTh);
	(*pcov)(3,3) += angle2;
	Int_t ok = 0;
	MpdKalmanFilter::Instance()->MnvertLocal(pcov->GetMatrixArray(), 5, 5, 5, ok);
	track->SetWeight(*pcov);
      }
      cov = *track->Weight2Cov();

      hit.SetPos(0.);
      hit.SetMeas(0,track->GetParam(2)); // track Phi
      //cout << i << " " << track->GetTrackID() << " " << track->GetLength() << " " << ((MpdKalmanHitR*)track->GetHits()->First())->GetLength() << endl;
      //Double_t pos = ((MpdKalmanHit*)track->GetHits()->Last())->GetPos();
      //MpdKalmanFilter::Instance()->PropagateParamR(track, &hit, kTRUE);
      iok = MpdKalmanFilter::Instance()->PropagateToHit(track, &hit, kTRUE);
      if (iok != 1) MpdKalmanFilter::Instance()->FindPca(track, vert);
      //track->SetPos(pos); // restore position
      track->SetParam(*track->GetParamNew()); // !!! track params at PCA
      track->SetNofIts(track->GetNofHits());
      
      usedItsHits += track->GetNofHits();
      
      if (track->GetNofHits() > 2) {
	/// writing to TpcTracksRefit
	track->SetUniqueID(std::get<1>(itr->second)->GetTrackID()); /// 6.4.2020 changed get<0> to get<1>
	//cout << "its and tpc track id equal " << track->GetTrackID() << " " << track->GetUniqueID() << endl;

	new ((*fTpcTracksRefit)[matchedCount]) MpdItsKalmanTrack(*track); /// was Tpc
        ///cout << "TPC + ITS track hits " << track->GetNofTrHits() << endl;
      } else {
	/// refit its track anyway without matching to tpc track
	/// this track *std::get<1>(itr->second) is refitted outward to 27.0, while i need here not refitted, original its track
	MpdItsKalmanTrack *temp = (MpdItsKalmanTrack*)fItsTracks->UncheckedAt(std::get<1>(itr->second)->GetUniqueID() - 1); /// index is uniqueid - 1
	
	/// added 16/9/2019 because GetChi2 was 0, not sure if this is correct
	//AZ temp->SetChi2(temp->GetChi2Its());
	/// 16.12.2019
	///std::get<1>(itr->second)->SetUniqueID(std::get<0>(itr->second)->GetTrackID()); /// its track id is saved to GetUniqueID(), tpc track id is saved in GetTrackID()
	//AZ temp->SetUniqueID(std::get<1>(itr->second)->GetTrackID()); /// 6.4.2020 changed get<0> to get<1>
	// cout << "its and tpc track id " << temp->GetTrackID() << " " << temp->GetUniqueID() << endl;
	// cout << "GetNofHits, GetNofTrHits, GetNofIts " << temp->GetNofHits() << " " 
	//      << temp->GetNofTrHits() << " " << temp->GetNofIts() << " " << endl;

	/// writing to TpcTracksRefit
	MpdItsKalmanTrack *trtr = // AZ - 17.04.2020
	  new ((*fTpcTracksRefit)[matchedCount]) MpdItsKalmanTrack(*temp); /// *std::get<1>(itr->second)
	trtr->SetChi2(temp->GetChi2Its()); //AZ
	trtr->SetUniqueID(std::get<1>(itr->second)->GetTrackID());
	///cout << "ITS only track hits " << temp->GetNofTrHits() << " uniqueid " << std::get<1>(itr->second)->GetUniqueID() - 1 << " " << temp->GetChi2Its() << endl;
      }
      matchedCount++;
      delete track;
    } /// if (!tpcUnique.count(std::get<2>(itr->second)) && !itsUnique.count(std::get<1>(itr->second))) {
  }
  ///added 9.10.2019
  for (multimap<Float_t, std::tuple<MpdItsKalmanTrack*, MpdItsKalmanTrack*, MpdTpcKalmanTrack*>>::iterator itr = multimapMatch.begin(); itr != multimapMatch.end(); ++itr) { /// tuple 1 was tpc
    if (!itsUnique.count(std::get<1>(itr->second)))
      {
	tracksWithoutMatch.insert(std::get<1>(itr->second));
	itsUnique.insert(std::get<1>(itr->second));
      }
  }

  Int_t i = matchedCount - 1;
  for (set<MpdItsKalmanTrack*>::iterator ittemp = tracksWithoutMatch.begin(); ittemp != tracksWithoutMatch.end(); ittemp++) {
    i++;
    //MpdItsKalmanTrack *trtr = new ((*fTpcTracksRefit)[i]) MpdItsKalmanTrack(*(*ittemp));
    //trtr->SetUniqueID(trtr->GetTrackID()); //AZ - 17.04.2020
  }

  cout << "fItsTracks size " << fItsTracks->GetEntriesFast() << endl;
  cout << "Its tracks without match: " << k << endl;
  cout << "Overall possible tpc+its tracks: " << tpck << endl;
  cout << "Overall matched tracks: " << matchedCount << endl;
  cout << "Overall good matched tracks: " << qualTr << endl;
  cout << "Overall used ITS hits when matching: " << usedItsHits << endl;
  cout << "wrongMatch: " << wrongMatch << endl;

  fout_v_match << "fItsTracks size " << fItsTracks->GetEntriesFast() << endl;
  fout_v_match << "Its tracks without match: " << k << endl;
  fout_v_match << "Overall possible tpc+its tracks: " << tpck << endl;
  fout_v_match << "Overall matched tracks: " << matchedCount << endl;
  fout_v_match << "Overall good matched tracks: " << qualTr << endl;
  fout_v_match << "wrongMatch: " << wrongMatch << endl;
  fout_v_match << "- track matching done -" << endl;
  fout_v_match.close();
}

//__________________________________________________________________________

ClassImp(MpdItsToTpcMatching)
