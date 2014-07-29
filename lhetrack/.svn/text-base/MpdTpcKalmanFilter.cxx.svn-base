/// \class MpdTpcKalmanFilter
/// 
/// Kalman filter track reconstructor in the MPD central detector
/// \author Alexander Zinchenko (LHEP, JINR, Dubna)

#include "MpdTpcKalmanFilter.h"
#include "MpdCodeTimer.h"
#include "MpdTpcHit.h"
#include "MpdTpcSectorGeo.h"
#include "MpdTpcKalmanTrack.h"
#include "MpdKalmanFilter.h"
#include "MpdKalmanTrack.h"
#include "MpdKalmanGeoScheme.h"
#include "MpdKalmanHit.h"
#include "MpdTpcDedxTask.h"
#include "MpdVertexZfinder.h"
#include "TpcCluster.h"
#include "TpcGeoPar.h"
#include "TpcPoint.h"
#include "TpcLheTrack.h"
#include "FairEventHeader.h"
#include "FairField.h"
#include "FairMCEventHeader.h"
#include "FairMCTrack.h"
#include "FairRootManager.h"
#include "FairRunAna.h"
#include "FairRuntimeDb.h"
#include "FairTask.h"
//#include "MpdTofGeoPar.h" //AZ

#include <TClonesArray.h>
#include <TGeoManager.h>
#include <TGeoMatrix.h>
#include <TGeoPgon.h>
#include <TGeoTrd1.h>
#include <TGeoTube.h>
#include <TRandom.h>
#include <TROOT.h>

#ifdef XROOTD
#include "TXNetFile.h"
#endif

#ifdef _OPENMP
#include "omp.h"
#include "sys/time.h"
#endif

const Double_t MpdTpcKalmanFilter::fgkChi2Cut = 20; //50; //20; //100;
FILE *lunTpc = 0x0; //fopen("dl.dat","w");

//__________________________________________________________________________
MpdTpcKalmanFilter::MpdTpcKalmanFilter() 
  : FairTask(),
    fNofEvents(0),
    fNTracks(0),
    fNPass(1),
    fHits(0x0),
    fKHits(new TClonesArray("MpdKalmanHit")),
    fTracks(new TClonesArray("MpdTpcKalmanTrack")),
    fTrackCand(new TClonesArray("MpdTpcKalmanTrack")),
    fLHEtracks(0x0),
    fMCtracks(0x0),
    fTpcPoints(0x0),
    fLayPointers(0x0),
    fhLays(0x0),
    fVertZ(0.0),
    fZflag(0), 
    fModular(0)
    //fPadPlane(TpcPadPlane::Instance())
    //fSecGeo(MpdTpcSectorGeo::Instance())
{
  /// Default constructor
}

//__________________________________________________________________________
MpdTpcKalmanFilter::MpdTpcKalmanFilter(const char *name,
                                       const char *title)
  : FairTask(name),
    fNofEvents(0),
    fNTracks(0),
    fNPass(1),
    fHits(0x0),
    fKHits(new TClonesArray("MpdKalmanHit")),
    fTracks(new TClonesArray("MpdTpcKalmanTrack")),
    fTrackCand(new TClonesArray("MpdTpcKalmanTrack")),
    fLHEtracks(0x0),
    fMCtracks(0x0),
    fTpcPoints(0x0),
    fLayPointers(0x0),
    fhLays(0x0),
    fVertZ(0.0),
    fZflag(0),
    fModular(0)
    //fPadPlane(TpcPadPlane::Instance())
    //fSecGeo(MpdTpcSectorGeo::Instance())
{
  /// Constructor
  FairTask *dedx = new MpdTpcDedxTask();
  Add(dedx);
}

//__________________________________________________________________________
MpdTpcKalmanFilter::~MpdTpcKalmanFilter() 
{
  /// Destructor
  delete fHits;
  delete fKHits;
  delete fTracks;
  delete fTrackCand;
  delete [] fLayPointers;
  delete fhLays;
}

//__________________________________________________________________________
InitStatus MpdTpcKalmanFilter::Init() {

  cout << "InitStatus MpdTpcKalmanFilter::Init\n\n";

  FairRootManager *manager = FairRootManager::Instance();
  fSecGeo = MpdTpcSectorGeo::Instance();

  fhLays = new TH1F("hLays","TPC layers",150,0,150);

  fHits = (TClonesArray *)manager->GetObject("TpcCluster");
  if (fHits == 0x0) fHits = (TClonesArray *)manager->GetObject("TpcHit");
  if ( ! fHits ) {
    cout << "-I- "<< GetName() << "::Init: No MpdTpcHit array!" << endl;
    return kERROR;
  }

  //fKHits->SetOwner(kTRUE);
  fLHEtracks = (TClonesArray *)manager->GetObject("LheGeantTrack");
  fMCtracks = (TClonesArray *)manager->GetObject("MCTrack");
  fTpcPoints = (TClonesArray *)manager->GetObject("TpcPoint");

#ifdef GEANT
  //fListMCtracks = (TClonesArray *)fManager->GetObject("MCTrack");
#endif

  fNTracks = fNofEvents = 0;
  fNPass = 2; //2;

  Register();

  // Get event number offset
  TFile* f = new TFile(FairRootManager::Instance()->GetInFile()->GetName(),"READ"); // EL 
#ifdef XROOTD 
  if (!f->IsOpen()) f = new TXNetFile(FairRootManager::Instance()->GetInFile()->GetName(),"READ"); 
#endif 
  cout << f->GetName() << " " << manager->GetObject("MCEventHeader.") << endl; 
  TTree *cbmsim = (TTree*) f->FindObjectAny("cbmsim"); 
  FairMCEventHeader *head = 0x0; //new FairMCEventHeader();
  cbmsim->SetBranchAddress("MCEventHeader.",&head);
  cbmsim->GetEntry(0);
  Int_t event = head->GetEventID() - 1;
  f->Close(); 
  delete f; 
  fNofEvents += event;

  /*TH1F *hZ = */new TH1F ("hZ","Z of vertex",100,-50,50);
  /*TH1F *hPt = */new TH1F ("hPt","Pt of cand.",100,0,50);
  /*TH1F *hChi2 = */new TH1F ("hChi2","Chi2 of track",100,0,50);
  /*TH1F *hNhits = */new TH1F ("hNhits","Points per track",100,0,100);

  return kSUCCESS;
}

// -----   Private method SetParContainers   -------------------------------
void MpdTpcKalmanFilter::SetParContainers() {

  //return;
  // Get run and runtime database
  FairRunAna* run = FairRunAna::Instance();
  if ( ! run ) Fatal("SetParContainers", "No analysis run");

  FairRuntimeDb* db = run->GetRuntimeDb();
  if ( ! db ) Fatal("SetParContainers", "No runtime database");

  // Get TPC geometry parameter container
  db->getContainer("TpcGeoPar");
  db->getContainer("MpdTofGeoPar"); 
}

//__________________________________________________________________________
InitStatus MpdTpcKalmanFilter::ReInit() 
{
  return kSUCCESS;
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::Reset() 
{
  ///
  cout << " MpdTpcKalmanFilter::Reset  " << endl;
  ProcInfo_t proc;
  gSystem->GetProcInfo(&proc);
  cout << " User CPU time: " << proc.fCpuUser << " Memory: resident " << proc.fMemResident << ", virtual " << proc.fMemVirtual << endl;

  fhLays->Reset();

  fKHits->Delete();
  fTracks->Delete();
  fTrackCand->Delete();
  delete [] fLayPointers;
  fLayPointers = 0x0;
  for (Int_t i = 0; i < fgkLays; ++i) 
    for (Int_t j = 0; j < fgkSecs; ++j) fLaySecBegPointers[i][j] = fLaySecEndPointers[i][j] = -1; 
  fNTracks = 0;
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::Register() 
{
  ///
  FairTask *itsHF = (FairTask*) FairRun::Instance()->GetTask("Ideal STS hit Producer");
  TClonesArray *itsP = (TClonesArray*) FairRootManager::Instance()->GetObject("StsPoint");
  if (itsHF && itsP) FairRootManager::Instance()->
    Register("TpcKalmanTrack","MpdKalmanTrack", fTracks, kFALSE);
  else FairRootManager::Instance()->
    Register("TpcKalmanTrack","MpdKalmanTrack", fTracks, kTRUE);

  FairRootManager::Instance()->Register("TpcKalmanHit","KfHit", fKHits, kFALSE);
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::Finish() 
{
  ///
  //fGeantTracks->GetEntriesFast() << endl;

  fKHits->Delete();
  fTracks->Delete();
  fTrackCand->Delete();
  //fLheHits->Delete();
  //((TH1F*)gROOT->FindObjectAny("hChi2"))->Write();
  //((TH1F*)gROOT->FindObjectAny("hNhits"))->Write();
  //TTree *tree = (TTree*) gROOT->FindObjectAny("MCtracks");
  //if (tree) tree->Write();
  //tree = (TTree*) gROOT->FindObjectAny("KFtracks");
  //if (tree) tree->Write();
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::Exec(Option_t * option) 
{
  ///
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),__FUNCTION__);
  static Int_t first = 1;
  if (first && fHits->GetEntriesFast()) {
    first = 0;
    // Check geometry
    if (fHits->First()->GetUniqueID()) fModular = 1;
    cout << "\n !!!!! ***** ***** ***** !!!!! " << endl;
    if (fModular) {
      FillGeoScheme();
      cout << " ***** TPC with modular design of readout chambers ***** " << endl;
    } else cout << " ***** TPC with cylindrical design ***** " << endl;
    cout << " !!!!! ***** ***** ***** !!!!! " << endl;
  }

  cout << "\n\n  ***  Event # " << ++fNofEvents << endl;
  cout << " =====   MpdTpcKalmanFilter   =====\n";

  Reset();

  // Create Kalman hits
  if (fHits->GetEntriesFast() == 0) return;
  TString name = fHits->UncheckedAt(0)->GetName();
  cout << " Name: " << name << endl;
  if (name.Contains("TpcCluster")) Cluster2KalmanHits();
  else if (fModular == 0) MakeKalmanHits();
  else MakeKalmanHitsModul();

  // Evaluate vertex Z
  MpdVertexZfinder *vertexZ = (MpdVertexZfinder*) FairRun::Instance()->GetTask("MpdVertexZfinder");
  if (vertexZ) {
    vertexZ->SetHits(fKHits,fhLays);
    fVertZ = vertexZ->FindZ(fLayPointers, fZflag);
  }

  for (Int_t i = 0; i < fNPass; ++i) {
    fTrackCand->Delete();
    GetTrackSeeds(i);

    cout << "  Total number of hits for tracking: " << fHits->GetEntriesFast() << endl;
    cout << "  Total number of track seeds: " << fTrackCand->GetEntriesFast() << endl;

    DoTracking(i);
    StoreTracks();
    cout << "  Total number of found tracks: " << fTracks->GetEntriesFast() << endl;
    if (i != fNPass - 1) ExcludeHits(); // exclude used hits
  }
  RemoveShorts(); // remove short tracks (Nhits < 4)
  cout << "  Total number of found long tracks: " << fTracks->GetEntriesFast() << endl;
  if (0) {
    // To improve dL-resolution 
    Int_t ntr = fTracks->GetEntriesFast();
    for (Int_t i = 0; i < ntr; ++i) {
      MpdKalmanTrack *track = (MpdKalmanTrack*) fTracks->UncheckedAt(i);
      //track->GetCovariance()->Print();
      //track->GetParamNew()->Print();
      MpdKalmanFilter::Instance()->Refit(track, -1);
      track->SetLength(0);
      MpdKalmanFilter::Instance()->Refit(track, 1, kTRUE);
      //track->GetParamNew()->Print();
      //track->GetCovariance()->Print();
    }
  }
  AddHits(); // add hit objects to tracks
  GoToBeamLine(); // propagate tracks to the beam line
  GoOut(); // go outward for matching with outer detectors

#ifdef GEANT
  //CheckTracks();
#ifdef TRACK
  //PrintTracks(0);
#endif
#endif
  
#ifdef MASSPROD
  //fTpcPoints->Clear("C");
  //  fTstPoints->Clear("C");
#endif

  /*
  FairMCEventHeader *mchead = (FairMCEventHeader*) FairRootManager::Instance()->GetObject("MCEventHeader.");
  Int_t event = mchead->GetEventID();
  //FairEventHeader *head = (FairEventHeader*) FairRootManager::Instance()->GetObject("EventHeader.");
  FairEventHeader *head = FairRunAna::Instance()->GetEvtHeader();
  head->SetEventId(event);
  */

  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),__FUNCTION__);
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::FillGeoScheme() 
{
  /// Fill Kalman filter geometry manager info

  MpdKalmanGeoScheme *geo = MpdKalmanFilter::Instance()->GetGeo();
  TVector2 p2;
  Int_t nRows[2] = {fSecGeo->NofRowsReg(0), fSecGeo->NofRowsReg(1)};
  Int_t nSec = fSecGeo->NofSectors();
  Double_t dPhi = fSecGeo->Dphi();

  // Get size along Z
  TString volName = "tpc01sv";
  TGeoVolume *sv = gGeoManager->GetVolume(volName);
  TGeoShape *shape = sv->GetShape();
  Double_t dZ = ((TGeoTube*)shape)->GetDZ();

  if (TString(shape->ClassName()) == "TGeoTube") {
    // Old sensitive volume
    TGeoVolume *endPlateG10 = gGeoManager->GetVolume("tpc01ppS");
    dZ -= ((TGeoTube*)endPlateG10->GetShape())->GetDZ() * 2;
    TGeoVolume *endPlateAl = gGeoManager->GetVolume("tpc01bpS");
    dZ -= ((TGeoTube*)endPlateAl->GetShape())->GetDZ() * 2;
  } else if (TString(shape->ClassName()) == "TGeoPgon") {
    // New sensitive volume
    Double_t phi1 = ((TGeoPgon*)shape)->Phi1();
    if (TMath::Abs(phi1-dPhi*TMath::RadToDeg()/2) > 0.001) 
      Fatal("MpdTpcKalmanFilter::FillGeoScheme()"," !!! Inconsistent sens. volume parameters !!! "); 
    dZ = ((TGeoPgon*)shape)->GetDZ() * 2;
    //FairGeoNode* membrane = (FairGeoNode*) (passNodes->FindObject("tpc01mb"));
    //if (!membrane) membrane = (FairGeoNode*) (sensNodes->FindObject("tpc01mb")); // it was a test version !
    TGeoVolume *membrane = gGeoManager->GetVolume("tpc01mb");
    dZ += ((TGeoTube*)membrane->GetShape())->GetDZ(); // membrane
  } else Fatal("MpdTpcKalmanFilter::FillGeoScheme()"," !!! Unknown sensitive volume shape !!! ");

  // Use TGeoManager to describe readout chamber geometry
  //TGeoMedium *medium = gGeoManager->GetMedium("TPCmixture");
  //TGeoVolume *tpcVolOrig = gGeoManager->GetVolume("tpc01sv");
  TGeoMedium *medium = gGeoManager->GetMedium("air");
  TGeoVolume *tpcVolOrig = gGeoManager->GetVolume("tpcChamber1");
  TGeoVolume *tpcVol = new TGeoVolume("tpc01sv_new", tpcVolOrig->GetShape(), medium);
  //TGeoVolume* tpcsec[2];
  //Double_t rads[2];
  Double_t dx1 = fSecGeo->GetMinY() * TMath::Tan(dPhi/2);
  Double_t dx2 = fSecGeo->GetMaxY() * TMath::Tan(dPhi/2);
  Double_t dy = dZ, dz = (fSecGeo->GetMaxY() - fSecGeo->GetMinY()) / 2.;
  Double_t rad = (fSecGeo->GetMaxY() + fSecGeo->GetMinY()) / 2.;
  TGeoVolume *tpcsec = gGeoManager->MakeTrd1("tpcsec", medium, dx1, dx2, dy, dz), *vol = NULL;
  //TGeoVolume * Division(const char* name, const char* mother, Int_t iaxis, Int_t ndiv, Double_t start, Double_t step, Int_t numed = 0, Option_t* option = "");
  //gGeoManager->Division("tpcrow", "tpcsec", 3, nRows, 0., 0., 0, "N");  

  // The code below is based on TGeoTrd1::Divide()
  Int_t ndiv = nRows[0] + nRows[1];
  Double_t start = -dz, end = dz, dx1n, dx2n, zmax, step = fSecGeo->PadHeight(), zmin = start - step;
  for (Int_t id = 0; id < ndiv; ++id) {
    zmin += step;
    if (id == nRows[0]) step = fSecGeo->PadHeight(1);
    zmax = zmin + step;
    dx1n = 0.5 * (dx1 * (dz - zmin) + dx2 * (dz + zmin)) / dz;
    dx2n = 0.5 * (dx1 * (dz - zmax) + dx2 * (dz + zmax)) / dz;
    shape = new TGeoTrd1(dx1n, dx2n, dy, step/2.);
    vol = new TGeoVolume("tpcrow", shape, tpcsec->GetMedium()); 
    TGeoHMatrix *matr = new TGeoHMatrix();
    Double_t transl[3] = {0.0, 0.0, zmin+step/2};
    matr->SetTranslation(transl);
    tpcsec->AddNode(vol, id+1, matr);
  }
  //

  for (Int_t isec = 0; isec < nSec; ++isec) {
    Double_t phi = dPhi * isec;
    TGeoTranslation t(rad*TMath::Cos(phi), rad*TMath::Sin(phi), 0.);
    TGeoRotation r;
    Double_t phiDeg = TMath::RadToDeg() * phi;
    //r.SetAngles(90,phiDeg+90,0,0,90,phiDeg);
    r.SetAngles(90,phiDeg-90,0,0,90,phiDeg);
    TGeoCombiTrans c(t,r);
    TGeoHMatrix *matr = new TGeoHMatrix(c); 
    //gGeoManager->RegisterMatrix(matr);
    //	AddNode(const TGeoVolume* vol, Int_t copy_no, TGeoMatrix* mat = 0, Option_t* option = "")
    tpcVol->AddNode(tpcsec, isec+1, matr);
  }
  //gGeoManager->ClearPhysicalNodes();
  //gGeoManager->ClearThreadData();
  //gGeoManager->ClearThreadsMap();
  gGeoManager->ReplaceVolume(tpcVolOrig, tpcVol); 
  //gGeoManager->RefreshPhysicalNodes();

  // Fill GeoScheme
  TVector3 p3norm, p3pos;
  Double_t padHeight = fSecGeo->PadHeight();
  Double_t rStart = fSecGeo->GetMinY() - padHeight / 2;
  Int_t nRows2 = fSecGeo->NofRows();

  for (Int_t isec = 0; isec < nSec; ++isec) {
    Double_t phi = dPhi * isec;
    p3norm.SetXYZ(TMath::Cos(phi), TMath::Sin(phi), 0.);
    Double_t x0 = rStart * p3norm.X(), y0 = rStart * p3norm.Y();
    padHeight = fSecGeo->PadHeight();
    for (Int_t lay = 0; lay < nRows2; ++lay) {
      Int_t detId = lay * 1000000 + isec;
      geo->SetNormal(detId, p3norm); // normal vector
      if (lay >= nRows[0]) padHeight = fSecGeo->PadHeight(1); // outer ROC region
      x0 += padHeight * p3norm.X();
      y0 += padHeight * p3norm.Y();
      TGeoNode *node = gGeoManager->FindNode(x0, y0, 0.);
      geo->SetPath(detId, gGeoManager->GetPath()); // TGeo path
      geo->SetDetId(gGeoManager->GetPath(), detId); // path to detId
      const Double_t *transl = gGeoManager->GetCurrentMatrix()->GetTranslation();
      p3pos.SetXYZ(transl[0], transl[1], 0.);
      geo->SetGlobalPos(detId, p3pos); // padrow position
      TGeoTrd1 *shape = (TGeoTrd1*) node->GetVolume()->GetShape();
      if (TString(shape->Class()->GetName()) != "TGeoTrd1")  { cout << 
	  " !!! MpdTpcKalmanFilter::FillGeoScheme(): Wrong shape " << endl; exit(0); }
      p2.Set((shape->GetDx1()+shape->GetDx2())/2, shape->GetDy());
      geo->SetSize(detId, p2); // sector size
    }
    //exit(0);
  }      
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::RemoveShorts()
{
  /// Remove short tracks (Nhits < 4)
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),__FUNCTION__);
  Int_t nReco = fTracks->GetEntriesFast();
  for (Int_t i = 0; i < nReco; ++i) {
    MpdKalmanTrack *track = (MpdKalmanTrack*) fTracks->UncheckedAt(i);
    if (track->GetNofHits() < 4) fTracks->RemoveAt(i);
  }
  fTracks->Compress();
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),__FUNCTION__);
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::GoToBeamLine()
{
  /// Propagate tracks to the beam line (mult. scat. and energy loss corrections)
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),__FUNCTION__);
  const Int_t nR = 7;
  //const Double_t rad[nR] = {27.035, 27.57, 28.105, 30.64, 33.15, 33.665, 34.178};
  //const Double_t dx[nR] = {0.0031, 0.00085, 0.0031, 0.0003, 0.001, 0.00085, 0.001};
  const Double_t rad[nR] = {27.00, 27.01, 27.31, 27.32, 33.82, 33.83, 34.13};
  const Double_t dx[nR] = {7.8e-4, 1.194e-2, 7.8e-4, 3.3e-4, 7.8e-4, 1.194e-2, 9.6e-4};
  Bool_t ok = 0;
  Int_t nReco = fTracks->GetEntriesFast();

  for (Int_t i = 0; i < nReco; ++i) {
    MpdTpcKalmanTrack *track = (MpdTpcKalmanTrack*) fTracks->UncheckedAt(i);

    MpdKalmanHit hit;
    hit.SetDetectorID(-999);
    hit.SetType(MpdKalmanHit::kFixedR);
    for (Int_t j = nR-1; j >= 0; --j) {
      hit.SetPos(rad[j]);
      //MpdKalmanFilter::Instance()->GetGeo()->SetGlobalPos(&hit,TVector3(rad[j],0.,0.),kTRUE); // put hit in GeoScheme
      ok = MpdKalmanFilter::Instance()->PropagateToHit(track,&hit,kTRUE);
      if (!ok) { 
	fTracks->RemoveAt(i);
	break;
      }
      //continue;
      // Add multiple scattering
      TMatrixDSym *cov = track->Weight2Cov();
      Double_t th = track->GetParamNew(3);
      Double_t cosTh = TMath::Cos(th);
      Double_t angle2 = MpdKalmanFilter::Instance()->Scattering(track, dx[j]);
      (*cov)(2,2) += (angle2 / cosTh / cosTh);
      (*cov)(3,3) += angle2;
      //cov->Print();
      Int_t iok = 0;
      MpdKalmanFilter::Instance()->MnvertLocal(cov->GetMatrixArray(), 5, 5, 5, iok);
      track->SetWeight(*cov);
    }
    if (!ok) continue;
    TMatrixDSym *cov = track->Weight2Cov();

    // Update track
    track->SetParam(*track->GetParamNew());
    track->SetPos(track->GetPosNew());
    // Correct for energy loss
    //*
    Double_t theta = TMath::PiOver2() - track->GetParam(3);
    Double_t pt = 1. / track->GetParam(4);
    Double_t ptCor = CorrectForLoss(TMath::Abs(pt), theta, track->GetTrackID()); // ionization loss correction
    track->SetParam(4,TMath::Sign(1./ptCor,pt));
    track->SetParamNew(*track->GetParam());
    //*/
    
    //*
    hit.SetPos(0.);
    hit.SetMeas(0,track->GetParam(2));
    //hit.SetRphi(track->GetParam(2)); // track Phi - check if it is necessary !!!!!!!
    //MpdKalmanFilter::Instance()->GetGeo()->SetGlobalPos(&hit,TVector3(0.,0.,0.),kTRUE); // put hit in GeoScheme
    //*/
    /*
    MpdKalmanHitZ hit;
    hit.SetZ(0.);
    */
    //cout << i << " " << track->GetTrackID() << " " << track->GetLength() << " " << ((MpdKalmanHitR*)track->GetHits()->First())->GetLength() << endl;
    Double_t pos = track->GetPosNew();
    //Double_t pos = ((MpdKalmanHitR*)track->GetHits()->Last())->GetR(); // position at last hit
    //MpdKalmanFilter::Instance()->PropagateParamR(track, &hit, kTRUE);
    //track->GetCovariance()->Print();
    //(track->GetWeightAtHit()->Invert()).Print();
    MpdKalmanFilter::Instance()->PropagateToHit(track, &hit, kTRUE);
    //track->Weight2Cov();
    //track->GetCovariance()->Print();
    track->SetPos(pos); // save position after passing inner shell
    track->SetParam(*track->GetParamNew()); // !!! track params at PCA
    //cout << i << " " << track->GetTrackID() << " " << track->GetLength() << " " << ((MpdKalmanHitR*)track->GetHits()->First())->GetLength() << endl;
    track->SetLengAtHit(track->GetLength()-track->GetLengAtHit()); // track length from PCA to the nearest hit
  }
  fTracks->Compress();
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),__FUNCTION__);
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::StoreTracks()
{
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),__FUNCTION__);
  /// Transfer tracks from fTrackCand to fTracks

  Int_t nFound = fTracks->GetEntriesFast();
  for (Int_t i = 0; i < fNTracks; ++i) {
    MpdTpcKalmanTrack *track = (MpdTpcKalmanTrack*) fTrackCand->UncheckedAt(i);
    track->Weight2Cov();
    new ((*fTracks)[nFound++]) MpdTpcKalmanTrack(*track);
    //fTrackCand->RemoveAt(i);
  }
  fNTracks = 0;
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),__FUNCTION__);
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::AddHits()
{
  /// Add hit objects to tracks and compute number of wrongly assigned hits
  /// (hits with ID different from ID of the track)
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),__FUNCTION__);
  Int_t nFound = fTracks->GetEntriesFast();
  for (Int_t i = 0; i < nFound; ++i) {
    MpdTpcKalmanTrack *track = (MpdTpcKalmanTrack*) fTracks->UncheckedAt(i);
    Int_t nHits = track->GetNofHits();
    track->SetNofHits(nHits);
    TClonesArray &trHits = *track->GetTrHits();
    SetTrackID(track); // set track ID as ID of majority of its hits

    Int_t nWrong = 0, motherID = track->GetTrackID();
    // Get track mother ID
    FairMCTrack *mctrack = (FairMCTrack*) fMCtracks->UncheckedAt(motherID);
    while (mctrack->GetMotherId() >= 0) {
      motherID = mctrack->GetMotherId();
      mctrack = (FairMCTrack*) fMCtracks->UncheckedAt(mctrack->GetMotherId());
    }

    TObjArray *hits = track->GetHits();
    for (Int_t j = 0; j < nHits; ++j) {
      MpdKalmanHit *hit = (MpdKalmanHit*) hits->UncheckedAt(j);
      new (trHits[j]) MpdKalmanHit(*hit);
      MpdTpcHit *p = GetTpcHit(hit);
      Int_t motherID1 = p->GetTrackID();
      // Get point mother ID
      mctrack = (FairMCTrack*) fMCtracks->UncheckedAt(motherID1);
      while (mctrack->GetMotherId() >= 0) {
        motherID1 = mctrack->GetMotherId();
        mctrack = (FairMCTrack*) fMCtracks->UncheckedAt(mctrack->GetMotherId());
      }
      if (motherID1 != motherID) nWrong++;
    }
    track->SetNofWrong(nWrong);
    track->SetLastLay(((MpdKalmanHit*)track->GetHits()->First())->GetLayer());
  }
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),__FUNCTION__);
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::SetTrackID(MpdTpcKalmanTrack* track)
{
  /// Set track ID as ID of majority of its hits

  const Int_t idMax = 9999;
  vector<Int_t> ids(idMax);
  Int_t nHits = track->GetNofHits(), locMax = 0, size = idMax;
  TObjArray *hits = track->GetHits();

  for (Int_t i = 0; i < nHits; ++i) {
    MpdKalmanHit *hit = (MpdKalmanHit*) hits->UncheckedAt(i);
    Int_t id = GetHitID(hit);
    if (id >= size) { size = id + 1; ids.resize(size); }
    ++ids[id];
    if (ids[id] > ids[locMax]) locMax = id;
  }
  if (ids[locMax] > 1) track->SetTrackID(locMax);
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::GetTrackSeeds(Int_t iPass) 
{
  /// Build track seeds from central detector hits
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),__FUNCTION__);
  MpdKalmanGeoScheme *geo = MpdKalmanFilter::Instance()->GetGeo();
  Int_t layMax0 = ((MpdKalmanHit*)fKHits->First())->GetLayer();
  TH1F *hPt = (TH1F*) gROOT->FindObject("hPt");
  TH1F *hZ = (TH1F*) gROOT->FindObject("hZ");

  // Estimate Z-position of vertex
  // Loop over layers
  Int_t layBeg = layMax0, layEnd = layBeg - 2, iDir = -1, dLays = 6; //10;
  //Int_t layMax = layMax0, dLays = 6; //10;
  Double_t zToler = 10;
  if (iPass > 0) { 
    //layBeg = 0; 
    layBeg = ((MpdKalmanHit*)fKHits->Last())->GetLayer();
    layEnd = layBeg + 2; 
    iDir = 1; 
    dLays = 1; 
    zToler = 30; 
  }
  if (fZflag < 0) zToler = 50; // bad quality of vertex estimate

  MpdTpcKalmanTrack *track = 0x0;
  MpdTpcHit *h1 = 0x0, *h2 = 0x0;
  TVector3 pos1, pos2, posLoc;
  Double_t rad1, phi1, rad2, phi2;
  for (Int_t lay = layBeg; lay != layEnd; lay+=iDir) {
    Int_t nHits1 = (Int_t) fhLays->GetCellContent(lay+1,0), isec = -1;
    Int_t nHits2 = (Int_t) fhLays->GetCellContent(lay+dLays*iDir+1,0);
    //cout << "Hits: " << nHits1 << " " << nHits2 << endl;
    // Loop over hits in first layer
    for (Int_t ihit1 = 0; ihit1 < nHits1; ++ihit1) {
      MpdKalmanHit *hit1 = (MpdKalmanHit*) fKHits->UncheckedAt(fLayPointers[lay]+ihit1);
      if (hit1->GetIndex() >= 0) h1 = (MpdTpcHit*) fHits->UncheckedAt(hit1->GetIndex());
      if (hit1->GetFlag() != 1) continue;

      if (fModular) {
	//posLoc.SetXYZ(-hit1->GetMeas(0), hit1->GetDist(), hit1->GetMeas(1));
	posLoc.SetXYZ(hit1->GetMeas(0), hit1->GetDist(), hit1->GetMeas(1));
	isec = hit1->GetDetectorID() % 1000000;
	//pos1 = fPadPlane->fromSectorReferenceFrame(posLoc, isec);
	fSecGeo->Local2Global(isec, posLoc, pos1);
	rad1 = pos1.Pt();
	phi1 = pos1.Phi();
	//cout << posLoc.X() << " " << posLoc.Y() << " " << posLoc.Z() << endl;
	//cout << pos1.X() << " " << pos1.Y() << " " << pos1.Z() << endl;
	//cout << rad1 << " " << phi1 << endl;
      } else {
	rad1 = hit1->GetPos();
	phi1 = hit1->GetMeas(0) / rad1;
      }

      // Loop over hits in second (closer to the beam for the first pass) layer
      for (Int_t ihit2 = 0; ihit2 < nHits2; ++ihit2) {
	MpdKalmanHit *hit2 = (MpdKalmanHit*) fKHits->UncheckedAt(fLayPointers[lay+dLays*iDir]+ihit2);
	if (hit2->GetIndex() >= 0) h2 = (MpdTpcHit*) fHits->UncheckedAt(hit2->GetIndex());
	//if (h2->GetTrackID() != h1->GetTrackID()) continue; //!!! for test - exact ID matching
	if (hit2->GetFlag() != 1) continue;

	if (fModular) {
	  //posLoc.SetXYZ(-hit2->GetMeas(0), hit2->GetDist(), hit2->GetMeas(1));
	  posLoc.SetXYZ(hit2->GetMeas(0), hit2->GetDist(), hit2->GetMeas(1));
	  Int_t isec1 = hit2->GetDetectorID() % 1000000;
	  //if (TMath::Abs(isec-isec1) > 1 && TMath::Abs(isec-isec1) < fPadPlane->nSectors()-1) continue;
	  //pos2 = fPadPlane->fromSectorReferenceFrame(posLoc, isec1);
	  if (TMath::Abs(isec-isec1) > 1 && TMath::Abs(isec-isec1) < fSecGeo->NofSectors()-1) continue;
	  fSecGeo->Local2Global(isec1, posLoc, pos2);
	  rad2 = pos2.Pt();
	  if ((rad2 - rad1) * iDir < 0.5) continue;
	  phi2 = pos2.Phi();
	} else {
	  rad2 = hit2->GetPos();
	  phi2 = hit2->GetMeas(0) / rad2;
	}

	Double_t dPhi = MpdKalmanFilter::Instance()->Proxim(phi1,phi2) - phi1;
	if (TMath::Abs(dPhi) > TMath::PiOver4()) continue;
	//Double_t zvert = pos2.Z() - (pos1.Z()-pos2.Z()) / (rad1-rad2) * rad2;
	Double_t zvert = hit2->GetMeas(1) - (hit1->GetMeas(1)-hit2->GetMeas(1)) / (rad1-rad2) * rad2;
	if (TMath::Abs(zvert-fVertZ) > zToler) continue;
	if (hZ) hZ->Fill(zvert);
	Double_t pt = EvalPt(hit1, hit2);
	//cout << fNTracks << " " << ihit1 << " " << ihit2 << " " << hit1->GetLayer() << " " << hit2->GetLayer() << " " << rad1 << " " << rad2 << " " << hit1->GetMeas(1) << " " << hit2->GetMeas(1) << " " << h1->GetTrackID() << " " << h2->GetTrackID() << " " << zvert << " " /*<< hit1->GetUsage() << " " << hit2->GetUsage() << " "*/ << pt << endl;
	if (hPt) hPt->Fill(TMath::Abs(pt));
	//if (TMath::Abs(pt) > 5) cout << " ***!!! " << pt << endl;
	//if (TMath::Abs(pt) < 0.05) continue; // skip low-Pt tracks
	if (TMath::Abs(pt) < 0.02) continue; // skip low-Pt tracks
	if (TMath::Abs(pt) > 10.) continue; // skip high-Pt tracks
	TVector3 vertex(0,0,0);
	if (fModular) {
	  if (iPass == 0) track = new ((*fTrackCand)[fNTracks++]) 
	    MpdTpcKalmanTrack(hit1, hit2, vertex, pos1, pos2, pt);
	  else track = new ((*fTrackCand)[fNTracks++]) 
	    MpdTpcKalmanTrack(hit2, hit1, vertex, pos2, pos1, pt);
	  TString s1 = geo->Path (hit1->GetDetectorID());
	  track->SetNodeNew(s1);
	  //cout << hit1->GetDetectorID() << " " << track->GetNodeNew() << endl;
	} else {
	  if (iPass == 0) track = new ((*fTrackCand)[fNTracks++]) MpdTpcKalmanTrack(hit1, hit2, vertex, pt);
	  else track = new ((*fTrackCand)[fNTracks++]) MpdTpcKalmanTrack(hit2, hit1, vertex, pt);
	}
	if (lay <= layMax0 - 1) track->SetDirection(MpdKalmanTrack::kOutward);
	if (iPass > 0) { 
	  track->GetHits()->Clear(); 
	  track->GetHits()->Add(hit1); 
	  //track->GetHits()->Add(hit2); 
	  //if (fModular) track->SetNodeNew(geo->Path(hit2->GetDetectorID())); // 29-apr-12
	  // 7-may-12
	  track->SetParam(1,hit1->GetMeas(1)); // hit closest to the beam line
	  track->SetPos(TMath::Min(rad1,rad2));
	  track->SetParam(0,track->GetParam(0)/track->GetPosNew()*track->GetPos());
	  track->SetPosNew(track->GetPos());
	  track->SetParamNew(*track->GetParam());
	}
	//*
	if (fModular) {
	  /*TString s1, s2;
	  s1 += hit1->GetDetectorID() % 1000000; // sector No.
	  s2 += hit2->GetDetectorID() % 1000000; // sector No.
	  if (iPass > 0) { track->SetNode(s1); track->SetNodeNew(s2); }
	  else { track->SetNode(s2); track->SetNodeNew(s1); }
	  */
	  /*
	  TString s1 = geo->Path(hit1->GetDetectorID());
	  TString s2 = geo->Path(hit2->GetDetectorID());
	  if (iPass > 0) track->SetNodeNew(s2); // trick
	  else track->SetNodeNew(s1);
	  */
	}
	//*/
	if (h1) track->SetTrackID(h1->GetTrackID());
	//((MpdKalmanTrack*)fTrackCand->UncheckedAt(fNTracks-1))->GetParam()->Print();
	//cout << fNTracks-1 << endl;
	//((TpcLheKalmanTrack*)fTrackCand->UncheckedAt(fNTracks-1))->Weight2Cov()->Print();
      }
    }
  }
  //hLays->Write();
  //hZ->Write();
  //hPt->Write();
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),__FUNCTION__);
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::Cluster2KalmanHits()
{
  /// Create Kalman hits from Rec. Points found from clusters

  Int_t lay, layMax = 0, nClus = fHits->GetEntriesFast();
  // !!!!!!!!! Geometry dependent values
  //Double_t rMin = 20.5, rMax = 109.5, dR = (rMax-rMin)/50; // dR = 1.78; // old version 
  //Double_t rMin = 29.195, rMax = 99.81, dR = (rMax-rMin)/50; // 1.4123; // new version (with dead material)
  Double_t rMin = 34.195, rMax = 99.81, dR = (rMax-rMin)/50; // 1.1962; // new version (with dead material)
  //Double_t rMin = 35.00, rMax = 100.00, dR = (rMax-rMin)/50; // 1.1962; // new version (with dead material)
  // !!!!!!!!!
  Double_t xyz[3], rPhiErr = 0.05, zErr = 0.1;
  //TClonesArray hits("TpcLheHit");

  for (int j = 0; j < nClus; ++j ) {
    TpcCluster* clus = (TpcCluster*) fHits->UncheckedAt(j);
    xyz[0] = clus->ret_x();
    xyz[1] = clus->ret_y();
    xyz[2] = clus->ret_z();
    //TpcLheHit *hit = new (hits[j]) TpcLheHit(xyz, 0); 
    //hit->SetR (TMath::Sqrt (xyz[0]*xyz[0]+xyz[1]*xyz[1]));
    Double_t rad = TMath::Sqrt (xyz[0] * xyz[0] + xyz[1] * xyz[1]);
    Double_t rPhi = TMath::ATan2(xyz[1],xyz[0]) * rad;
    lay = (Int_t) ((rad - rMin) / dR);
    lay = TMath::Max (lay, 0);
    layMax = TMath::Max (lay, layMax);
    //hit->SetLayer(lay);
    fhLays->Fill(lay+0.1);
    // Create Kalman hits
    //MpdKalmanHit *hitK = new ((*fKHits)[j]) MpdKalmanHit(rad, rPhi, xyz[2],
    //		 rPhiErr, zErr, lay, -1);
    //(Int_t detID, Int_t nDim, HitType hitType, Double_t *meas, Double_t *err, Double_t *cosSin, Double_t signal, Double_t dist, Int_t index)
    Double_t meas[2] = {rPhi, xyz[2]};
    Double_t err[2] = {rPhiErr, zErr};
    Double_t cossin[2] = {1., 0.};
    MpdKalmanHit *hitK = new ((*fKHits)[j]) MpdKalmanHit(0, 2, MpdKalmanHit::kFixedR, meas, err, cossin, 0., rad, -1);
  }
  cout << " Max layer = " << layMax << " " << fKHits->GetEntriesFast() << endl;
  fKHits->Sort();

  fLayPointers = new Int_t [layMax+1];
  Int_t ipos = 0;
  for (Int_t i = layMax; i >= 0; --i) {
    //cout << i << " " << fhLays->GetCellContent(i+1,0) << endl;
    //if (ipos) cout << ((TpcLheHit*)fHits->UncheckedAt(ipos))->GetLayer() << " "
    //     << ((TpcLheHit*)fHits->UncheckedAt(ipos-1))->GetLayer() << endl;
    fLayPointers[i] = ipos;
    ipos += (Int_t) fhLays->GetCellContent(i+1,0);
  }
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::MakeKalmanHits()
{
  /// Create Kalman hits.

  Int_t nHits = fHits->GetEntriesFast();
  cout << " Number of merged hits: " << nHits << endl;
  Int_t nKh = 0, lay = 0, layMax = 0;
  for (int j = 0; j < nHits; ++j ) {
    MpdTpcHit* hit = (MpdTpcHit*) fHits->UncheckedAt(j);
    //if (hit->GetTrackID() != 1783) continue; // keep only one track
    lay = hit->GetLayer();
    layMax = TMath::Max (lay, layMax);
    fhLays->Fill(lay+0.1);
    // Add errors
    Double_t dRPhi = 0, dZ = 0;
    gRandom->Rannor(dRPhi,dZ);
    Double_t z = hit->GetZ() + dZ * hit->GetDz(); //add error
    Double_t rPhi = hit->GetRphi() + dRPhi * hit->GetDx(); //add error
    Double_t phi = rPhi / hit->GetR();
    //hit->SetX(hit->GetR()*TMath::Cos(phi));
    //hit->SetY(hit->GetR()*TMath::Sin(phi));
    // Create Kalman hits
    //(Int_t detID, Int_t nDim, HitType hitType, Double_t *meas, Double_t *err, Double_t *cosSin, Double_t signal, Double_t dist, Int_t index)
    Double_t meas[2] = {rPhi, z};
    Double_t err[2] = {hit->GetDx(), hit->GetDz()};
    Double_t cossin[2] = {1., 0.};
    MpdKalmanHit *hitK = new ((*fKHits)[nKh++]) 
      MpdKalmanHit(lay*1000000+nKh-1, 2, MpdKalmanHit::kFixedR, meas, err, cossin, hit->GetEnergyLoss()/hit->GetStep(), hit->GetR(), j);
    hitK->SetLength(hit->GetLength());
    //hitK->SetDedx (hit->GetEdep()/hit->GetStep());
    //MpdKalmanFilter::Instance()->GetGeo()->SetGlobalPos(hitK,TVector3(hit->GetX(),hit->GetY(),hit->GetZ()));
  }

  fLayPointers = new Int_t [layMax+1];
  Int_t ipos = 0;
  for (Int_t i = layMax; i >= 0; --i) {
    //cout << i << " " << fhLays->GetCellContent(i+1,0) << endl;
    //if (ipos) cout << ((TpcLheHit*)fHits->UncheckedAt(ipos))->GetLayer() << " "
    //     << ((TpcLheHit*)fHits->UncheckedAt(ipos-1))->GetLayer() << endl;
    fLayPointers[i] = ipos;
    fLaySecBegPointers[i][0] = ipos;
    ipos += (Int_t) fhLays->GetCellContent(i+1,0);
    fLaySecEndPointers[i][0] = ipos;
  }
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::MakeKalmanHitsModul()
{
  /// Create Kalman hits for the modular geometry of readout chambers.
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),__FUNCTION__);
  Int_t nHits = fHits->GetEntriesFast();
  cout << " Number of merged hits: " << nHits << endl;
  Int_t nKh = 0, lay = 0, layMax = 0;
  for (int j = 0; j < nHits; ++j ) {
    MpdTpcHit* hit = (MpdTpcHit*) fHits->UncheckedAt(j);
    lay = hit->GetLayer();
    layMax = TMath::Max (lay, layMax);
    fhLays->Fill(lay+0.1);
    // Add errors
    Double_t dX = 0, dZ = 0;
    gRandom->Rannor(dX, dZ);
    // Create Kalman hits
    //(Int_t detID, Int_t nDim, HitType hitType, Double_t *meas, Double_t *err, Double_t *cosSin, Double_t signal, Double_t dist, Int_t index)
    Double_t err[2] = {hit->GetDx(), hit->GetDz()};
    //Double_t meas[2] = {-hit->GetLocalX()-dX*err[0], hit->GetLocalZ()+dZ*err[1]};
    Double_t meas[2] = {hit->GetLocalX()+dX*err[0], hit->GetLocalZ()+dZ*err[1]};
    Double_t cossin[2] = {1., 0.};
    Int_t padID = hit->GetDetectorID();
    //TpcPadID padIDobj = TpcPadID::numberToPadID(padID);
    //padID = padIDobj.sector();
    padID = fSecGeo->Sector(padID);
    MpdKalmanHit *hitK = new ((*fKHits)[nKh++]) 
      MpdKalmanHit(lay*1000000+padID, 2, MpdKalmanHit::kFixedP, meas, err, cossin, hit->GetEnergyLoss()/hit->GetStep(), hit->GetLocalY(), j);
    hitK->SetLength(hit->GetLength());
    //hitK->SetDedx (hit->GetEdep()/hit->GetStep());
    //MpdKalmanFilter::Instance()->GetGeo()->SetGlobalPos(hitK,TVector3(hit->GetX(),hit->GetY(),hit->GetZ()));
  }
  fKHits->Sort();

  fLayPointers = new Int_t [layMax+1];
  Int_t ipos = 0;
  for (Int_t i = layMax; i >= 0; --i) {
    //cout << i << " " << fhLays->GetCellContent(i+1,0) << endl;
    //if (ipos) cout << ((TpcLheHit*)fHits->UncheckedAt(ipos))->GetLayer() << " "
    //     << ((TpcLheHit*)fHits->UncheckedAt(ipos-1))->GetLayer() << endl;
    fLayPointers[i] = ipos;
    ipos += (Int_t) fhLays->GetCellContent(i+1,0);
  }

  // Fill indices for different sectors
  /*
  Int_t iseco = -1, isec = 0;
  for (Int_t i = layMax; i >= 0; --i) {
    Int_t nLay = GetNofHitsInLayer(lay);
    Int_t indx0 = GetHitsInLayer(lay);
    ipos = indx0;

    for (Int_t indx = 0; indx != nLay; ++indx) {
      MpdKalmanHit *hit = (MpdKalmanHit*) fKHits->UncheckedAt(indx0+indx);
      isec = hit->GetDetectorID() % 1000000;
      if (isec != iseco) {
	fLaySecBegPointers[i].insert(pair<Int_t,Int_t>(isec,ipos+indx));
	if (iseco > 0) fLaySecEndPointers[i].insert(pair<Int_t,Int_t>(iseco,ipos+indx));
	iseco = isec;
      }	
    }
  }
  fLaySecEndPointers[0].insert(pair<Int_t,Int_t>(isec,nHits));
  */
  nHits = fKHits->GetEntriesFast();
  for (Int_t j = 0; j < nHits; ++j ) {
    MpdKalmanHit *hit = (MpdKalmanHit*) fKHits->UncheckedAt(j);
    lay = hit->GetLayer();
    Int_t isec = hit->GetDetectorID() % 1000000;
    if (fLaySecBegPointers[lay][isec] < 0) {
      fLaySecBegPointers[lay][isec] = j;
      fLaySecEndPointers[lay][isec] = j + 1;
    } else {
      if (fLaySecBegPointers[lay][isec] > j) fLaySecBegPointers[lay][isec] = j;
      if (fLaySecEndPointers[lay][isec] < j+1) fLaySecEndPointers[lay][isec] = j + 1;
    }
  }

  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),__FUNCTION__);
}

//__________________________________________________________________________
Double_t MpdTpcKalmanFilter::EvalPt(const MpdKalmanHit *hit1, const MpdKalmanHit *hit2) 
{
  /// Evaluate signed track Pt (curvature) assuming the track coming from the 
  /// primary vertex

  TVector3 pos, posLoc;
  Double_t rad1, phi1, rad2, phi2;

  if (fModular) {
    posLoc.SetXYZ(-hit1->GetMeas(0), hit1->GetDist(), 0.);
    Int_t isec = hit1->GetDetectorID() % 1000000;
    //pos = fPadPlane->fromSectorReferenceFrame(posLoc, isec);
    fSecGeo->Local2Global(isec, posLoc, pos);
    rad1 = pos.Pt();
    phi1 = pos.Phi();
    posLoc.SetXYZ(-hit2->GetMeas(0), hit2->GetDist(), 0.);
    isec = hit2->GetDetectorID() % 1000000;
    //pos = fPadPlane->fromSectorReferenceFrame(posLoc, isec);
    fSecGeo->Local2Global(isec, posLoc, pos);
    rad2 = pos.Pt();
    phi2 = pos.Phi();
  } else {
    rad1 = hit1->GetPos();
    rad2 = hit2->GetPos();
    phi1 = hit1->GetMeas(0) / rad1;
    phi2 = hit2->GetMeas(0) / rad2;
  }

  TVector2 vec1(rad1*TMath::Cos(phi1)-0.,rad1*TMath::Sin(phi1)-0.);
  TVector2 vec2(rad2*TMath::Cos(phi2)-0.,rad2*TMath::Sin(phi2)-0.);
  TVector2 vec21 = vec1 - vec2;
  Double_t cosAlpha = vec2 * vec21 / vec2.Mod() / vec21.Mod();
  Double_t rad = vec1.Mod() / 2. / TMath::Sin(TMath::ACos(cosAlpha));
  Double_t bz = FairRunAna::Instance()->GetField()->GetBz(0.,0.,0.);
  Double_t factor = 0.003 * bz / 10.; // 0.3 * 0.01 * 5kG / 10
  Double_t charge = phi1 - MpdKalmanFilter::Instance()->Proxim(phi1,phi2);
  if (hit1->GetLayer() < hit2->GetLayer()) charge = -charge;
  return factor * TMath::Abs(rad) * TMath::Sign(1., -charge);
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::DoTracking(Int_t iPass)
{
  /// Run Kalman tracking

  //struct timeval tvStart, tvEnd;
  //struct timezone tz;
  //gettimeofday(&tvStart, &tz);

  //fTracks->Sort(); // in descending order in Pt
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),__FUNCTION__);
  Int_t nTracks = fTrackCand->GetEntriesFast();
  TH1F *hChi2 = (TH1F*) gROOT->FindObjectAny("hChi2");
  TH1F *hNhits = (TH1F*) gROOT->FindObjectAny("hNhits");

  //#pragma omp parallel for
  for (Int_t i = 0; i < nTracks; ++i) {
    Int_t iok = 0;
    MpdTpcKalmanTrack *track = (MpdTpcKalmanTrack*) fTrackCand->UncheckedAt(i);

    //cout << " Track seed No. " << i << " " << track->GetTrackID() << " " << endl;
    // << track->GetPosNew() << endl;
    //track->GetParamNew()->Print();
    // Check if the seed hit was used already
    MpdKalmanHit *hit = (MpdKalmanHit*) track->GetHits()->First();
    if (hit->GetFlag() != 1) iok = -1;
    else {
      if (track->GetDirection() == MpdKalmanTrack::kOutward) { 
	//cout << " !!! " << track->GetNode() << " !!! " << track->GetNodeNew() <<endl;	
	GoOutward(track); 
	/*cout << i << " " << track->GetChi2() << endl;*/ 
	if (track->GetChi2()>1000) {
	  //cout << " !!! " << " " << track->GetNofHits() << " " << track->GetNode() << " " << track->GetNodeNew() << endl; 
	}
      }
      iok = RunKalmanFilter(track);
      //if (fNofEvents > 54) 
      //cout << iok << " " << track->GetChi2() << " " << track->GetNofHits() << endl;
    }
    //if (iOK != 0) track->SetStatus(-1);
    if (iok != 0) {
      //fTracks->RemoveAt(i);
      //cout << track->GetNofHits() << endl;
      //#pragma omp critical
      {
        fTrackCand->RemoveAt(i);
      }
      //delete track; // !!! Object has been deleted already
    } else {
      /*
      Int_t nHits = track->GetNofHits();
      //#pragma omp critical
      {
        hChi2->Fill(track->GetChi2());
      }
      //#pragma omp critical
      {
        hNhits->Fill(nHits+0.1);
      }
      */
      //if (track->GetChi2() < 1.) cout << " Strange: " << track->GetChi2() << " " << nHits << " " << track->GetDirection() << endl;
      // Mark hits as being used 
      /*
      TObjArray *hits = track->GetHits();
      for (Int_t j = 0; j < nHits; ++j) {
	hit = (MpdKalmanHit*) hits->UncheckedAt(j);
	hit->SetFlag(-1);
      }
      */
    }
  }
  //fTracks->Compress();
  fTrackCand->Compress();
  /*
  nTracks = fTrackCand->GetEntriesFast();
  for (Int_t i = 0; i < nTracks; ++i) {
    TpcLheKalmanTrack *track = (TpcLheKalmanTrack*) fTrackCand->UncheckedAt(i);
    Int_t nHits = track->GetNofHits(), iCharge = 0;
    TObjArray *hits = track->GetHits();
    TVector3 pmom, pmom1;
    for (Int_t j = 0; j < nHits; ++j) {
      TpcLheHit *hit = (TpcLheHit*) hits->UncheckedAt(j);
      if (j == nHits-1) {
	// Compare with MC track
	Int_t nMC = fLHEtracks->GetEntriesFast();
	for (Int_t itr = 0; itr < nMC; ++itr) {
	  TpcLheTrack *trMC = (TpcLheTrack*) fLHEtracks->UncheckedAt(itr);
	  if (trMC->GetTrackNumber() != hit->GetTrackID()) continue;
	  pmom = trMC->GetMomentum();
	  break;
	}	
	TpcPoint *point = (TpcPoint*) fTpcPoints->UncheckedAt(hit->GetUniqueID());
	point->Momentum(pmom1);
	iCharge = -hit->GetRefIndex();
      }
      printf(" %4d",hit->GetTrackID());
    }
    printf(" hits\n");
    cout << " Momentum: " << i << " " << 1./track->GetParamNew(4) << " " << pmom1.Pt()*iCharge << " " << pmom.Pt() << " " << track->GetChi2() << endl;
  } // for (Int_t i = 0; i < nTracks;
  */

  // Remove doubles
  RemoveDoubles();
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),__FUNCTION__);

  //gettimeofday(&tvEnd, &tz);
  //int dif = 1000000*((int)tvEnd.tv_sec - (int)tvStart.tv_sec) + (tvEnd.tv_usec - tvStart.tv_usec);
  //cout<<"TPC::DoTracking "<<dif<<" ns\n";
}

//__________________________________________________________________________
Int_t MpdTpcKalmanFilter::RunKalmanFilter(MpdTpcKalmanTrack *track)
{
  /// Run Kalman filter

  //cout << fHits->GetEntriesFast() << endl;
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),__FUNCTION__);
  const Double_t dStepMax = 9; // max track length distance between consecutive hits
  MpdKalmanGeoScheme *geo = MpdKalmanFilter::Instance()->GetGeo();

  Int_t layMax = ((MpdKalmanHit*)fKHits->First())->GetLayer();
  MpdKalmanHit *hitOK = (MpdKalmanHit*) track->GetHits()->Last();
  Int_t layOK = hitOK->GetLayer();
  MpdKalmanTrack::TrackDir trackDir = track->GetDirection();
  //Int_t layBeg = layOK - 1, layEnd = -1, dLay = -1;
  Int_t layBeg = layOK - 1, layEnd = ((MpdKalmanHit*)fKHits->Last())->GetLayer() - 1, dLay = -1;
  if (trackDir == MpdKalmanTrack::kOutward) {
    layBeg = layOK + 1;
    layEnd = layMax + 1;
    dLay = 1;
  }
  
  //Int_t indxOK = hits->IndexOf(hitOK);
  //Int_t nHits = hits->GetEntriesFast();
  Int_t miss = 0, iOK = 1;
  if (fModular) iOK = 0;
  TMatrixDSym pointWeight(5), pointWeightTmp(5), saveWeight(5), weightOK = *track->GetWeight();
  TMatrixD param(5,1), paramTmp(5,1), paramOK = *track->GetParamNew();
  Double_t saveR = 0., rOK = track->GetPosNew(), saveLeng = 0., lengOK = 0.;
  TString saveNode, nodeOK = track->GetNodeNew();
  //cout << " Starting hit: " << hitOK->GetLayer() << " " << hitOK->GetTrackID() << " " << hitOK->GetUsage() << endl;
  
  // Loop over layers
  Int_t nLay;
  for (Int_t lay = layBeg; lay != layEnd; lay+=dLay) {
    //#pragma omp critical
    {
      //nLay = GetNofHitsInLayer(lay);
    }
    //Int_t indx0 = GetHitsInLayer(lay);

    if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),"Time1");
    Double_t dChi2Min = 1.e+6, padH = lay < MpdTpcSectorGeo::Instance()->NofRowsReg(0) ?
      MpdTpcSectorGeo::Instance()->PadHeight() : MpdTpcSectorGeo::Instance()->PadHeight(1);
    MpdKalmanHit *hitMin = 0x0;
    //cout << " lay, nLay: " << lay << " " << nLay << " " << indx0 << endl;
    Int_t indxBeg = 0, indxEnd = nLay, dIndx = 1, secFirst = -1, isecHit = -1, isecDif = 0;
    if (trackDir == MpdKalmanTrack::kOutward) {
      indxBeg = nLay - 1;
      indxEnd = -1;
      dIndx = -1;
    }
    Int_t isec = 0;
    if (fModular) isec = SectorNo(track->GetNodeNew()); // sector number
    Int_t sector3[3] = {-1, -1, -1};
    MpdTpcKalmanTrack* trackBr[3] = {NULL, NULL, NULL};

    // Loop over hits of 3 sectors (central (where track is located) and +- 1)
    if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),"Hits");

    Int_t i3sec[3] = { 0, -1, 1};
    for (Int_t iadj = 0; iadj < 3; ++iadj) {
      Int_t isecLoop = isec + i3sec[iadj];
      //if (isecLoop < 0) isecLoop += fPadPlane->nSectors();
      //else if (isecLoop > fPadPlane->nSectors() - 1) isecLoop -= fPadPlane->nSectors();
      if (isecLoop < 0) isecLoop += fSecGeo->NofSectors();
      else if (isecLoop > fSecGeo->NofSectors() - 1) isecLoop -= fSecGeo->NofSectors();

      if (fLaySecBegPointers[lay][isecLoop] < 0) continue;
      indxBeg = fLaySecBegPointers[lay][isecLoop];
      indxEnd = fLaySecEndPointers[lay][isecLoop];
      if (trackDir == MpdKalmanTrack::kOutward) {
	Int_t tmp = indxBeg;
	indxBeg = indxEnd - 1;
	indxEnd = tmp - 1;
      }
      MpdKalmanHit *hit = NULL;

      for (Int_t indx = indxBeg; indx != indxEnd; indx+=dIndx) {
	//MpdKalmanHit *hit = (MpdKalmanHit*) fKHits->UncheckedAt(indx0+indx);
	hit = (MpdKalmanHit*) fKHits->UncheckedAt(indx);
	// Exclude used hits
	if (hit->GetFlag() != 1) continue;
	// !!! Exact ID match
	//if (GetPoint(hit)->GetTrackID() != track->GetTrackID()) continue;
	//if (GetTpcHit(hit)->GetTrackID() != track->GetTrackID()) continue;

	// Check if the hit within some window (15x9cm for the moment - check!!!)
	if (!fModular) {
	  if (trackBr[0] == NULL) trackBr[0] = new MpdTpcKalmanTrack(*track);
	  if (TMath::Abs(hit->GetMeas(1)-trackBr[0]->GetParamNew(1)) > 9) continue;
	  if (TMath::Abs(MpdKalmanFilter::Instance()->Proxim(trackBr[0],hit)
			 -trackBr[0]->GetParamNew(0)) > 15) continue;
	} else {
	  isecHit = hit->GetDetectorID() % 1000000;
	  isecDif = isecHit - isec;
	  // Track and hit not in adjacent sectors
	  //if (TMath::Abs(isecDif) > 1 && TMath::Abs(isecDif) < fPadPlane->nSectors()-1) break;
	  //if (TMath::Abs(isecDif) > 1) isecDif -= TMath::Sign(fPadPlane->nSectors(),isecDif);
	  if (TMath::Abs(isecDif) > 1 && TMath::Abs(isecDif) < fSecGeo->NofSectors()-1) break;
	  if (TMath::Abs(isecDif) > 1) isecDif -= TMath::Sign(fSecGeo->NofSectors(),isecDif);
	  ++isecDif;
	  if (isecHit == secFirst && sector3[isecDif] < 0) break; // excluded sector
	  if (isecHit != secFirst && trackBr[isecDif] == NULL) trackBr[isecDif] = new MpdTpcKalmanTrack(*track);
	  if (TMath::Abs(hit->GetMeas(1)-trackBr[isecDif]->GetParamNew(1)) > 9) continue;
	}

	Double_t leng = trackBr[isecDif]->GetLength();
	if (fModular && isecHit != secFirst) {
	  // Modular geometry
	  secFirst = isecHit;
	  sector3[isecDif] = 1;
	  if (trackBr[isecDif]->GetNode() == "") trackBr[isecDif]->SetNodeNew("");
	  //if (!MpdKalmanFilter::Instance()->PropagateToHit(&tr1,hit,kTRUE,kTRUE) ||
	  //  TMath::Abs(leng - tr1.GetLength()) < MpdKalmanFilter::Instance()->fgkEpsilon) { 
	  if (!MpdKalmanFilter::Instance()->PropagateToHit(trackBr[isecDif],hit,kTRUE,kTRUE)) {
	    // Not reaching padrow
	    sector3[isecDif] = -1; 
	    //iOK = 0;
	    break; //continue; 
	  }
	  //tr1.GetParamNew()->Print();
	  //cout << " New node: " << tr1.GetNodeNew() << endl;
	  iOK = 1;
	  // Check for edge effect
	  if (TMath::Abs(trackBr[isecDif]->GetParamNew(0)) > geo->Size(hit).X() + 0.5) {
	    // Outside target sector
	    sector3[isecDif] = -1; 
	    break; //continue;
	  }
	  trackBr[isecDif]->SetNode(trackBr[isecDif]->GetNodeNew());
	} else if (fModular == 0) {
	  if (!MpdKalmanFilter::Instance()->PropagateToHit(trackBr[0],hit)) { iOK = 0; break; }
	  sector3[0] = 1;
	} // if (fModular && isecHit != secFirst)

	// Add multiple scattering in TPC gas (TPCmixture)
	if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),"Scat");
	Double_t step = trackBr[isecDif]->GetLength() - leng;
	Double_t th = trackBr[isecDif]->GetParamNew(3);
	Double_t cosTh = TMath::Cos(th);
	if (1 && step > 1.e-4) {
	  Double_t x0 = 13363.6; // rad. length - TPCMixture
	  TMatrixDSym *cov = trackBr[isecDif]->Weight2Cov();
	  Double_t angle2 = MpdKalmanFilter::Instance()->Scattering(trackBr[isecDif], x0, step);
	  //cout << " Scat: " << hit->GetLayer() << " " << step << " " << TMath::Sqrt(angle2) << endl;
	  (*cov)(2,2) += (angle2 / cosTh / cosTh);
	  (*cov)(3,3) += angle2;
	  Int_t iok = 0;
	  MpdKalmanFilter::Instance()->MnvertLocal(cov->GetMatrixArray(), 5, 5, 5, iok);
	  trackBr[isecDif]->SetWeight(*cov);
	}
	if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),"Scat");
	Double_t dChi2 = 999999.0;
	if (trackBr[isecDif]->GetLength() - track->GetLength() < padH / TMath::Abs(cosTh) * 1.8) 
	  dChi2 = MpdKalmanFilter::Instance()->FilterHit(trackBr[isecDif],hit,pointWeight,param);
	/*cout << lay << " " << isecHit << " " << isec << " " << dChi2 << " " << track->GetTrackID() 
	  << " " << GetTpcHit(hit)->GetTrackID() << endl;*/

	if (dChi2 < dChi2Min) {
	  dChi2Min = dChi2;
	  hitMin = hit;
	  saveWeight = *trackBr[isecDif]->GetWeight();
	  saveR = trackBr[isecDif]->GetPosNew();
	  saveLeng = trackBr[isecDif]->GetLength(); 
	  saveNode = trackBr[isecDif]->GetNodeNew();
	  // temporary storage for the current track
	  paramTmp = param;
	  pointWeightTmp = pointWeight;
	  /*
	    cout << " New min dChi2 = " << dChi2 << " " << hitMin->GetRphi() << " " << hitMin->GetZ() << endl;
	    cout << track->GetParamNew(0) << " " << track->GetParamNew(1) << endl;
	    cout << hit->GetRphi() << " " << hit->GetZ() << endl;
	    cout << param(0,0) << " " << param(1,0) << endl;
	  */
	  //paramTmp.Print();
	}
      } // for (Int_t indx = indxBeg; indx != indxEnd;
      // No hits in the sector where the track is located (or its padrow can't be reached)- try both neighbours
      //26-dec-2012 if (fModular && (trackBr[1] == NULL || sector3[1] < 0)) continue;
      if (fModular && trackBr[1] == NULL) continue;
      if (fModular && sector3[1] < 0) {
	if (trackBr[isecDif]->GetParam(0) > 0) ++iadj; // skip one step in the loop
	continue;
      }
      if (!fModular || isecHit < 0 || TMath::Abs(trackBr[isecDif]->GetParamNew(0)) < geo->Size(hit).X() - 1.0) break;
      // Close to sector edge - try adjacent sector
      //i3sec = (trackBr[isecDif]->GetParamNew(0) < 0) ? -1 : 1;
      //if (-trackBr[isecDif]->GetParamNew(0) > 0) ++iadj; // skip one step in the loop
      if (trackBr[isecDif]->GetParamNew(0) > 0) ++iadj; // skip one step in the loop
    } //for (Int_t iadj = 0; iadj < 3; ++iadj)
    if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),"Time1");
    if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),"Hits");

    if (!iOK) {
      // Curling track
      //*
      track->SetPos(rOK); //
      track->SetPosNew(rOK); //
      track->SetParam(paramOK); //
      track->SetParamNew(paramOK); //
      track->SetWeight(weightOK); //
      track->SetLength(lengOK); //
      track->SetNode(nodeOK);
      track->SetNodeNew(nodeOK);
      //*/
      for (Int_t jj = 0; jj < 3; ++jj) if (trackBr[jj]) delete trackBr[jj];
      if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),__FUNCTION__);
      return -1;
    } else if (dChi2Min < fgkChi2Cut) {
      //if (fNofEvents > 54) 
      //cout << lay << " " << hitMin->GetDetectorID() % 1000000 << " " << isec << " " << dChi2Min << " " 
      //  << track->GetTrackID() << " " << GetTpcHit(hitMin)->GetTrackID() << " " << saveLeng-track->GetLength() << endl;
      //layOK = lay;
      hitOK = hitMin;
      track->GetHits()->Add(hitOK);
      miss = 0;
      // Restore track params at the best hit
      track->SetChi2(track->GetChi2()+dChi2Min);
      //if (track->GetTrackID() >= 0) cout << " *** Adding hit: " << track->GetTrackID() << " " << GetHitID(hitOK) << " " << hitOK->GetLayer() << " " << hitOK->GetDetectorID() << " "/*<< hitOK->GetTrackID() << " " << hitOK->GetUsage() << " "*/ << dChi2Min << " " << track->GetChi2() << " " << track->GetLength() << " " << saveLeng-track->GetLength() << endl;
      saveWeight += pointWeightTmp;
      track->SetWeight(saveWeight);
      track->SetPos(saveR); 
      track->SetPosNew(saveR);
      track->SetLength(saveLeng);
      track->SetParam(paramTmp); 
      track->SetParamNew(paramTmp);
      track->SetNode(saveNode);
      track->SetNodeNew(saveNode);
      if (track->GetDirection() == MpdKalmanTrack::kInward) {
	// Save track params at last hit
	track->SetLengAtHit(saveLeng); 
	track->SetParamAtHit(paramTmp);
	track->SetWeightAtHit(saveWeight);
      } else {
	// 14-may-12
	track->GetSteps().insert(pair<Int_t,Double_t>(lay,saveLeng));
      }
      // Check if the accepted hit is the same as the seeded hit
      //if (hitOK->GetLayer() == f2ndHit->GetLayer() && hitOK != f2ndHit) return -1; // abandon track
      paramOK = paramTmp;
      rOK = saveR;
      lengOK = saveLeng;
      weightOK = saveWeight;
      nodeOK = saveNode;
    } else {
      ++miss;	
      Int_t ibr = -1;	
      for (Int_t jj = 0; jj < 3; ++jj) { 
	if (sector3[jj] > 0) { ibr = jj; break; }
      }
      if (ibr < 0 || TMath::Abs(trackBr[ibr]->GetLength() - track->GetLength()) > dStepMax) {
	//cout << " !!! Too few branches " << sector3.size() << endl; 
	//exit(0); 
	track->SetPos(rOK); //
	track->SetPosNew(rOK); //
	track->SetParam(paramOK); //
	track->SetParamNew(paramOK); //
	track->SetWeight(weightOK); //
	track->SetLength(lengOK); //
	//27-04-2012 track->SetNode(nodeOK);
	track->SetNodeNew(nodeOK);
      } else {
	Int_t ip = ibr;
	Double_t leng = 999999.;
	for (Int_t jj = ip ; jj < 3; ++jj) {
	  if (sector3[jj] < 0) continue;
	  if (trackBr[jj]->GetLength() < leng) { ip = jj; leng = trackBr[ip]->GetLength(); } 
	}	  
	// 12-may-12
	Int_t secPos1 = trackBr[ip]->GetNodeNew().Index("tpcsec");
	Int_t pos1 = trackBr[ip]->GetNodeNew().Index("/",secPos1);
	Int_t secPos2 = track->GetNodeNew().Index("tpcsec");
	Int_t pos2 = track->GetNodeNew().Index("/",secPos2);
	if (trackBr[ip]->GetNodeNew()(secPos1,pos1-secPos1) != track->GetNodeNew()(secPos2,pos2-secPos2)) {
	  track->SetPos(rOK); //
	  track->SetPosNew(rOK); //
	  track->SetParam(paramOK); //
	  track->SetParamNew(paramOK); //
	  track->SetWeight(weightOK); //
	  track->SetLength(lengOK); //
	  track->SetNodeNew(nodeOK);
	} else {
	  track->SetPos(trackBr[ip]->GetPosNew()); //
	  track->SetPosNew(trackBr[ip]->GetPosNew()); //
	  track->SetParam(*trackBr[ip]->GetParamNew()); //
	  track->SetParamNew(*trackBr[ip]->GetParamNew()); //
	  track->SetWeight(*trackBr[ip]->GetWeight()); //
	  track->SetLength(trackBr[ip]->GetLength()); //
	  track->SetNode(trackBr[ip]->GetNodeNew());
	  track->SetNodeNew(trackBr[ip]->GetNodeNew());
	}
      }	
      if (miss > 1) {
	/*
	track->SetPos(rOK); //
	track->SetPosNew(rOK); //
	track->SetParam(paramOK); //
	track->SetParamNew(paramOK); //
	*/
	//return -1;
	track->SetPos(rOK); //
	track->SetPosNew(rOK); //
	track->SetParam(paramOK); //
	track->SetParamNew(paramOK); //
	track->SetWeight(weightOK); //
	track->SetLength(lengOK); //
	track->SetNodeNew(nodeOK);
	for (Int_t jj = 0; jj < 3; ++jj) if (trackBr[jj]) delete trackBr[jj];
	if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),__FUNCTION__);
	return 0; // keep short tracks
      }
    }
    //cout << " lay, miss: " << lay << " " << miss << " " << dChi2Min << " " << fChi2 << endl;
    for (Int_t jj = 0; jj < 3; ++jj) if (trackBr[jj]) delete trackBr[jj];
  } // for (Int_t lay = layBeg; lay != layEnd; lay+=dLay) {

  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),__FUNCTION__);
  return 0;
}

//__________________________________________________________________________
Int_t MpdTpcKalmanFilter::SectorNo(const char* cpath)
{
  /// Extract sector number from TGeo path

  TString path(cpath);
  Int_t ipos = path.Index("tpcrow_");
  if (ipos < 0) return -1;
  TString sub = TString(path(ipos-3, 2)); // check for 2 digits
  if (!sub.IsDigit()) sub = TString(path(ipos-2, 1)); // 1 digit
  return sub.Atoi() - 1;
}

//__________________________________________________________________________
MpdTpcHit* MpdTpcKalmanFilter::GetTpcHit(MpdKalmanHit *hit)
{
  /// Get MpdTpcHit pointer for the Kalman hit

  return (MpdTpcHit*) fHits->UncheckedAt(hit->GetIndex());
}

//__________________________________________________________________________
Int_t MpdTpcKalmanFilter::GetHitID(MpdKalmanHit *hit)
{
  /// Get hit ID from MpdTpcHit ID

  MpdTpcHit *h = GetTpcHit(hit);
  return h->GetTrackID();
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::GoOutward(MpdTpcKalmanTrack *track)
{
  /// Propagate track in the outward direction

  TMatrixDSym weight = *track->GetWeight(); // save starting weight matrix
  Int_t ok = RunKalmanFilter(track);
  Int_t lastLay = ((MpdKalmanHit*)track->GetHits()->Last())->GetLayer();
  //cout << ok << " " << track->GetChi2() << " " << track->GetNofHits() << " " << lastLay << " " << ((MpdKalmanHit*)track->GetHits()->Last())->GetMeas(1) << endl;
  track->SetLastLay(lastLay);
  track->SetLength(0.);
  BackTrace(track, weight);
  //cout << track->GetChi2() << " " << track->GetTrackID() << " " << track->GetNofHits() << endl;
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::BackTrace(MpdTpcKalmanTrack *track, TMatrixDSym &weight0, Int_t iDir)
{
  /// Propagate track using already found hits

  track->StartBack();
  if (iDir > 0) track->SetDirection(MpdKalmanTrack::kInward);
  else track->SetDirection(MpdKalmanTrack::kOutward);
  Int_t nHits = track->GetNofHits();
  //28-apr-2012 if (nHits == 1) track->SetWeight(weight0); // restore original weight
  //if (fModular && track->GetNode() == "") track->SetNodeNew(""); // 27-04-2012

  TMatrixD param(5,1);
  TMatrixDSym weight(5), pointWeight(5);

  Int_t ibeg = iDir > 0 ? 0 : nHits-1;
  Int_t iend = iDir > 0 ? nHits-1 : 0;
  Int_t lay0 = -1, lay = -1, ifirst = 1;
  iend += iDir;

  MpdKalmanHit *hit = 0x0;
  map<Int_t,Double_t>& stepMap = track->GetSteps();
  for (Int_t i = ibeg; i != iend; i+=iDir) {
    //for (Int_t i = 1; i < nHits; ++i) {
    if (i == ibeg && iDir > 0) track->SetLength(0.); // for correct track length
    hit = (MpdKalmanHit*) track->GetHits()->UncheckedAt(i);
    lay0 = lay;
    lay = hit->GetLayer();
    if (i == ibeg) continue; // skip first hit

    Double_t leng = track->GetLength(), stepBack = -1;
    if (stepMap.find(lay) != stepMap.end() && stepMap.find(lay0) != stepMap.end()) 
      stepBack = TMath::Abs (stepMap[lay] - stepMap[lay0]);
    Bool_t ok = MpdKalmanFilter::Instance()->PropagateToHit(track, hit, kTRUE, kTRUE, stepBack);
    if (!ok) continue;
    //cout << track->GetLength() << endl;
    // Add multiple scattering in TPC gas (TPCmixture)
    Double_t step = track->GetLength() - leng;
    if (1 && step > 1.e-4) {
      Double_t x0 = 13363.6; // rad. length
      TMatrixDSym *cov = track->Weight2Cov();
      Double_t th = track->GetParamNew(3);
      Double_t cosTh = TMath::Cos(th);
      Double_t angle2 = MpdKalmanFilter::Instance()->Scattering(track, x0, step);
      //cout << " Scat: " << hit->GetLayer() << " " << step << " " << TMath::Sqrt(angle2) << endl;
      (*cov)(2,2) += (angle2 / cosTh / cosTh);
      (*cov)(3,3) += angle2;
      Int_t iok = 0;
      MpdKalmanFilter::Instance()->MnvertLocal(cov->GetMatrixArray(), 5, 5, 5, iok);
      track->SetWeight(*cov);
    }
    Double_t dChi2 = MpdKalmanFilter::Instance()->FilterHit(track,hit,pointWeight,param);
    if (ifirst && dChi2 > 1000) {
      ifirst = 0;
      cout << " !!! Back: " << dChi2 << " " << nHits << " " <<  hit->GetLayer() << " " << ok << " " << step << " " << track->GetNode() << " " << track->GetNodeNew() << endl;
    }
    track->SetChi2(track->GetChi2()+dChi2);
    weight = *track->GetWeight();
    weight += pointWeight;
    track->SetWeight(weight);
    track->SetParamNew(param);
    //if (i == 0 && iDir > 0) track->SetLength(0.); // for correct track length
  }
  // Save track params at last hit
  track->SetLengAtHit(track->GetLength());
  track->SetParamAtHit(*track->GetParamNew());
  track->SetWeightAtHit(*track->GetWeight());
  track->GetHits()->UnSort(); // 20-02-2012
  track->GetHits()->Sort(); // 20-02-2012
 }

//__________________________________________________________________________
void MpdTpcKalmanFilter::GoOut()
{
  /// Backpropagate tracks in the outward direction for matching with outer detectors
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Start(Class()->GetName(),__FUNCTION__);
  Int_t nReco = fTracks->GetEntriesFast(), isec = 0;
  TMatrixDSym tmp;
  TVector3 posLoc, pos;
  for (Int_t i = 0; i < nReco; ++i) {
    MpdTpcKalmanTrack *track = (MpdTpcKalmanTrack*) fTracks->UncheckedAt(i);
    //track->GetSteps().clear();
    MpdTpcKalmanTrack tr = *track;

    Int_t id = tr.GetTrackID();

    tr.SetParam(*tr.GetParamAtHit());
    tr.SetParamNew(*tr.GetParamAtHit());
    tr.SetWeight(*tr.GetWeightAtHit());
    tr.SetLength(tr.GetLengAtHit());
    MpdKalmanHit *hit = (MpdKalmanHit*) tr.GetTrHits()->Last();
    tr.SetPos(hit->GetPos());
    if (fModular) {
      tr.SetNode(MpdKalmanFilter::Instance()->GetGeo()->Path(hit->GetDetectorID()));
      tr.SetNodeNew(tr.GetNode());
      //posLoc.SetXYZ(-hit->GetMeas(0), hit->GetDist(), hit->GetMeas(1));
      posLoc.SetXYZ(hit->GetMeas(0), hit->GetDist(), hit->GetMeas(1));
      isec = hit->GetDetectorID() % 1000000;
      //pos = fPadPlane->fromSectorReferenceFrame(posLoc, isec);
      fSecGeo->Local2Global(isec, posLoc, pos);
      gGeoManager->cd(tr.GetNodeNew());
      gGeoManager->CdUp();
      Double_t v7[3], v77[3];
      pos.GetXYZ(v7);
      gGeoManager->MasterToLocal(v7,v77);
      tr.SetPos(v77[2]);
    }
    tr.SetPosNew(tr.GetPos()); 
    BackTrace(&tr,tmp,-1);
    MpdKalmanHit hitTmp;
    hitTmp.SetType(MpdKalmanHit::kFixedR);
    if (fModular) {
      // Transform to global frame
      /*
      hit = (MpdKalmanHit*) tr.GetTrHits()->First();
      posLoc.SetXYZ(-hit->GetMeas(0), hit->GetDist(), hit->GetMeas(1));
      isec = hit->GetDetectorID() % 1000000;
      pos = fPadPlane->fromSectorReferenceFrame(posLoc, isec);
      */
      //Double_t v7[3] = {tr.GetParamNew(0), tr.GetParamNew(1), tr.GetPosNew()}, v77[3];
      Double_t v7[3] = {-tr.GetParamNew(0), tr.GetParamNew(1), tr.GetPosNew()}, v77[3];
      gGeoManager->cd(tr.GetNodeNew());
      gGeoManager->CdUp();
      gGeoManager->LocalToMaster(v7,v77);
      //MpdKalmanHit hitTmp;
      hitTmp.SetDist(TMath::Sqrt(v77[0]*v77[0]+v77[1]*v77[1]) + 0.2);
      //hitTmp.SetType(MpdKalmanHit::kFixedR);
    } else hitTmp.SetDist(tr.GetPosNew() + 0.2);
    Bool_t ok = MpdKalmanFilter::Instance()->PropagateToHit(&tr,&hitTmp);
    track->SetLengAtHit(tr.GetLength());
    track->SetParamAtHit(*tr.GetParamNew());
    track->SetWeightAtHit(*tr.GetWeight());
    track->SetPosAtHit(tr.GetPosNew());
  }
  // Below is just for debugging
  if (0) {
    TVector3 pos;
    TClonesArray *tofPoints = (TClonesArray*) FairRootManager::Instance()->GetObject("TOFPoint");
    Int_t nTof = tofPoints->GetEntriesFast();
    //cout << nTof << endl;
    for (Int_t i = 0; i < nReco; ++i) {
      MpdTpcKalmanTrack *track = (MpdTpcKalmanTrack*) fTracks->UncheckedAt(i);
      MpdTpcKalmanTrack tr = *track;
      //cout << i << " " << tr.GetTrackID() << endl;
      tr.SetParam(*tr.GetParamAtHit());
      tr.SetParamNew(*tr.GetParamAtHit());
      tr.SetWeight(*tr.GetWeightAtHit());
      tr.SetLength(tr.GetLengAtHit());
      tr.SetPos(((MpdKalmanHit*)tr.GetTrHits()->First())->GetPos());
      tr.SetPosNew(tr.GetPos());
      for (Int_t j = 0; j < nTof; ++j) {
        FairMCPoint *p = (FairMCPoint*) tofPoints->UncheckedAt(j);
        if (p->GetTrackID() != tr.GetTrackID()) continue;
        p->Position(pos);
        MpdKalmanHit hitTmp;
        hitTmp.SetDist(pos.Pt());
        hitTmp.SetType(MpdKalmanHit::kFixedR);
	MpdKalmanFilter::Instance()->PropagateToHit(&tr,&hitTmp);
        Double_t dl = tr.GetLength() - p->GetLength();
        //cout << dl << " " << tr.GetPosNew()-pos.Pt() << " " << tr.GetParamNew(1)-pos.Z() << " " << pos.Z() << endl;
	if (lunTpc) fprintf(lunTpc, "%10.3f %10.3f %10.3f %10.3f \n",
			    dl, tr.GetPosNew()-pos.Pt(), tr.GetParamNew(1)-pos.Z(), pos.Z());

	  break;
      }
    }
  }
  if (MpdCodeTimer::Active()) MpdCodeTimer::Instance()->Stop(Class()->GetName(),__FUNCTION__);

}

//__________________________________________________________________________
void MpdTpcKalmanFilter::RemoveDoubles()
{
  /// Remove double tracks (keep the ones with better quality)

  Int_t ntracks = fTrackCand->GetEntriesFast();
  cout << " Total tracks: " << ntracks << endl;
  MpdTpcKalmanTrack *tr1, *tr2;

  for (Int_t i = 0; i < ntracks; i++) {
    tr1 = (MpdTpcKalmanTrack*) fTrackCand->UncheckedAt(i);
    if (tr1 == 0x0) continue;
    for (Int_t j = i+1; j < ntracks; j++) {   			// j = 0 -> j = i+1
      //if (j == i) continue;					// add comment
      tr2 = (MpdTpcKalmanTrack*) fTrackCand->UncheckedAt(j);
      if (tr2 == 0x0) continue;

      //Int_t nHitsCommon = GetNofCommonHits(tr1, tr2);
      //if ((float)nHitsCommon / TMath::Min(tr1->GetNofHits(),tr2->GetNofHits()) < 0.5) continue;
      if (!AreTracksDoubles(tr1, tr2)) continue;

      if (tr2->GetNofHits() < tr1->GetNofHits()) fTrackCand->RemoveAt(j);
      else {
	if ((tr2->GetNofHits() > tr1->GetNofHits()) || (tr2->GetChi2() < tr1->GetChi2())){
	  fTrackCand->RemoveAt(i);
	  break;
	} else fTrackCand->RemoveAt(j);
      }
    } // for j
  } //for i

  fTrackCand->Compress();
  fNTracks = fTrackCand->GetEntriesFast();
}

//__________________________________________________________________________
//Int_t MpdTpcKalmanFilter::GetNofCommonHits(TpcLheKalmanTrack *tr1, TpcLheKalmanTrack *tr2)
Int_t MpdTpcKalmanFilter::GetNofCommonHits(MpdKalmanTrack *tr1, MpdKalmanTrack *tr2)
{
  /// Compute number of common hits in 2 tracks

  TObjArray *hits1 = tr1->GetHits(), *hits2 = tr2->GetHits(); 
  Int_t nh1 = hits1->GetEntriesFast(), nh2 = hits2->GetEntriesFast();
  Int_t nCom = 0, id = -1;

  for (Int_t i = 0; i < nh1; ++i) {
    MpdKalmanHit *hit1 = (MpdKalmanHit*) hits1->UncheckedAt(i);
    for (Int_t j = 0; j < nh2; ++j) {
      MpdKalmanHit *hit2 = (MpdKalmanHit*) hits2->UncheckedAt(j);
      if (hit1 == hit2) {
	++nCom;
	//id = hit1->GetTrackID();
	break;
      }
      if (hit2->GetLayer() < hit1->GetLayer()) break; // already closer to beam
    }
  }
  /*
  if (nCom > 0) cout << " nCom: " << nCom << " " << nh1 << " " << nh2 << " " << id 
  	     << " " << 1./tr1->GetParamNew(4) << " " << 1./tr2->GetParamNew(4) 
  	     << " " << tr1->GetChi2() << " " << tr2->GetChi2() << endl;
  */
  return nCom;
}

//__________________________________________________________________________
Bool_t MpdTpcKalmanFilter::AreTracksDoubles(MpdKalmanTrack *tr1, MpdKalmanTrack *tr2)
{
  /// Searching common hits in 2 tracks to determine doubles
  //track1 consists of less count of hits than track2

  MpdKalmanTrack *track1, *track2;
  if (tr1->GetNofHits() > tr2->GetNofHits())
      track1 = tr2, track2 = tr1;
  else
      track1 = tr1, track2 = tr2;

  Int_t limCommonPoint = (track1->GetNofHits()+1) / 2; // at least many common hits should be found

  TObjArray *hits1 = track1->GetHits(), *hits2 = track2->GetHits();
  Int_t nh1 = hits1->GetEntriesFast(), nh2 = hits2->GetEntriesFast(), nHitsCommon = 0, j = 0;

  for (Int_t i = 0; i < nh1; i++){
    MpdKalmanHit *hit1 = (MpdKalmanHit*) hits1->UncheckedAt(i);
    for ( ; j < nh2; j++){
      MpdKalmanHit *hit2 = (MpdKalmanHit*) hits2->UncheckedAt(j);

      //is hit common for two tracks compared
      if (hit1 == hit2) {
        nHitsCommon++;
        break;
      }

      if (hit2->GetLayer() < hit1->GetLayer()) break; // already closer to beam
    }

    if (i+limCommonPoint-nHitsCommon > nh1) return kFALSE; // there'll be not enough common hits already
  }

  //if count of common hits is greater limit
  if (nHitsCommon < limCommonPoint) return kFALSE;

  return kTRUE;
}

//__________________________________________________________________________
void MpdTpcKalmanFilter::ExcludeHits()
{
  /// Exclude hits, already used for tracking, from consideration during the next passes

  Int_t nReco = fTracks->GetEntriesFast();
  cout << " nReco: " << nReco << endl;
  for (Int_t i = 0; i < nReco; ++i) {
    MpdKalmanTrack *track = (MpdKalmanTrack*) fTracks->UncheckedAt(i);
    Int_t nhitsKF = track->GetNofHits();
    TObjArray *hits = track->GetHits();
    for (Int_t j = 0; j < nhitsKF; ++j) {
      MpdKalmanHit *hit = (MpdKalmanHit*) hits->UncheckedAt(j);
      hit->SetFlag(-1);
    }
  }
}

//__________________________________________________________________________
Double_t MpdTpcKalmanFilter::CorrectForLoss(Double_t pt, Double_t the, Int_t id)
{
  /// Apply momentum correction due to ionization loss in the beam pipe (1 mm of Be)
  /// and TPC inner shell (tpc_v6) - for pions and electrons
  /// Attention!!! Neither pion nor muon with p = 0.050 GeV/c survive - they decay 
  /// before entering TPC 
  const Double_t piMass = 0.13957, eMass = 0.00051;
  Double_t mass;

  Int_t elec = GetParticleId(id);
  if (elec) mass = eMass;
  else mass = piMass;
  return CorrectForLoss(TMath::Abs(pt), the, mass, 1); // ionization loss correction
}

//__________________________________________________________________________
/*
Double_t MpdTpcKalmanFilter::CorrectForLoss(Double_t pt, Double_t the, Int_t id)
{
  /// Apply momentum correction due to ionization loss in the beam pipe (1 mm of Be)
  /// and TPC inner shell (tpc_v6) - for pions and electrons
  /// Attention!!! Neither pion nor muon with p = 0.050 GeV/c survive - they decay 
  /// before entering TPC 

  const Int_t nDim = 13;
  const Double_t mom[nDim]=  {0.075, 0.100, 0.200, 0.300, 0.400, 0.500, 0.700, 0.900,
                              1.200, 1.500, 2.000, 2.500, 3.500};
  const Double_t tkin[nDim]= {0.019, 0.032, 0.104, 0.191, 0.284, 0.380, 0.574, 0.771,
			      1.069, 1.367, 1.865, 2.364, 3.363};
  const Double_t dedxp[nDim]={8.700, 4.900, 2.350, 1.950, 1.850, 1.850, 1.750, 1.750,
			      1.750, 1.750, 1.850, 1.850, 1.850}; // peak
  const Double_t dedxm[nDim]={8.773, 5.002, 2.597, 2.259, 2.175, 2.143, 2.129, 2.131,
			      2.152, 2.154, 2.182, 2.193, 2.239}; // mean of fit Gauss*Landau
  const Double_t piMass = 0.13957, eMass = 0.00051;
  Double_t dt, mass;

  Int_t elec = GetParticleId(id);
  if (elec) mass = eMass;
  else mass = piMass;
  Double_t mass2 = mass * mass;

  Double_t p = pt / TMath::Sin(the);
  Double_t t = TMath::Sqrt (p*p + mass2) - mass;

  if (elec) dt = 2.9; // 2.9 MeV loss for electrons
  else {
    if (t < tkin[0]) dt = dedxm[0];
    //if (t < tkin[0]) {
      //dt = dedxm[0] + (dedxm[1]-dedxm[0])/(tkin[1]-tkin[0]) * (t-tkin[0]);
      //dt = TMath::Min (dt,10.);
    //}
    else if (t > tkin[nDim-1]) dt = dedxm[nDim-1];
    else {
      // Binary search
      Int_t i1 = 0, i2 = nDim-1, i = i2;
      do {
	i = i1 + (i2-i1) / 2;
	if (t > tkin[i]) i1 = i;
	else i2 = i;
	//cout << i << " " << i1 << " " << i2 << " " << t << " " << tkin[i1] << " " << tkin[i2] << endl;
      } while (i2 - i1 > 1);
      // Linear interpolation
      dt = dedxm[i1] + (dedxm[i2]-dedxm[i1])/(tkin[i2]-tkin[i1]) * (t-tkin[i1]);
    }
  }
  dt /= TMath::Sin(the);

  t += dt * 1.e-3;
  Double_t e = mass + t;
  p = TMath::Sqrt (e*e - mass2);
  pt = p * TMath::Sin(the);
  return pt;
}
*/
/*
//__________________________________________________________________________
Double_t MpdTpcKalmanFilter::CorrectForLoss(Double_t pt, Double_t the, Int_t id)
{
  /// Apply momentum correction due to ionization loss in the beam pipe (0.5 mm of Al)
  /// and TPC inner shell 

  //return pt; ///
  const Int_t nDim = 13;
  const Double_t mom[nDim]={0.075,0.100,0.200,0.300,0.400,0.500,0.700,0.900,
                            1.200,1.500,2.000,2.500,3.500};
  const Double_t tkin[nDim]={0.019,0.032,0.104,0.191,0.284,0.380,0.574,0.771,
			    1.069,1.367,1.865,2.364,3.363};
  const Double_t dedxp[nDim]={3.125,1.950,0.975,0.800,0.800,0.750,0.750,0.750,
			      0.750,0.750,0.750,0.775,0.775}; // peak
  const Double_t dedxm[nDim]={3.195,2.085,1.100,0.952,0.918,0.898,0.898,0.890,
			      0.887,0.886,0.901,0.904,0.913}; // mean
  const Double_t piMass = 0.13957, eMass = 0.00051;
  Double_t dt, mass;
  
  Int_t elec = GetParticleId(id);
  if (elec) mass = eMass;
  else mass = piMass;
  Double_t mass2 = mass * mass;

  Double_t p = pt / TMath::Sin(the);
  Double_t t = TMath::Sqrt (p*p + mass2) - mass;

  if (elec) dt = 1.45; // 1.45 MeV loss for electrons

  else {
    //if (t < tkin[0]) dt = dedxm[0];
    if (t < tkin[0]) {
      dt = dedxm[0] + (dedxm[1]-dedxm[0])/(tkin[1]-tkin[0]) * (t-tkin[0]);
      dt = TMath::Min (dt,10.);
    }
    else if (t > tkin[nDim-1]) dt = dedxm[nDim-1];
    else {
      // Binary search
      Int_t i1 = 0, i2 = nDim-1, i = i2;
      do {
	i = i1 + (i2-i1) / 2;
	if (t > tkin[i]) i1 = i;
	else i2 = i;
	//cout << i << " " << i1 << " " << i2 << " " << t << " " << tkin[i1] << " " << tkin[i2] << endl;
      } while (i2 - i1 > 1);
      // Linear interpolation
      dt = dedxm[i1] + (dedxm[i2]-dedxm[i1])/(tkin[i2]-tkin[i1]) * (t-tkin[i1]);
    }
  }
  dt /= TMath::Sin(the);

  t += dt * 1.e-3;
  Double_t e = mass + t;
  p = TMath::Sqrt (e*e - mass2);
  pt = p * TMath::Sin(the);
  return pt;
}
*/
//__________________________________________________________________________
Int_t MpdTpcKalmanFilter::GetParticleId(Int_t id)
{
  /// Particle ID: 
  /// !!! based on MC information at the moment !!!

  FairMCTrack* mcTr = (FairMCTrack*) fMCtracks->UncheckedAt(id);
  Int_t pdg = mcTr->GetPdgCode();
  if (TMath::Abs(pdg) == 11) return 1;
  return 0;
}

//__________________________________________________________________________
//Bool_t MpdTpcKalmanFilter::SameOrigin(TpcLheHit *hit, Int_t idKF, Int_t *mcTracks)
Bool_t MpdTpcKalmanFilter::SameOrigin(TpcPoint *hit, Int_t idKF, Int_t *mcTracks)
{
  /// Check if two hits belong to tracks from the same mother

  //cout << " Wrongs: " << hit->GetTrackID() << " " << idKF << " " << mcTracks[hit->GetTrackID()] << " " << mcTracks[idKF] << endl;
  if (mcTracks[hit->GetTrackID()] <= 0 && mcTracks[idKF] <= 0) return kFALSE; // both hits are from primaries
  else if (mcTracks[hit->GetTrackID()] > 0) {
    // Check mother
    FairMCTrack *track = (FairMCTrack*) fMCtracks->UncheckedAt(hit->GetTrackID());
    while (track->GetMotherId() > 0) {
      //cout << " Wrongs1: " << track->GetMotherId() << endl;
      if (track->GetMotherId() == idKF) return kTRUE;
      track = (FairMCTrack*) fMCtracks->UncheckedAt(track->GetMotherId());
    }
  } else {
    FairMCTrack *track = (FairMCTrack*) fMCtracks->UncheckedAt(idKF);
    while (track->GetMotherId() > 0) {
      //cout << " Wrongs2: " << track->GetMotherId() << endl;
      if (track->GetMotherId() == hit->GetTrackID()) return kTRUE;
      track = (FairMCTrack*) fMCtracks->UncheckedAt(track->GetMotherId());
    }
  }
  return kFALSE;
}

//__________________________________________________________________________
Bool_t MpdTpcKalmanFilter::Refit(MpdKalmanTrack *track, Double_t mass, Int_t charge)
{
  /// Refit track in TPC using track hits (toward beam line) for some
  /// particle mass and charge hypothesis 

  //#pragma omp critical
  {
    track->GetHits()->Sort();
  }
  track->SetChi2(0.);

  //if (GetNofHits() == 1) return; 
  track->SetWeight(*track->GetWeightAtHit());
  TMatrixDSym *w = track->GetWeight();
  Int_t nHits = track->GetNofHits(), nHits2 = nHits * nHits;;
  for (Int_t i = 0; i < 5; ++i) {
    for (Int_t j = i; j < 5; ++j) {
      //if (j == i) (*w)(i,j) /= 1000.;
      if (j == i) (*w)(i,j) /= nHits2;
      //if (j == i) (*w)(i,j) /= 1;
      else (*w)(i,j) = (*w)(j,i) = 0.;
    }
  }

  TMatrixD param(5,1);
  TMatrixDSym weight(5), pointWeight(5);
  Int_t iDir = 1;
  Int_t ibeg = iDir > 0 ? 0 : nHits-1;
  Int_t iend = iDir > 0 ? nHits-1 : 0;
  iend += iDir;

  MpdKalmanHit *hit = 0x0;
  track->SetPosNew(track->GetPosAtHit()+0.2);
  track->SetParamNew(*track->GetParamAtHit());
  track->SetNodeNew("");
  Double_t leng = 0.0;
  track->SetLength(leng);
  TString mass2 = "";
  mass2 += mass * mass;

  for (Int_t i = ibeg; i != iend; i+=iDir) {
    hit = (MpdKalmanHit*) track->GetHits()->UncheckedAt(i); 
    //if (!MpdKalmanFilter::Instance()->PropagateToHit(track, hit, kTRUE, kTRUE)) return kFALSE;
    if (!MpdKalmanFilter::Instance()->PropagateToHit(track, hit, kTRUE, kTRUE)) {
      cout << " III " << i << " " << hit->GetDist() << endl;
      //return kFALSE;
    }
    Double_t step = track->GetLength() - leng;
    Double_t th = track->GetParamNew(3);
    Double_t cosTh = TMath::Cos(th);
    if (step > 1.e-4) {
      Double_t x0 = 13363.6; // rad. length - TPCMixture
      TMatrixDSym *cov = track->Weight2Cov();
      Double_t angle2 = MpdKalmanFilter::Instance()->Scattering(track, x0, step, mass2, charge);
      //cout << " Scat: " << hit->GetLayer() << " " << step << " " << TMath::Sqrt(angle2) << endl;
      (*cov)(2,2) += (angle2 / cosTh / cosTh);
      (*cov)(3,3) += angle2;
      Int_t iok = 0;
      MpdKalmanFilter::Instance()->MnvertLocal(cov->GetMatrixArray(), 5, 5, 5, iok);
      track->SetWeight(*cov);
    }
    Double_t dChi2 = MpdKalmanFilter::Instance()->FilterHit(track, hit, pointWeight, param);
    track->SetChi2(track->GetChi2()+dChi2);
    weight = *track->GetWeight();
    weight += pointWeight;
    track->SetWeight(weight);
    track->SetParamNew(param);
    leng = track->GetLength();
    //cout << i << " " << dChi2 << " " << 1./track->GetParamNew(4) << endl;
  }

  // Go to the beam line
  const Int_t nR = 7;
  const Double_t rad[nR] = {27.00, 27.01, 27.31, 27.32, 33.82, 33.83, 34.13};
  const Double_t dx[nR] = {7.8e-4, 1.194e-2, 7.8e-4, 3.3e-4, 7.8e-4, 1.194e-2, 9.6e-4};
  Bool_t ok = 0;

  MpdKalmanHit hitK;
  hitK.SetType(MpdKalmanHit::kFixedR);
  for (Int_t j = nR-1; j >= 0; --j) {
    hitK.SetPos(rad[j]);

    ok = MpdKalmanFilter::Instance()->PropagateToHit(track,&hitK,kTRUE);

    // Add multiple scattering
    TMatrixDSym *cov = track->Weight2Cov();
    Double_t th = track->GetParamNew(3);
    Double_t cosTh = TMath::Cos(th);
    Double_t angle2 = MpdKalmanFilter::Instance()->Scattering(track, dx[j], mass2, charge);
    (*cov)(2,2) += (angle2 / cosTh / cosTh);
    (*cov)(3,3) += angle2;
    //cov->Print();
    Int_t iok = 0;
    MpdKalmanFilter::Instance()->MnvertLocal(cov->GetMatrixArray(), 5, 5, 5, iok);
    track->SetWeight(*cov);
  }

  //if (!ok) continue;
  TMatrixDSym *cov = track->Weight2Cov();

  // Update track
  track->SetParam(*track->GetParamNew());
  track->SetPos(track->GetPosNew());
  // Correct for energy loss
  //*
  Double_t theta = TMath::PiOver2() - track->GetParam(3);
  Double_t pt = 1. / track->GetParam(4);
  Double_t ptCor = CorrectForLoss(TMath::Abs(pt), theta, mass, charge); // ionization loss correction
  track->SetParam(4,TMath::Sign(1./ptCor,pt));
  track->SetParamNew(*track->GetParam());
  //*/
  
  //*
  hitK.SetPos(0.);
  hitK.SetMeas(0,track->GetParam(2));
  //hit.SetRphi(track->GetParam(2)); // track Phi - check if it is necessary !!!!!!!
  Double_t pos = track->GetPosNew();
  MpdKalmanFilter::Instance()->PropagateToHit(track, &hitK, kTRUE);
  track->SetPos(pos); // save position after passing inner shell
  track->SetParam(*track->GetParamNew()); // !!! track params at PCA
  //track->SetLengAtHit(track->GetLength()-track->GetLengAtHit()); // track length from PCA to the nearest hit

  if (0) {
    TVector3 pos = MpdKalmanFilter::Instance()->GetGeo()->GlobalPos(hit);
    cout << " Hit: " << pos.Pt()*pos.Phi() << " " << pos.Pt() << " " << pos.Z() << endl;
    cout << " Track: " << nHits << " " << track->GetParamNew(0) << " " << track->GetPosNew() << " " << track->GetParamNew(1) << " " << track->GetChi2() << endl;
  }
  return kTRUE;
}

//__________________________________________________________________________
Double_t MpdTpcKalmanFilter::CorrectForLoss(Double_t pt, Double_t the, Double_t mass, Int_t charge)
{
  // Ionization loss correction

  // Pion
  const Int_t nPi = 13; // pi-
  //const Double_t momPi[nPi] =  {0.075, 0.100, 0.200, 0.300, 0.400, 0.500, 0.700, 0.900,
  //  			          1.200, 1.500, 2.000, 2.500, 3.500};
  const Double_t momPi[nPi] =  {0.055, 0.091, 0.197, 0.297, 0.398, 0.498, 0.698, 0.898,
				1.198, 1.498, 1.998, 2.498, 3.498};
  const Double_t dedxPi[nPi] = {8.773, 5.002, 2.597, 2.259, 2.175, 2.143, 2.129, 2.131,
				2.152, 2.154, 2.182, 2.193, 2.239}; // mean of fit Gauss*Landau
  // Kaon
  const Int_t nK = 12; // K-
  //const Double_t momK[nK] =  {0.200, 0.250, 0.300, 0.400, 0.500, 0.700, 0.900, 1.200,
  //  			        1.500, 2.000, 2.500, 3.500};
  const Double_t momK[nK] =  {0.167, 0.233, 0.289, 0.393, 0.495, 0.697, 0.897, 1.198,
                              1.498, 1.998, 2.498, 3.498};
  const Double_t dedxK[nK] = {11.40, 7.584, 5.858, 4.128, 3.307, 2.635, 2.394, 2.253,
			      2.314, 2.256, 2.253, 2.249}; // mean of fit Gauss*Landau
  // Proton
  const Int_t nP = 11;
  //const Double_t momP[nP] =  {0.300, 0.350, 0.400, 0.500, 0.700, 0.900, 1.200, 1.500, 2.000,
  //		                2.500, 3.500};
  const Double_t momP[nP] =  {0.239, 0.313, 0.375, 0.485, 0.693, 0.895, 1.196, 1.498, 1.997,
			      2.498, 3.498};
  const Double_t dedxP[nP] = {16.87, 12.31, 9.751, 6.948, 4.460, 3.483, 2.808, 2.508, 2.295,
			      2.221, 2.164}; // mean of fit Gauss*Landau

  // Deuteron
  const Int_t nD = 10;
  //const Double_t momD[nD] =  {0.450, 0.500, 0.600, 0.700, 0.900, 1.200, 1.500, 2.000,
  //		                2.500, 3.500};
  const Double_t momD[nD] =  {0.303, 0.406, 0.547, 0.666, 0.882, 1.190, 1.493, 1.996,
			      2.497, 3.497};
  const Double_t dedxD[nD] = {28.85, 22.02, 15.40, 11.73, 7.962, 5.369, 4.159, 3.178,
			      2.705, 2.298}; // mean of fit Gauss*Landau

  // Triton
  const Int_t nT = 10;
  //const Double_t momT[nT] =  {0.600, 0.650, 0.700, 0.800, 1.000, 1.200, 1.500, 2.000,
  //                            2.500, 3.500};
  const Double_t momT[nT] =  {0.400, 0.512, 0.594, 0.730, 0.962, 1.176, 1.485, 1.992,
                              2.494, 3.496};
  const Double_t dedxT[nT] = {35.02, 27.97, 23.83, 18.43, 12.58, 9.341, 6.835, 4.730,
                              3.662, 2.814}; // mean of fit Gauss*Landau

  // He3
  const Int_t nHe3 = 11;
  //const Double_t momHe3[nHe3] =  {0.850, 0.900, 1.000, 1.100, 1.200, 1.400, 1.600, 2.000, 2.500,
  //			            3.000, 3.500};
  const Double_t momHe3[nHe3] =  {0.455, 0.623, 0.819, 0.967, 1.096, 1.330, 1.548, 1.967, 2.478,
				  2.983, 3.485};
  const Double_t dedxHe3[nHe3] = {89.13, 72.42, 55.85, 45.98, 39.47, 30.53, 25.15, 19.07, 14.73,
				  12.76, 11.42}; // mean of fit Gauss*Landau

  charge = TMath::Abs(charge);
  Double_t p = pt / TMath::Sin(the) * charge, mass2 = mass * mass;
  Double_t t = TMath::Sqrt (p*p + mass2) - mass, dt = 0.0;

  if (mass < 0.1) dt = 2.9; // 2.9 MeV loss for electrons
  else if (mass < 0.2) dt = MpdKalmanFilter::Instance()->Interp(nPi, momPi, dedxPi, p); // Pion
  else if (mass < 0.6) dt = MpdKalmanFilter::Instance()->Interp(nK, momK, dedxK, p); // Kaon
  else if (mass < 1.0) dt = MpdKalmanFilter::Instance()->Interp(nP, momP, dedxP, p); // Proton
  else if (mass < 2.0) dt = MpdKalmanFilter::Instance()->Interp(nD, momD, dedxD, p); // Deuteron
  else if (mass < 2.9 && charge == 1) dt = MpdKalmanFilter::Instance()->Interp(nT, momT, dedxT, p); // Triton
  else if (mass < 2.9) dt = MpdKalmanFilter::Instance()->Interp(nHe3, momHe3, dedxHe3, p); // He3

  dt /= TMath::Sin(the);
  t += dt * 1.e-3;
  Double_t e = mass + t;
  p = TMath::Sqrt (e*e - mass2);
  pt = p * TMath::Sin(the);
  if (charge) pt /= charge;
  return pt;
}

ClassImp(MpdTpcKalmanFilter)
