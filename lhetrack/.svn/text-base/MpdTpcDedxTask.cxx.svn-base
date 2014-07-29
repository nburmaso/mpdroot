// -------------------------------------------------------------------------
// -----                    MpdTpcDedxTask source file                 -----
// -----                 Created 9/02/10  by A. Zinchenko              -----
// -------------------------------------------------------------------------

/**  MpdTpcDedxTask.cxx
 *@author A.Zinchenko <Alexander.Zinchenko@jinr.ru>
 **
 ** dE/dx determination in MPD TPC
 **/

#include "MpdTpcDedxTask.h"
#include "MpdTpcKalmanTrack.h"

#include "FairRootManager.h"

#include <TMath.h>

#include <iostream>
#include <vector>

using std::cout;
using std::endl;
using std::vector;

//__________________________________________________________________________
MpdTpcDedxTask::MpdTpcDedxTask(const char *name, Int_t iVerbose )
  :FairTask(name, iVerbose)
{
    fHistoDir = NULL;
    fTracks = NULL, fMCTracks = NULL;
}


//__________________________________________________________________________
MpdTpcDedxTask::~MpdTpcDedxTask()
{
  //delete fKHits;
  //delete fTracks;
  //delete fTrackCand;
}

//__________________________________________________________________________
InitStatus MpdTpcDedxTask::Init()
{
  fTracks = 0x0;
  fTracks =(TClonesArray *) FairRootManager::Instance()->GetObject("ItsTrack");
  if (fTracks == 0x0) fTracks =(TClonesArray *) FairRootManager::Instance()->GetObject("TpcKalmanTrack");

  fMCTracks =(TClonesArray *) FairRootManager::Instance()->GetObject("MCTrack");

  return kSUCCESS;
}

//__________________________________________________________________________
void MpdTpcDedxTask::Reset() 
{
  ///
}

//__________________________________________________________________________
void MpdTpcDedxTask::SetParContainers()
{
}

//__________________________________________________________________________
void MpdTpcDedxTask::Finish()
{
  //Write();
}

//__________________________________________________________________________
void MpdTpcDedxTask::Exec(Option_t * option)
{

  static Int_t eventCounter = 0;    
  const Double_t trunc = 0.7; // truncation parameter
  cout << " MpdTpcDedxTask event " << ++eventCounter << endl;

  Reset();

  Int_t nTracks = fTracks->GetEntriesFast();
  for (Int_t itr = 0; itr < nTracks; ++itr) {
    MpdTpcKalmanTrack *track = (MpdTpcKalmanTrack*) fTracks->UncheckedAt(itr);
    TClonesArray *hits = track->GetTrHits();
    Int_t nHits = hits->GetEntriesFast(), nOK = 0;
    Double_t *dedx = new Double_t [nHits];
    for (Int_t j = 0; j < nHits; ++j) {
      MpdKalmanHit *hit = (MpdKalmanHit*) hits->UncheckedAt(j);
      //cout << j << " " << hit->GetUniqueID() << " " << hit->GetDedx() << endl;
      if (hit->GetUniqueID() == 1) continue; // ITS hit
      if (hit->GetSignal() <= 0.) continue; // strange
      dedx[nOK++] = hit->GetSignal();
    }
    if (nOK == 0) continue;
    Int_t *indx = new Int_t [nOK];
    TMath::Sort(nOK,dedx,indx,kFALSE);
    Double_t sum = 0.;
    Int_t nTrunc = TMath::Nint (nOK * trunc);
    if (nTrunc == 0) nTrunc = 1;
    for (Int_t j = 0; j < nTrunc; ++j) sum += dedx[indx[j]];
    track->SetPartID (sum / nTrunc);
    delete [] dedx;
    delete [] indx;
  }
}

//__________________________________________________________________________
void MpdTpcDedxTask::Write()
{
  /// Write

  TFile histoFile("Vertex.root","RECREATE");
  Writedir2current(fHistoDir);
  histoFile.Close();
}

//__________________________________________________________________________
void MpdTpcDedxTask::Writedir2current( TObject *obj )
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

ClassImp(MpdTpcDedxTask);
