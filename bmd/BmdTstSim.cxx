/**********************************************************************
 *
 *         Class BmdTstSim
 *
 *  Author:   RO
 *  e-mail:   rogachevsky@jinr.ru
 *  Version:  19-May-2019
 *
 *********************************************************************/

#include "BmdTstSim.h"
#include "BmdPoint.h"
#include "TpcPoint.h"

#include "FairBaseParSet.h"
#include "MpdMCTrack.h"
#include "FairRootManager.h"
#include "FairRunAna.h"
#include "FairRuntimeDb.h"
#include "FairLogger.h"

using namespace std;

//_____________________________________________________________________
BmdTstSim::BmdTstSim()
// : FairTask ("BmdTstSim")
{
  fVerbose = 0;
  nevents = 0;
  fTree = 0;
  fH3 = 0;
}

//_____________________________________________________________________
BmdTstSim::BmdTstSim(const char *name, const char *title, Int_t verbose)
    : FairTask(name) {
  if (verbose)
    LOG(info) << "-- BmdTstSim::BmdTstSim begin" << endl;

  fVerbose = verbose;
  nevents = 0;

  fTree = 0;
  fTreeSummary = 0;
  fH3 = 0;
  //  CreateMyTree();

  if (fVerbose)
    LOG(INFO) << "BmdTstSim::BmdTstSim end" << endl;
}

//_____________________________________________________________________
BmdTstSim::BmdTstSim(const char *name, const char *title, Int_t verbose,
                     Int_t flag) {
  if (verbose)
    LOG(info) << "-- BmdTstSim::BmdTstSim begin" << endl;

  fVerbose = verbose;
  nevents = 0;

  fTree = 0;
  fTreeSummary = 0;
  //  CreateMyTree();

  if (fVerbose)
    LOG(INFO) << " BmdTstSim::BmdTstSim Init starting" << endl;

  Init();

  if (fVerbose)
    LOG(INFO) << "-- BmdTstSim::BmdTstSim end" << endl;
}

//_____________________________________________________________________
BmdTstSim::~BmdTstSim() {
  if (fTree)
    fTree->Write();
  if (fTreeSummary)
    fTreeSummary->Write();

  if (fH3)
    fH3->Write();
}

//_____________________________________________________________________
InitStatus BmdTstSim::Init() {
  //---

  if (fVerbose)
    LOG(info) << "-- BmdTstSim::Init begin" << endl;

  FairRootManager *ioman = FairRootManager::Instance();
  if (!ioman) {
    cout << "-E- BmdTstSim::Init: "
         << "RootManager not instantised!" << endl;
    return kERROR;
  }

  fMCTrackArray = (TClonesArray *)ioman->GetObject("MCTrack");
  if (!fMCTrackArray) {
    cout << "-E- BmdTstSim::Init: No MCTrack array!" << endl;
    return kERROR;
  }

  fMCBmdPointArray = (TClonesArray *)ioman->GetObject("BmdPoint");
  if (!fMCBmdPointArray) {
    cout << "-E- BmdTstSim::Init: No BmdPoint array!" << endl;
    return kERROR;
  }

  fMCTpcPointArray = (TClonesArray *)ioman->GetObject("TpcPoint");
  if (!fMCTpcPointArray) {
    cout << "-E- BmdTstSim::Init: No TpcPoint array!" << endl;
    //    return kERROR;
  }

  fMCEventHeader = (FairMCEventHeader *)ioman->GetObject("MCEventHeader.");
  if (!fMCEventHeader) {
    cout << "-E- BmdTstSim::Init: No MCEventHeader array!" << endl;
    //    return kERROR;
  }

  if (fVerbose)
    LOG(info) << "-- BmdTstSim::Init end" << endl;

  CreateMyTree();
  fTree->SetDirectory(ioman->GetOutFile());
  fTreeSummary->SetDirectory(ioman->GetOutFile());
  if (fH3)
    fH3->SetDirectory(ioman->GetOutFile());
  ioman->Register("bmdpoints", "BMD", fTree, kTRUE);
  ioman->Register("bmdsummary", "BMD", fTreeSummary, kTRUE);
  if (fH3)
    ioman->Register("H3", "BMD", fH3, kTRUE);

  if (fVerbose) {
    LOG(info) << "-- BmdTstSim::Init bmdpoints: "
         << (fTree->GetCurrentFile()->GetName()) << endl;
    LOG(info) << "-- BmdTstSim::Init bmdsummary: "
         << (fTreeSummary->GetCurrentFile()->GetName()) << endl;
  }

  return kSUCCESS;
}


//_____________________________________________________________________
InitStatus BmdTstSim::ReInit() {
  //   //  SetParContainers();
  //    FairRunAna* ana = FairRunAna::Instance();
  //    FairRuntimeDb* rtdb=ana->GetRuntimeDb();
  //    BmdGeoPar *fPar=(BmdGeoPar*)(rtdb->getContainer("BmdGeoPar"));
  return kSUCCESS;
}

//_____________________________________________________________________
void BmdTstSim::Exec(Option_t *option) {

  ///////
  if (fVerbose > 3)
    LOG(info) << "-- BmdTstSim::Exec begin" << endl;

  Double_t elossall = 0, elossbmd1 = 0, elossbmd2 = 0, e = 0, ebmd1[4],
           ebmd2[4];
  Int_t nPoints, nMCTracks;

  if (fTree) {

    TVector3 position, momentum, vertex;
    BmdPoint *pPoint = NULL;
    MpdMCTrack *pTrack = NULL;
    Int_t pdg = 0;

    Float_t fArgs[30];

    for (int iii = 0; iii < 4; iii++) {
      ebmd1[iii] = 0;
      ebmd2[iii] = 0;
    }

    nPoints = fMCBmdPointArray->GetEntriesFast();
    nMCTracks = fMCTrackArray->GetEntriesFast();

    if (fVerbose) {
      cout << " BmdPoint: " << nPoints << " entries" << endl;
      cout << " MCTrack:     " << nMCTracks << " entries" << endl;
    }

    for (Int_t i = 0; i < nPoints; i++) {

      //       if (fVerbose>2)
      // 	cout << " .";

      pPoint = (BmdPoint *)fMCBmdPointArray->UncheckedAt(i);
      if (!pPoint) {
        if (fVerbose > 2) {
          cout << " -";
        }
        continue;
      }

      if (pPoint->GetTrackID() == -2) {
        if (fVerbose > 2) {
          //	  cout << i << ":" <<(pPoint->GetTrackID()) << "!";
          cout << "BmdTstSim::Exec:  MCTrack not saved: particle number:" << i
               << " must be too slow !" << endl;
        }
        continue;
      }

      pTrack = (MpdMCTrack *)fMCTrackArray->UncheckedAt(pPoint->GetTrackID());
      if (!pTrack) {
        if (fVerbose > 2) {
          cout << i << ":" << (pPoint->GetTrackID()) << "!";
        }
        continue;
      }

      try {
        // 	  if ((fVerbose>2)&&(nMCTracks==174937)) {
        // 	    cout << i << ":" <<(pPoint->GetTrackID()) << " track addr:"
        // << pTrack<< "! ";
        // 	  }
        pdg = pTrack->GetPdgCode();
        fArgs[0] = pdg;
      } catch (...) {
        cout << "BmdTstSim::Exec:  Exception 1: particle number:" << i
             << " track number:" << (pPoint->GetTrackID())
             << " track addr:" << pTrack << endl;
      }

      fArgs[1] = pTrack->GetMotherId();

      pTrack->GetMomentum(momentum);
      fArgs[2] = momentum.X();
      fArgs[3] = momentum.Y();
      fArgs[4] = momentum.Z();

      pTrack->GetStartVertex(vertex);
      fArgs[5] = vertex.X();
      fArgs[6] = vertex.Y();
      fArgs[7] = vertex.Z();

      Int_t tZ = -1000, tA = -1000;

      TParticlePDG *particle =
          TDatabasePDG::Instance()->GetParticle((Int_t)floor(fArgs[0]));
      if (particle) {
        fArgs[8] = particle->Mass();
        tZ = TMath::Nint(particle->Charge() / 3.);
        if (pdg > 1e9) {
          tA = ((pdg % 10000) / 10);
          if (!tZ)
            tZ = TMath::Nint((pdg - 1e9) / 10000);
        }
      } else {
        fArgs[8] = 0;
        if (pdg > 1e9) {
          tZ = TMath::Nint((pdg - 1e9) / 10000);
          tA = ((pdg % 10000) / 10);
        }
      }

      fArgs[9] = TMath::Sqrt(fArgs[2] * fArgs[2] + fArgs[3] * fArgs[3] +
                             fArgs[4] * fArgs[4] + fArgs[8] * fArgs[8]);

      fArgs[10] = TMath::Sqrt(fArgs[2] * fArgs[2] + fArgs[3] * fArgs[3]);
      fArgs[11] = TMath::Sqrt(fArgs[2] * fArgs[2] + fArgs[3] * fArgs[3] +
                              fArgs[4] * fArgs[4]);
      fArgs[12] =
          0.5 * TMath::Log((fArgs[9] + fArgs[4]) / (fArgs[9] - fArgs[4]));

      fArgs[13] = 1; // not implemented on the level of FairMCTrack now
      // fArgs[13]=pTrack->GetStartTime();

      fArgs[14] = pPoint->GetTrackID();
      //      fArgs[15]= pPoint->GetEventID();
      fArgs[15] = nevents;
      fArgs[16] = pPoint->GetX();
      fArgs[17] = pPoint->GetY();
      fArgs[18] = pPoint->GetZ();
      fArgs[19] = pPoint->GetPx();
      fArgs[20] = pPoint->GetPy();
      fArgs[21] = pPoint->GetPz();
      fArgs[22] = pPoint->GetTime();
      fArgs[23] = pPoint->GetLength();
      fArgs[24] = pPoint->GetEnergyLoss();
      //       fArgs[25]= pPoint->GetModule();
      //       fArgs[26]= pPoint->GetRow();
      //       fArgs[27]= pPoint->GetCopy();
      fArgs[25] = pPoint->GetDetectorID();
      // fArgs[26]= pPoint->GetCopy();
      // fArgs[27]= pPoint->GetCopyMother();

      if (fArgs[1] != -1) {
        pTrack =
            (MpdMCTrack *)fMCTrackArray->UncheckedAt(pTrack->GetMotherId());
        if (pTrack) {
          fArgs[28] = pTrack->GetPdgCode();
          TVector3 vertexTemp;
          pTrack->GetStartVertex(vertexTemp);
          fArgs[29] = vertexTemp.Z();
        } else {
          fArgs[28] = -2000000;
          fArgs[29] = -2000000;
        }
      } else {
        fArgs[28] = -1000000;
        fArgs[29] = -1000000;
      }

      // gertsen COMMENT BECAUSE OF OUT OF RANGE!!!
      // fArgs[30]=tZ;
      // fArgs[31]=tA;

      elossall += pPoint->GetEnergyLoss();

      Double_t ekin = fArgs[9] - fArgs[8];

      //      if ((fArgs[25]<=30)&&(fArgs[1]==-1))

      if (fArgs[1] == -1) { // only primary particles:

        //	if (fArgs[27]==1)                        // bmd1
        if (fArgs[18] > 0) // bmd1
          elossbmd1 += pPoint->GetEnergyLoss();
        else
          elossbmd2 += pPoint->GetEnergyLoss();

        if (fArgs[26] == 1) { // bmd entrances

          e += ekin; // kinetic energy of all primary particles at both bmd
                     // entrances

          //	  if (fArgs[27]==1) {               // bmd1
          if (fArgs[18] > 0) { // bmd1
            ebmd1[0] += ekin;  // all
            switch (pdg) {
            case 2212:          // protons
              ebmd1[1] += ekin; // nucleons
              ebmd1[3] += ekin; // protons
              break;
            case 2112:          // neutrons
              ebmd1[1] += ekin; // nucleons
              break;
            case 211: // pions
            case -211:
              ebmd1[2] += fArgs[9];
              break;
            default:
              break;
            }
          } else { // bmd2
            ebmd2[0] += ekin;
            switch (pdg) {
            case 2212:          // protons
              ebmd2[1] += ekin; // nucleons
              ebmd2[3] += ekin; // protons
              break;
            case 2112:          // neutrons
              ebmd2[1] += ekin; // nucleons
              break;
            case 211: // pions
            case -211:
              ebmd2[2] += fArgs[9];
              break;
            default:
              break;
            }
          }

        } // end of bmd entrance

        fTree->Fill(fArgs);

      } // end of primary particles

      //     fTree->Fill(fArgs);
    }
    //  cout << " .finished!" << endl;

    nevents++;
  }
  Double_t b = 0;

  if (fMCEventHeader) {
    b = ((FairMCEventHeader *)fMCEventHeader)->GetB();
  }

  if (fTreeSummary) {

    Int_t nTpcTracks = 0;
    Int_t trackId;

    if (fMCTpcPointArray) {

      std::map<Int_t, Bool_t> tpc_tracks;
      tpc_tracks.clear();

      Int_t i_output = 5;
      for (Int_t i = 0; i < fMCTpcPointArray->GetEntriesFast(); i++) {

        trackId = ((TpcPoint *)fMCTpcPointArray->UncheckedAt(i))->GetTrackID();

        if ((trackId >= 0) && (trackId < nMCTracks)) {

          if (((MpdMCTrack *)fMCTrackArray->UncheckedAt(trackId))
                  ->GetMotherId() == -1) {

            if (tpc_tracks.find(trackId) == tpc_tracks.end())
              tpc_tracks[trackId] = kTRUE;
          }
        } else {
          if (i_output > 0) {
            cout << "-E- Wrong TpcPoint GetTrackID = " << trackId
                 << "  for i=" << i << "   b=" << b << endl;
            i_output--;
          }
        } // else
      }   // for (Int_t i=0;i<fMCTpcPointArray->GetEntriesFast();i++)
      nTpcTracks = tpc_tracks.size();
    }

    fTreeSummary->Fill(elossall, elossbmd1, elossbmd2, nTpcTracks, b, e,
                       ebmd1[0], ebmd1[1], ebmd1[2], ebmd1[3], ebmd2[0],
                       ebmd2[1], ebmd2[2], ebmd2[3]);
    //    cout << " . " << (fTreeSummary) << endl;

    fH3->Fill(elossall, nTpcTracks, b);
  } else
    cout << " !";

  if (fVerbose > 3)
    LOG(info) << "-- BmdTstSim::Exec end" << endl;
}

//_____________________________________________________________________
void BmdTstSim::Finish() {
  fMCTrackArray->Clear();
  fMCBmdPointArray->Clear();
  if (fMCTpcPointArray)
    fMCTpcPointArray->Clear();
}

//_____________________________________________________________________
void BmdTstSim::CreateMyTree() {
  if (!fTree)
    fTree = new TNtuple(
        "bmdpoints", "bmd points",
        "tpdg:tmotherid:tpx:tpy:tpz:tx:ty:tz:tmass:tenergy:tpt:tp:trapidity:"
        "tstarttime:ptrackid:peventid:px:py:pz:ppx:ppy:ppz:ptime:plength:"
        "peloss:pmcvolumeid:pcopy:pcopymother:tmotherpdg:tmotherz:tZ:tA",
        1000000);

  if (!fTreeSummary)
    fTreeSummary = new TNtupleD(
        "bmdsummary", "bmd summary",
        "elossall:elossbmd1:elossbmd2:tpctracks:b:ekin:ekin1:nucleons1:pions1:"
        "protons1:ekin2:nucleons2:pions2:protons2",
        //				 "elossall:elossbmd1:elossbmd2:tpctracks:b:ekin:ebmd1[4]:ebmd2[4]",
        1000000);

  //    if (fVerbose) {
  //     LOG(info) << "-- BmdTstSim::CreateMyTree bmdpoints:" <<
  //     (fTree->GetCurrentFile()->GetName())<< endl; cout << "--
  //     BmdTstSim::CreateMyTree bmdsummary:" <<
  //     (fTreeSummary->GetCurrentFile()->GetName())<< endl;
  //    }

  if (!fH3) {
    fH3 = new TH3F("H3", "Counts(ELoss,TpcTracks,b)", 40, 0, 40, 120, 0, 1200,
                   32, 0, 16);
    //     fH3 = new TH3F
    //     ("H3","Counts(ELoss,TpcTracks,b)",50,0,10,120,0,1200,40,0,2);
    //    fH3->SetDirectory(0);
  }
}

ClassImp(BmdTstSim)
