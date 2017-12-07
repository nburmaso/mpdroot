// Author: Oleg Rogachevsky
// Update: 2009-10-07 17:56:17+0400
// Copyright: 2009 (C) MPD coll.
//
// fill dst task

#include "MpdFillDstTask.h"
#include "MpdKalmanTrack.h"
#include "MpdTpcKalmanTrack.h"
#include "MpdEctKalmanTrack.h"
#include "MpdTofMatchingData.h"
#include "MpdVertex.h"
#include "MpdParticleIdentification.h"

#include "MpdFieldCreator.h"
#include "MpdMapPar.h"
#include "FairMCTrack.h"
#include "FairRootManager.h"
#include "FairRunAna.h"
#include "FairRuntimeDb.h"

#include <TMath.h>
#include <TMatrixD.h>
#include "TGeoManager.h"
#include "TGeoBBox.h"
#include "TGeoTube.h"

#include <assert.h>
#include <set>
#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

//using std::cout;
//using std::flush;
//using std::endl;


// -----   constructor with names ------------------------------------

MpdFillDstTask::MpdFillDstTask(const char *name, const char *title)
: FairTask(name) {
    fEvent = NULL;
    fZdcSkeletonesSaved = 0;
}
// -----   Destructor -----------------------------------------------

MpdFillDstTask::~MpdFillDstTask() {
    if (fEvent) delete fEvent;
}
// -------------------------------------------------------------------

InitStatus MpdFillDstTask::Init() {
    fEvent = new MpdEvent();

    FairRootManager *manager = FairRootManager::Instance();

    fKFTracks = (TClonesArray *) manager->GetObject("TpcKalmanTrack");
    fKFEctTracks = (TClonesArray *) manager->GetObject("EctTrack");
    fMCTracks = (TClonesArray *) manager->GetObject("MCTrack");
    fMCEventHeader = (FairMCEventHeader*) manager->GetObject("MCEventHeader.");
    fTofMatching = (TClonesArray *) manager->GetObject("TOFMatching");
    fEtofMatching = (TClonesArray *) manager->GetObject("ETOFMatching");
    fVertex = (TClonesArray *) manager->GetObject("Vertex"); //AZ

    fELossZdc1Value = (TClonesArray *) manager->GetObject("ELossZdc1Value"); //EL
    fELossZdc2Value = (TClonesArray *) manager->GetObject("ELossZdc2Value"); //EL
    fELossZdc1Histo = (TClonesArray *) manager->GetObject("ELossZdc1Histo"); //EL
    fELossZdc2Histo = (TClonesArray *) manager->GetObject("ELossZdc2Histo"); //EL
    fHistZdc1En = (TH2F *) manager->GetObject("HistZdc1En"); //EL
    fHistZdc2En = (TH2F *) manager->GetObject("HistZdc2En"); //EL
    fZdcSkeletonesSaved = 0;

    FairRootManager::Instance()->Register("MCEventHeader.", "MC", fMCEventHeader, kTRUE);
    FairRootManager::Instance()->Register("MCTrack", "MC", fMCTracks, kTRUE);
    //FairRootManager::Instance()->Register("MpdEvent","MpdEvents", fEvents, kTRUE);
    FairRootManager::Instance()->Register("MPDEvent.", "MpdEvent", fEvent, kTRUE);
    // FairRootManager::Instance()->Register("EZdc1","EZdc1", fELossZdc1Histo, kTRUE);
    // FairRootManager::Instance()->Register("EZdc2","EZdc2", fELossZdc2Histo, kTRUE);

    //fhTrackMotherId = new TH1F("TrackMotherId","",1000,-5,995);
    //fhTrackPrimaryPDG = new TH1F("TrackPrimaryPDG","",1000,-500,500);
    //fhTrackVertex = new TH2F("TrackVertex","",1000,-500,500,1000,-500,500);
    //fhTruthVertex = new TH2F("TruthVertex","",1000,-500,500,1000,-500,500);

    return kSUCCESS;
}
// -------------------------------------------------------------------

void MpdFillDstTask::Exec(Option_t * option) {
    Reset(); // Clear previos event information

    if (!fZdcSkeletonesSaved) { // empty skeletones saved only once 
        FairRootManager* ioman = FairRootManager::Instance();
        if (fHistZdc1En) {
            fHistZdc1En->SetDirectory((TFile*) ioman->GetOutFile());
            fHistZdc2En->SetDirectory((TFile*) ioman->GetOutFile());
            fHistZdc1En->Write();
            fHistZdc2En->Write();
            fZdcSkeletonesSaved = 1;
        }
    }

    Int_t nReco = fKFTracks ? fKFTracks->GetEntriesFast() : 0;
    Int_t nEctReco = fKFEctTracks ? fKFEctTracks->GetEntriesFast() : 0;
    cout << "\n-I- [MpdFillDstTask::Exec] " << nReco + nEctReco << " reconstruced tracks to write" << endl;

    FairRunAna *fRun = FairRunAna::Instance();
    fEvent->SetRunInfoRunId(fRun->GetRunId());

    FairField *fPar = fRun->GetField();
    fEvent->SetRunInfoMagneticFieldZ(fPar->GetType());

    if (fVertex) {
        MpdVertex *vertex = (MpdVertex*) fVertex->UncheckedAt(0);
        fEvent->SetPrimaryVerticesX(vertex->GetX());
        fEvent->SetPrimaryVerticesY(vertex->GetY());
        fEvent->SetPrimaryVerticesZ(vertex->GetZ());
    } else {
        // Vertex info is not available
        fEvent->SetPrimaryVerticesX(0.);
        fEvent->SetPrimaryVerticesY(0.);
        fEvent->SetPrimaryVerticesZ(0.);
    }

    // check clone track into ECT
    typedef std::set<Int_t> trackSet;
    trackSet EctTrackSet;
    for (Int_t index = 0; index < nEctReco; index++) // cycle by ECT KF tracks
    {
        MpdEctKalmanTrack *track = (MpdEctKalmanTrack*) fKFEctTracks->UncheckedAt(index);
        if (track->IsFromTpc()) EctTrackSet.insert(trackSet::value_type(track->GetTpcIndex()));
    }

    Int_t nMatching = fTofMatching ? fTofMatching->GetEntriesFast() : 0;
    MpdTofMatchingData *pMatchingData;
    bool matchingDataExist;

    //AZ MpdParticleIdentification *identificator = new MpdParticleIdentification();
    MpdParticleIdentification identificator;

    for (Int_t i = 0; i < nReco; i++) {
        // check clone track into ECT	
        if (nEctReco > 0) {
            trackSet::iterator it = EctTrackSet.find(i);
            if (it != EctTrackSet.end()) continue;
        }

        MpdKalmanTrack *kftrack = (MpdKalmanTrack*) fKFTracks->UncheckedAt(i);
        MpdTpcKalmanTrack *kfTPCtrack = (MpdTpcKalmanTrack*) fKFTracks->UncheckedAt(i);

        MpdTrack *track = fEvent->AddGlobalTrack();
        if (kfTPCtrack->GetRecoQuality()) track->SetEdgeCut(kTRUE);
        track->SetID(kftrack->GetTrackID());
        track->SetNofHits(kftrack->GetNofHits());
        track->SetdEdXTPC(kftrack->GetPartID());
        Float_t Ppi, Pk, Pe, Pp;

        if (!identificator.GetTpcProbs(kftrack->Momentum3().Mag(), kftrack->GetPartID(), kftrack->GetNofHits(), Ppi, Pk, Pp, Pe, 0)) { //0 - equal bayesian coefficients
            track->SetTPCpidProb(Pe, Ppi, Pk, Pp, BIT(2));
        }
        matchingDataExist = false;
        for (Int_t tofIndex = 0; tofIndex < nMatching; tofIndex++) {
            pMatchingData = (MpdTofMatchingData*) fTofMatching->UncheckedAt(tofIndex);
            if (pMatchingData->GetKFTrackIndex() == i) {
                matchingDataExist = true;
                break;
            } // first matching
        }

        if (matchingDataExist) {
            track->SetTofBeta(pMatchingData->GetBeta());
            track->SetTofMass2(pMatchingData->GetMass2());
            track->SetTofHitIndex(pMatchingData->GetTofHitIndex());

            if (!identificator.GetTofProbs(pMatchingData->GetMomentum().Mag(), pMatchingData->GetBeta(), Ppi, Pk, Pp, Pe, 0)) {
                track->SetTOFpidProb(Pe, Ppi, Pk, Pp, BIT(1));
            }
        }
        Float_t tpcProbs[4] = {track->GetTPCPidProbPion(), track->GetTPCPidProbKaon(), track->GetTPCPidProbProton(), track->GetTPCPidProbElectron()};
        Float_t tofProbs[4] = {track->GetTOFPidProbPion(), track->GetTOFPidProbKaon(), track->GetTOFPidProbProton(), track->GetTOFPidProbElectron()};
        Float_t combProbs[4]; //probabilities combined from TOF & TPC
        identificator.GetCombinedProbs(tofProbs, tpcProbs, combProbs, 4);
        Ppi = combProbs[0];
        Pk = combProbs[1];
        Pp = combProbs[2];
        Pe = combProbs[3];
        track->SetCombPidProb(Pe, Ppi, Pk, Pp);

        if (kftrack->GetParam(4) == 0.) track->SetPt(TMath::Sqrt(-1)); /*NaN*/
        else track->SetPt(1. / kftrack->GetParam(4)); /*signed Pt*/

        track->SetTheta(TMath::PiOver2() - kftrack->GetParam(3)); // Theta: angle from beam line
        track->SetPhi(kftrack->GetParam(2)); // Phi

        TMatrixD Cov = *kftrack->GetCovariance(); // Error matrix
        track->SetPtError(TMath::Sqrt(Cov(4, 4))); // Pt error
        track->SetThetaError(TMath::Sqrt(Cov(3, 3))); // Theta error
        track->SetPhiError(TMath::Sqrt(Cov(2, 2))); // Phi error

        track->SetChi2(kftrack->GetChi2());

        Double_t phi = kftrack->GetParam(0) / kftrack->GetPosNew();
        track->SetFirstPointX(kftrack->GetPosNew() * TMath::Cos(phi)); // closest to beam line
        track->SetFirstPointY(kftrack->GetPosNew() * TMath::Sin(phi));
        track->SetFirstPointZ(kftrack->GetParam(1));

        track->SetLastPointX(0.); // AZ - currently not available
        track->SetLastPointY(0.); // AZ - currently not available
        track->SetLastPointZ(0.); // AZ - currently not available 
    }


    Int_t nEMatching = fEtofMatching ? fEtofMatching->GetEntries() : 0;
    MpdTofMatchingData *pEMatchingData;

    for (Int_t i = 0; i < nEctReco; i++) {
        MpdKalmanTrack *kftrack = (MpdKalmanTrack*) fKFEctTracks->UncheckedAt(i);

        MpdTrack *track = fEvent->AddGlobalTrack();
        track->SetID(kftrack->GetTrackID());
        track->SetNofHits(kftrack->GetNofHits());
        matchingDataExist = false;
        for (Int_t etofIndex = 0; etofIndex < nEMatching; etofIndex++) {
            pEMatchingData = (MpdTofMatchingData*) fEtofMatching->UncheckedAt(etofIndex);
            if (pEMatchingData->GetKFTrackIndex() == i) {
                matchingDataExist = true;
                break;
            } // first matching
        }

        if (matchingDataExist) {
            track->SetTofMass2(pEMatchingData->GetMass2());
            track->SetTofHitIndex(pEMatchingData->GetTofHitIndex());
        }

        if (kftrack->GetParam(4) == 0.) track->SetPt(TMath::Sqrt(-1)); /*NaN*/
        else track->SetPt(1. / kftrack->GetParam(4)); /*signed Pt*/

        track->SetTheta(TMath::PiOver2() - kftrack->GetParam(3)); // Theta: angle from beam line
        track->SetPhi(kftrack->GetParam(2)); // Phi

        TMatrixD Cov = *kftrack->GetCovariance(); // Error matrix
        track->SetPtError(TMath::Sqrt(Cov(4, 4))); // Pt error
        track->SetThetaError(TMath::Sqrt(Cov(3, 3))); // Theta error
        track->SetPhiError(TMath::Sqrt(Cov(2, 2))); // Phi error

        track->SetChi2(kftrack->GetChi2());

        Double_t phi = kftrack->GetParam(0) / kftrack->GetPosNew();
        track->SetFirstPointX(kftrack->GetPosNew() * TMath::Cos(phi)); // closest to beam line
        track->SetFirstPointY(kftrack->GetPosNew() * TMath::Sin(phi));
        track->SetFirstPointZ(kftrack->GetParam(1));

        track->SetLastPointX(0.); // AZ - currently not available
        track->SetLastPointY(0.); // AZ - currently not available
        track->SetLastPointZ(0.); // AZ - currently not available 
    }
}

// -------------------------------------------------------------------
//Delete MC tracks being outside the MPD

void MpdFillDstTask::CleanMC() {
    cout << "-I- [MpdFillDstTask::Exec] Cleaning from outer decays..." << flush;

    Int_t nMC = fMCTracks->GetEntriesFast(), motherId;
    FairMCTrack *track;

    //reading outer radius of TOF
    FairRunAna::Instance()->GetRuntimeDb()->getContainer("FairBaseParSet");
    assert(gGeoManager);
    TGeoTube* geoTube = (TGeoTube*) gGeoManager->GetVolume("tof1")->GetShape();
    Double_t rMax = geoTube->GetRmax();
    //cout <<"rMax="<<rMax<<endl;

    vector<int> removedMother;
    Int_t i, j;
    for (i = 0; i < nMC; i++) {
        track = (FairMCTrack*) fMCTracks->UncheckedAt(i);

        //filling hostograms
        motherId = track->GetMotherId();
        //fhTrackMotherId->Fill(motherId);
        if (motherId == -1) {
            //fhTrackPrimaryPDG->Fill(track->GetPdgCode());
        } else {
            //fhTrackVertex->Fill(track->GetStartX(),track->GetStartY());
            //if decay was out of MPD
            if (sqrt(track->GetStartX() * track->GetStartX() + track->GetStartY() * track->GetStartY()) > rMax) {
                fMCTracks->Remove(track);
                removedMother.push_back(i);
            } else {
                //motherId < childId (i) REQUIRED
                /*for (j = 0; j < cntRemovedMother; j++){
                    if (motherId == removedMother[j]){
                        fMCTracks->Remove(track);
                        removedMother[cntRemovedMother] = i;
                        cntRemovedMother++;
                        break;
                    }
                }
                if (j == cntRemovedMother) fhTruthVertex->Fill(track->GetStartX(),track->GetStartY());
                else cnt++;*/
                /*Int_t left = 0, right = cntRemovedMother-1, middle;
                while (left <= right){
                    middle = (left + right)/2;
                    if (motherId > removedMother[middle])
                        left = middle + 1;
                    else{
                        if (motherId < removedMother[middle])
                            right = middle - 1;
                        else
                            break;
                    }
                }
                if (left > right) fhTruthVertex->Fill(track->GetStartX(),track->GetStartY());
                else{
                    fMCTracks->Remove(track);
                    removedMother[cntRemovedMother] = i;
                    cntRemovedMother++;
                }*/
                //binary search
                if (binary_search(removedMother.begin(), removedMother.end(), motherId)) {
                    fMCTracks->Remove(track);
                    removedMother.push_back(i);
                }
                //else
                //    fhTruthVertex->Fill(track->GetStartX(),track->GetStartY());
            }
        }
    }

    fMCTracks->Compress();
    cout << endl;
}

// -------------------------------------------------------------------

void MpdFillDstTask::Reset() {
    fEvent->Reset();
}
// -------------------------------------------------------------------

void MpdFillDstTask::Finish() {
    //cout<<"\n-I- [MpdFillDstTask::Finish] "<< endl;
    //fEvents->Dump();
    //cout << "\n";

    /* !this code gives some problems! -- !add extra event and so segfault!
    FairRootManager *fManager = FairRootManager::Instance();
    fManager->Fill();
     */
    //fhTrackMotherId->Write();
    //fhTrackPrimaryPDG->Write();
    //fhTrackVertex->Write();
    //fhTruthVertex->Write();

    delete fEvent;
    fEvent = NULL;
}

// -------------------------------------------------------------------

/*MpdEvent *MpdFillDstTask::AddEvent(Option_t * option)
{
  TClonesArray &events = *fEvents;
  Int_t size = events.GetEntriesFast(); // It is really always only one record
  MpdEvent* event = new(events[size]) MpdEvent();
  return event;
}*/

MpdTrack *MpdFillDstTask::AddPrimaryTrack() {
    return NULL;
}
// -------------------------------------------------------------------
ClassImp(MpdFillDstTask);
