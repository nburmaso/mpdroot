/**
 * \brief Example of how to read a file (list of files) using miniDst classes
 *
 * One can use either miniDst file or a list of miniDst files (inFile.lis or
 * inFile.list) as an input, and preform physics analysis
 *
 * \author Grigory Nigmatkulov
 * \date May 29, 2018
 */

// inFile - is a name of name.miniDst.root file or a name
//          of a name.lis(t) files that contains a list of
//          name1.miniDst.root files

#include <Rtypes.h>
#include <TString.h>
#include <TChain.h>
#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TVector3.h>

#include <vector>

using namespace std;

R__ADD_INCLUDE_PATH($VMCWORKDIR)
#include "macro/mpd/mpdloadlibs.C"

void analyseMiniDst(const Char_t *inFile = "",
        const Char_t *oFileName = "oProcTest.root") {

    MpdMiniDstReader* miniDstReader = new MpdMiniDstReader(inFile);
    miniDstReader->Init();

    //Long64_t events2read = miniDstReader->chain()->GetEntries();

    // This is a way if you want to spead up IO
    std::cout << "Explicit read status for some branches" << std::endl;
    miniDstReader->SetStatus("*", 0);
    miniDstReader->SetStatus("Event*", 1);
    miniDstReader->SetStatus("Track*", 1);
    miniDstReader->SetStatus("BTofHit*", 1);
    miniDstReader->SetStatus("BTofPidTraits*", 1);
    miniDstReader->SetStatus("BECalHit*", 1);
    miniDstReader->SetStatus("BECalPidTraits*", 1);
    miniDstReader->SetStatus("TrackCovMatrix*", 1);
    miniDstReader->SetStatus("McEvent*", 1);
    miniDstReader->SetStatus("McTrack*", 1);

    std::cout << "Status has been set" << std::endl;

    if (!miniDstReader->chain()) {
        std::cout << "No chain has been found." << std::endl;
    }
    Long64_t eventsInTree = miniDstReader->tree()->GetEntries();
    std::cout << "eventsInTree: " << eventsInTree << std::endl;
    Long64_t events2read = miniDstReader->chain()->GetEntries();

    std::cout << "Number of events to read: " << events2read
            << std::endl;

    TFile *oFile = new TFile(oFileName, "RECREATE");

    // Histogramming

    // Event
    TH1F *hRefMult = new TH1F("hRefMult",
            "Reference multiplicity;refMult",
            500, -0.5, 499.5);
    TH2F *hVtxXvsY = new TH2F("hVtxXvsY",
            "hVtxXvsY",
            200, -10., 10., 200, -10., 10.);
    TH1F *hVtxZ = new TH1F("hVtxZ", "hVtxZ",
            140, -70., 70.);

    // Track
    TH1F *hGlobalPtot = new TH1F("hGlobalPtot",
            "Global track momentum;p (GeV/c)",
            100, 0., 1.);
    TH1F *hGlobalPtotCut = new TH1F("hGlobalPtotCut",
            "Global track momentum after cut;p (GeV/c)",
            100, 0., 1.);
    TH1F *hPrimaryPtot = new TH1F("hPrimaryPtot",
            "Primary track momentum;p (GeV/c)",
            100, 0., 1.);
    TH1F *hPrimaryPtotCut = new TH1F("hPrimaryPtotCut",
            "Primary track momentum after cut;p (GeV/c)",
            100, 0., 1.);
    TH1F *hTransvMomentum = new TH1F("hTransvMomentum",
            "Track transverse momentum;p_{T} (GeV/c)",
            200, 0., 2.);
    TH2F * hGlobalPhiVsPt[2];
    for (int i = 0; i < 2; i++) {
        hGlobalPhiVsPt[i] = new TH2F(Form("hGlobalPhiVsPt_%d", i),
                Form("#phi vs. p_{T} for charge: %d;p_{T} (GeV/c);#phi (rad)", (i == 0) ? 1 : -1),
                300, 0., 3.,
                630, -3.15, 3.15);
    }
    
    TH1F* hFractionGenTracks = new TH1F("Fraction of reconstructed generator tracks", "Fraction of reconstructed generator tracks; fraction; N", 100, 0., 1.);
    TH2F* hPsimPreco1 = new TH2F("pSim vs. pReco ", "pSim vs. pReco (reco2mc); pSim [GeV/c]; pReco [GeV/c]", 200, 0.2, 3., 200, 0.2, 3.);

    // McTrack
    TH1F* hSplitTracks = new TH1F("Split reconstructed tracks ", "Split reconstructed tracks (One McMini to number of Mini's); nSplit; N", 10., 0., 10.);
    TH2F* hPsimPreco = new TH2F("pSim vs. pReco", "pSim vs. pReco (mc2reco); pSim [GeV/c]; pReco [GeV/c]", 200, 0.2, 3., 200, 0.2, 3.);

    // BTof pid traits
    TH1F *hTofBeta = new TH1F("hTofBeta", "BTofPidTraits #beta;#beta",
            2000, 0., 2.);
    
    // BTofHit
    TH1F* hFiredStrips = new TH1F("TofFiredStrips", "TofFiredStrips; strip; N",
            25, 0., 25.);
    
    // BEcalHit
     TH2F* hEdepNtracks = new TH2F("eDep vs. Ntracks", "eDep vs. Ntracks; eDep [GeV]; Ntracks", 200, 0., 1., 7, 0., 7.);

    // Loop over events
    for (Long64_t iEvent = 0; iEvent < events2read; iEvent++) {

        std::cout << "Working on event #[" << (iEvent + 1)
                << "/" << events2read << "]" << std::endl;

        Bool_t readEvent = miniDstReader->readMiniEvent(iEvent);
        if (!readEvent) {
            std::cout << "Something went wrong! Nothing to analyze..." << std::endl;
            break;
        }

        // Retrieve miniDst
        MpdMiniDst *dst = miniDstReader->miniDst();

        // Retrieve event information
        MpdMiniEvent *event = dst->event();
        if (!event)
            continue;

        hRefMult->Fill(event->refMult());

        TVector3 pVtx = event->primaryVertex();
        hVtxXvsY->Fill(event->primaryVertex().X(), event->primaryVertex().Y());
        hVtxZ->Fill(event->primaryVertex().Z());

        // 1. Track loop over miniMcTracks. They are primary tracks from generator
        // There is a link between <<each miniMcTrack and a set of indices for reconstructed miniTracks>> for considering miniMcTrack
        for (Int_t iTrk = 0; iTrk < dst->numberOfMcTracks(); iTrk++) {

            // Retrieve i-th miniMcTrack
            MpdMiniMcTrack* miniMcTrack = dst->mcTrack(iTrk);

            if (!miniMcTrack)
                continue;

            // Get vector of indices <<mcMiniTrack --> miniTrack>>
            vector <UShort_t> miniTrackIdx = miniMcTrack->recoTrackIds();

            hSplitTracks->Fill(miniTrackIdx.size());

            // Skipping mcMiniTrack with no one found miniTrack
            if (miniTrackIdx.size() == 0)
                continue;
            
            // Getting corresponding miniTracks ...
            vector <MpdMiniTrack*> miniTracks;
            for (auto it : miniTrackIdx)
                miniTracks.push_back(dst->track(it));

            Double_t pSim = miniMcTrack->p().Mag();

            if (miniTrackIdx.size() == 1) // One mcMini to One Mini track is being considered  
                hPsimPreco->Fill(pSim, miniTracks[0]->gMom().Mag());
        }

        // 2. Track loop over reconstructed miniTracks ...
        Int_t nFoundMcMiniTracks = 0;
        for (Int_t iTrk = 0; iTrk < dst->numberOfTracks(); iTrk++) {

            // Retrieve i-th mini track
            MpdMiniTrack *miniTrack = dst->track(iTrk);

            if (!miniTrack) continue;
            //std::cout << "Track #[" << (iTrk+1) << "/" << nTracks << "]"  << std::endl;

            hGlobalPtot->Fill(miniTrack->gMom().Mag());
            if (miniTrack->isPrimary()) {
                hPrimaryPtot->Fill(miniTrack->pMom().Mag());
            }

            // Simple single-track cut
            if (miniTrack->gMom().Mag() < 0.1 ||
                    miniTrack->gDCA(pVtx).Mag() > 50.) {
                continue;
            }
            hTransvMomentum->Fill(miniTrack->gMom().Pt());
            hGlobalPtotCut->Fill(miniTrack->gMom().Mag());
            if (miniTrack->isPrimary()) {
                hPrimaryPtotCut->Fill(miniTrack->pMom().Mag());
            }
            if (miniTrack->charge() > 0) {
                hGlobalPhiVsPt[0]->Fill(miniTrack->gMom().Pt(),
                        miniTrack->gMom().Phi());
            } else {
                hGlobalPhiVsPt[1]->Fill(miniTrack->gMom().Pt(),
                        miniTrack->gMom().Phi());
            }
                        
            Int_t mcTrackIdx = miniTrack->mcTrackId();
            if (mcTrackIdx != -1) {
                nFoundMcMiniTracks++;
             
                MpdMiniMcTrack* mcTrack = dst->mcTrack(mcTrackIdx);
                
                hPsimPreco1->Fill(mcTrack->p().Mag(), miniTrack->gMom().Mag());
            }

            // Check if track has a TOF signal ...
            if (miniTrack->isBTofTrack()) {
                // Retrieve corresponding trait
                MpdMiniBTofPidTraits *trait = dst->btofPidTraits(iTrk);
                if (!trait) 
                     continue;
                // Fill beta 
                hTofBeta->Fill(trait->btofBeta());
                
                // A mechanism how to get the corresponding mini track and, probably, Monte Carlo mini track that corresponds to the considering mini track ... 
                MpdMiniTrack* track = dst->track(trait->trackIndex());
                Int_t mcIdx = track->mcTrackId();
                if (mcIdx != -1) 
                    MpdMiniMcTrack* mcTrack = dst->mcTrack(mcIdx);               
            } //if( isTofTrack() )
            
            // Check if track has an EMC signal ...
            if (miniTrack->isBECalTrack()) {
                MpdMiniBECalPidTraits *trait = dst->becalPidTraits(iTrk);
                if (!trait) 
                    continue;
              
                // A mechanism how to get the corresponding mini track and, probably, Monte Carlo mini track that corresponds to the considering mini track ... 
                MpdMiniTrack* track = dst->track(trait->trackIndex());
                Int_t mcIdx = track->mcTrackId();
                if (mcIdx != -1)
                    MpdMiniMcTrack* mcTrack = dst->mcTrack(mcIdx);
            }

        } //for(Int_t iTrk=0; iTrk<nTracks; iTrk++)
        
        hFractionGenTracks->Fill(1. * nFoundMcMiniTracks / dst->numberOfTracks());

        ////////////////////////////
        // Hit (cluster) analysis //
        ////////////////////////////
        
        // Tof
        for (Int_t iHit = 0; iHit < dst->numberOfBTofHits(); iHit++) {            
            MpdMiniBTofHit* hit = dst->btofHit(iHit);
            hFiredStrips->Fill(hit->strip());
        }
        
        // Emc
        for (Int_t iCluster = 0; iCluster < dst->numberOfBECalHits(); iCluster++) {            
            MpdMiniBECalHit* cluster = dst->becalHit(iCluster);
            
            Int_t nTracks = cluster->numberOfTracks();
            Double_t ene = cluster->energy();
            
            hEdepNtracks->Fill(ene, nTracks);
        }
     
    } //for(Long64_t iEvent=0; iEvent<events2read; iEvent++)

    oFile->Write();
    oFile->Close();
    miniDstReader->Finish();
}
