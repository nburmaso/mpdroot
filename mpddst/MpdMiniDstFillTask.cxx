#include "MpdMiniDstFillTask.h"

// Initialize values for static data members of MpdMiniArrays
#include <MpdMiniArrays.cxx>

MpdMiniDstFillTask::MpdMiniDstFillTask() {

}

MpdMiniDstFillTask::MpdMiniDstFillTask(TString outName) :
fEvents(nullptr),
mMiniDst(new MpdMiniDst()),
mBField(.5), // mag. field in T
mOutputFileName(outName),
mOutputFile(nullptr),
mTTree(nullptr),
mSplit(99),
mCompression(9),
mBufferSize(65536 * 4),
mMiniArrays(nullptr),
fIsUseTpc(kTRUE),
fIsUseToF(kTRUE),
fIsUseEcal(kFALSE),
fIsUseMcTracks(kFALSE) {
    streamerOff();
    createArrays();
}

MpdMiniDstFillTask::~MpdMiniDstFillTask() {
    delete mMiniDst;
}

InitStatus MpdMiniDstFillTask::Init() {
    if (mOutputFileName.IsNull())
        return kERROR;

    // Creating output tree with miniDST
    mOutputFile = new TFile(mOutputFileName.Data(), "RECREATE");
    LOG_INFO << " Output file: " << mOutputFileName.Data() << " created." << endm;
    mOutputFile->SetCompressionLevel(mCompression);
    int bufsize = mBufferSize;
    if (mSplit) bufsize /= 4;
    mTTree = new TTree("MiniDst", "MpdMiniDst", mSplit);
    mTTree->SetAutoSave(1000000);
    for (Int_t i = 0; i < MpdMiniArrays::NAllMiniArrays; ++i) {
        Bool_t toBeSkipped = kFALSE;
        TString name = TString(MpdMiniArrays::miniArrayNames[i]);
        if (name.Contains("ECal") && !fIsUseEcal)
            toBeSkipped = kTRUE;
        else if (name.Contains("BTof") && !fIsUseToF)
            toBeSkipped = kTRUE;
        else if (name.Contains("Mc") && !fIsUseMcTracks)
            toBeSkipped = kTRUE;
        else if (name.Contains("Track") && !fIsUseTpc)
            toBeSkipped = kTRUE;

        if (!toBeSkipped)
            mTTree->Branch(MpdMiniArrays::miniArrayNames[i], &mMiniArrays[i], bufsize, mSplit);
    }

    FairRootManager* ioman = FairRootManager::Instance();

    // Get MC event header
    fEventHeaders = (FairMCEventHeader*) ioman->GetObject("MCEventHeader.");

    // Get information on event
    fEvents = (MpdEvent*) ioman->GetObject("MPDEvent.");

    // Get information on reconstructed vertex
    fVertices = (TClonesArray*) ioman->GetObject("Vertex");

    // Get information on tracks in TPC
    fTpcTracks = (TClonesArray*) ioman->GetObject("TpcKalmanTrack");

    // Get information on hits in ToF and matching with the TPC tracks
    fTofHits = (TClonesArray*) ioman->GetObject("TOFHit");
    fTofMatching = (TClonesArray*) ioman->GetObject("TOFMatching");

    // Get information on Monte Carlo tracks ...
    fMCTracks = (TClonesArray*) ioman->GetObject("MCTrack");

    return kSUCCESS;
}

void MpdMiniDstFillTask::Exec(Option_t* option) {
    fillEvent();
    fillTracks();
    fillBTofHits();

    mMiniDst->printTracks();
    mTTree->Fill();
}

void MpdMiniDstFillTask::Finish() {
    if (mOutputFile) {
        mOutputFile->Write();
        mOutputFile->Close();
    }
}

void MpdMiniDstFillTask::streamerOff() {
    MpdMiniEvent::Class()->IgnoreTObjectStreamer();
    MpdMiniTrack::Class()->IgnoreTObjectStreamer();
    MpdMiniBTofHit::Class()->IgnoreTObjectStreamer();
    MpdMiniBECalHit::Class()->IgnoreTObjectStreamer();
    MpdMiniBTofPidTraits::Class()->IgnoreTObjectStreamer();
    MpdMiniBECalPidTraits::Class()->IgnoreTObjectStreamer();
    MpdMiniTrackCovMatrix::Class()->IgnoreTObjectStreamer();
    MpdMiniMcEvent::Class()->IgnoreTObjectStreamer();
    MpdMiniMcTrack::Class()->IgnoreTObjectStreamer();
}

void MpdMiniDstFillTask::createArrays() {
    mMiniArrays = new TClonesArray*[MpdMiniArrays::NAllMiniArrays];
    for (Int_t iArr = 0; iArr < MpdMiniArrays::NAllMiniArrays; iArr++)
        mMiniArrays[iArr] = new TClonesArray(MpdMiniArrays::miniArrayTypes[iArr], MpdMiniArrays::miniArraySizes[iArr]);

    mMiniDst->set(mMiniArrays);
}

void MpdMiniDstFillTask::fillEvent() {
    TClonesArray* miniEventHeaders = mMiniArrays[MpdMiniArrays::Event];
    miniEventHeaders->Delete();

    MpdMiniEvent* miniEvent = new ((*miniEventHeaders)[miniEventHeaders->GetEntriesFast()]) MpdMiniEvent();

    MpdVertex* vertex = (MpdVertex*) fVertices->UncheckedAt(0);
    // Primary vertex
    miniEvent->setPrimaryVertexPosition(TVector3(vertex->GetX(), vertex->GetY(), vertex->GetZ()));

    // Number of glob. tracks per event
    TClonesArray* glTracks = (TClonesArray*) fEvents->GetGlobalTracks();
    miniEvent->setNumberOfGlobalTracks(glTracks->GetEntriesFast());
}

void MpdMiniDstFillTask::fillTracks() {
    // Monte Carlo tracks
    TClonesArray* miniTracksSimu = mMiniArrays[MpdMiniArrays::McTrack];
    miniTracksSimu->Delete();

    for (Int_t iSimuTrack = 0; iSimuTrack < fMCTracks->GetEntriesFast(); iSimuTrack++) {
        FairMCTrack* mcTrack = (FairMCTrack*) fMCTracks->UncheckedAt(iSimuTrack);

        MpdMiniMcTrack* miniTrack = new ((*miniTracksSimu)[miniTracksSimu->GetEntriesFast()]) MpdMiniMcTrack();
        miniTrack->setId(iSimuTrack);
        miniTrack->setPdgId(mcTrack->GetPdgCode());
        miniTrack->setPx(mcTrack->GetPx());
        miniTrack->setPy(mcTrack->GetPy());
        miniTrack->setPz(mcTrack->GetPz());
        miniTrack->setEnergy(mcTrack->GetEnergy());
        miniTrack->setX(mcTrack->GetStartX());
        miniTrack->setY(mcTrack->GetStartY());
        miniTrack->setZ(mcTrack->GetStartZ());
        miniTrack->setT(mcTrack->GetStartT());
    }

    // Reconstructed tracks
    TClonesArray* miniTracksReco = mMiniArrays[MpdMiniArrays::Track];
    TClonesArray* miniTracksRecoCovMatrices = mMiniArrays[MpdMiniArrays::TrackCovMatrix];

    miniTracksReco->Delete();
    miniTracksRecoCovMatrices->Delete();

    // Get global tracks from event
    TClonesArray* glTracks = (TClonesArray*) fEvents->GetGlobalTracks();

    for (Int_t iGlobTrack = 0; iGlobTrack < glTracks->GetEntriesFast(); iGlobTrack++) {
        MpdTrack* glTrack = (MpdTrack*) glTracks->UncheckedAt(iGlobTrack);

        // Create miniTrack for each global track ...
        MpdMiniTrack* miniTrack = new ((*miniTracksReco)[miniTracksReco->GetEntriesFast()]) MpdMiniTrack();
        miniTrack->setId(glTrack->GetID());
        miniTrack->setChi2(glTrack->GetChi2());
        miniTrack->setNHits(glTrack->GetNofHits());
        miniTrack->setNSigmaElectron(glTrack->GetNSigmaElectron());
        miniTrack->setNSigmaPion(glTrack->GetNSigmaPion());
        miniTrack->setNSigmaKaon(glTrack->GetNSigmaKaon());
        miniTrack->setNSigmaProton(glTrack->GetNSigmaProton());
        miniTrack->setDedx(glTrack->GetdEdXTPC());

        // Setting initial approx. for momentum ...
        TVector3 globMom(glTrack->GetPx(), glTrack->GetPy(), glTrack->GetPz());

        // Getting primary vertex  ...
        MpdVertex* vertex = (MpdVertex*) fVertices->UncheckedAt(0);
        TVector3 primVertex = TVector3(vertex->GetX(), vertex->GetY(), vertex->GetZ());

        // Setting DCA ...
        TVector3 globOrigin(glTrack->GetDCAX() + primVertex.X(), glTrack->GetDCAY() + primVertex.Y(), glTrack->GetDCAZ() + primVertex.Z());
        miniTrack->setOrigin(globOrigin.x(), globOrigin.y(), globOrigin.Z());

        Int_t q = glTrack->GetCharge();
        MpdMiniPhysicalHelix* helix = new MpdMiniPhysicalHelix(globMom, globOrigin, mBField, q);
        helix->moveOrigin(helix->pathLength(primVertex));

        // Momentum at primary vertex (Do we need to do it?)
        globMom = helix->momentum(mBField);

        miniTrack->setGlobalMomentum(globMom.X(), globMom.Y(), globMom.Z());

        delete helix;

        // Get Kalman track for each glob. track
        for (Int_t iKalman = 0; iKalman < fTpcTracks->GetEntriesFast(); iKalman++) {
            MpdTpcKalmanTrack* kalmTrack = (MpdTpcKalmanTrack*) fTpcTracks->UncheckedAt(iKalman);
            if (kalmTrack->GetTrackID() != glTrack->GetID())
                continue;

            MpdMiniTrackCovMatrix* miniTrackCovMatrix = new ((*miniTracksRecoCovMatrices)[miniTracksRecoCovMatrices->GetEntriesFast()]) MpdMiniTrackCovMatrix();
            miniTrackCovMatrix->setPti(1. / kalmTrack->GetParam(4));

            const Double_t* matrixElements = kalmTrack->GetCovariance()->GetMatrixArray();
            const Int_t nElements = kalmTrack->GetCovariance()->GetNoElements();

            vector <Float_t> sigmas;
            vector <Float_t> correlations;

            Int_t shift = Int_t(TMath::Sqrt(nElements));

            for (Int_t iEle = 0; iEle < nElements; iEle++)
                if (iEle % Int_t(TMath::Sqrt(nElements) + 1) == 0) {
                    sigmas.push_back(matrixElements[iEle]);

                    for (Int_t jEle = 1; jEle < shift; jEle++) {
                        correlations.push_back(matrixElements[iEle + jEle]);
                    }

                    shift--;
                }

            miniTrackCovMatrix->setSigmas(sigmas);
            miniTrackCovMatrix->setCorrelations(correlations);
        }

        // Set ToF hit index if matched ...
        miniTrack->setBTofPidTraitsIndex(glTrack->GetTofHitIndex());
    }

    // Get primary tracks from event
    TClonesArray* primaryTracks = (TClonesArray*) fEvents->GetPrimaryTracks();

    for (Int_t iPrimaryTrack = 0; iPrimaryTrack < primaryTracks->GetEntriesFast(); iPrimaryTrack++) {
        MpdTrack* primaryTrack = (MpdTrack*) primaryTracks->UncheckedAt(iPrimaryTrack);

        // Process here primary tracks ...
    }

    // Get ToF information ...
    TClonesArray* miniToF = mMiniArrays[MpdMiniArrays::BTofPidTraits];
    miniToF->Delete();

    for (Int_t iMatchedTrack = 0; iMatchedTrack < fTofMatching->GetEntriesFast(); iMatchedTrack++) {
        MpdTofMatchingData* dataMatch = (MpdTofMatchingData*) fTofMatching->UncheckedAt(iMatchedTrack);

        // Create pidToF for tracks having a matched hit in ToF ...
        MpdMiniBTofPidTraits* pidToF = new ((*miniToF)[miniToF->GetEntriesFast()]) MpdMiniBTofPidTraits();
        pidToF->setBeta(dataMatch->GetBeta());
        pidToF->setTrackIndex(dataMatch->GetKFTrackIndex());

        // Get a ToF hit matched to the track ...
        MpdTofHit* tofHit = (MpdTofHit*) fTofHits->UncheckedAt(dataMatch->GetTofHitIndex());

        pidToF->setTOF(tofHit->GetTime());
        pidToF->setHitPositionXYZ(tofHit->GetX(), tofHit->GetY(), tofHit->GetZ());
    }
}

void MpdMiniDstFillTask::fillBTofHits() {
    TClonesArray* miniToF = mMiniArrays[MpdMiniArrays::BTofHit];
    miniToF->Delete();

    // Loop over ToF hits
    for (Int_t iHit = 0; iHit < fTofHits->GetEntriesFast(); iHit++) {
        MpdMiniBTofHit* tofHit = new ((*miniToF)[miniToF->GetEntriesFast()]) MpdMiniBTofHit();
        tofHit->setId(iHit);


    }
}