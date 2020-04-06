// MiniDst files
#include "MpdMiniDstFillTask.h"

// Initialize values for static data members of MpdMiniArrays
#include "MpdMiniArrays.cxx"
#include "MpdTpcKalmanTrack.h"

MpdMiniDstFillTask::MpdMiniDstFillTask() {
    // Default constructor
    /* empty */
}

MpdMiniDstFillTask::MpdMiniDstFillTask(TString name) :
fEvents(nullptr),
mMiniDst(new MpdMiniDst()),
mBField(0.5), // mag. field in T (MUST be changed to the real magnetic field
mOutputFile(nullptr),
mTTree(nullptr),
mSplit(99),
mCompression(9),
mBufferSize(65536 * 4),
mMiniArrays(nullptr),
fIsUseCovMatrix(kTRUE),
fEmcDigits(nullptr),
fEmcClusters(nullptr) {

    // Standard constructor
    mOutputFileName = name.ReplaceAll(".root", ".MiniDst.root");

    streamerOff();
    createArrays();
}

MpdMiniDstFillTask::~MpdMiniDstFillTask() {
    // Destructor
    delete mMiniDst;
}

InitStatus MpdMiniDstFillTask::Init() {
    // Output name must exist
    if (mOutputFileName.IsNull())
        return kERROR;

    // Creating output tree with miniDST
    mOutputFile = new TFile(mOutputFileName.Data(), "RECREATE");
    // Inform about the creation
    LOG_INFO << " Output file: " << mOutputFileName.Data() << " created." << endm;
    // Set compression level
    mOutputFile->SetCompressionLevel(mCompression);
    int bufsize = mBufferSize;
    if (mSplit) {
        bufsize /= 4;
    }
    // Create TTree
    mTTree = new TTree("MiniDst", "MpdMiniDst", mSplit);
    mTTree->SetAutoSave(1000000);

    // Create arrays
    for (Int_t i = 0; i < MpdMiniArrays::NAllMiniArrays; ++i) {
        mTTree->Branch(MpdMiniArrays::miniArrayNames[i], &mMiniArrays[i], bufsize, mSplit);
    }

    // Initialize FairRoot manager
    FairRootManager* ioman = FairRootManager::Instance();
    if (!ioman) {
        std::cout << "[ERROR] MpdMiniDstFillTask::Init - Not FairRootManager instance has been found"
                << std::endl;
        exit(0);
    }

    // Reading all possible input branches ...

    // Get MC event header
    fEventHeaders = (FairMCEventHeader*) ioman->GetObject("MCEventHeader.");
    if (!fEventHeaders) {
        std::cout << "[WARNING] MpdMiniDstFillTask::Init - No MCEventHeader has been found"
                << std::endl;
    }

    // Get information on event
    fEvents = (MpdEvent*) ioman->GetObject("MPDEvent.");
    if (!fEvents) {
        std::cout << "[WARNING] MpdMiniDstFillTask::Init - No MpdEvent has been found"
                << std::endl;
    }

    // Get information on reconstructed vertex
    fVertices = (TClonesArray*) ioman->GetObject("Vertex");
    if (!fVertices) {
        std::cout << "[WARNING] MpdMiniDstFillTask::Init - No Vertex has been found"
                << std::endl;
    }

    // Retrieve information about tracks in TPC
    fTpcTracks = (TClonesArray*) ioman->GetObject("TpcKalmanTrack");
    if (!fTpcTracks) {
        std::cout << "[WARNING] MpdMiniDstFillTask::Init - No TpcKalmanTrack has been found"
                << std::endl;
    }

    // Retrieve information about TOF hits
    fTofHits = (TClonesArray*) ioman->GetObject("TOFHit");
    if (!fTofHits) {
        std::cout << "[WARNING] MpdMiniDstFillTask::Init - No TOFHit has been found"
                << std::endl;
    }

    // Get information about TPC tracks that match TOF
    fTofMatching = (TClonesArray*) ioman->GetObject("TOFMatching");
    if (!fTofMatching) {
        std::cout << "[WARNING] MpdMiniDstFillTask::Init - No TOFMatching has been found"
                << std::endl;
    }

    // Retrieve MC Tracks
    fMCTracks = (TClonesArray*) ioman->GetObject("MCTrack");
    if (!fMCTracks) {
        std::cout << "[WARNING] MpdMiniDstFillTask::Init - No MCTracks have been found"
                << std::endl;
    }

    // Retrieve Generator (Primary) tracks ...
    fGenTracks = (TClonesArray*) ioman->GetObject("GenTracks");
    if (!fGenTracks) {
        std::cout << "[WARNING] MpdMiniDstFillTask::Init - No GenTracks have been found"
                << std::endl;
    }

    // Retrieve information on EMC digits ...
    fEmcDigits = (TClonesArray*) ioman->GetObject("EmcDigit");
    if (!fEmcDigits) {
        std::cout << "[WARNING] MpdMiniDstFillTask::Init - No EmcDigits have been found"
                << std::endl;
    }

    // Retrieve information on EMC created clusters ...
    fEmcClusters = (TObjArray*) ioman->GetObject("EmcCluster");
    if (!fEmcClusters) {
        std::cout << "[WARNING] MpdMiniDstFillTask::Init - No EmcClusters have been found"
                << std::endl;
    }

    return kSUCCESS;
}

void MpdMiniDstFillTask::Exec(Option_t* option) {

    // Main magic happens here. The methods calls for branch fills
    fillEvent();
    fillTracks();
    fillBTofHits();
    fillECalHits();

    if (fVerbose != 0) {
        mMiniDst->printTracks();
    }

    // Fill TTree
    mTTree->Fill();
}

void MpdMiniDstFillTask::Finish() {
    // Write data to the output file and close it
    if (mOutputFile) {
        mOutputFile->Write();
        mOutputFile->Close();
    }
}

void MpdMiniDstFillTask::streamerOff() {
    // Switch off streamers
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
    // Create MiniDst arrays
    mMiniArrays = new TClonesArray*[MpdMiniArrays::NAllMiniArrays];

    for (Int_t iArr = 0; iArr < MpdMiniArrays::NAllMiniArrays; iArr++) {
        mMiniArrays[iArr] = new TClonesArray(MpdMiniArrays::miniArrayTypes[iArr], MpdMiniArrays::miniArraySizes[iArr]);
    }

    // Set pointers to the arrays
    mMiniDst->set(mMiniArrays);
}

void MpdMiniDstFillTask::fillEvent() {

    // Fill event information
    TClonesArray* miniEventHeaders = mMiniArrays[MpdMiniArrays::Event];
    miniEventHeaders->Delete();

    // Create new event information
    MpdMiniEvent* miniEvent = new ((*miniEventHeaders)[miniEventHeaders->GetEntriesFast()]) MpdMiniEvent();
    if (!miniEvent) {
        std::cout << "[ERROR] MpdMiniDstFillTask::fillEvent - No MiniEvent has been created"
                << std::endl;
        // In this case we need to quit (probably completely)
        return;
    }

    // Retrieve information about reconstructed primary vertex
    MpdVertex* vertex = (MpdVertex*) fVertices->UncheckedAt(0);

    // Primary vertex position
    if (vertex) {
        miniEvent->setPrimaryVertexPosition(vertex->GetX(),
                vertex->GetY(),
                vertex->GetZ());
    } else {
        // In case that vertex has not been reconstructed
        miniEvent->setPrimaryVertexPosition(-999., -999., -999.);
    }

    // Number of global tracks in the current event
    if (!fEvents) {
        std::cout << "[ERROR] MpdMiniDstFillTask::fillEvent - No event information has been found"
                << std::endl;
        return;
    }

    // Retrieve global tracks
    TClonesArray* glTracks = (TClonesArray*) fEvents->GetGlobalTracks();
    miniEvent->setNumberOfGlobalTracks(glTracks->GetEntriesFast());

    // Fill McEvent info
    TClonesArray* miniMcEventHeaders = mMiniArrays[MpdMiniArrays::McEvent];
    miniMcEventHeaders->Delete();

    // Retrieve number of primary tracks
    Int_t nPrim = fEventHeaders->GetNPrim();
    Int_t nColl = -1;
    Double_t rp = -1;
    // Impact parameter
    Double_t b = fEventHeaders->GetB();
    // Run number
    UInt_t runId = fEventHeaders->GetRunID();
    // Event number
    UInt_t eventId = fEventHeaders->GetEventID();
    // Time
    Double_t time = fEventHeaders->GetT();
    // Fill MC vertex
    TVector3 vtx;
    fEventHeaders->GetVertex(vtx);

    // Create and fill MiniMcEvent
    new ((*miniMcEventHeaders)[miniMcEventHeaders->GetEntriesFast()]) MpdMiniMcEvent(runId, eventId, rp, b, nPrim, nColl, vtx, time);
}

void MpdMiniDstFillTask::fillTracks() {
    // Map for MC to Kalman track ids correspondence
    std::map< Int_t, std::vector< UShort_t > > mc2reco;

    // Fill McTracks if exist
    for (Int_t iSimuTrack = 0; iSimuTrack < fMCTracks->GetEntriesFast(); iSimuTrack++) {
        std::vector< UShort_t > ids;

        // Retrieve MCTrack
        MpdMCTrack* mcTrack = (MpdMCTrack*) fMCTracks->UncheckedAt(iSimuTrack);
        // Skip non-existing tracks
        if (!mcTrack || (mcTrack->GetMotherId() != -1))
            continue;
        // Fill mc2reco with empty reco ids in order to fill it in the Kalman track loop
        mc2reco[iSimuTrack] = ids;
    }

    // Reconstructed tracks and their covariance matrices
    TClonesArray* miniTracksReco = mMiniArrays[MpdMiniArrays::Track];
    TClonesArray* miniTracksRecoCovMatrices = mMiniArrays[MpdMiniArrays::TrackCovMatrix];
    // Create TOF-matching information
    TClonesArray* miniToF = mMiniArrays[MpdMiniArrays::BTofPidTraits];
    // Create EMC matching information
    TClonesArray* miniEmc = mMiniArrays[MpdMiniArrays::BECalPidTraits];

    miniTracksReco->Delete();
    miniTracksRecoCovMatrices->Delete();
    miniToF->Delete();
    miniEmc->Delete();

    // Get global tracks from event
    TClonesArray* glTracks = (TClonesArray*) fEvents->GetGlobalTracks();

    // Reconstructed primary vertex in event
    MpdVertex* vtx = (MpdVertex*) fVertices->UncheckedAt(0);

    // Retrieve primary track indices in the fTpcTracks array that were used
    // for the primary vertex reconstruction
    TArrayI* ind = vtx->GetIndices();

    vector< Int_t > indices;
    for (Int_t iEle = 0; iEle < ind->GetSize(); iEle++)
        indices.push_back(ind->At(iEle));

    // Fill global and primary track information
    for (Int_t iTpcKalmanTrack = 0; iTpcKalmanTrack < fTpcTracks->GetEntriesFast(); iTpcKalmanTrack++) {
        // Retrieve i-th TpcKalmanTrack
        MpdTpcKalmanTrack* tpcTrack = (MpdTpcKalmanTrack*) fTpcTracks->UncheckedAt(iTpcKalmanTrack);
        // Skip non-existing tracks
        if (!tpcTrack)
            continue;

        // Create miniTrack (TPC) + BTofPidTraits (TOF) + BECALPidTraits (EMC) + cov. matrix for each TpcKalmanTrack
        MpdMiniTrack* miniTrack = new ((*miniTracksReco)[miniTracksReco->GetEntriesFast()]) MpdMiniTrack();
        MpdMiniBTofPidTraits* bTofPidTraits = new ((*miniToF)[miniToF->GetEntriesFast()]) MpdMiniBTofPidTraits();
        MpdMiniBECalPidTraits* bEmcPidTraits = new ((*miniEmc)[miniEmc->GetEntriesFast()]) MpdMiniBECalPidTraits();
        MpdMiniTrackCovMatrix* miniTrackCovMatrix = new ((*miniTracksRecoCovMatrices)[miniTracksRecoCovMatrices->GetEntriesFast()]) MpdMiniTrackCovMatrix();

        // Find all Kalman tracks that were reconstructed from the parent MC track
        auto itr = mc2reco.find(tpcTrack->GetTrackID());
        if (itr != mc2reco.end())
            itr->second.push_back(miniTracksReco->GetEntriesFast() - 1);

        // Set track parameters
        miniTrack->setId(tpcTrack->GetTrackID());
        miniTrack->setChi2(tpcTrack->GetChi2());
        miniTrack->setNHits(tpcTrack->GetNofHits() * tpcTrack->Charge());

        // Fill track covariance matrix if needed
        if (fIsUseCovMatrix)
            fillCovMatrix(tpcTrack, miniTrackCovMatrix);

        // Let's find primary tracks from the whole set of reconstructed
        // tracks from the current event
        Bool_t isPrimary = kFALSE;

        for (auto it : indices) {
            if (TMath::Abs(it - iTpcKalmanTrack) == 0) {
                isPrimary = kTRUE;
                break;
            }
        }

        // If track is primary then fill primary momentum
        if (isPrimary)
            RefitToVp(miniTrack, iTpcKalmanTrack, vtx);
        else
            miniTrack->setPrimaryMomentum(TVector3(0., 0., 0.));

        // Doing a link to the corresponding global track to get more
        // parameters not available directly by the tpcTrack
        for (Int_t iGlobTrack = 0; iGlobTrack < glTracks->GetEntriesFast(); iGlobTrack++) {
            // Retrieve global track
            MpdTrack* glTrack = (MpdTrack*) glTracks->UncheckedAt(iGlobTrack);
            // Track must exist
            if (!glTrack) continue;

            // Global track must have corresponding Kalman track
            if (tpcTrack->GetTrackID() != glTrack->GetID()) continue;

            // Set nSigma and dE/dx info
            miniTrack->setNSigmaElectron(glTrack->GetNSigmaElectron());
            miniTrack->setNSigmaPion(glTrack->GetNSigmaPion());
            miniTrack->setNSigmaKaon(glTrack->GetNSigmaKaon());
            miniTrack->setNSigmaProton(glTrack->GetNSigmaProton());
            miniTrack->setDedx(glTrack->GetdEdXTPC());

            // Setting global track momentum at DCA to primary vertex
            TVector3 globMom(glTrack->GetPx(), glTrack->GetPy(), glTrack->GetPz());
            miniTrack->setGlobalMomentum(globMom.X(), globMom.Y(), globMom.Z());

            // Getting primary vertex the first primary vertex
            MpdVertex* vertex = (MpdVertex*) fVertices->UncheckedAt(0);

            // Get primary vertex position
            TVector3 primVertex(vertex->GetX(), vertex->GetY(), vertex->GetZ());
            TVector3 firstPoint(glTrack->GetFirstPointX(),
                    glTrack->GetFirstPointY(),
                    glTrack->GetFirstPointZ());

            // Physical helix instantiation
            MpdMiniPhysicalHelix helix(globMom, firstPoint,
                    mBField * kilogauss,
                    glTrack->GetCharge());
            double pathLength = helix.pathLength(primVertex);
            TVector3 dcaPosition = helix.at(pathLength);
            miniTrack->setOrigin(dcaPosition.X(),
                    dcaPosition.Y(),
                    dcaPosition.Z());
        } // for (Int_t iGlobTrack = 0; iGlobTrack < glTracks->GetEntriesFast(); iGlobTrack++)

        // Getting tof matching information ...
        Int_t idxMini = miniTracksReco->GetEntriesFast() - 1;
        DoTofMatching(iTpcKalmanTrack, idxMini, miniTrack, bTofPidTraits);

        // Getting ecal matching information ...
        DoEcalMathching(iTpcKalmanTrack, idxMini, miniTrack, bEmcPidTraits);


    } // for (Int_t iTpcKalmanTrack = 0; iTpcKalmanTrack < fTpcTracks->GetEntriesFast(); iTpcKalmanTrack++) {

    // Monte Carlo tracks
    TClonesArray* miniTracksSimu = mMiniArrays[MpdMiniArrays::McTrack];
    miniTracksSimu->Delete();

    // Fill McTracks if exist
    for (Int_t iSimuTrack = 0; iSimuTrack < fMCTracks->GetEntriesFast(); iSimuTrack++) {
        // Retrieve MCTrack
        MpdMCTrack* mcTrack = (MpdMCTrack*) fMCTracks->UncheckedAt(iSimuTrack);
        // Skip non-existing tracks
        if (!mcTrack) continue;

        // One has to store only tracks from the generator level
        // with pointers to reconstructed global (primary) tracks
        if (mcTrack->GetMotherId() != -1) continue;

        // Create new MiniMcTrack
        MpdMiniMcTrack* miniTrack = new ((*miniTracksSimu)[miniTracksSimu->GetEntriesFast()]) MpdMiniMcTrack();
        if (!miniTrack) {
            std::cout << "[WARNING] MpdMiniDstFillTask::fillTracks - No miniTrack has been found"
                    << std::endl;
            continue;
        }

        // Set McTrack information
        miniTrack->setId(iSimuTrack);
        miniTrack->setPdgId(mcTrack->GetPdgCode());
        miniTrack->setPx(mcTrack->GetPx());
        miniTrack->setPy(mcTrack->GetPy());
        miniTrack->setPz(mcTrack->GetPz());
        miniTrack->setEnergy(mcTrack->GetEnergy());

        miniTrack->setGlobalTrackIds(mc2reco[iSimuTrack]);

        // Assume that there is no branch with GenTracks ...
        miniTrack->setX(-1.);
        miniTrack->setY(-1.);
        miniTrack->setZ(-1.);
        miniTrack->setT(-1.);

        if (fGenTracks)
            for (Int_t iGenTrack = 0; iGenTrack < fGenTracks->GetEntriesFast(); iGenTrack++) {
                MpdGenTrack* genTrack = (MpdGenTrack*) fGenTracks->UncheckedAt(iGenTrack);
                if (genTrack->GetIsUsed())
                    continue;

                Double_t absMomDiff = TMath::Abs(genTrack->GetMomentum().Mag() - mcTrack->GetP());
                if (absMomDiff < DBL_EPSILON) {
                    genTrack->SetIsUsed(kTRUE);

                    TLorentzVector spaceTime = genTrack->GetCoordinates();
                    miniTrack->setX(spaceTime.X());
                    miniTrack->setY(spaceTime.Y());
                    miniTrack->setZ(spaceTime.Z());
                    miniTrack->setT(spaceTime.T());
                }
            }

    } // for (Int_t iSimuTrack = 0; iSimuTrack < fMCTracks->GetEntriesFast(); iSimuTrack++)

    // Doing correspondence <<miniTrack --> miniMcTrack>> 
    // 1. Preparing reco2mc 
    map <UShort_t, Int_t> reco2mc;
    for (Int_t iMini = 0; iMini < miniTracksReco->GetEntriesFast(); iMini++)
        reco2mc[iMini] = -1;

    for (auto it : mc2reco) {
        vector <UShort_t> indicesMini = it.second;

        for (auto idx : indicesMini)
            reco2mc[idx] = it.first;
    }

    // 2. Setting <<miniTrack --> miniMcTrack>> info 
    for (Int_t iMini = 0; iMini < miniTracksReco->GetEntriesFast(); iMini++) {
        MpdMiniTrack* miniTrack = (MpdMiniTrack*) miniTracksReco->UncheckedAt(iMini);

        auto itr = reco2mc.find(iMini);
        if (itr != reco2mc.end())
            miniTrack->setMcTrackIds(itr->second);
    }
}

//_________________

void MpdMiniDstFillTask::fillBTofHits() {
    // Instantiate MpdMiniBTofHit array
    TClonesArray* miniToF = mMiniArrays[MpdMiniArrays::BTofHit];
    miniToF->Delete();

    // Loop over TOF hits
    for (Int_t iHit = 0; iHit < fTofHits->GetEntriesFast(); iHit++) {
        MpdTofHit* tofHit = (MpdTofHit*) fTofHits->UncheckedAt(iHit);

        if (!tofHit)
            continue;

        MpdMiniBTofHit* miniTofHit = new ((*miniToF)[miniToF->GetEntriesFast()]) MpdMiniBTofHit();
        miniTofHit->setId(tofHit->GetDetectorID());
        miniTofHit->setHitPositionXYZ(tofHit->GetX(), tofHit->GetY(), tofHit->GetZ());
        miniTofHit->setTOF(tofHit->GetTime());

        for (Int_t iMatch = 0; iMatch < fTofMatching->GetEntriesFast(); iMatch++) {

            // Retrieve TOF-matching information
            MpdTofMatchingData* dataMatch = (MpdTofMatchingData*) fTofMatching->UncheckedAt(iMatch);
            Int_t idx = dataMatch->GetTofHitIndex();

            if (idx == iHit) {
                miniTofHit->setBTofMatchFlag(kTRUE);
                break;
            }
        }
    }
}

//_________________

void MpdMiniDstFillTask::fillECalHits() {
    // Instantiate MpdMiniBECalHit array
    TClonesArray* miniEmc = mMiniArrays[MpdMiniArrays::BECalHit];
    miniEmc->Delete();

    // Loop over emc clusters ...
    if (fEmcClusters)
        for (Int_t iCluster = 0; iCluster < fEmcClusters->GetEntriesFast(); iCluster++) {
            MpdEmcClusterKI* cluster = (MpdEmcClusterKI*) fEmcClusters->UncheckedAt(iCluster);
            if (!cluster)
                continue;

            MpdMiniBECalHit* miniEcalHit = new ((*miniEmc)[miniEmc->GetEntriesFast()]) MpdMiniBECalHit();

            miniEcalHit->setEnergy(cluster->GetE());
            miniEcalHit->setTime(cluster->GetTime());
            miniEcalHit->setNumberOfTracks(cluster->GetNumberOfTracks());
            miniEcalHit->SetXYZ(cluster->GetX(), cluster->GetY(), cluster->GetZ());
            miniEcalHit->SetDPhi(cluster->GetDPhi());
            miniEcalHit->SetDz(cluster->GetDZ());

            if (cluster->GetTrackIndex() != -1)
                miniEcalHit->setBEcalMatchFlag(kTRUE);

            Int_t nDigits = cluster->GetMultiplicity();

            vector <Int_t> lightedCells;
            for (Int_t iDigi = 0; iDigi < nDigits; iDigi++) {
                Int_t cell = -1;
                Float_t e = -1.;
                cluster->GetDigitParams(iDigi, cell, e);
                lightedCells.push_back(cell);
            }

            miniEcalHit->setCellIds(lightedCells);
        }
}

void MpdMiniDstFillTask::RefitToVp(MpdMiniTrack* miniTrack, Int_t iTpcKalmanTrack, MpdVertex* vtx) {
    // Get primary tracks from event
    // Done by smooth tracks from primary vertex (update momentum and track length -
    // covariance matrix is not updated !!!)
    // Got from MpdKfPrimaryVertexFinder::Smooth()
    MpdKalmanHit hit;
    TMatrixD c(3, 3), xk(3, 1), ck0(5, 1);
    TMatrixD a(5, 3), b(5, 3);
    TVector3 vert;
    vtx->Position(vert);
    xk(0, 0) = vert.X();
    xk(1, 0) = vert.Y();
    xk(2, 0) = vert.Z();
    Double_t rad = vert.Pt();

    MpdKalmanTrack* track = (MpdKalmanTrack*) fTpcTracks->UncheckedAt(iTpcKalmanTrack);
    MpdKalmanTrack* trVert = new MpdKalmanTrack(*track);

    MpdKalmanTrack track1 = *track;
    track1.SetParamNew(*track1.GetParam());
    track1.SetPos(track1.GetPosNew());
    track1.ReSetWeight();
    track1.SetLength(0.);
    TMatrixD g = *track1.GetWeight(); // track weight matrix

    if (track->GetNode() == "") {
        hit.SetType(MpdKalmanHit::kFixedR);
        hit.SetPos(track->GetPos());
    } else {
        hit.SetType(MpdKalmanHit::kFixedP);
        TString detName = track->GetNode();
        if (track->GetUniqueID()) {
            // ITS
            detName = detName(16, detName.Length());
            detName += "#0";
        }
        MpdKalmanGeoScheme *geo = MpdKalmanFilter::Instance()->GetGeo();
        hit.SetDetectorID(geo->DetId(detName));

        // Find distance from the current track position to the last point (plane) -
        // to define direction (mainly for ITS)
        TVector3 pos = geo->GlobalPos(&hit);
        TVector3 norm = geo->Normal(&hit);
        Double_t v7[7] = {0.0};
        track1.SetNode("");
        MpdKalmanFilter::Instance()->SetGeantParamB(&track1, v7, 1);
        Double_t d = -(pos * norm); // Ax+By+Cz+D=0, A=nx, B=ny, C=nz
        TVector3 v3(v7[0], v7[1], v7[2]);
        d += v3 * norm;
        if (d < 0) {
            track1.SetDirection(MpdKalmanTrack::kOutward);
        }
    } // else

    MpdKalmanFilter::Instance()->PropagateToHit(&track1, &hit, kTRUE, kTRUE);

    //TMatrixD* par2 = track->GetParamNew();
    //cout << par2 << endl;
    ComputeAandB(xk, track, track1, a, b, ck0); // compute matrices of derivatives

    // W = (Bt*G*B)'
    TMatrixD tmp(g, TMatrixD::kMult, b);
    TMatrixD w(b, TMatrixD::kTransposeMult, tmp);
    w.Invert();

    TMatrixD m = *track1.GetParamNew();
    m -= ck0; // m-ck0

    // qk = W*Bt*G*(m-ck0-A*xk)
    TMatrixD tmp21(a, TMatrixD::kMult, xk);
    tmp21 *= -1;
    tmp21 += m; // m-ck0-A*xk
    TMatrixD tmp22(g, TMatrixD::kMult, tmp21);
    TMatrixD tmp23(b, TMatrixD::kTransposeMult, tmp22);
    TMatrixD qk(w, TMatrixD::kMult, tmp23);

    // Update momentum and last coordinate
    TMatrixD* parPointer = nullptr;

    if (track->GetParamNew())
        parPointer = track->GetParamNew();
    else
        parPointer = &m;

    TMatrixD par = *parPointer;
    for (Int_t i = 0; i < 3; ++i) par(i + 2, 0) = qk(i, 0);
    par(0, 0) = rad * vert.Phi();
    par(1, 0) = vert.Z();
    trVert->SetParam(par);
    trVert->SetPosNew(rad);

    Double_t pT = 1. / trVert->GetParam(4); // Signed pT
    Double_t phi = trVert->GetParam(2);
    Double_t theta = TMath::PiOver2() - trVert->GetParam(3);

    Double_t Px = TMath::Abs(pT) * TMath::Cos(phi);
    Double_t Py = TMath::Abs(pT) * TMath::Sin(phi);

    Double_t Pz = -1.;
    if (TMath::Sin(theta) != 0.) {
        Pz = TMath::Abs(pT) / TMath::Tan(theta);
    }

    miniTrack->setPrimaryMomentum(TVector3(Px, Py, Pz));
}
//_________________

void MpdMiniDstFillTask::ComputeAandB(TMatrixD &xk0, const MpdKalmanTrack *track,
        const MpdKalmanTrack &trackM,
        TMatrixD &a, TMatrixD &b, TMatrixD &ck0) {

    // Compute matrices of derivatives w.r.t. vertex coordinates and track momentum

    Double_t vert0[3], zero[3] = {0}, *vert = xk0.GetMatrixArray();
    for (Int_t i = 0; i < 3; ++i) vert0[i] = vert[i];

    MpdKalmanTrack trackk = *track;
    trackk.SetPos(trackk.GetPosNew());
    //trackk.GetParam()->Print();
    // Propagate track to PCA w.r.t. point xk0
    MpdKalmanFilter::Instance()->FindPca(&trackk, vert0);
    //MpdKalmanFilter::Instance()->FindPca(&trackk,zero); // just for test
    //std::cout << trackk.GetPosNew() << std::endl;
    trackk.SetParam(*trackk.GetParamNew());
    //trackk.GetParam()->Print();

    // Put track at xk0
    Double_t r = TMath::Sqrt(vert0[0] * vert0[0] + vert0[1] * vert0[1]);
    Double_t phi = trackk.GetParamNew(2); // track Phi
    if (r > 1.e-7) phi = TMath::ATan2(vert0[1], vert0[0]);
    trackk.SetPos(r);
    trackk.SetParam(0, r * phi);
    trackk.SetParam(1, vert0[2]);
    trackk.SetNode("");
    MpdKalmanTrack track0 = trackk;

    // Propagate track to chosen radius
    MpdKalmanHit hit;
    //hit.SetR(35.);
    //hit = *(MpdKalmanHitR*)track->GetTrHits()->Last();
    if (track->GetNode() == "") {
        hit.SetType(MpdKalmanHit::kFixedR);
        //hit.SetR(35.);
        //hit = *(MpdKalmanHitR*)track->GetTrHits()->Last();
        hit.SetPos(track->GetPos());
        MpdKalmanFilter::Instance()->PropagateParamR(&trackk, &hit, kFALSE);
        //trackk.GetParamNew()->Print();
        Proxim(trackM, trackk);
    } else {
        hit.SetType(MpdKalmanHit::kFixedP);
        TString detName = track->GetNode();
        if (track->GetUniqueID()) {
            // ITS
            detName = detName(16, detName.Length());
            detName += "#0";
        }
        MpdKalmanGeoScheme *geo = MpdKalmanFilter::Instance()->GetGeo();
        hit.SetDetectorID(geo->DetId(detName));
        // Find distance from the current track position to the last point (plane) -
        // to define direction (mainly for ITS)
        TVector3 pos = geo->GlobalPos(&hit);
        TVector3 norm = geo->Normal(&hit);
        Double_t v7[7] = {0.0};
        MpdKalmanFilter::Instance()->SetGeantParamB(&trackk, v7, 1);
        Double_t d = -(pos * norm); // Ax+By+Cz+D=0, A=nx, B=ny, C=nz
        TVector3 v3(v7[0], v7[1], v7[2]);
        d += v3 * norm;
        if (d < 0) trackk.SetDirection(MpdKalmanTrack::kOutward);
        MpdKalmanFilter::Instance()->PropagateParamP(&trackk, &hit, kFALSE, kTRUE);
        track0.SetDirection(trackk.GetDirection());
    }

    //Double_t shift = 0.01; // 100 um coordinate shift
    Double_t shift = 0.1; // 1 mm coordinate shift
    for (Int_t i = 0; i < 3; ++i) {
        MpdKalmanTrack track1 = track0;
        vert0[i] += shift;
        if (i > 0) vert0[i - 1] -= shift;
        r = TMath::Sqrt(vert0[0] * vert0[0] + vert0[1] * vert0[1]);
        if (r > 1.e-7) phi = TMath::ATan2(vert0[1], vert0[0]);
        else phi = track0.GetParamNew(2); // track Phi
        track1.SetPos(r);
        track1.SetParam(0, r * phi);
        track1.SetParam(1, vert0[2]);
        if (track->GetNode() == "") {
            MpdKalmanFilter::Instance()->PropagateParamR(&track1, &hit, kFALSE);
            Proxim(trackk, track1);
            //Proxim(track1,trackk);
        } else MpdKalmanFilter::Instance()->PropagateParamP(&track1, &hit, kFALSE, kTRUE);
        // Derivatives
        for (Int_t j = 0; j < 5; ++j) {
            a(j, i) = (track1.GetParamNew(j) - trackk.GetParamNew(j)) / shift;
        }
    }

    for (Int_t i = 0; i < 3; ++i) {
        MpdKalmanTrack track1 = track0;
        Int_t j = i + 2;
        shift = (*track->GetCovariance())(j, j);
        shift = TMath::Sqrt(shift);
        if (j == 4) shift *= TMath::Sign(1., -track0.GetParamNew(j)); // 1/p
        track1.SetParam(j, track0.GetParamNew(j) + shift);
        //if (j == 2 && track1.GetParamNew(j)*TMath::Sign(1.,track1.GetParamNew(j)) > TMath::Pi())
        //track1.SetParam(j,track0.GetParamNew(j)-shift);
        if (track->GetNode() == "") {
            MpdKalmanFilter::Instance()->PropagateParamR(&track1, &hit, kFALSE);
            Proxim(trackk, track1);
            //Proxim(track1,trackk);
        } else MpdKalmanFilter::Instance()->PropagateParamP(&track1, &hit, kFALSE, kTRUE);
        // Derivatives
        for (Int_t k = 0; k < 5; ++k) {
            b(k, i) = (track1.GetParamNew(k) - trackk.GetParamNew(k)) / shift;
        }
    }

    TMatrixD qk0(3, 1);
    for (Int_t i = 0; i < 3; ++i) qk0(i, 0) = track0.GetParamNew(i + 2);
    //qk0.Print();
    ck0 = *trackk.GetParamNew();
    ck0 -= TMatrixD(a, TMatrixD::kMult, xk0);
    ck0 -= TMatrixD(b, TMatrixD::kMult, qk0);
}

//________________

void MpdMiniDstFillTask::Proxim(const MpdKalmanTrack &track0, MpdKalmanTrack &track) {

    if (track0.GetType() != MpdKalmanTrack::kBarrel) {
        std::cout << " !!! Implemented only for kBarrel tracks !!!" << std::endl;
        exit(0);
    }

    Double_t tmp = track.GetParamNew(0);
    Double_t phi0 = track0.GetParamNew(0) / track0.GetPosNew();
    Double_t phi = track.GetParamNew(0) / track.GetPosNew();
    phi = MpdKalmanFilter::Instance()->Proxim(phi0, phi);
    TMatrixD *par = track.GetParamNew();
    (*par)(0, 0) = phi * track.GetPosNew();
    phi0 = track0.GetParamNew(2);
    phi = track.GetParamNew(2);
    phi = MpdKalmanFilter::Instance()->Proxim(phi0, phi);
    (*par)(2, 0) = phi;
    track.SetParamNew(*par);
}

void MpdMiniDstFillTask::fillCovMatrix(MpdTpcKalmanTrack* tpcTrack, MpdMiniTrackCovMatrix* miniTrackCovMatrix) {
    miniTrackCovMatrix->setPti(1. / tpcTrack->GetParam(4));

    const Double_t* matrixElements = tpcTrack->GetCovariance()->GetMatrixArray();
    const Int_t nElements = tpcTrack->GetCovariance()->GetNoElements();

    vector <Float_t> sigmas;
    vector <Float_t> correlations;

    Int_t shift = Int_t(TMath::Sqrt(nElements));

    for (Int_t iEle = 0; iEle < nElements; iEle++) {
        if (iEle % Int_t(TMath::Sqrt(nElements) + 1) == 0) {
            sigmas.push_back(matrixElements[iEle]);

            for (Int_t jEle = 1; jEle < shift; jEle++) {
                correlations.push_back(matrixElements[iEle + jEle]);
            }

            shift--;
        }
    }

    miniTrackCovMatrix->setSigmas(sigmas);
    miniTrackCovMatrix->setCorrelations(correlations);
}

void MpdMiniDstFillTask::DoTofMatching(Int_t kalmanIdx, Int_t miniIdx, MpdMiniTrack* track, MpdMiniBTofPidTraits* pid) {
    for (Int_t iMatch = 0; iMatch < fTofMatching->GetEntriesFast(); iMatch++) {

        // Retrieve TOF-matching information
        MpdTofMatchingData* dataMatch = (MpdTofMatchingData*) fTofMatching->UncheckedAt(iMatch);

        // TOF matching information should exist
        if (!dataMatch || (dataMatch->GetKFTrackIndex() != kalmanIdx))
            continue;

        track->setBTofPidTraitsIndex(0); // It means a presense of matching with tof

        pid->setTrackIndex(miniIdx);
        pid->setHitIndex(dataMatch->GetTofHitIndex());
        pid->setBeta(dataMatch->GetBeta());
        pid->SetM2(dataMatch->GetMass2());
        pid->SetLength(dataMatch->GetTrackLength());

        break;
    }
}

void MpdMiniDstFillTask::DoEcalMathching(Int_t kalmanIdx, Int_t miniIdx, MpdMiniTrack* track, MpdMiniBECalPidTraits* pid) {
    if (fEmcClusters)
        for (Int_t iCluster = 0; iCluster < fEmcClusters->GetEntriesFast(); iCluster++) {

            MpdEmcClusterKI* cluster = (MpdEmcClusterKI*) fEmcClusters->UncheckedAt(iCluster);

            if (!cluster || cluster->GetTrackIndex() != kalmanIdx)
                continue;

            track->setBECalPidTraitsIndex(0); // It means a presense of matching with emc
            pid->setTrackIndex(miniIdx);

            break;
        }
}
