#include "MpdMiniDstFillTask.h"

// Initialize values for static data members of MpdMiniArrays
#include <MpdMiniArrays.cxx>

MpdMiniDstFillTask::MpdMiniDstFillTask() {

}

MpdMiniDstFillTask::MpdMiniDstFillTask(TString name) :
fEvents(nullptr),
mMiniDst(new MpdMiniDst()),
mBField(.5), // mag. field in T
mOutputFile(nullptr),
mTTree(nullptr),
mSplit(99),
mCompression(9),
mBufferSize(65536 * 4),
mMiniArrays(nullptr),
fIsUseTpc(kTRUE),
fIsUseToF(kTRUE),
fIsUseEcal(kFALSE),
fIsUseMcTracks(kTRUE) {   
    mOutputFileName = name.ReplaceAll(".root", ".MiniDst.root");
    
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

    // Get information on tracks in TPC (Right now we believe these tracks only!!!!)
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

    if (fVerbose != 0)
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

    // Fill McTrack info ...
    TClonesArray* miniMcEventHeaders = mMiniArrays[MpdMiniArrays::McEvent];
    miniMcEventHeaders->Delete();

    Int_t nPrim = fEventHeaders->GetNPrim();
    Int_t nColl = -1;
    Double_t rp = -1;
    Double_t b = fEventHeaders->GetB();
    UInt_t runId = fEventHeaders->GetRunID();
    UInt_t eventId = fEventHeaders->GetEventID();
    Double_t time = fEventHeaders->GetT();
    TVector3 vtx;
    fEventHeaders->GetVertex(vtx);

    new ((*miniMcEventHeaders)[miniMcEventHeaders->GetEntriesFast()])
            MpdMiniMcEvent(runId, eventId, rp, b, nPrim, nColl, vtx, time);
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
        miniTrack->setT(-1.);
        miniTrack->setParentIndex(mcTrack->GetMotherId());
    }

    // Reconstructed tracks
    TClonesArray* miniTracksReco = mMiniArrays[MpdMiniArrays::Track];
    TClonesArray* miniTracksRecoCovMatrices = mMiniArrays[MpdMiniArrays::TrackCovMatrix];

    miniTracksReco->Delete();
    miniTracksRecoCovMatrices->Delete();

    // Get global tracks from event
    TClonesArray* glTracks = (TClonesArray*) fEvents->GetGlobalTracks();

    // Reconstructed primary vertex in event ...
    MpdVertex* vtx = (MpdVertex*) fVertices->UncheckedAt(0);

    // Indices of primary tracks in the fTpcTracks array ...
    TArrayI* ind = vtx->GetIndices();

    vector <Int_t> indices;
    for (Int_t iEle = 0; iEle < ind->GetSize(); iEle++)
        indices.push_back(ind->At(iEle));

    for (Int_t iTpcKalmanTrack = 0; iTpcKalmanTrack < fTpcTracks->GetEntriesFast(); iTpcKalmanTrack++) {
        MpdTpcKalmanTrack* tpcTrack = (MpdTpcKalmanTrack*) fTpcTracks->UncheckedAt(iTpcKalmanTrack);

        // Create miniTrack for each TpcKalmanTrack ...
        MpdMiniTrack* miniTrack = new ((*miniTracksReco)[miniTracksReco->GetEntriesFast()]) MpdMiniTrack();
        miniTrack->setId(tpcTrack->GetTrackID());
        miniTrack->setChi2(tpcTrack->GetChi2());
        miniTrack->setNHits(tpcTrack->GetNofHits());

        // Fill covariance matrix in a limited way ...
        MpdMiniTrackCovMatrix* miniTrackCovMatrix = new ((*miniTracksRecoCovMatrices)[miniTracksRecoCovMatrices->GetEntriesFast()]) MpdMiniTrackCovMatrix();
        miniTrackCovMatrix->setPti(1. / tpcTrack->GetParam(4));

        const Double_t* matrixElements = tpcTrack->GetCovariance()->GetMatrixArray();
        const Int_t nElements = tpcTrack->GetCovariance()->GetNoElements();

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

        // Let's find primary tracks from the whole sets of reconstructed tracks per current event ...
        Bool_t isPrimary = kFALSE;

        for (auto it : indices) {
            if (TMath::Abs(it - iTpcKalmanTrack) == 0) {
                isPrimary = kTRUE;
                break;
            }
        }

        // Primary track ...
        if (isPrimary) {
            // Get primary tracks from event ...
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

            MpdTpcKalmanTrack* track = (MpdTpcKalmanTrack*) fTpcTracks->UncheckedAt(iTpcKalmanTrack);
            MpdTpcKalmanTrack* trVert = new MpdTpcKalmanTrack(*track);

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
                if (d < 0) track1.SetDirection(MpdKalmanTrack::kOutward);
            }

            MpdKalmanFilter::Instance()->PropagateToHit(&track1, &hit, kTRUE, kTRUE);

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

            // Update momentum and last coordinates
            TMatrixD par = *track->GetParamNew();
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
            if (TMath::Sin(theta) != 0.)
                Pz = TMath::Abs(pT) / TMath::Tan(theta);

            miniTrack->setPrimaryMomentum(TVector3(Px, Py, Pz));
        }// Global track ... 
        else
            miniTrack->setPrimaryMomentum(TVector3(-100., -100., -100.));

        // Doing a link to the corresponding glob. track to get more params not available directly by the tpcTrack ...
        for (Int_t iGlobTrack = 0; iGlobTrack < glTracks->GetEntriesFast(); iGlobTrack++) {
            MpdTrack* glTrack = (MpdTrack*) glTracks->UncheckedAt(iGlobTrack);

            if (tpcTrack->GetTrackID() != glTrack->GetID())
                continue;

            // Set ToF hit index if matched ...
            miniTrack->setBTofPidTraitsIndex(glTrack->GetTofHitIndex());

            // Set Nsigma and dE / dx info ...
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
        }
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

void MpdMiniDstFillTask::ComputeAandB(TMatrixD &xk0, const MpdKalmanTrack *track,
        const MpdKalmanTrack &trackM,
        TMatrixD &a, TMatrixD &b, TMatrixD &ck0) {
    /// Compute matrices of derivatives w.r.t. vertex coordinates and track momentum

    Double_t vert0[3], zero[3] = {0}, *vert = xk0.GetMatrixArray();
    for (Int_t i = 0; i < 3; ++i) vert0[i] = vert[i];

    MpdKalmanTrack trackk = *track;
    trackk.SetPos(trackk.GetPosNew());
    //trackk.GetParam()->Print();
    // Propagate track to PCA w.r.t. point xk0
    MpdKalmanFilter::Instance()->FindPca(&trackk, vert0);
    //MpdKalmanFilter::Instance()->FindPca(&trackk,zero); // just for test
    //cout << trackk.GetPosNew() << endl;
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

void MpdMiniDstFillTask::Proxim(const MpdKalmanTrack &track0, MpdKalmanTrack &track) {
    if (track0.GetType() != MpdKalmanTrack::kBarrel) {
        cout << " !!! Implemented only for kBarrel tracks !!!" << endl;
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