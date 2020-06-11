#include "MpdMcDstGenerator.h"

TClonesArray** McDst::mcArrays = nullptr;

MpdMcDstGenerator::MpdMcDstGenerator() :
fEventNumber(0),
myReader(nullptr),
fGenTracks(nullptr) {

}

MpdMcDstGenerator::MpdMcDstGenerator(TString fileName) :
fEventNumber(0),
myReader(nullptr),
fGenTracks(nullptr) {
    cout << "-I MpdMcDstGenerator: Opening input file " << fileName << endl;

    myReader = new McDstReader(fileName.Data());
    myReader->Init();

    myReader->setStatus("*", 0);
    myReader->setStatus("Event", 1);
    myReader->setStatus("Particle", 1);

    if (!myReader->chain()) {
        cout << "No chain has been found." << endl;
        return;
    }

    Long64_t eventsInTree = myReader->tree()->GetEntries();
    cout << "eventsInTree: " << eventsInTree << endl;

    Long64_t events2read = myReader->chain()->GetEntries();
    cout << "Number of events to read: " << events2read << endl;

    // Create structure to store generator tracks ...
    FairRunSim* runSim = FairRunSim::Instance();
    MpdGenTrackTask* tracksInfo = new MpdGenTrackTask();
    runSim->AddTask(tracksInfo);

    fGenTracks = tracksInfo->GetTracksInfo();
}

Bool_t MpdMcDstGenerator::ReadEvent(FairPrimaryGenerator* primGen) {
    fGenTracks->Delete(); // Clear array with gen. tracks ...
    myReader->chain()->GetEntry(fEventNumber);

    McDst* dst = myReader->mcDst();

    McEvent* event = dst->event();

    if (!event) {
        cout << "Something went wrong when getting event, stopping here ..." << endl;
        return kFALSE;
    }

    FairMCEventHeader* header = primGen->GetEvent();
    if (header && (!header->IsSet())) {
        header->SetEventID(event->eventNr());
        header->SetNPrim(event->npart());
        header->SetB(event->impact());
        header->MarkSet(kTRUE);
    }

    UInt_t nTracks = dst->numberOfParticles();

    for (Int_t iTrack = 0; iTrack < nTracks; iTrack++) {
        McParticle* particle = dst->particle(iTrack);

        if (!particle)
            continue;

        primGen->AddTrack(particle->pdg(), particle->px(), particle->py(), particle->pz(), 0., 0., 0.);

        // Push a new track to the array of gen. tracks ...
        MpdGenTrack* track = new ((*fGenTracks)[fGenTracks->GetEntriesFast()]) MpdGenTrack();
        track->SetXYZT(particle->x(), particle->y(), particle->z(), particle->t());
        track->SetPxyz(particle->px(), particle->py(), particle->pz()); // Do we need them? FIXME
        track->SetPdg(particle->pdg());
        track->SetE(particle->e());
    }

    fEventNumber++;
    return kTRUE;
}

MpdMcDstGenerator::~MpdMcDstGenerator() {
    myReader->Finish();

    delete fGenTracks;
}
ClassImp(MpdMcDstGenerator);