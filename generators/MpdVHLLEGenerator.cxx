#include "MpdVHLLEGenerator.h"

MpdVHLLEGenerator::MpdVHLLEGenerator()
: FairGenerator(),
fInputFile(NULL),
fFileName("") ,
fFreezout(NULL){
}

MpdVHLLEGenerator::MpdVHLLEGenerator(TString fileName, Bool_t isCascade)
: FairGenerator(),
fInputFile(NULL),
fFileName(fileName),
fFreezout(NULL){
    //  fFileName = fileName;
    cout << "-I MpdVHLLEGenerator: Opening input file " << fFileName << endl;
    fInputFile = new TFile(fFileName.Data());
    if (!fInputFile) {
        Fatal("MpdVHLLEGenerator", "Cannot open input file.");
        exit(1);
    }

    SetCascade(isCascade);
    cout << "ACTIVATED BRANCH IS: " << fBranch << endl;

    fDstTree = new TChain(fBranch.Data());
    fDstTree->Add(fFileName);

    fDstTree->SetBranchAddress("px", fPx);
    fDstTree->SetBranchAddress("py", fPy);
    fDstTree->SetBranchAddress("pz", fPz);
    fDstTree->SetBranchAddress("x", fX);
    fDstTree->SetBranchAddress("y", fY);
    fDstTree->SetBranchAddress("z", fZ);
    fDstTree->SetBranchAddress("E", fE);
    fDstTree->SetBranchAddress("t", fT);
    fDstTree->SetBranchAddress("npart", &fNpart);
    fDstTree->SetBranchAddress("id", fPID);

    fEventNumber = 0;
    MpdFreezoutGenerator *freezgen = MpdFreezoutGenerator::Instance();
    fFreezout = freezgen->GetArray();
}

MpdVHLLEGenerator::~MpdVHLLEGenerator() {
    delete fInputFile;
    delete fDstTree;
}

Bool_t MpdVHLLEGenerator::ReadEvent(FairPrimaryGenerator* primGen) {

    // ---> Check for input file
    if (!fInputFile) {
        cout << "-E MpdVHLLEGenerator: Input file not open! " << endl;
        return kFALSE;
    }

    // ---> Check for primary generator
    if (!primGen) {
        cout << "-E- MpdVHLLEGenerator::ReadEvent: "
                << "No PrimaryGenerator!" << endl;
        return kFALSE;
    }

    fDstTree->GetEntry(fEventNumber);
    cout << "-I MpdVHLLEGenerator: Event " << fEventNumber << " Multiplicity " << fNpart << endl;

    FairMCEventHeader* event = primGen->GetEvent();
    if (event && (!event->IsSet())) {
        event->SetEventID(fEventNumber);
        // event->SetB(b);
        event->MarkSet(kTRUE);
    }
    fFreezout->Clear();
    for (Int_t iTrack = 0; iTrack < fNpart; iTrack++) {
        primGen->AddTrack(fPID[iTrack], fPx[iTrack], fPy[iTrack], fPz[iTrack], fX[iTrack], fY[iTrack], fZ[iTrack]);
        TLorentzVector *freezpos = (TLorentzVector*)fFreezout->ConstructedAt(iTrack);
        freezpos->SetXYZT(fX[iTrack],fY[iTrack],fZ[iTrack],fT[iTrack]);
        // cout << iTrack << " " << fPID[iTrack] << " " <<
        // fPx[iTrack] << " " << fPy[iTrack] << " " << fPz[iTrack] << " " <<
        // fX[iTrack] << " " << fY[iTrack] << " " << fZ[iTrack] << endl;
    }

    fEventNumber++;

    return kTRUE;
}
ClassImp(MpdVHLLEGenerator);

