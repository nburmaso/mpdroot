#include "MpdTpcMisalignment.h"

#include <TRotation.h>
#include <TVector3.h>

#ifdef _TEST_
#include <set>
#endif

#ifdef _WRITETXTDATA_
#include <cstdio>
#include <vector>
#include <set>
#include <unordered_set>
#include <TFile.h>
#include <TTree.h>
#endif

MpdTpcMisalignment::MpdTpcMisalignment()
    : FairTask("TPC Hits Misalignment"), MpdTpcAlignmentParams(), fPersistence(kFALSE), fSecGeo(NULL)
{
}

MpdTpcMisalignment::~MpdTpcMisalignment()
{
}

InitStatus MpdTpcMisalignment::Init()
{
    std::cout << "MpdTpcMisalignment::Init started" << std::endl;
    
    Int_t status = ProcessParamsFile();
    if (status != 0)
    {
        Error("MpdTpcMisalignment::Init","Alignment params read error %d in MpdTpcAlignmentParams::ProcessParamsFile", status);
        return kFATAL;
    }
    
#ifdef _TEST_
    std::cout << "MpdTpcAlignment::Init read param file err " << status << std::endl;
    for (Int_t iSect = 0; iSect < fNumofSectors; ++iSect)
    {
        std::cout << "Sector " << iSect << std::endl;
        fTpcSectorShift[iSect].Print();
        fTpcSectorRot[iSect].Print();
    }
#endif
    
    FairRootManager* frman = FairRootManager::Instance();
    if (frman == 0) {
        Error("MpdTpcMisalignment::Init","RootManager not found!");
        return kFATAL;
    }
    
    fHits = (TClonesArray*) frman->GetObject("TpcRecPoint");
    if (fHits == 0) {
        Error("MpdTpcMisalignment::Init","TPC Hits not found");
        return kFATAL;
    }
    else {
        FairRootManager::Instance()->Register("TpcRecPoint", "TpcHits", fHits, kTRUE);
    }
    
    fSecGeo = MpdTpcSectorGeo::Instance();
    if (fSecGeo == NULL) {
        Error("MpdTpcMisalignment::Init","TPC Sector Geo init error");
        return kFATAL;
    }

#ifdef _TEST_    
    fHitsMisalign = new TClonesArray("MpdTpcHit"); 
    frman->Register("TpcRecPointMisalign", "TpcAlign", fHitsMisalign, fPersistence);
#endif
    
    std::cout << "MpdTpcMisalignment::Init finished OK" << std::endl;
    return kSUCCESS;
}

void MpdTpcMisalignment::Exec(Option_t* opt)
{
#ifdef _WRITETXTDATA_
    std::FILE * origTxt = std::fopen("hits_orig.txt", "w");
    std::FILE * misalignTxt = std::fopen("hits_misalign.txt", "w");
    
    std::fprintf(origTxt, "x\ty\tz\r\n");
    std::fprintf(misalignTxt, "x\ty\tz\r\n");
    
    Int_t n_tracks = 0;
    Int_t n_pts = 0;
    std::vector<Double_t> pts_orig_x;
    std::vector<Double_t> pts_orig_y;
    std::vector<Double_t> pts_orig_z;
    std::vector<Double_t> pts_misalign_x;
    std::vector<Double_t> pts_misalign_y;
    std::vector<Double_t> pts_misalign_z;
    std::vector<Int_t> pts_track;

    TFile * out_file = TFile::Open("hits_misalign_and_orig.root","RECREATE");
    TTree * out_tree = new TTree("pts_misalign_and_orig", "TPC hits misalign and original");
    out_tree->Branch("n_pts", &n_pts);
    out_tree->Branch("pts_orig_x", "std::vector<Double_t>", &pts_orig_x);
    out_tree->Branch("pts_orig_y", "std::vector<Double_t>", &pts_orig_y);
    out_tree->Branch("pts_orig_z", "std::vector<Double_t>", &pts_orig_z);
    out_tree->Branch("pts_misalign_x", "std::vector<Double_t>", &pts_misalign_x);
    out_tree->Branch("pts_misalign_y", "std::vector<Double_t>", &pts_misalign_y);
    out_tree->Branch("pts_misalign_z", "std::vector<Double_t>", &pts_misalign_z);
    out_tree->Branch("n_tracks", &n_tracks);
    out_tree->Branch("pts_track", "std::vector<Int_t>", &pts_track);
    
    std::unordered_set<Int_t> tr_rename;
#endif
    
    std::cout << "MpdTpcMisalignment::Exec started" << std::endl;
    
#ifdef _TEST_
    std::set<Int_t> sectsTest;
    fHitsMisalign->Delete();
#endif
    
    Int_t nHits = fHits->GetEntriesFast();
    std::cout << "MpdTpcMisalignment::Exec Total number of Hits: " << nHits << std::endl;
    for (Int_t i = 0; i < nHits; ++i)
    {
        MpdTpcHit * hit = (MpdTpcHit*)fHits->UncheckedAt(i);
        
        TVector3 globPos;
        hit->Position(globPos);
        std::pair<TVector3, Int_t> locPos = GlobalToLocal(globPos);
        
#ifdef _WRITETXTDATA_
        std::fprintf(origTxt, "%E\t%E\t%E\r\n", globPos.X(), globPos.Y(), globPos.Z());
        
        pts_orig_x.push_back(globPos.X());
        pts_orig_y.push_back(globPos.Y());
        pts_orig_z.push_back(globPos.Z());
        pts_track.push_back(hit->GetTrackID());
        tr_rename.insert(hit->GetTrackID());
#endif
        
#ifdef _TEST_
        TVector3 locPosTest(locPos.first), globPosTest(globPos);
#endif
        
        Int_t iSect = locPos.second;
        
        TRotation rot;
        rot.RotateX(fTpcSectorRot[iSect].X() * TMath::DegToRad());
        rot.RotateY(fTpcSectorRot[iSect].Y() * TMath::DegToRad());
        rot.RotateZ(fTpcSectorRot[iSect].Z() * TMath::DegToRad());
        locPos.first *= rot;
        
        locPos.first.SetX(locPos.first.X() + fTpcSectorShift[iSect].X());
        locPos.first.SetY(locPos.first.Y() + fTpcSectorShift[iSect].Y());
        locPos.first.SetZ(locPos.first.Z() + fTpcSectorShift[iSect].Z());
        
        globPos = LocalToGlobal(locPos);
        
#ifdef _WRITETXTDATA_
        std::fprintf(misalignTxt, "%E\t%E\t%E\r\n", globPos.X(), globPos.Y(), globPos.Z());
        
        pts_misalign_x.push_back(globPos.X());
        pts_misalign_y.push_back(globPos.Y());
        pts_misalign_z.push_back(globPos.Z());
#endif
        
        TVector3 locPosSectorGeo;
        fSecGeo->Global2Local(globPos, locPosSectorGeo);

#ifdef _TEST_        
        Int_t iHitMisalign = fHitsMisalign->GetEntriesFast();
        MpdTpcHit * hitMisalign = new ((*fHitsMisalign)[iHitMisalign]) MpdTpcHit(*hit);
        hitMisalign->SetPosition(globPos);
        hitMisalign->SetLocalPosition(locPosSectorGeo);
#else
        hit->SetPosition(globPos);
        hit->SetLocalPosition(locPosSectorGeo);
#endif
        
#ifdef _TEST_
        if (sectsTest.find(iSect) == sectsTest.end())
        {
            std::cout << "!! TEST " << iSect << std::endl;
            std::cout << "--- Glob Pos ---" << std::endl;
            globPosTest.Print();
            globPos.Print();
            std::cout << "angle " << TMath::ATan2(globPos.X(), globPos.Y()) * TMath::RadToDeg() << " deg" << std::endl;
            std::cout << "--- Loc Pos ---" << std::endl;
            locPosTest.Print();
            locPos.first.Print();
            std::cout << "------" << std::endl;
        }
        sectsTest.insert(iSect);
#endif
    }
    
#ifdef _WRITETXTDATA_
    std::fclose(origTxt);
    std::fclose(misalignTxt);
    
    for (Int_t i = 0; i < nHits; ++i)
        pts_track[i] = std::distance(tr_rename.begin(), tr_rename.find(pts_track[i]));
    n_pts = nHits;
    n_tracks = tr_rename.size();
    
    out_tree->Fill();
    out_file->WriteTObject(out_tree);
    out_file->Close();
#endif
    
    std::cout << "MpdTpcMisalignment::Exec finished OK" << std::endl;
}

void MpdTpcMisalignment::SetPersistence(Bool_t val)
{
    fPersistence = val;
}

ClassImp(MpdTpcMisalignment)
