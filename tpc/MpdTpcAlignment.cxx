#include "MpdTpcAlignment.h"

#include <TVector3.h>

#ifdef _TEST_
#include <set>
#endif

MpdTpcAlignment::MpdTpcAlignment()
    : FairTask("TPC Hits Alignment"), MpdTpcAlignmentParams(), fPersistence(kFALSE), fSecGeo(NULL)
{
}

MpdTpcAlignment::~MpdTpcAlignment()
{
}

InitStatus MpdTpcAlignment::Init()
{
    std::cout << "MpdTpcAlignment::Init started" << std::endl;
    
    Int_t status = ProcessParamsFile();
    if (status != 0)
    {
        Error("MpdTpcAlignment::Init","Alignment params read error %d in MpdTpcAlignmentParams::ProcessParamsFile", status);
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
        Error("MpdTpcAlignment::Init","RootManager not found!");
        return kFATAL;
    }
    
#ifdef _TEST_
    fHitsMisalign = (TClonesArray*) frman->GetObject("TpcRecPointMisalign");
#else
    fHitsMisalign = (TClonesArray*) frman->GetObject("TpcRecPoint");
    if (fHitsMisalign == 0) {
        Error("MpdTpcAlignment::Init","TPC Misalign Hits not found");
        return kERROR;
    }
#endif
    
    fSecGeo = MpdTpcSectorGeo::Instance();
    if (fSecGeo == NULL) {
        Error("MpdTpcMisalignment::Init","TPC Sector Geo init error");
        return kFATAL;
    }
    
#ifdef _TEST_
    fHits = (TClonesArray*) frman->GetObject("TpcRecPoint");
    if (fHits == 0) {
        Error("MpdTpcAlignment::Init","TPC Hits not found");
        return kFATAL;
    }
    
    fHitsAlign = new TClonesArray("MpdTpcHit"); 
    frman->Register("TpcRecPointAlign", "TpcAlign", fHitsAlign, fPersistence);
#endif
    
    std::cout << "MpdTpcAlignment::Init finished OK" << std::endl;
    return kSUCCESS;
}

void MpdTpcAlignment::Exec(Option_t* opt)
{
    std::cout << "MpdTpcAlignment::Exec started" << std::endl;
    
#ifdef _TEST_
    std::set<Int_t> sectsTest;
    fHitsAlign->Delete();
#endif
    
    Int_t nHits = fHitsMisalign->GetEntriesFast();
    std::cout << "MpdTpcAlignment::Exec Total number of Misalign Hits: " << nHits << std::endl;
    for (Int_t i = 0; i < nHits; ++i)
    {
        MpdTpcHit * hitMisalign = (MpdTpcHit*)fHitsMisalign->UncheckedAt(i);
        
        TVector3 globPos;
        hitMisalign->Position(globPos);
        std::pair<TVector3, Int_t> locPos = GlobalToLocal(globPos);
        
#ifdef _TEST_
        MpdTpcHit * hit = (MpdTpcHit*)fHits->UncheckedAt(i);
        
        TVector3 globPosTest;
        hit->Position(globPosTest);
        std::pair<TVector3, Int_t> locPosTest = GlobalToLocal(globPosTest);
        
        TVector3 locPosMisTest(locPos.first), globPosMisTest(globPos);
#endif
        
        Int_t iSect = locPos.second;
        
        locPos.first.SetX(locPos.first.X() - fTpcSectorShift[iSect].X());
        locPos.first.SetY(locPos.first.Y() - fTpcSectorShift[iSect].Y());
        locPos.first.SetZ(locPos.first.Z() - fTpcSectorShift[iSect].Z());
        
        MpdRotation rot;
        rot.RotateX(fTpcSectorRot[iSect].X() * TMath::DegToRad());
        rot.RotateY(fTpcSectorRot[iSect].Y() * TMath::DegToRad());
        rot.RotateZ(fTpcSectorRot[iSect].Z() * TMath::DegToRad());
/*bool a = true;
if (a)
{
std::cout << "orig" << std::endl;
std::cout << rot.XX() << " " << rot.XY() << " " << rot.XZ() << " " << std::endl;
std::cout << rot.YX() << " " << rot.YY() << " " << rot.YZ() << " " << std::endl;
std::cout << rot.ZX() << " " << rot.ZY() << " " << rot.ZZ() << " " << std::endl;
std::cout << std::endl;
}*/
        rot = rot.Inverse();
        rot.SetZX(0.);
        rot.SetZY(0.);
/*if (a)
{
std::cout << "modified" << std::endl;
std::cout << rot.XX() << " " << rot.XY() << " " << rot.XZ() << " " << std::endl;
std::cout << rot.YX() << " " << rot.YY() << " " << rot.YZ() << " " << std::endl;
std::cout << rot.ZX() << " " << rot.ZY() << " " << rot.ZZ() << " " << std::endl;
std::cout << std::endl;
}
a = false;*/
        locPos.first *= rot;
        
        globPos = LocalToGlobal(locPos);
        
        TVector3 locPosSectorGeo;
        fSecGeo->Global2Local(globPos, locPosSectorGeo);
        
#ifdef _TEST_
        Int_t iHitAlign = fHitsAlign->GetEntriesFast();
        MpdTpcHit * hitAlign = new ((*fHitsAlign)[iHitAlign]) MpdTpcHit(*hitMisalign);
        hitAlign->SetPosition(globPos);
        hitAlign->SetLocalPosition(locPosSectorGeo);
#else
        hitMisalign->SetPosition(globPos);
        hitMisalign->SetLocalPosition(locPosSectorGeo);
#endif
        
#ifdef _TEST_
        if (sectsTest.find(iSect) == sectsTest.end())
        {
            std::cout << "!! TEST " << iSect << std::endl;
            std::cout << "--- Glob Pos ---" << std::endl;
            globPosTest.Print();
            globPosMisTest.Print();
            globPos.Print();
            std::cout << "angle " << TMath::ATan2(globPos.X(), globPos.Y()) * TMath::RadToDeg() << " deg" << std::endl;
            std::cout << "--- Loc Pos ---" << std::endl;
            locPosTest.first.Print();
            locPosMisTest.Print();
            locPos.first.Print();
            std::cout << "------" << std::endl;
        }
        sectsTest.insert(iSect);
#endif
    }
    
    std::cout << "MpdTpcAlignment::Exec finished OK" << std::endl;
}

void MpdTpcAlignment::SetPersistence(Bool_t val)
{
    fPersistence = val;
}

ClassImp(MpdTpcAlignment)
