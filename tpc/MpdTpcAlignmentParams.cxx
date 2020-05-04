#include "MpdTpcAlignmentParams.h"

#include <TMath.h>
#include <TXMLEngine.h>

#include <iostream>

MpdTpcAlignmentParams::MpdTpcAlignmentParams()
    : fAlignParamsFile("")
{
}

MpdTpcAlignmentParams::MpdTpcAlignmentParams(std::string path)
    : MpdTpcAlignmentParams()
{
    fAlignParamsFile = path;
}

MpdTpcAlignmentParams::~MpdTpcAlignmentParams()
{
}

void MpdTpcAlignmentParams::SetAlignParamsFile(std::string path)
{
    fAlignParamsFile = path;
}

Int_t MpdTpcAlignmentParams::SectorByGlobal(const TVector3 & globXYZ) const
{
    Float_t phiGlob = TMath::ATan2(globXYZ.X(), globXYZ.Y());
    //Double_t phiGlob_bkp = phiGlob;
    if (phiGlob < 0) phiGlob += TMath::TwoPi();
    Int_t iSec = (Int_t) (phiGlob / fSectDphi + 0.5);
    if (iSec == fNumofSectors / 2) iSec = 0;
    if (globXYZ.Z() < 0.0) iSec += fNumofSectors / 2;
    /*if (globXYZ.Z() < 0.0) 
    {
        iSec = fNumofSectors - iSec;
        if (iSec == fNumofSectors) iSec = fNumofSectors / 2;
    }*/
    //if (iSec == 23) std::cout << phiGlob_bkp * TMath::RadToDeg() << std::endl;
    return iSec;
}

TVector3 MpdTpcAlignmentParams::GlobalToLocal(const TVector3 & globXYZ, Int_t iSect) const
{
    if (iSect < 0 || iSect >= fNumofSectors)
        iSect = SectorByGlobal(globXYZ);
    
    TVector3 locXYZ(globXYZ);
    
    locXYZ.RotateZ(-fSectDphi * iSect);
    
    if (locXYZ.Z() > 0)
        locXYZ.RotateY(TMath::Pi());
    
    locXYZ.SetY(locXYZ.Y() - fSectLowEdgeY);
    locXYZ.SetZ(locXYZ.Z() + fDriftLength);
    
    return locXYZ;
}

std::pair<TVector3, Int_t> MpdTpcAlignmentParams::GlobalToLocal(const TVector3 & globXYZ) const
{
    Int_t iSect = SectorByGlobal(globXYZ);
    
    return std::pair<TVector3, Int_t> (GlobalToLocal(globXYZ, iSect), iSect);
}

TVector3 MpdTpcAlignmentParams::LocalToGlobal(const TVector3 & locXYZ, const Int_t & iSect) const
{
    TVector3 globXYZ(locXYZ);
    
    globXYZ.SetZ(locXYZ.Z() - fDriftLength);
    globXYZ.SetY(locXYZ.Y() + fSectLowEdgeY);
    
    if (iSect < fNumofSectors / 2)
        globXYZ.RotateY(TMath::Pi());
    
    globXYZ.RotateZ(fSectDphi * iSect);
    
    return globXYZ;
}

TVector3 MpdTpcAlignmentParams::LocalToGlobal(std::pair<TVector3, Int_t> & locPos) const
{
    return LocalToGlobal(locPos.first, locPos.second);
}

Int_t MpdTpcAlignmentParams::ProcessParamsFile()
{
    if (fAlignParamsFile.empty())
    {
        Error("MpdTpcAlignmentParams::ProcessParamsFile","Params file not specified");
        return 1;
    }
    
    TXMLEngine* xml = new TXMLEngine;
    XMLDocPointer_t xmldoc = xml->ParseFile(fAlignParamsFile.c_str());
    if (xmldoc == NULL)
    {
        Error("MpdTpcAlignmentParams::ProcessParamsFile","XML Engine parse failed");
        delete xml;
        return 2;
    }
    
    fTpcSectorShift.resize(fNumofSectors);
    fTpcSectorRot.resize(fNumofSectors);
    
    XMLNodePointer_t mainnode = xml->DocGetRootElement(xmldoc);
    
    for (Int_t iSect = 0; iSect < fNumofSectors; ++iSect)
    {
        XMLNodePointer_t sector = iSect == 0 ? xml->GetChild(mainnode) : xml->GetNext(sector);
        if (sector == NULL) return 3;
        
        XMLAttrPointer_t n_ptr = xml->GetFirstAttr(sector);
        Double_t n = std::stod(xml->GetAttrValue(n_ptr));
        
        XMLNodePointer_t sectParam = xml->GetChild(sector);
        while (sectParam != NULL)
        {
            std::string paramName(xml->GetNodeName(sectParam));
            Double_t paramVal = std::stod(xml->GetNodeContent(sectParam));
            
            if (paramName == "ShiftX")
                fTpcSectorShift[n].SetX(paramVal);
            else if (paramName == "ShiftY")
                fTpcSectorShift[n].SetY(paramVal);
            else if (paramName == "ShiftZ")
                fTpcSectorShift[n].SetZ(paramVal);
            else if (paramName == "RotationX")
                fTpcSectorRot[n].SetX(paramVal);
            else if (paramName == "RotationY")
                fTpcSectorRot[n].SetY(paramVal);
            else if (paramName == "RotationZ")
                fTpcSectorRot[n].SetZ(paramVal);
            
            sectParam = xml->GetNext(sectParam);
        }
    }
    
    xml->FreeDoc(xmldoc);
    delete xml;
    
    return 0;
}

ClassImp(MpdTpcAlignmentParams)
