#ifndef MPDTPCALIGNMENTPARAMS_H
#define MPDTPCALIGNMENTPARAMS_H

#include <TVector3.h>

#include <vector>
#include <string>
#include <utility>

class MpdTpcAlignmentParams
{
protected:
    static constexpr Double_t fDriftLength = 163.0;
    static constexpr Double_t fSectLowEdgeY = 38.9;
    static constexpr Int_t fNumofSectors = 24;
    static constexpr Double_t fSectDphi = TMath::TwoPi() / (fNumofSectors / 2);
    
protected:
    MpdTpcAlignmentParams();
    MpdTpcAlignmentParams(std::string path);
    ~MpdTpcAlignmentParams();
    
public:
    virtual void SetAlignParamsFile(std::string path);
    
protected:
    virtual Int_t SectorByGlobal(const TVector3 & globXYZ) const;
    virtual Int_t ProcessParamsFile();
    virtual TVector3 GlobalToLocal(const TVector3 & globXYZ, Int_t iSect) const;
    virtual std::pair<TVector3, Int_t> GlobalToLocal(const TVector3 & globXYZ) const;
    virtual TVector3 LocalToGlobal(const TVector3 & locXYZ, const Int_t & iSect) const;
    virtual TVector3 LocalToGlobal(std::pair<TVector3, Int_t> & locPos) const;
    
protected:
    std::string fAlignParamsFile;
    
    std::vector<TVector3> fTpcSectorShift;
    std::vector<TVector3> fTpcSectorRot;
    
    ClassDef(MpdTpcAlignmentParams,0)
};

#endif
