#ifndef _MPDTPCHIT_H_
#define _MPDTPCHIT_H_

/// \ingroup tpc
/// \class MpdTpcHit
/// \brief Hit in MPD TPC
///
/// \author Alexander Zinchenko, LHEP JINR Dubna - extension of TpcHit

#include <FairHit.h>
#include <TObject.h>

class MpdTpcHit : public FairHit
{
private:
    Int_t	fiPad;
    Int_t	fiBin;
    Int_t	fLayer;
    
    Double32_t    fQ;
    Double32_t    fStep;
    Double32_t    fLength;
    
    // Sector coordinate system
    Double32_t    fLocalX;
    Double32_t    fLocalY;
    Double32_t    fLocalZ;
    
public:
    enum k_LinkType { PointIndex = 1, MCTrackIndex = 2 };

public:
    MpdTpcHit() : FairHit(),
      fiPad(-1), fiBin(-1), fLayer(-1), fQ(0), fStep(0), fLength(0), fLocalX(0), fLocalY(0), fLocalZ(0)
      { }
    
    MpdTpcHit(Int_t iPad, Int_t iBin) : FairHit(),
	fiPad(iPad), fiBin(iBin), fLayer(-1), fQ(0), fStep(0), fLength(0), fLocalX(0), fLocalY(0), fLocalZ(0)
      { }
    
    MpdTpcHit(Int_t detUID, TVector3 posHit, TVector3 posHitErr, Int_t pointIndx);

//     TpcHit(const TpcHit &hit) : FairHit((const FairHit &)hit) {
//         fiBin = hit.fiBin;
//         fiPad = hit.fiPad;
//         fQ = hit.fQ;
//         fLocalX = hit.fLocalX;
//         fLocalY = hit.fLocalY;
//         fLocalZ = hit.fLocalZ;
//     }
    
    virtual ~MpdTpcHit() {}

    /** Accessors **/
    Int_t GetModular() { return GetUniqueID(); }
    Int_t GetPad() { return fiPad; }
    Int_t GetBin() { return fiBin; }
    Int_t GetLayer() { return fLayer; }
    Double_t GetQ() const { return fQ; }
    Double_t GetStep() const { return fStep; } // step during MC transport
    Double_t GetLength() const { return fLength; } // track length
    // For backward compatibility
    //Int_t GetTrackID() const { return GetLinksWithType(Int_t type).GetLink(Int_t pos); } 
    Int_t GetTrackID() const { return GetLinksWithType(MpdTpcHit::MCTrackIndex).GetLink(0).GetIndex(); } 
    Double_t GetR() const { return fLocalY; } 
    Double_t GetRphi() const { return fLocalX; }
    Double_t GetEnergyLoss() const { return fQ; }
    
    Double_t GetLocalX() const { return fLocalX; }
    Double_t GetLocalY() const { return fLocalY; }
    Double_t GetLocalZ() const { return fLocalZ; }
    void LocalPosition(TVector3 & pos) const { pos.SetXYZ(fLocalX, fLocalY, fLocalZ); }
    
    /** Modifiers **/
    void SetModular(Int_t imod) { SetUniqueID(imod); }
    void SetPad(Int_t ipad) { fiPad = ipad; }
    void SetBin(Int_t ibin) { fiBin = ibin; }
    void SetLayer(Int_t lay) { fLayer = lay; }
    void SetQ(Double_t q) { fQ = q; }
    void SetStep(Double_t step) { fStep = step; }
    void SetLength(Double_t length) { fLength = length; }
    // For backward compatibility
    void SetR(Double_t r) { fLocalY = r; }
    void SetRphi(Double_t rphi) { fLocalX = rphi; }
    void SetEnergyLoss(Double_t edep) { fQ = edep; }

    void SetLocalX(Double_t x)  { fLocalX = x; }
    void SetLocalY(Double_t y)  { fLocalY = y; }
    void SetLocalZ(Double_t z)  { fLocalZ = z; }
    void SetLocalXYZ(Double_t x, Double_t y, Double_t z) { fLocalX = x; fLocalY = y; fLocalZ = z; }
    void SetLocalPosition(const TVector3& pos) { 
        fLocalX = pos.X(); fLocalY = pos.Y(); fLocalZ = pos.Z(); 
    }
    
    Bool_t IsSortable() const { return kTRUE; }
    Int_t Compare(const TObject* hit) const; // "Compare" function for sorting

    ClassDef(MpdTpcHit, 1)
};

#endif // _MPDTPCHIT_H_
