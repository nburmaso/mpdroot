/**
 * Holds information about ECal tower
 *
 * The class holds information about the tower from
 * the Barrel Electromagnetic Calorimeter (ECal)
 *
 */

#ifndef MpdMiniBECalHit_h
#define MpdMiniBECalHit_h

// ROOT headers
#include "TObject.h"
#include "TMath.h"

#include <vector>

using namespace std;
//_________________

class MpdMiniBECalHit : public TObject {
public:
    /// Default constructor
    MpdMiniBECalHit();
    /// Copy constructor
    MpdMiniBECalHit(const MpdMiniBECalHit &hit);
    /// Destructor
    virtual ~MpdMiniBECalHit();
    /// Print tower information
    virtual void Print(const Char_t* option = "") const;

    //
    // Getters
    //
    
    // Return a vector of lighted cells 
    vector <Int_t> cellIds() {
        return fCellIds;
    }
       
    /// Return matching flag (false - no match, true - one-to-one)

    Bool_t becalMatchFlag() const {
        return mBEcalMatchFlag;
    }
    
    /// Return sector index

    UShort_t sector(Int_t cellId) const {
        return cellId / 768;
    }
    /// Return chamber index

    UShort_t chamber(Int_t cellId) const {
        return cellId / 19200;
    }
    /// Return crate index

    UShort_t crate(Int_t cellId) const {
        return cellId / 128;
    }
    /// Return energy of the cluster

    Float_t energy() const {
        return fEnergy;
    }
    /// Return hit mean time

    Float_t time() const {
        return fTime;
    }
    /// Return flag

    Int_t flag() const {
        return (Int_t) fFlag;
    }
    /// Return number of tracks that deposited energy in the tower

    Int_t numberOfTracks() const {
        return (Int_t) fNumOfTracks;
    }

    /// Get phi and rho of the cluster

    Float_t GetPhi() const {
        return TMath::ATan2(fY, fX);
    };

    Float_t GetRho() const {
        return TMath::Sqrt(fX * fX + fY * fY);
    };

    /// Get coordinates of the cluster
    Float_t GetX() const {
        return fX;
    };

    Float_t GetY() const {
        return fY;
    };

    Float_t GetZ() const {
        return fZ;
    };

    /// Get dPhi and dZ of the cluster

    Float_t GetDPhi() const {
        return fdPhi;
    };

    Float_t GetDZ() const {
        return fdZ;
    };

    /// Get radius vector of the cluster 

    Float_t GetRad() const {
        return TMath::Sqrt(fX * fX + fY * fY + fZ * fZ);
    };

    //
    // Setters
    //

    /// Set cell (detector) ID

    void setCellIds(vector <Int_t> ids) {
        fCellIds = ids;
    }
    /// Set deposited energy in the cluster

    void setEnergy(Float_t energy) {
        fEnergy = energy;
    }
    /// Set cluster time

    void setTime(Float_t t) {
        fTime = t;
    }
    /// Set flag
    void setFlag(Int_t flag);
    /// Set number of associated tracks
    void setNumberOfTracks(Int_t num);
    
    /// Set cluster coordinates
    void SetXYZ(Float_t x, Float_t y, Float_t z) {
        fX = x;
        fY = y;
        fZ = z;
    }
    
    /// Set dPhi and dZ
    void SetDPhi(Float_t dPhi) {
        fdPhi = dPhi;               
    }
    
    void SetDz(Float_t dz) {
        fdZ = dz;
    }
    
    // Set matching flag
    void setBEcalMatchFlag(Bool_t flag) {
        mBEcalMatchFlag = flag;
    }

protected:

    // The three indices below are useless and should be replaced
    // with normal encoding when the numbering scheme will be known

    /// Cell ID [0 .. 38399], 38400 cells in total
    /// Each element of the vector corresponds to a set of cellID's for the cluster
    vector <Int_t> fCellIds;
    
    /// false - no match, true - matched
    Bool_t mBEcalMatchFlag;

    /// Energy deposited in the cluster
    Float16_t fEnergy;
    /// Cluster time
    Float16_t fTime;

    /// Flag for general purposes [TDC, event tagging...]
    Char_t fFlag;
    /// Number of tracks in the cluster
    UChar_t fNumOfTracks;

    // cluster coordinates in global system
    Float_t fX; // x-coordinate of cluster
    Float_t fY; // y-coordinate of cluster
    Float_t fZ; // z-coordinate of cluster

    // Distance to closest track
    Float_t fdPhi; // dPhi
    Float_t fdZ; // dZed

    ClassDef(MpdMiniBECalHit, 2)
};

#endif // #define MpdMiniBECalHit_h
