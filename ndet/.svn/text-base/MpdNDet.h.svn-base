/**  MpdNDet.h
 *@author Mikhail Prokudin
 **
 ** Defines the active detector NDet with geometry coded here.
 **/


#ifndef MPDNDET_H
#define MPDNDET_H


#include "FairDetector.h"

#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TVector3.h"

#include <list>

class MpdNDetPoint; 
class MpdNDetPointLite; 
class FairVolume;
class TGeoTranslation;

#define kNumberOfNdetSensitiveVolumes 2

class MpdNDet : public FairDetector
{

public:

  /** Default constructor **/
  MpdNDet();


  /** Standard constructor.
   *@param name    detetcor name
   *@param active  sensitivity flag
   **/
  MpdNDet(const char* name, Bool_t active, 
	  const char* fileGeo="ecal_FastMC.geo");


  /** Destructor **/
  virtual ~MpdNDet();


  /** Virtual method ProcessHits
   **
   ** Defines the action to be taken when a step is inside the
   ** active volume. Creates MpdNDet and adds them to the
   ** collection.
   *@param vol  Pointer to the active volume
   **/
  virtual Bool_t  ProcessHits(FairVolume* vol = NULL);


  /** Virtual method Construct geometry
   **
   ** Constructs the NDet geometry
   **/
  virtual void ConstructGeometry();

  virtual void EndOfEvent();
  virtual void BeginEvent();
  virtual void Reset();
  virtual void Print() const;
  virtual void CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset);
  virtual void Register();
  virtual void ChangeHit(MpdNDetPointLite* oldHit=NULL);
  virtual void FinishPrimary();

  virtual void Initialize();

  /** Accessor to the hit collection **/                       
  virtual TClonesArray* GetCollection(Int_t iColl) const;
  virtual void SetSpecialPhysicsCuts();
  
  /** Get cell coordinates according
   ** to parameter container **/
  static Bool_t GetCellCoord(Int_t fVolumeID, Float_t &x, Float_t &y, Int_t& tenergy);
  /** Get cell coordinates according
   ** to current MpdNdetInf **/
  static Bool_t GetCellCoordInf(Int_t fVolumeID, Float_t &x, Float_t &y, Int_t& tenergy);
protected:
  MpdNDetPoint* AddHit(Int_t trackID, Int_t detID, TVector3 pos,
                       TVector3 mom, Double_t time, Double_t length,
                       Double_t eLoss);
  MpdNDetPointLite* AddLiteHit(Int_t trackID, Int_t detID, Double32_t time, Double32_t eLoss);

private:
  Bool_t FillLitePoint(Int_t volnum);
  void FillWallPoint();
  /** Private method ResetParameters
   **
   ** Resets the private members for the track parameters
   **/
  void ResetParameters();
  void SetNdetCuts(Int_t medium);
  MpdNDetPointLite* FindHit(Int_t VolId, Int_t TrackId);

private:
  Option_t* fDebug;	//!

  /** returns type of volume **/
  Int_t GetVolType(Int_t volnum);
  /** Track information to be stored until the track leaves the
      active volume. **/
  /**  track index **/
  Int_t          fTrackID;           //!  
  /** volume id **/
  Int_t          fVolumeID;          //!  
  /** position **/
  TLorentzVector fPos;               //!  
  /** momentum **/
  TLorentzVector fMom;               //!  
  /** time **/
  Double32_t     fTime;              //!
  /** length **/
  Double32_t     fLength;            //!
  /** energy loss **/
  Double32_t     fELoss;             //!
  /** **/ 
  Int_t          fPosIndex;          //!

  /** MC point collection on NDET wall **/ 
  TClonesArray*  fNDetCollection;    //! 
  /** MC point collection inside NDET **/ 
  TClonesArray*  fLiteCollection;    //! 
  /** Number of first hit for current primary **/
  Int_t fFirstNumber;	//!
  /** Map of volumes in NDET
   ** fVolArr[0]==code of sensivite wall
   ** fVolArr[1]==code of Scin
   **/
  Int_t fVolArr[kNumberOfNdetSensitiveVolumes];

  /**  Inner radius of air **/
  Double_t fZSize;
  Double_t fAirInnerRad;
  Double_t fAirThickness;
  Double_t fScinThickness;
  Double_t fECut;
  Double_t fHCut;
  Int_t fZDivisions;//along beam 
  Int_t fRDivisions;//phi divisions

  /** Volume ID of calorimeter structure **/
  Int_t fStructureId;		//!
  Int_t fScinId;		//!
  /** Initialize medium with given name **/
  Int_t InitMedium(const char* name);
  /** Initialize all calorimter media **/
  void InitMedia();
  ClassDef(MpdNDet,1)

};

inline void MpdNDet::ResetParameters()
{
  fTrackID = fVolumeID = 0;
  fPos.SetXYZM(0.0, 0.0, 0.0, 0.0);
  fMom.SetXYZM(0.0, 0.0, 0.0, 0.0);
  fTime = fLength = fELoss = 0;
  fPosIndex = 0;
};


#endif
