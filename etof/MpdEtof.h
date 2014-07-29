//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_ETOF_H
#define __MPD_ETOF_H 1


#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TVector3.h"

#include "MpdTofUtils.h"

#include "FairDetector.h"

class MpdEtofPoint;
class FairVolume;
//------------------------------------------------------------------------------------------------------------------------
class MpdEtof : public FairDetector
{

public:
  	MpdEtof(const char* name = "ETOF", Bool_t active = kTRUE);
	virtual ~MpdEtof();

        virtual Bool_t  ProcessHits(FairVolume* vol = 0);
  	virtual void EndOfEvent();
  	virtual void Register();
  	virtual TClonesArray* GetCollection(Int_t iColl) const;
	virtual void Print() const;
	virtual void Reset();
 	virtual void CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset);
 	virtual void ConstructGeometry(); 
  
  	static Bool_t	ParseTGeoManager(MpdTofUtils::RegVec&, MpdTofUtils::ModMMap&, MpdTofUtils::PadMap&);  
  
private:

	// Track information to be stored until the track leaves the active volume.
  	Int_t          fTrackID;           //!  track index
  	Int_t          fVolumeID;          //!  volume id
  	TLorentzVector fPos;               //!  position
  	TLorentzVector fMom;               //!  momentum
  	Double32_t     fTime;              //!  time
  	Double32_t     fLength;            //!  length
  	Double32_t     fELoss;             //!  energy loss
  	
  	Int_t fPosIndex;                   //!
  	TClonesArray* fTofCollection;      //! Hit collection

  	Double_t localPos[3];
  	Double_t globalPos[3];
  	
  	MpdEtofPoint* AddHit(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t time, Double_t length, Double_t eLoss); 
  	void ResetParameters();


  ClassDef(MpdEtof,1) 
};

//------------------------------------------------------------------------------------------------------------------------
inline void MpdEtof::ResetParameters() 
{
	fTrackID = fVolumeID = 0;
	fPos.SetXYZM(0.0, 0.0, 0.0, 0.0);
	fMom.SetXYZM(0.0, 0.0, 0.0, 0.0);
	fTime = fLength = fELoss = 0;
	fPosIndex = 0;
};
//------------------------------------------------------------------------------------------------------------------------
#endif
