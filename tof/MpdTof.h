//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_H
#define __MPD_TOF_H 1

//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTof, LRectangle, LStrip
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include <TLorentzVector.h>
#include <TVector3.h>

#include "MpdTofUtils.h"

#include "FairDetector.h"
//------------------------------------------------------------------------------------------------------------------------
class MpdTofPoint;
class FairVolume;
class TClonesArray;
//------------------------------------------------------------------------------------------------------------------------
class MpdTof : public FairDetector
{
	// Track information to be stored until the track leaves the active volume.
  	Int_t		fTrackID;           //!  track index
  	Int_t		fVolumeID;          //!  volume id
  	TLorentzVector	fPos;               //!  position
  	TLorentzVector	fMom;               //!  momentum
  	Double_t	fTime;              //!  time
  	Double_t	fLength;            //!  length
  	Double_t	fELoss;             //!  energy loss

  	Int_t 		fPosIndex;		//!
  	TClonesArray	*aTofHits;		//! Hit collection
  	const double	nan;			//!

	void		ConstructAsciiGeometry();
  	MpdTofPoint* 	AddPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t time, Double_t length, Double_t eLoss); 
  	void 		ResetParameters();
  	
public:
  	MpdTof(const char* name = "TOF", Bool_t active = kTRUE);
	virtual ~MpdTof();


        virtual Bool_t  	ProcessHits(FairVolume* vol = nullptr);
  	virtual void 		EndOfEvent();
  	virtual void 		Register();
  	virtual TClonesArray* 	GetCollection(Int_t iColl) const;
	virtual void 		Print() const;
	virtual void 		Reset();
 	virtual void 		CopyClones(TClonesArray* from, TClonesArray* to, Int_t offset);
 	virtual void 		ConstructGeometry();
	virtual Bool_t 		CheckIfSensitive(std::string name);

ClassDef(MpdTof,3) 
};

//------------------------------------------------------------------------------------------------------------------------
inline void MpdTof::ResetParameters() 
{
	fTrackID = fVolumeID = 0;
	fPos.SetXYZM(nan, nan, nan, nan);
	fMom.SetXYZM(nan, nan, nan, nan);
	fTime = fLength = fELoss = nan;
	fPosIndex = 0;
};
//------------------------------------------------------------------------------------------------------------------------
#endif
