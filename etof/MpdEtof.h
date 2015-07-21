//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_ETOF_H
#define __MPD_ETOF_H 1


#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TVector3.h"

#include "MpdTof.h"
#include "MpdTofUtils.h"

#include "FairDetector.h"

class MpdEtofPoint;
class FairVolume;
//------------------------------------------------------------------------------------------------------------------------
class MpdEtof : public FairDetector
{
	// Track information to be stored until the track leaves the active volume.
  	Int_t          fTrackID;           //!  track index
  	Int_t          fVolumeID;          //!  volume id
  	TLorentzVector fPos;               //!  position
  	TLorentzVector fMom;               //!  momentum
  	Double32_t     fTime;              //!  time
  	Double32_t     fLength;            //!  length
  	Double32_t     fELoss;             //!  energy loss
  	
  	Int_t fPosIndex;                   //!
  	TClonesArray* aTofHits;      	   //! Hit collection

  	const double	nan;
  	
 	static MpdTof::intervalTreeType		mDetectorsR; //!
	static MpdTof::intervalTreeType		mDetectorsPhi; //!	 	
  	static MpdTof::MStripType		mStrips; //!
  
  	MpdEtofPoint* 	AddPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t time, Double_t length, Double_t eLoss); 
  	void 		ResetParameters();
  	  		
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
  
  	static void		FindNeighborStrips(TH1D* h1 = nullptr, TH2D* h2 = nullptr, bool doTest = false);
	static void		ParseTGeoManager(TH2D* h1 = nullptr, bool forced = false);
		
	static const LStrip*	FindStrip(Int_t UID);
	static const MpdTof::intervalTreeType*	GetDetR() { return &mDetectorsR; };
	static const MpdTof::intervalTreeType*	GetDetPhi() { return &mDetectorsPhi; };	
	static const MpdTof::MStripType*	GetStripMap() { return &mStrips; };

ClassDef(MpdEtof,2) 
};

//------------------------------------------------------------------------------------------------------------------------
inline void MpdEtof::ResetParameters() 
{
	fTrackID = fVolumeID = 0;
	fPos.SetXYZM(nan, nan, nan, nan);
	fMom.SetXYZM(nan, nan, nan, nan);
	fTime = fLength = fELoss = nan;
	fPosIndex = 0;
};
//------------------------------------------------------------------------------------------------------------------------
#endif
