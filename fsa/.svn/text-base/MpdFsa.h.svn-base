//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                         MpdFsa header file                    -----
// -------------------------------------------------------------------------

/**  MpdFsa.h
 **
 ** Defines the active detector FSA. Constructs the geometry and
 ** registeres MCPoints.
 **/



#ifndef MPDFSA_H
#define MPDFSA_H


#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TVector3.h"
#include "FairDetector.h"

class MpdFsaPoint;
class FairVolume;

//------------------------------------------------------------------------------------------------------------------------
class MpdFsa : public FairDetector
{

public:

   	// *@param name    detector name
   	// *@param active  sensitivity flag
  	MpdFsa(const char* name, Bool_t active);

	MpdFsa();
	virtual ~MpdFsa();

	// Defines the action to be taken when a step is inside the
	// active volume. Creates MpdFsaPoints and adds them to the collection.
	// @param vol  Pointer to the active volume
        virtual Bool_t  ProcessHits(FairVolume* vol = 0);

   	// If verbosity level is set, print hit collection at the
   	// end of the event and resets it afterwards.
  	virtual void EndOfEvent();

   	// Registers the hit collection in the ROOT manager.
  	virtual void Register();

  	// Accessor to the hit collection 
  	virtual TClonesArray* GetCollection(Int_t iColl) const;

   	// Screen output of hit collection.
	virtual void Print() const;

   	// Clears the hit collection
	virtual void Reset();

	// *@param cl1     Origin
	// *@param cl2     Target
	// *@param offset  Index offset
 	virtual void CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset);

	// Constructs the FSA geometry
 	virtual void ConstructGeometry();
  
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
  	TClonesArray* fFsaCollection;      //! Hit collection


	// Adds a MpdFsaPoint to the HitCollection
  	MpdFsaPoint* AddHit(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t time, Double_t length, Double_t eLoss); 
 
	// Resets the private members for the track parameters
  	void ResetParameters();


  ClassDef(MpdFsa,1) 

};

//------------------------------------------------------------------------------------------------------------------------
inline void MpdFsa::ResetParameters() 
{
	fTrackID = fVolumeID = 0;
	fPos.SetXYZM(0.0, 0.0, 0.0, 0.0);
	fMom.SetXYZM(0.0, 0.0, 0.0, 0.0);
	fTime = fLength = fELoss = 0;
	fPosIndex = 0;
};
//------------------------------------------------------------------------------------------------------------------------
#endif
