//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                         MpdCpc header file                    -----
// -------------------------------------------------------------------------

/**  MpdCpc.h
 **
 ** Defines the active detector CPC. Constructs the geometry and
 ** registeres MCPoints.
 **/



#ifndef MPDCPC_H
#define MPDCPC_H


#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TVector3.h"
#include "FairDetector.h"

class MpdCpcPoint;
class FairVolume;

//------------------------------------------------------------------------------------------------------------------------
class MpdCpc : public FairDetector
{

public:

   	// *@param name    detector name
   	// *@param active  sensitivity flag
  	MpdCpc(const char* name, Bool_t active);

	MpdCpc();
	virtual ~MpdCpc();

	// Defines the action to be taken when a step is inside the
	// active volume. Creates MpdCpcPoints and adds them to the collection.
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

	// Constructs the CPC geometry
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
  	TClonesArray* fCpcCollection;      //! Hit collection


	// Adds a MpdCpcPoint to the HitCollection
  	MpdCpcPoint* AddHit(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t time, Double_t length, Double_t eLoss); 
 
	// Resets the private members for the track parameters
  	void ResetParameters();


  ClassDef(MpdCpc,1) 

};

//------------------------------------------------------------------------------------------------------------------------
inline void MpdCpc::ResetParameters() 
{
	fTrackID = fVolumeID = 0;
	fPos.SetXYZM(0.0, 0.0, 0.0, 0.0);
	fMom.SetXYZM(0.0, 0.0, 0.0, 0.0);
	fTime = fLength = fELoss = 0;
	fPosIndex = 0;
};
//------------------------------------------------------------------------------------------------------------------------
#endif
