//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                         MpdStrawendcap header file                    -----
// -------------------------------------------------------------------------

/**  MpdStrawendcap.h
 **
 ** Defines the active detector Strawendcap. Constructs the geometry and
 ** registeres MCPoints.
 **/



#ifndef MPDStrawendcap_H
#define MPDStrawendcap_H


#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TVector3.h"
#include "FairDetector.h"

class MpdStrawendcapPoint;
class FairVolume;

//------------------------------------------------------------------------------------------------------------------------
class MpdStrawendcap : public FairDetector
{

public:

   	// *@param name    detector name
   	// *@param active  sensitivity flag
  	MpdStrawendcap(const char* name, Bool_t active);

	MpdStrawendcap();
	virtual ~MpdStrawendcap();

	// Defines the action to be taken when a step is inside the
	// active volume. Creates MpdStrawendcapPoints and adds them to the collection.
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

	// Constructs the Strawendcap geometry
 	virtual void ConstructGeometry();
  
private:

	// Track information to be stored until the track leaves the active volume.
  	Int_t          fTrackID;           //!  track index
  	Int_t          fVolumeID;          //!  volume id
  	TVector3       fPos;               //!  position
  	TVector3       fPosLocal;          //!  position local to gas chamber
  	TLorentzVector fMom;               //!  momentum
  	Double32_t     fTime;              //!  time
  	Double32_t     fLength;            //!  length
  	Double32_t     fELoss;             //!  energy loss
  	Int_t	       fIsPrimary; 	   //! is track primary?	
  	Double_t       fCharge;		   //! track charge
  	Double_t       fRadius;		   //! hit radius
  	Int_t          fPdgId;             //! pdg id of particle
  	
  	TVector3 fPosIn;             //!  entry position in global frame                                                                    
  	TVector3 fPosOut;            //!  exit position in global frame                                                                     

  	Int_t fPosIndex;                   //!
  	TClonesArray* fStrawendcapCollection;      //! Hit collection

	int DistAndPoints(TVector3 p1, TVector3 p2, TVector3 p3, TVector3 p4, TVector3& pa, TVector3& pb);
        TVector3 GlobalToLocal(TVector3& global);
        TVector3 LocalToGlobal(TVector3& local);
        
	// Adds a MpdStrawendcapPoint to the HitCollection
  	MpdStrawendcapPoint* AddHit(Int_t trackID, Int_t detID, TVector3 pos, Double_t radius, TVector3 mom, Double_t time, 
  	Double_t length, Double_t eLoss, Int_t isPrimary, Double_t charge, Int_t fPdgId, TVector3 trackPos); 
 
	// Resets the private members for the track parameters
  	void ResetParameters();


  ClassDef(MpdStrawendcap,1) 

};

//------------------------------------------------------------------------------------------------------------------------
inline void MpdStrawendcap::ResetParameters() 
{
	fTrackID = fVolumeID = 0;
	fPos.SetXYZ(0.0, 0.0, 0.0);
	fMom.SetXYZM(0.0, 0.0, 0.0, 0.0);
	fTime = fLength = fELoss = 0;
	fPosIndex = 0;
};
//------------------------------------------------------------------------------------------------------------------------
#endif
