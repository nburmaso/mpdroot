//------------------------------------------------------------------------------------------------------------------------
#include <iostream>

#include "TClonesArray.h"

#include "FairRootManager.h"

#include "MpdTofHitProducerIdeal.h"
#include "MpdTofHit.h"
#include "MpdTofPoint.h"

//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducerIdeal::MpdTofHitProducerIdeal() 
 : FairTask("Ideal TOF Hit Producer") { }
//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducerIdeal::~MpdTofHitProducerIdeal() { }
//------------------------------------------------------------------------------------------------------------------------
InitStatus	MpdTofHitProducerIdeal::Init() 
{
        FairRootManager* ioman = FairRootManager::Instance();
	if(!ioman) 
	{
    		cout<<"\n-E- [MpdTofHitProducerIdeal::Init]: RootManager not instantised!"<<flush;
		return kFATAL;
	}

	fPointArray = (TClonesArray*) ioman->GetObject("TOFPoint");
	if(!fPointArray) 
	{
		cout<<"\n-W- [MpdTofHitProducerIdeal::Init]: No TOFPoint array!";
		return kERROR;
	}

	// Create and register output array
	fHitArray = new TClonesArray("MpdTofHit");
	ioman->Register("TOFHit", "TOF", fHitArray, kTRUE);

	cout<<"-I- MpdTofHitProducerIdeal: Intialisation successfull."<<endl;
return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTofHitProducerIdeal::Exec(Option_t* opt) 
{
	// Reset output array
	if(!fHitArray) Fatal("Exec", "No TofHitArray");

	fHitArray->Clear();

	// Declare some variables
	MpdTofPoint* point = NULL;
	Int_t detID   = 0;        // Detector ID
        //Int_t trackID = 0;        // Track index
	Double_t x, y, z;         // Position
	Double_t dx = 0.0001;     // Position error
	Double_t tof = 0.;        // Time of flight
	TVector3 pos, dpos;       // Position and error vectors

	// Loop over TofPoints
	Int_t nPoints = fPointArray->GetEntriesFast();
	for(Int_t iPoint=0; iPoint<nPoints; iPoint++) 
	{
		point = (MpdTofPoint*) fPointArray->At(iPoint);
		if(!point) continue;

		// Detector ID
		detID = point->GetDetectorID();

		// MCTrack ID
                //trackID = point->GetTrackID();

		// Determine hit position
		x  = point->GetX();
 		y  = point->GetY();
		z  = point->GetZ();

		// Time of flight
		tof = point->GetTime();

		// Create new hit
		pos.SetXYZ(x,y,z);
		dpos.SetXYZ(dx, dx, 0.);
		new ((*fHitArray)[iPoint]) MpdTofHit(detID, pos, dpos, iPoint, tof);

	}   // Loop over MCPoints

	// Event summary
	cout<<"\n-I- MpdTofHitProducerIdeal: "<<nPoints<<" TofPoints, "<<nPoints<<" Hits created.";
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdTofHitProducerIdeal)
