//------------------------------------------------------------------------------------------------------------------------
#include <iostream>

#include "TClonesArray.h"

#include "FairRootManager.h"
#include "MpdEtofHit.h"
#include "MpdEtofPoint.h"

#include "MpdEtofHitProducerIdeal.h"
//------------------------------------------------------------------------------------------------------------------------
MpdEtofHitProducerIdeal::MpdEtofHitProducerIdeal() 
 : FairTask("Ideal ETOF Hit Producer") { }
//------------------------------------------------------------------------------------------------------------------------
MpdEtofHitProducerIdeal::~MpdEtofHitProducerIdeal() { }
//------------------------------------------------------------------------------------------------------------------------
InitStatus	MpdEtofHitProducerIdeal::Init() 
{
        FairRootManager* ioman = FairRootManager::Instance();
	if(!ioman) 
	{
    		cout<<"\n-E- [MpdEtofHitProducerIdeal::Init]: RootManager not instantised!"<<flush;
		return kFATAL;
	}

	fPointArray = (TClonesArray*) ioman->GetObject("ETOFPoint");
	if(!fPointArray) 
	{
		cout<<"\n-W- [MpdEtofHitProducerIdeal::Init]: No TOFPoint array!";
		return kERROR;
	}

	// Create and register output array
	fHitArray = new TClonesArray("MpdTofHit");
	ioman->Register("ETOFHit", "ETOF", fHitArray, kTRUE);

	cout<<"-I- MpdEtofHitProducerIdeal: Intialisation successfull."<<endl;
return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdEtofHitProducerIdeal::Exec(Option_t* opt) 
{
	// Reset output array
	if(!fHitArray) Fatal("Exec", "No TofHitArray");

	fHitArray->Clear();

	// Declare some variables
	MpdEtofPoint* point = NULL;
	Int_t detID   = 0;        // Detector ID
	Double_t x, y, z;         // Position
	Double_t dx = 0.0001;     // Position error
	Double_t tof = 0.;        // Time of flight
	TVector3 pos, dpos;       // Position and error vectors

	// Loop over TofPoints
	Int_t nPoints = fPointArray->GetEntriesFast();
	for(Int_t iPoint=0; iPoint<nPoints; iPoint++) 
	{
		point = (MpdEtofPoint*) fPointArray->At(iPoint);
		if(!point) continue;

		// Detector ID
		detID = point->GetDetectorID();

		// MCTrack ID
                //Int_t trackID = point->GetTrackID();

		// Determine hit position
		x  = point->GetX();
 		y  = point->GetY();
		z  = point->GetZ();

		// Time of flight
		tof = point->GetTime();

		// Create new hit
		pos.SetXYZ(x,y,z);
		dpos.SetXYZ(dx, dx, 0.);
		new ((*fHitArray)[iPoint]) MpdEtofHit(detID, pos, dpos, iPoint, tof);

	}   // Loop over MCPoints

	// Event summary
	cout<<"\n-I- MpdEtofHitProducerIdeal: "<<nPoints<<" TofPoints, "<<nPoints<<" Hits created.";
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdEtofHitProducerIdeal)
