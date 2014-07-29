// -------------------------------------------------------------------------
// -----                      CbmSttTrack source file                  -----
// -----                  Created 28/03/06  by R. Castelijns           -----
// -------------------------------------------------------------------------


#include <iostream>
#include "CbmSttHit.h"
#include "CbmSttTrack.h"


// -----   Default constructor   -------------------------------------------
CbmSttTrack::CbmSttTrack() 
{
    fRefAngle = 0.;
    fPidHypo  = 0;
    fFlag     = 0;
    fChi2Long = 0.;
    fChi2Rad  = 0.;
}
// -------------------------------------------------------------------------



// -----   Destructor   ----------------------------------------------------
CbmSttTrack::~CbmSttTrack() 
{
    fHitMap.clear();
}
// -------------------------------------------------------------------------



// -----   Public method AddHit   --------------------------------------
void CbmSttTrack::AddHit(Int_t hitID, CbmSttHit* mHit) 
{
    Double_t 
	wireX = mHit->GetX(),
	wireY = mHit->GetY(),
	wireZ = mHit->GetZ();

//    fHitMap[wireZ] = hitID;
    fHitMap[wireX * wireX + wireY * wireY] = hitID;
}
// -------------------------------------------------------------------------



// -----   Public method Print   -------------------------------------------
void CbmSttTrack::Print() 
{
  cout << " Number of attached hits : " 
       << fHits.GetSize()  << endl;
  cout << " Parameters at first point: " << endl;
  fParamFirst.Print();
  cout << " Chi2Long: " << fChi2Long << ", Chi2Rad: " << fChi2Rad << ", Quality flag " << fFlag << endl;
}
// -------------------------------------------------------------------------




void CbmSttTrack::SortHits() 
{
    // find the biggest z difference between hits
    // get the hit closest to the target
    
    Int_t 
	index=0;
  
    map<Double_t, Int_t>::iterator 
	it;
    
    fHits.Reset();
    fHits.Set(fHitMap.size());
    index = 0;
    
    for (it = fHitMap.begin(); it != fHitMap.end(); ++it)
    {
	fHits[index] = it->second;
	index++;
    }
}


// -------------------------------------------------------------------------

Bool_t CbmSttTrack::AlreadyHasHit(Int_t iHit)
{
    Bool_t
	retval = kFALSE;
    
    map<Double_t, Int_t>::iterator 
	it;
    
    for (it = fHitMap.begin(); it != fHitMap.end(); ++it)
    {
	if (it->second == iHit)
	{
	    retval = kTRUE;
	    break;
	}
    }  
    
    return retval;
}

ClassImp(CbmSttTrack)
