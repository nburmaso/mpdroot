//------------------------------------------------------------------------------------------------------------------------
#include <iostream>
#include <assert.h>

#include <TRandom2.h>
#include <TClonesArray.h>
#include <TGeoManager.h>
#include <TGeoVolume.h>
#include <TGeoNode.h>
#include <TGeoMatrix.h>
#include <TMath.h>
#include <TRandom.h>
#include <TVector3.h>
#include <TH2D.h>

#include "MpdDch.h"
#include "MpdDchHit.h"
#include "MpdDchPoint.h"
#include "FairRun.h"
#include "FairRuntimeDb.h"
#include "FairRootManager.h"
#include "FairDetector.h"
#include "FairMCTrack.h"

struct __ltstr
{
	bool operator()(Double_t s1, Double_t s2) const
  	{
    		return s1 > s2;
  	}
};

#include "MpdDchHitProducer.h"
//------------------------------------------------------------------------------------------------------------------------
MpdDchHitProducer::MpdDchHitProducer(const char* fileGeo, Int_t verbose, Bool_t test) 
 : FairTask("Dch HitProducer", verbose), fDoTest(test), fRSigma(0.2000), fRPhiSigma(0.0200)
{
	pRandom = new TRandom2;
	
	if(fDoTest)
    	{	
		htTime = new TH1D("htTime", "Time delay, ns", 1000, 0., 1000.); htTime->SetDirectory(0); fList.Add(htTime);
		htTime->SetXTitle("#Delta_{time delay}, ns"); htTime->SetYTitle("Events");		
		htTimeA = (TH1D*) htTime->Clone("htTimeA"); htTimeA->SetDirectory(0); fList.Add(htTimeA);	

		htGasDrift = new TH1D("htGasDrift", "", 100, 0., 0.5); 	htGasDrift->SetDirectory(0); 	fList.Add(htGasDrift);
		htGasDrift->SetXTitle("#Delta_{gas drift}, cm"); htGasDrift->SetYTitle("Events");
		htGasDriftA = (TH1D*) htGasDrift->Clone("htGasDriftA");	htGasDriftA->SetDirectory(0); 	fList.Add(htGasDriftA);	

		htPerp = new TH1D("htPerp", "", 360, -140.+0.25, 140.+0.25); htPerp->SetDirectory(0);	fList.Add(htPerp);
		htPerp->SetXTitle("wire position, cm"); htPerp->SetYTitle("Events");
		htPerpA = (TH1D*) htPerp->Clone("htPerpA"); htPerpA->SetDirectory(0); 			fList.Add(htPerpA);

		htOccup = new TH1D("htOccup", "Cell occupancy", 100, 0.5, 100.5); 
		htOccup->SetDirectory(0); fList.Add(htOccup);				
		htXYlocal = new TH2D("htXYlocal", "Local XY", 1000, 0., 0.6, 1000, -200., 200.); 		
		htXYlocal->SetDirectory(0); fList.Add(htXYlocal);
		htRvsR = new TH2D("htRvsR", "R global vs R  local", 1000, 0., 150., 1000, 0., 150.); 		
		htRvsR->SetDirectory(0); fList.Add(htRvsR);
		htWireN = new TH1D("htWireN", "#wire", 16400, -200., 16000. + 200.);  
		htWireN->SetDirectory(0); fList.Add(htWireN);	
		htMCTime = new TH1D("htMCTime", "MC time", 100, 0., 1000.); 
		htMCTime->SetDirectory(0); fList.Add(htMCTime);			
	}
}
//------------------------------------------------------------------------------------------------------------------------
MpdDchHitProducer::~MpdDchHitProducer() 
{
	delete pRandom;	
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus 		MpdDchHitProducer::Init() 
{
 	Info("Init", "Begin hit producer initialization.");
	FairRootManager *ioman = FairRootManager::Instance();

  	if(ioman==0){ Error("MpdDchHitProducer::Init","FairRootManager XUINJA"); return kERROR; }
  
  	pDchPoints  = (TClonesArray *) ioman->GetObject("DchPoint");
  	pMCTracks   = (TClonesArray *) ioman->GetObject("MCTrack"); 

  	if(!pDchPoints || !pMCTracks){ Error("MpdDchHitProducer::Init","Branch not found!"); return kERROR; }
  
  	// Create and register output array
  	pHitCollection = new TClonesArray("MpdDchHit");  
  	ioman->Register("DchHit","Dch", pHitCollection, kTRUE);	
  
 	Info("Init", "Initialization finished succesfully.");	
	 
return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
void			MpdDchHitProducer::Rotate(Int_t proj, Double_t x,Double_t y, Double_t& xRot, Double_t& yRot, Bool_t back)
{
    	// Transform to the rotated coordinate system
	// [0-3]==[x,y,u,v]  0, 90, -45, 45
	static const Double_t	cosPhi_45 = TMath::Cos(-45.*TMath::DegToRad()), sinPhi_45 = TMath::Sin(-45.*TMath::DegToRad()), 
				cosPhi45 = TMath::Cos(45.*TMath::DegToRad()), sinPhi45 = TMath::Sin(45.*TMath::DegToRad()) ;	
	assert(proj<4);
	const Int_t map[]={0, 4, 3, 2};// 0<-->0, 1<-->4, 2<-->3, 3<-->2
	if(back) proj = map[proj];
	
	//Double_t u = -h->GetX() * cosSin[1] + h->GetY() * cosSin[0];
	//Double_t v = h->GetX() * cosSin[0] + h->GetY() * cosSin[1];
	switch(proj)
	{
		case 0:		// 0 degree
		  //xRot = x;
		  //yRot = y;		
	    		xRot = y;
    			yRot = x;		
			return;		
		case 1:		// 90 degree
		  //xRot = y;
		  //yRot = -x;	
			xRot = -x;
    			yRot = y;	
			return;
		case 2:		// -45 degree
		  //xRot =  x * cosPhi_45 + y * sinPhi_45;
		  //yRot = -x * sinPhi_45 + y * cosPhi_45;
    			xRot =  -x * sinPhi_45 + y * cosPhi_45;
    			yRot = x * cosPhi_45 + y * sinPhi_45;
			return;
		case 3:		// 45 degree
		  //xRot =  x * cosPhi45 + y * sinPhi45;
		  //yRot = -x * sinPhi45 + y * cosPhi45;
    			xRot =  -x * sinPhi45 + y * cosPhi45;
    			yRot = x * cosPhi45 + y * sinPhi45;
			return;	
		case 4:		// -90 degree
		  //xRot = -y;
		  //yRot = x;
			xRot = x;
    			yRot = -y;
			return;	
											
		default: assert(false);
	}
}
//------------------------------------------------------------------------------------------------------------------------
inline Double_t			MpdDchHitProducer::GetPhi(Int_t proj)
{
	static const Double_t	Phi_45 = -45.*TMath::DegToRad(), Phi45 = 45.*TMath::DegToRad(), Phi90 = 90.*TMath::DegToRad();

	switch(proj)
	{
		case 0:	return 0.;	// 0 degree				
		case 1:	return Phi90;	// 90 degree	
		case 2:	return Phi_45;	// -45 degree
		case 3:	return Phi45;	// 45 degree						
		default: assert(false);
	}
}
//------------------------------------------------------------------------------------------------------------------------
Double_t		MpdDchHitProducer::GetDriftLenght(Int_t proj, Int_t gasgap, Double_t x, Double_t& wirePos)
{
	//   ... -1	0	1	...  - first(0) gap wire  position (X) [cm]
	//   ...    -0.5   0.5     1.5  ...  - second(1) gap wire  position (X) [cm]
	
	wirePos = (gasgap == 0) ? TMath::Nint(x) : TMath::Nint(x + 0.5) - 0.5;
		
	//AZ return TMath::Abs(x - wirePos);	// [cm]
return x - wirePos;	// [cm]
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t 			MpdDchHitProducer::HitExist(Double_t delta) // [ns] 
{
  //const static Double_t _Time[4]	= {0.,	20.,	20.01,	20.02}; 	// 0.,	2.,	10.,	40.
 	const static Double_t _Time[4]	= {0.,	2.,	2.01,	2.02}; 	// 0.,	2.,	10.,	40.
 	const static Double_t _Eff[4]	= {0.,	0.01,	0.99,	1.0};	// 0.,	0.2,	0.8,	1.

// 	Efficiency 	
//-------------------------------------
// 1.                          x
// 0.9      x
// 
//        
// 0.6   x     
//       
// 0 x
//   0   5  10                  50 ns
//-------------------------------------
		
  	const static Double_t slope1 = (_Eff[1] - _Eff[0]) / (_Time[1] - _Time[0]);
  	const static Double_t slope2 = (_Eff[2] - _Eff[1]) / (_Time[2] - _Time[1]);
  	const static Double_t slope3 = (_Eff[3] - _Eff[2]) / (_Time[3] - _Time[2]);		
		
  	Double_t efficiency;
  	if(	delta > _Time[3]) return true;
  	else if(delta > _Time[2] && 	delta < _Time[3]) 	efficiency = _Eff[2] + (delta - _Time[2]) * slope3;
  	else if(delta > _Time[1] && 	delta < _Time[2]) 	efficiency = _Eff[1] + (delta - _Time[1]) * slope2;
  	else if(			delta < _Time[1]) 	efficiency = delta * slope1;		  

///cout<<"\n   eff="<<efficiency<<" "<<delta;
	
    	if(pRandom->Rndm() < efficiency) return true;
	
return false;	
}
//------------------------------------------------------------------------------------------------------------------------
Double_t		MpdDchHitProducer::GetTShift(Double_t driftLength, Double_t wirePos, Double_t R, Double_t& L)
{
	const static Double_t gasDriftSpeed =  5.e-3; // 50 mkm/ns  ==  5.e-3 cm/ns 
	const static Double_t wireDriftSpeed =  20; // 5 ns/m  ==  20 cm/ns 	
	const static Double_t WeelR_2 = 135*135; // cm

	driftLength = driftLength > 0 ? driftLength : -1.*driftLength;
	L = sqrt(WeelR_2 -  wirePos*wirePos); 	 // half wire length
	if(wirePos > -9.6 && wirePos < 9.6)	L = L - TMath::Abs(R);// two wires 
	else					L = L + R; // one wire 
	
////cout<<"\n t1="<<driftLength / gasDriftSpeed<<"  ("<<driftLength<<") t2="<<(wireLength + R) / wireDriftSpeed<<" L="<<wireLength<<" Y="<<R;
	
return driftLength / gasDriftSpeed + L / wireDriftSpeed;
}
//------------------------------------------------------------------------------------------------------------------------
Int_t		MpdDchHitProducer::WireID(Int_t uid, Double_t wirePos, Double_t R)
{
	uid--; // uid [0,15]
	// wirePos [-135,135]cm
	// tube R 9.6 cm

	if(wirePos > -9.6 && wirePos < 9.6) if(R > 0)return (int)(wirePos + 1000.*uid + 500); // two wires 

return (int)( wirePos + 1000.*uid);  // one wire 
}
//------------------------------------------------------------------------------------------------------------------------
void 			MpdDchHitProducer::Exec(Option_t* opt) 
{
  	pHitCollection->Delete();
	Int_t hitID = 0;
	fMapOccup.clear();

	Int_t nDchPoint = pDchPoints->GetEntriesFast();
	cout<<"\n-I- [MpdDchHitProducer::Exec] "<<nDchPoint<<" points in Dch for this event."<<flush;

	MpdDchPoint 	*pPoint;
	MpdDchHit	*pHit;
        Double_t	R, Rphi, x, y, xRot, yRot, driftLength, wirePos, L;
	Double_t 	dRphi = 0, dR = 0;
	TVector3	pos, dpos;	
        Int_t  		uid, wheel, gasgap, proj;
	
	for(Int_t pointIndex = 0; pointIndex < nDchPoint; pointIndex++ )  // <---Loop over the DCH points
	{
		pPoint = (MpdDchPoint*) pDchPoints->UncheckedAt(pointIndex);

///////		if( ((FairMCTrack*)pMCTracks->UncheckedAt(pPoint->GetTrackID()))->GetMotherId() != -1)continue; // primary ONLY !!!
		
		uid = pPoint->GetDetectorID();
		wheel = MpdDch::GetWheel(uid); 		// [0-1] == [inner,outer]
		proj = MpdDch::GetProj(uid);		// [0-3] == [x,y,u,v] 
		gasgap = MpdDch::GetGasGap(uid);	// [0-1] == [inner,outer] 

		Rotate(proj, x = pPoint->GetX(), y = pPoint->GetY(), xRot, yRot); // GlobalToLocal
		driftLength = GetDriftLenght(proj, gasgap, xRot, wirePos); // [cm]
		
		R =  yRot; 
		Rphi = driftLength;
		pRandom->Rannor(dRphi,dR);
				
		if(fDoTest)
		{ 
			htWireN->Fill(WireID(uid, wirePos, R));
			htXYlocal->Fill(Rphi, R);
			htRvsR->Fill(sqrt(x*x + y*y), sqrt(R*R + wirePos*wirePos));
			htMCTime->Fill(pPoint->GetTime());
		}
				
/////		xRot = pRandom->Gaus(xRot, fRSigma/10.); // [cm]
	
/////		Rotate(proj, xRot, yRot, x, y, true); // back rotate
/////		pos.SetXYZ(x, y, pPoint->GetZ());
		
		pos.SetXYZ(pPoint->GetX(), pPoint->GetY(), pPoint->GetZ());
		dpos.SetXYZ(0,0,0); ////////// ???????
		
		pHit = AddHit(hitID, uid, pos, dpos, pPoint->GetTrackID(), pointIndex, 0);
		pHit->SetPhi(GetPhi(proj));
    		pHit->SetMeas(Rphi + dRphi * fRPhiSigma); 	// R-Phi
    		pHit->SetError(fRPhiSigma);
    		pHit->SetMeas(R + dR * fRSigma, 1);		// R
    		pHit->SetError(fRSigma, 1);
						
		pHit->SetDrift(driftLength);
		pHit->SetWirePosition(wirePos);
		pHit->SetTShift(pPoint->GetTime() + GetTShift(driftLength, wirePos, R, L));			
		pHit->SetWireDelay(L);
		
		fMapOccup.insert(occupMap::value_type(WireID(uid, wirePos, R), hitID++)); // <Hash(wirePos,UID) == cellID, index> pair

	} // <---Loop over the DCH points
	
	// Double-track resolution
	Int_t 		counter;
	Double_t 	timeDelta, gasDriftDelta;
	MpdDchHit 	*hit1, *hit2, *tmp;
	
	typedef map<Double_t, MpdDchHit*, __ltstr> 	delayMap; delayMap mapDelay; // <time delay, index> pair, (invert sorting)
	
	for(occupIter It = fMapOccup.begin(); It != fMapOccup.end(); )
	{
		counter = fMapOccup.count(It->first);	// hits in one cell
		if(fDoTest) htOccup->Fill(counter);
		
                if (counter == 1) 	++It;	// single hit (nothing to do)
		else if(counter == 2)		// double hit
		{
			
                        hit1 = (MpdDchHit*) pHitCollection->UncheckedAt(It->second); ++It; // slower hit
                        hit2 = (MpdDchHit*) pHitCollection->UncheckedAt(It->second); ++It; // faster hit
			timeDelta = hit1->GetTShift() - hit2->GetTShift(); // [ns]
			
			if(timeDelta < 0.) // swap
			{
				timeDelta *= -1.;
				tmp = hit1;
				hit1 = hit2;
				hit2 = tmp;			
			}
							
			if(fDoTest)
			{
				htTime->Fill(timeDelta);
				htPerp->Fill(wirePos = hit1->GetWirePosition());				
				htGasDrift->Fill(gasDriftDelta = TMath::Abs(hit1->GetDrift() - hit2->GetDrift()));																							
			}
				
			if(!HitExist(timeDelta)) // overlap
			{								
				hit2->AddLinks(hit1->GetLinks()); pHitCollection->Remove(hit1);  // index2 faster

				// ->SetFlag(warning#=2); // FIXME: define errorFlags enum				
			}
			else if(fDoTest)
			{
				htTimeA->Fill(timeDelta);
				htPerpA->Fill(wirePos);				
				htGasDriftA->Fill(gasDriftDelta);														
			}
		}
		else if(counter > 2)		// multiple hit
		{
			// update map
			mapDelay.clear();
			for(Int_t i = 0; i < counter; i++)
			{
				hit1 = (MpdDchHit*) pHitCollection->UncheckedAt(It->second);
				mapDelay.insert(delayMap::value_type(hit1->GetTShift(), hit1));
                                ++It;
			}
		
			 // Cycle from biggest to smallest time delay
			for(delayMap::iterator it = mapDelay.begin(); ; )
			{
				hit1 = it->second; 				// slower hit
                                ++it;
				
				if(it != mapDelay.end()) hit2 = it->second; 	// faster hit
				else break;		// last pair
				
				timeDelta = hit1->GetTShift() - hit2->GetTShift();
									
				if(fDoTest)
				{ 
					htTime->Fill(timeDelta);
					htPerp->Fill(wirePos = hit1->GetWirePosition());					
					htGasDrift->Fill(gasDriftDelta = TMath::Abs(hit1->GetDrift() - hit2->GetDrift()));	
				}	
				
				if(!HitExist(timeDelta)) // overlap, remove larger delay time
				{ 									
					hit2->AddLinks(hit1->GetLinks()); 
					pHitCollection->Remove(hit1); 
					
					// ->SetFlag(warning#=3); // FIXME: define errorFlags enum										
				} 
				else if(fDoTest) 
				{
					htTimeA->Fill(timeDelta);
					htPerpA->Fill(wirePos);										
					htGasDriftA->Fill(gasDriftDelta);										
				}
				
			} // cycle by mapDelay
		
				
		} // multiple hit		
	}

	pHitCollection->Compress();
	pHitCollection->Sort(); // in ascending order in abs(Z)
	
 	cout<<" "<<pHitCollection->GetEntriesFast()<<"("<<hitID<<") hits created.\n";
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdDchHitProducer::Finish()
{
	if(fDoTest)
    	{				
		TFile file("test.MpdDchHitProducer.root", "RECREATE");
					
		TH1D *h1 = (TH1D*) htTimeA->Clone("htTimeEff"); h1->Divide(htTime); h1->SetYTitle("Efficiency"); h1->Write();		
		h1 = (TH1D*) htGasDriftA->Clone("htGasDriftEff"); h1->Divide(htGasDrift); h1->SetYTitle("Efficiency"); h1->Write();
		h1 = (TH1D*) htPerpA->Clone("htPerpEff"); h1->Divide(htPerp); h1->SetYTitle("Efficiency"); h1->Write();		
								
      		fList.Write(); 	
      		file.Close();
    	}
}
//------------------------------------------------------------------------------------------------------------------------
MpdDchHit* MpdDchHitProducer::AddHit(Int_t index, Int_t detID, const TVector3& posHit, const TVector3& posHitErr, 
					Int_t trackIndex, Int_t pointIndex, Int_t flag)
{
	MpdDchHit *pHit	=  new ((*pHitCollection)[index]) MpdDchHit(detID, posHit, posHitErr, pointIndex, flag);
	pHit->AddLink(FairLink(1, pointIndex)); 
	pHit->AddLink(FairLink(2, trackIndex)); 
	
return pHit;
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdDchHitProducer)
