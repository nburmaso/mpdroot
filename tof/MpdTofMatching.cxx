//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTofMatching
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include "MpdEctKalmanTrack.h"
#include "MpdTofPoint.h"
#include "MpdTof.h"
#include "MpdTofMatching.h"

#include "FairRootManager.h"
#include "FairMCTrack.h"
#include "FairRunAna.h"
#include "FairLogger.h"

#include <TMath.h>
#include <TFile.h>
#include <TRandom3.h>
#include <TStorage.h>

#include <iostream>
#include <fstream>

#ifdef _OPENMP
#include "omp.h"
#include <sys/time.h>
#endif

using namespace std;

//------------------------------------------------------------------------------------------------------------------------
MpdTofMatchingData::MpdTofMatchingData(Int_t kfTrackId, Int_t tofHitId, const MpdTofHit* hit, Int_t pid, Int_t flag,
				Double_t length, const TVector3& est_point_R, const TVector3& est_point_Plane, 
				Double_t est_point_PlanePhi, Double_t est_point_PlaneTheta,  const TVector3& P, Int_t charge)
:   fKFTrackIndex(kfTrackId), fTofHitIndex(tofHitId), fPDGcode(pid), fFlag(flag), fLength(length)
{
	fPx = P.Px(); fPy = P.Py(); fPz = P.Pz(); fCharge = charge;
	fX = hit->GetX(); fY = hit->GetY(); fZ = hit->GetZ();
	fTime = hit->GetTime();
	fDetectorUID = hit->GetDetectorID();
	
	fEstPointR[0] = est_point_R.X(); fEstPointR[1] = est_point_R.Y(); fEstPointR[2] = est_point_R.Z();
	fTofImpactPoint[0] = est_point_Plane.X(); fTofImpactPoint[1] = est_point_Plane.Y(); fTofImpactPoint[2] = est_point_Plane.Z();
	fTofImpactPointPhi = est_point_PlanePhi; fTofImpactPointTheta = est_point_PlaneTheta;
	
	fBeta  = (fLength / 100.)  / (fTime  * 1.e-9) / TMath::C();// [cm/nc] -> m/c
	Double_t beta2  = fBeta*fBeta;	
	Double_t gamma2 = 1. / (1. - beta2);			
	Double_t Mom = GetMomentum().Mag();
	fMass2 = (Mom * Mom) / ( gamma2 * beta2 );		
} 
//------------------------------------------------------------------------------------------------------------------------
void 	MpdTofMatchingData::Print(void) const
{
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " PDGcode=%i, UID=%i, flag=%i, P(%f,%f,%f), length=%f", fPDGcode, fDetectorUID, fFlag, fPx, fPy, fPz, fLength);
}
//------------------------------------------------------------------------------------------------------------------------	
ClassImp(MpdTofMatchingData)
//------------------------------------------------------------------------------------------------------------------------
void		Convert2barrel(const TVector3& vec3, TVector2& vec2) // [mm]
{
	// projecting 3D point(X,Y,Z) to 2D(Z, XY) barrel surface point
	static const double R = 155.; // [cm]
	double angle =  TMath::ATan2(vec3.Y(), vec3.X()); // [-pi, pi]

	vec2.Set(vec3.Z(), angle * R); // XY = (angle / 2 pi )  * (2 pi R)
}
//------------------------------------------------------------------------------------------------------------------------
bool 	CheckSingularityCrossing(const TVector3& A, const TVector3& B)
{
	static const double Pi_2 = 3.14159265358979323846 / 2.;
	if(A.Phi()*B.Phi() < 0.)
	if(fabs(A.Phi()) > Pi_2  && fabs(B.Phi()) > Pi_2  )	return true;	// cross singularity

return false; // OK 
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofMatching::MpdTofMatching(const char *name, Int_t verbose, Bool_t test)
  : FairTask(name, verbose), fDoTest(test), fMCDataExist(false), fTestFlnm("test.MpdTofMatching.root"),  htCandNmb(NULL), fTofBarrelRadius(155.), // [cm]
    padGrid(NULL), moduleGrid(NULL)
{
	pMatchingFilter = new TofMatchingFilter;

        pKF = NULL, pTofHits = NULL, pMCTracks = NULL, pKFTracks = NULL, pKFectTracks = NULL, pMatchingCollection = NULL;
	
	if(fDoTest)
    	{
		htKfMc = MpdTofUtils::Make2D("htKfMc", 1000,0.,100., 1000, 0., 5., &fList, "est KF point on pad <-> TofPoint;#Delta, cm;P, GeV/c");
	
		hPads_Theta_Phi = MpdTofUtils::Make2D("pads_Theta_Phi", 1000, 0., 180., 1000, -190., 190., &fList, "Test, pad centers, Theta vs. Phi;#theta, degree;#phi, degree"); 	
		hTest_C_D = MpdTofUtils::Make2D("test_C_D", 1000, 0., 200., 10, -0.5, 9.5, &fList, ";#Delta, cm;k_side");	
		
		htTMatch = MpdTofUtils::Make2D("htTMatch", 1000, 0., 5., 1000, -5., 5., &fList, ";P, GeV/c;#eta");
		htMisMatch = MpdTofUtils::Make2D("htMisMatch", htTMatch, &fList);
		htKFTrack = MpdTofUtils::Make2D("htKFTrack", htTMatch, &fList);

		htTrackPerEvent = MpdTofUtils::Make2D("htTrackPerEvent", 1000, -0.5, 2999.5, 1000, -0.5,2999.5, &fList, "KF tracks vs KF tracks&point;N_{tracks};N_{tracks&point} ");		
		htCandNmb = MpdTofUtils::Make2D("htCandNmb", 100, -0.5, 99.5, 1000, -0.5, 999.5, &fList, "Number of candidate hits;N candidates;iteration");	
	}
        else
            htCandNmb = NULL, htTrackPerEvent = NULL,  htKfMc = NULL,  hPads_Theta_Phi = NULL, 
                hTest_C_D = NULL, htKFTrack = NULL, htTMatch = NULL, htMisMatch = NULL;
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofMatching::~MpdTofMatching()
{
    	delete pMatchingFilter;
	if(padGrid) delete padGrid;
	if(moduleGrid) delete moduleGrid;
}
//------------------------------------------------------------------------------------------------------------------------
InitStatus	  MpdTofMatching::Init()
{
  	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " <MpdTofMatching::Init> Begin etof matching initialization.");

	FairRootManager *ioman = FairRootManager::Instance(); assert(ioman);
  	pTofPoints = (TClonesArray*) ioman->GetObject("TOFPoint");
	pTofHits  = (TClonesArray*) ioman->GetObject("TOFHit");
  	pMCTracks   = (TClonesArray*) ioman->GetObject("MCTrack"); 
	pKFTracks   = (TClonesArray*) ioman->GetObject("TpcKalmanTrack"); 
	pKFectTracks   = (TClonesArray*) ioman->GetObject("EctTrack"); 
	if(pTofPoints && pMCTracks) fMCDataExist = true;
	
  	if(!pTofHits  || !pKFTracks){ FairLogger::GetLogger()->Error(MESSAGE_ORIGIN, "Branch not found!"); return kERROR; }

	// Read geo parameters 
	MpdTofUtils::ModMMap	mmapModules; 
	MpdTofUtils::PadMap 	mapPads;
	MpdTofUtils::RegVec	vecRegions;
	TString parFlnm = MpdTofUtils::GetTofParFlnm(fGeoFlNm);
  	if(!MpdTofUtils::ReadParamFromXML(parFlnm.Data(), vecRegions, mmapModules, mapPads)) // file don't exist
	{		
		if(!MpdTof::ParseTGeoManager(hTest_C_D, vecRegions, mmapModules, mapPads)) return kFATAL;
		MpdTofUtils::WriteParamToXML(parFlnm.Data(), "TOF_parameters", "The MPD TOF geo parameters.", vecRegions, mmapModules, mapPads);		
	}
	
	// create spatial grid
	padGrid = new TGridPad("padGrid", 0.04, 0.05, 2, true);
	moduleGrid = new TGridModule("moduleGrid", 1., 1., 2, true);	
	moduleGrid->SetYsizeTo2Pi(fTofBarrelRadius); // R, [cm]-> [mm]
		
	// Install module rectangles
	long UID; MpdTofUtils::modPar moduleData;
	TVector3  ReperModCenter, ReperModPerp;
	for(MpdTofUtils::ModIter iter = mmapModules.begin(), iterEnd = mmapModules.end(); iter != iterEnd; iter++)
	{
		moduleData = (iter->second);

		if( iter->first ==1 && moduleData.module ==12)
		{ 
			ReperModCenter = moduleData.center; // only REPER module processing
			ReperModPerp = moduleData.perp;
		}

		UID = moduleGrid->InstallRectangle(moduleData.point[0], moduleData.point[1], moduleData.point[2], moduleData.point[3], Convert2barrel, CheckSingularityCrossing);
		moduleGrid->ActivateRectangle(UID, moduleData);
	}

	// Install pad rectangles from one module
	long padID; MpdTofUtils::padPar *padData;
	for(MpdTofUtils::PadIter iter = mapPads.begin(), iterEnd = mapPads.end(); iter != iterEnd; iter++)
	{
		padData = &(iter->second);
		padID = padData->pad <<4;

		if(padData->region ==1 && padData->module == 12) 	// only REPER module pads installing
		padGrid->InstallRectangle(padData->point[0], padData->point[1], padData->point[2], padData->point[3], padID, Convert2barrel, CheckSingularityCrossing); 
	}
	
	// initialize grid
	padGrid->Init();
	moduleGrid->Init(); 

	// calc transform from reper module RF to Master RF 	
	TGeoHMatrix geoReper1;
	moduleGrid->Transform(&geoReper1, ReperModCenter, ReperModPerp);
	fGeoReper1_1 = geoReper1.Inverse();

	if(fDoTest)	
                for(MpdTofUtils::PadIter iter = mapPads.begin(); iter != mapPads.end(); ++iter) 	// cycle by pads
			hPads_Theta_Phi->Fill((&iter->second)->center.Theta() * TMath::RadToDeg(), (&iter->second)->center.Phi() * TMath::RadToDeg());
	
  	pKF = MpdKalmanFilter::Instance("KF","KF");
	
	// Create and register output array
  	pMatchingCollection = new TClonesArray("MpdTofMatchingData");
  	ioman->Register("TOFMatching", "Tof", pMatchingCollection, kTRUE);

return kSUCCESS;
}
//------------------------------------------------------------------------------------------------------------------------
void 		MpdTofMatching::Exec(Option_t *option)
{
 	const double tofMatchingThresh = 2.4; // 3 sigma, [cm] 
 
      //struct timeval tvStart, tvEnd;
        //struct timezone tz;
        //gettimeofday(&tvStart, &tz);

        pMatchingCollection->Delete();
	pMatchingFilter->Reset();
	
        Int_t nTofPoints = -1, nMCTracks = -1, selectedTracks  = 0;
	if(fMCDataExist){ nTofPoints = pTofPoints->GetEntriesFast(); nMCTracks = pMCTracks ->GetEntriesFast();}
	Int_t nTofHits = pTofHits->GetEntriesFast();  	
	Int_t nKFTracks = pKFTracks->GetEntriesFast();
	
	Int_t nKFectTracks = (pKFectTracks) ?  pKFectTracks->GetEntriesFast() : 0;
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " <MpdTofMatching::Exec> %i TofPoints, %i TofHits, %i MCTracks, %i KFTracks for this event.", nTofPoints, nTofHits, nMCTracks, nKFTracks);

	// Check clone tracks into ECT
	typedef set<Int_t>  trackSet;
	trackSet EctTrackSet; 
	
	if(pKFectTracks)
	for(Int_t index = 0; index < nKFectTracks; index++) // cycle by ECT KF tracks
	{
		MpdEctKalmanTrack *KfeTrack = (MpdEctKalmanTrack*) pKFectTracks->UncheckedAt(index);
		if(KfeTrack->IsFromTpc()) EctTrackSet.insert(trackSet::value_type(KfeTrack->GetTpcIndex()));
	}

	// Mapping points to MC tracks
	typedef multimap<Int_t, MpdTofPoint*> mapP2T; // pair< MCtrackID, MpdTofPoint*>
	mapP2T 			mapPoints2Tracks;
	if(fMCDataExist)
	{
		MpdTofPoint *mcTofPoint; mapP2T::iterator 	iterP2T;
		for(Int_t index = 0; index < nTofPoints; index++)  // cycle by MpdTofPoint
		{
			mcTofPoint = (MpdTofPoint*) pTofPoints->UncheckedAt(index);

			Int_t trackID = mcTofPoint->GetTrackID();
			Double_t time = mcTofPoint->GetTime();

			iterP2T = mapPoints2Tracks.find(trackID);
			if(iterP2T != mapPoints2Tracks.end()) // same trackID already inserted, insert to position (sorting by time)
			{
				int count = mapPoints2Tracks.count(trackID);
				for(int i = 0; i < count; i++, iterP2T++) // cycle by hits with same trackID
				{
 					if(time < iterP2T->second->GetTime())
					{
						mapPoints2Tracks.insert(iterP2T, mapP2T::value_type(trackID, mcTofPoint));
						break;	
					}

					if(i == count-1) mapPoints2Tracks.insert(++iterP2T, mapP2T::value_type(trackID, mcTofPoint)); // insert to last		
				}
			}
			else mapPoints2Tracks.insert(mapP2T::value_type(trackID, mcTofPoint));

		} // cycle by MpdTofPoint
	}

	// Activate pads by hits
	padGrid->ResetActivation();
	for(Int_t hitIndex = 0; hitIndex < nTofHits; hitIndex++ ) // cycle by TofHits
	{			
		MpdTofHit *TofHit = (MpdTofHit*) pTofHits->UncheckedAt(hitIndex);
		padGrid->ActivateRectangle(TofHit->GetDetectorID(), hitIndex);
	}

        #ifdef _OPENMP
            omp_lock_t coutLock;
            omp_init_lock(&coutLock);
        #endif

        #pragma omp parallel
	{
          bool 				DoMCTest;
          Int_t  				flag, mcTrackIndex,  mcPID = 0;
          TVector3 			div, hitPosition, estPointR, estPointPl, Momentum;
          MpdTpcKalmanTrack 	*KfTrack;
          MpdTofPoint 			*mcTofPoint;
          MpdTofHit 			*TofHit;
	
        #pragma omp for
        for(Int_t KfIndex = 0; KfIndex < nKFTracks; KfIndex++) 	// cycle by TPC KF tracks
	{   
		flag = 0;

		// check clone track into ECT	
		if(pKFectTracks)
		{	
			trackSet::iterator it = EctTrackSet.find(KfIndex);			
			if(it != EctTrackSet.end()) continue; // matching with ETof			
		}
		
		DoMCTest = fMCDataExist && fDoTest;
		KfTrack = (MpdTpcKalmanTrack*) pKFTracks->UncheckedAt(KfIndex);	
                estPointR = EstTrackOnR(KfTrack); 		// Estimate point on cylinder

		if(fMCDataExist)
		{
			mcTrackIndex = KfTrack->GetTrackID();
			FairMCTrack *mcTrack = (FairMCTrack*) pMCTracks->UncheckedAt(mcTrackIndex);
                        mcPID = mcTrack->GetPdgCode();	

			if(DoMCTest && mcTrack->GetNPoints(kTOF)) // KF track have TOFpoint 
			{
				selectedTracks++;
                		#pragma omp critical
                        	{
					htKFTrack->Fill(KfTrack->Momentum(), KfTrack->Momentum3().Eta());
                        	}
			}
		}

		MpdTpcKalmanTrack ReFittedTrack(RefitTrack(KfTrack));

		// Looking for modules	
		TGridModule::linksMAP 	moduleList; Double_t trackLength;  Int_t charge;
		if(moduleGrid->FindRectangle(estPointR, &moduleList, Convert2barrel, 0, 1,1, 1,1)) // true, if modules found
		{
			TGridModule::linksIter modIter; TGridPad::linksIter padIter; TGridPad::linksMAP padList;
			MpdTofUtils::modPar *moduleData; MpdTofUtils::padPar *padData;
			TVector3 localPos; long moduleID; Double_t delta;

			for(modIter = moduleList.begin(); modIter != moduleList.end(); modIter++) // cycle by selected modules
			{
				moduleData = &(modIter->second);
				moduleID = (moduleData->region<<24) + (moduleData->module<<14);

				if(EstTrackOnPlane(ReFittedTrack, moduleData->center, moduleData->perp, estPointPl, trackLength, Momentum, charge)) // Estimate point on plane; true, if point exist
				{
					if(DoMCTest)
					{
						mapP2T::iterator iter = mapPoints2Tracks.find(mcTrackIndex);
						if(iter != mapPoints2Tracks.end())
						if(moduleData->region == iter->second->GetRegion() && moduleData->module == iter->second->GetModule())
						{
							TVector3 pos; iter->second->Position(pos);
		                			#pragma omp critical
                            				{
								htKfMc->Fill((pos - estPointPl).Mag(), KfTrack->Momentum()); // mcTofPoint <-> est. KFtrack point 
							}
						}
					}

					// Calc. module transform matrix
					TGeoHMatrix geoReper2;
					moduleGrid->Transform(&geoReper2, moduleData->center, moduleData->perp); // transform =  translation * rotation, [cm]->[mm]
					TGeoHMatrix geoRES(geoReper2 * fGeoReper1_1); 

					// Transform the point to reper module
					moduleGrid->MasterToLocal(estPointPl, localPos, &geoRES ); // [cm]->[mm]

					// Looking for pads
					if(padGrid->FindRectangle(localPos, &padList, Convert2barrel, moduleID, 60,2, 60,2)) // true, if pads found
					{
						for(padIter = padList.begin(); padIter != padList.end(); padIter++) // cycle by selected pads
						{
							int  hitIndex = padIter->second;	
							TofHit = (MpdTofHit*) pTofHits->UncheckedAt(hitIndex);
							flag += TofHit->GetFlag();	
							TofHit->Position(hitPosition); div = hitPosition - estPointPl; delta = div.Mag();						
							pMatchingFilter->AddCandidate(mcTrackIndex, KfIndex, hitIndex, TofHit, mcPID, flag, trackLength, estPointR, estPointPl, Momentum, charge, delta);
						} // cycle by selected pads	
					}

				}
			} // cycle by selected modules
		}

	} // cycle by KF tracks
        } //pragma omp parallel

	if(fDoTest) htTrackPerEvent->Fill(nKFTracks, selectedTracks);
//pMatchingFilter->Dump();//debug

	Int_t MatchingOK = pMatchingFilter->Processing(nKFTracks, htCandNmb);
	pMatchingFilter->FillMatchings(this);

	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " <MpdTofMatching::Exec> MatchingOK = %i ", MatchingOK);	

        //gettimeofday(&tvEnd, &tz);
        //int dif = 1000000*((int)tvEnd.tv_sec - (int)tvStart.tv_sec) + (tvEnd.tv_usec - tvStart.tv_usec);
        //cout<<"TOFMatching::Exec "<<dif<<" ns\n";
}
//------------------------------------------------------------------------------------------------------------------------
void	MpdTofMatching::Finish()
{
	if(fDoTest)
    	{				
      		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " <MpdTofMatching::Finish> Update  %s file. ", fTestFlnm.Data());
		TFile *ptr = gFile;

		TFile file(fTestFlnm.Data(), "RECREATE");
      		fList.Write(); 
      		file.Close();

		padGrid->Write("test.pad.root");
		moduleGrid->Write("test.module.root");

		gFile = ptr;
    	}
}
//------------------------------------------------------------------------------------------------------------------------
void 	MpdTofMatching::AddEntry(Int_t index, Int_t kfTrackId, Int_t tofHitId, const MpdTofHit* hit, Int_t pid, Int_t flag,
				Double_t length, const TVector3& est_point_R, const TVector3& est_point_Plane, 
				Double_t est_point_PlanePhi, Double_t est_point_PlaneTheta,  const TVector3& Mom, Int_t charge)
{
	MpdTofMatchingData *data = new  ((*pMatchingCollection)[index]) MpdTofMatchingData(kfTrackId, tofHitId, hit, pid,  flag,
						length, est_point_R, est_point_Plane, est_point_PlanePhi, est_point_PlaneTheta, Mom, charge);
	
	if(fVerbose > 1) data->Print();
}
//------------------------------------------------------------------------------------------------------------------------
TVector3	MpdTofMatching::EstTrackOnR(const MpdTpcKalmanTrack *tr)const
{
	MpdKalmanHit hEnd; 
	hEnd.SetType(MpdKalmanHit::kFixedR);
	MpdTpcKalmanTrack tr1(*tr);
	TObjArray *hits = tr1.GetHits();
	
	if (hits->GetEntriesFast() == 0) 
	{
	  Int_t nh = tr1.GetNofTrHits();
	  for (Int_t j = 0; j < nh; ++j) hits->Add(tr1.GetTrHits()->UncheckedAt(j));
	  tr1.SetNofHits(nh);
	}
	
	tr1.SetParam(*tr1.GetParamAtHit());
        tr1.SetParamNew(*tr1.GetParamAtHit());
        tr1.SetWeight(*tr1.GetWeightAtHit());
        tr1.SetLength(tr1.GetLengAtHit());
        MpdKalmanHit *h = (MpdKalmanHit*) tr1.GetTrHits()->First();
        tr1.SetPos(tr1.GetPosAtHit());
        if (h->GetType() == MpdKalmanHit::kFixedZ) tr1.SetType(MpdKalmanTrack::kEndcap);
        tr1.SetPosNew(tr1.GetPos());

	hEnd.SetPos(fTofBarrelRadius); // barrel TOF radius, [cm]
	pKF->PropagateToHit(&tr1,&hEnd,kTRUE);

        TVector3 pos(tr1.GetPosNew(), 0.,0.);
	pos.SetPhi(tr1.GetParamNew(0)/tr1.GetPosNew());
	pos.SetZ(tr1.GetParamNew(1));

return pos;
}
//------------------------------------------------------------------------------------------------------------------------
MpdTpcKalmanTrack 		MpdTofMatching::RefitTrack(const MpdTpcKalmanTrack *tr)
{
	MpdTpcKalmanTrack tr1(*tr);
	tr1.SetDirection(MpdKalmanTrack::kOutward);
	TObjArray *hits = tr1.GetHits();
	
	if (hits->GetEntriesFast() == 0) 
	{
	  Int_t nh = tr1.GetNofTrHits();
	  for (Int_t j = 0; j < nh; ++j) hits->Add(tr1.GetTrHits()->UncheckedAt(j));
	  tr1.SetNofHits(nh);
	}
	
	tr1.SetParam(*tr1.GetParamAtHit());
        tr1.SetParamNew(*tr1.GetParamAtHit());
        tr1.SetWeight(*tr1.GetWeightAtHit());
        tr1.SetLength(tr1.GetLengAtHit());
        tr1.SetPos(tr1.GetPosAtHit());
        tr1.SetPosNew(tr1.GetPos());

return tr1;
}
//------------------------------------------------------------------------------------------------------------------------
bool		MpdTofMatching::EstTrackOnPlane(const MpdTpcKalmanTrack& tr, const TVector3& point, const TVector3& perp, 
				TVector3& pos, Double_t& length,  TVector3& Mom, Int_t& charge) const
{
	MpdTpcKalmanTrack tr1(tr);
	Double_t plane[6] = {point.X(), point.Y(), point.Z(),perp.X(), perp.Y(), perp.Z()};
	if (!pKF->PropagateParamP(&tr1,plane,kTRUE)) return false;

	Double_t R = tr1.GetPosNew(); 
	if(TMath::IsNaN(R)) return false; 

	length = tr1.GetLength();
	Mom = tr1.Momentum3();
	charge = tr1.Charge();
	
	pos.SetXYZ(R, 0., 0.);
	pos.SetPhi(tr1.GetParamNew(0)/R);
	pos.SetZ(tr1.GetParamNew(1));

return true;
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdTofMatching)
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
void	TofMatchingFilter::AddCandidate(Int_t mcTrackIndex, Int_t KfIndex, Int_t hitIndex, MpdTofHit *TofHit, Int_t mcPID, Int_t flag, 
	Double_t trackLength, TVector3 estPointR,  TVector3 estPointPl, TVector3 Momentum, Int_t charge, Double_t delta)
{										
	TofMatchingCandidate *ptr;	
	candIter iterC = InsertCand(KfIndex, ptr = new TofMatchingCandidate(mcTrackIndex, hitIndex, mcPID, flag, charge, Momentum, TofHit, delta, estPointR, estPointPl, trackLength));
	ptr->iterL = InsertLink(hitIndex, HitData(KfIndex, iterC, delta, -1., -1.));

//cout<<"\n   TofMatchingFilter::AddCandidate "<<ptr<<"  mcTrackIndex= "<<iterC->second->mcTrackIndex<<"  hitIndex="<< iterC->second->hitIndex
//<<" KfIndex= "<<ptr->iterL->second.KfIndex<<flush;
}
//------------------------------------------------------------------------------------------------------------------------
TofMatchingFilter::candIter TofMatchingFilter::FindClosestTrack(linkIter itLink, Int_t size, Double_t& minDelta)	
{
assert(size != 0); // debug
	candIter		retvalue = mmapCand.end(); // debug
	
	Int_t KfIndex;
	Double_t deltaR, deltaPhi, delta; minDelta = 1.e+10; // big value
	
	for(int i=0; i< size; i++) // cycle by concurrent
	{
		delta =  itLink->second.delta;	
		if(delta < minDelta){ retvalue = itLink->second.iterC; minDelta = delta; } // best (closest) candidate		
		++itLink;
	}
assert(retvalue != mmapCand.end()); // debug		
return retvalue;
}
//------------------------------------------------------------------------------------------------------------------------
TofMatchingFilter::candIter TofMatchingFilter::FindClosestHit(Int_t KfIndex)
{
	candIter  retCandIter = mmapCand.end();	// debug
	
	candIter itC1 = mmapCand.find(KfIndex);
	candIter itC2 = mmapCand.upper_bound(KfIndex);

	Double_t deltaR, deltaPhi, delta, minDelta = 1.e+10; // big value	
	for(candIter itC = itC1; itC != itC2; ++itC) // cycle by candidate hits
	{
		delta =((TofMatchingCandidate*) itC->second) ->delta;

		if(delta < minDelta) // best (closest) candidate
		{ 
			retCandIter = itC; 
			minDelta = delta; 	
		} 
	}
assert(retCandIter != mmapCand.end()); // debug
return retCandIter;
}		
//------------------------------------------------------------------------------------------------------------------------
Int_t 	TofMatchingFilter::Processing(Int_t nKFTracks, TH2D* h2)
{
//cout<<"\n  WWWW	TofMatchingFilter::Processing --------------------------------------------------------------"<<flush;

	RecreateCounterMaps(nKFTracks);
	Int_t MatchingOK = ProcessSimpleTracks(); 	// process only one hit simple tracks
	RecreateCounterMaps(nKFTracks); 		// update CounterMaps after ProcessSimpleTracks

	TofMatchingCandidate 	*pMatchCand;
	Int_t counter, iterNmb = 0;	
	counterIter iter;
	
newIteration:
	iterNmb++;

	if(iterNmb > 5000){ FairLogger::GetLogger()->Warning(MESSAGE_ORIGIN, " <TofMatchingFilter::Processing> Too many tries."); goto end; }
	
	if(h2) // fDoTest == true
	{
		for(int nCand = 0; nCand < 100; nCand++)
			if(mmapCounters.find(nCand) != mmapCounters.end()) h2->Fill(nCand, iterNmb);	
	}
	
	// ----------->> Processing tracks with one candidate 
	iter = mmapCounters.find(1);
	if(iter != mmapCounters.end())				// exist
	{
		Int_t KfIndex = iter->second;		

		candIter itCand = mmapCand.find(KfIndex);	// exist one entry
assert(itCand != mmapCand.end()); // debug

		Int_t hitIndex = itCand->second->hitIndex;
		int conNmb  = mmapLinks.count(hitIndex);		// number of concurrent track
		linkIter itLink = mmapLinks.find(hitIndex);		// iter to first track
assert(itLink != mmapLinks.end()); // debug
//cout<<"\n  WWWWT 1 "<<iterNmb<<" track="<<KfIndex<<" hit="<<hitIndex<<"   conNmb="<<conNmb;
			
		if(conNmb == 1)		AcceptCandidate(itCand);	// no concurrent track			
		else if(conNmb>1)
		{
			Double_t delta;
			itCand = FindClosestTrack(itLink, conNmb, delta); 
			AcceptCandidate(itCand);
		}	
	
		goto newIteration;	// try new iteration after map entries erased
	
	} // <<<----------- Processing tracks with one candidate 	
	
	// ----------->> Processing tracks with two candidates 	
	iter = mmapCounters.find(2);
	if(iter != mmapCounters.end())				// exist	
	{
		Int_t KfIndex = iter->second;
		candIter itCand1 = mmapCand.find(KfIndex);
assert(itCand1 != mmapCand.end()); // debug
		Int_t hitIndex = itCand1->second->hitIndex;				// first candidate
		int conNmb  = mmapLinks.count(hitIndex);				// number of concurrent track
		linkIter itLink = mmapLinks.find(hitIndex);				// iter to first track
assert(itLink != mmapLinks.end()); // debug		
//cout<<"\n  WWWWT 2 "<<iterNmb<<" track="<<KfIndex<<" hit="<<hitIndex;
							
		Double_t delta1 = -1., delta2 = -1.;
		candIter itClosest1, itClosest2;
		if(conNmb>0) itClosest1 = FindClosestTrack(itLink, conNmb, delta1); 
			
		candIter itCand2 = itCand1; itCand2++;
		hitIndex = itCand2->second->hitIndex;					// second candidate		
		conNmb  = mmapLinks.count(hitIndex);				// number of concurrent track
		itLink = mmapLinks.find(hitIndex);						// iter to first track
//cout<<" hit2="<<hitIndex;
			
		if(conNmb>0) itClosest2 = FindClosestTrack(itLink, conNmb, delta2); 

//Commit();		
//		cout<<"\n  delta1="<<delta1<<"  delta2="<<delta2;
		if(delta1 > 0. &&  delta2 > 0.)	// both exist
		{
			if(delta1 <  delta2)	AcceptCandidate(itClosest1); 
			else 			AcceptCandidate(itClosest2); 		 
		}
		else if(delta1 > 0.) AcceptCandidate(itClosest1); // only 1 exist
		else if(delta2 > 0.) AcceptCandidate(itClosest2); // only 2 exist
//Status();						
		goto newIteration;	// try new iteration after map entries erased			
	} // <<<----------- Processing tracks with two candidates
	
	// ----------->> Processing tracks with 3 candidates 
	iter = mmapCounters.find(3);
	if(iter != mmapCounters.end())					
	{
//cout<<"\n  WWWWT 3 "<<iterNmb<<" "<<iter->second;
		Int_t KfIndex = iter->second;
		AcceptCandidate(FindClosestHit(KfIndex)); 
		goto newIteration;		
	} 
	// <<<----------- Processing tracks with 3 candidates 
	
	iter = mmapCounters.find(4);
	if(iter != mmapCounters.end())					
	{
//cout<<"\n  WWWWT 4 "<<iterNmb<<" "<<iter->second;
		Int_t KfIndex = iter->second;	
		AcceptCandidate(FindClosestHit(KfIndex)); 
		goto newIteration;		
	} 

	iter = mmapCounters.upper_bound(4);
	if(iter != mmapCounters.end())					
	{
//cout<<"\n  WWWWT > 4 "<<iterNmb<<" "<<iter->second;
		Int_t KfIndex = iter->second;	
		AcceptCandidate(FindClosestHit(KfIndex)); 
		goto newIteration;		
	}
	
end:	
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " <TofMatchingFilter::Processing> Finished with  %i iterations.", iterNmb);		
	
return 	mmapAccepted.size();
}	
//------------------------------------------------------------------------------------------------------------------------
void	TofMatchingFilter::FillMatchings(MpdTofMatching* ptr)
{
	Int_t entryID = 0;
	TofMatchingCandidate 	*pMatchCand;
	for(candIter iter = mmapAccepted.begin(); iter != mmapAccepted.end(); ++iter)
	{
	//	pMatchCand = dynamic_cast<TofMatchingCandidate*> ( iter->second ); if(pMatchCand == NULL) continue;
		pMatchCand = (TofMatchingCandidate*) iter->second;
		
		// add Entry to collection		
		ptr->AddEntry(entryID++, iter->first, pMatchCand->hitIndex, pMatchCand->TofHit, pMatchCand->mcPID, pMatchCand->flag, 
			pMatchCand->trackLength, pMatchCand->estPointR, pMatchCand->estPointPl, 
			0., 0., pMatchCand->Momentum, pMatchCand->charge);
		
		if(ptr->fDoTest)			
		if(pMatchCand->TofHit->CheckTrackID(pMatchCand->mcTrackIndex))	 // TRUE Matching		
			ptr->htTMatch->Fill(pMatchCand->Momentum.Mag(), pMatchCand->Momentum.Eta());	
		else 
			ptr->htMisMatch->Fill(pMatchCand->Momentum.Mag(), pMatchCand->Momentum.Eta());						
	}
}
//------------------------------------------------------------------------------------------------------------------------	
