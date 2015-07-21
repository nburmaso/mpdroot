#include <iostream>
#include <algorithm>
#include <limits>

#include <TGeoManager.h>
#include <TClonesArray.h>
#include <TVirtualMC.h>
#include <TGeoManager.h>
#include <TGeoBBox.h>
#include <TGeoMatrix.h>

#include "FairGeoInterface.h"
#include "FairGeoLoader.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "FairRootManager.h"
#include "FairStack.h"
#include "FairRuntimeDb.h"
#include "FairRunAna.h"
#include "FairVolume.h"

#include "MpdEtofGeo.h"
#include "MpdEtofPoint.h"
#include "MpdEtofGeoPar.h"

#include "MpdEtof.h"

ClassImp(MpdEtof)

MpdTof::MStripType		MpdEtof::mStrips;
MpdTof::intervalTreeType		MpdEtof::mDetectorsR;
MpdTof::intervalTreeType		MpdEtof::mDetectorsPhi;
//------------------------------------------------------------------------------------------------------------------------
MpdEtof::MpdEtof(const char* name, Bool_t active)
 : FairDetector(name, active),  nan(std::numeric_limits<double>::quiet_NaN())
{  
	aTofHits = new TClonesArray("MpdEtofPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdEtof::~MpdEtof() 
{
	if(aTofHits){ aTofHits->Delete(); delete aTofHits; }
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t  MpdEtof::ProcessHits(FairVolume* vol)
{ 
	// Set parameters at entrance of volume. Reset ELoss.
	if(gMC->IsTrackEntering()) 
	{
		fELoss  = 0.;
		fTime   = gMC->TrackTime() * 1.0e09;
		fLength = gMC->TrackLength();
		gMC->TrackPosition(fPos);
		gMC->TrackMomentum(fMom);		
	}

	// Sum energy loss for all steps in the active volume
	fELoss += gMC->Edep();
	
	// Create MpdTofPoint at exit of active volume
	if(fELoss > 0 &&  (gMC->IsTrackExiting() || gMC->IsTrackStop() || gMC->IsTrackDisappeared()) ) 
	{	
		fTrackID = gMC->GetStack()->GetCurrentTrackNumber();
		Int_t  region, module, pad;
				
		TString padname = vol->GetName(); padname.Remove(0, 7); pad = padname.Atoi();//gMC->CurrentVolOffID(1, pad);
		gMC->CurrentVolOffID(2, module);
		gMC->CurrentVolOffID(3, region);					
		fVolumeID = MpdEtofPoint::GetVolumeUID(region, module, pad);
//cout<<"\n UID: "<<region<<" "<<module<<" "<<pad<<"   "<<fVolumeID<<" "<<vol->GetName();
		
		AddPoint(fTrackID, fVolumeID, TVector3(fPos.X(),  fPos.Y(),  fPos.Z()), TVector3(fMom.Px(), fMom.Py(), fMom.Pz()), fTime, fLength, fELoss);

		((FairStack*)gMC->GetStack())->AddPoint(kETOF);

    		ResetParameters();
  	}

return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdEtof::EndOfEvent() 
{
	if(fVerboseLevel) Print();
  	aTofHits->Delete();
  	fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdEtof::Register(){ FairRootManager::Instance()->Register("ETOFPoint", "Etof", aTofHits, kTRUE); }
//------------------------------------------------------------------------------------------------------------------------
TClonesArray* MpdEtof::GetCollection(Int_t iColl) const 
{
	if(iColl == 0) 	return aTofHits;
	
return NULL;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdEtof::Print() const 
{
	Int_t nHits = aTofHits->GetEntriesFast();
	cout << "-I- MpdEtof: " << nHits << " points registered in this event." << endl;
	
	if(fVerboseLevel > 1) for(Int_t i=0; i<nHits; i++) (*aTofHits)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------
void MpdEtof::Reset(){ aTofHits->Delete(); ResetParameters(); }
//------------------------------------------------------------------------------------------------------------------------
void MpdEtof::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
	Int_t nEntries = cl1->GetEntriesFast();
	cout << "-I- MpdEtof: " << nEntries << " entries to add." << endl;
	TClonesArray& clref = *cl2;
	MpdEtofPoint* oldpoint = NULL;
	
	for(Int_t i=0; i<nEntries; i++) 
	{
		oldpoint = (MpdEtofPoint*) cl1->At(i);
		Int_t index = oldpoint->GetTrackID() + offset;
		oldpoint->SetTrackID(index);
		new (clref[fPosIndex]) MpdEtofPoint(*oldpoint);
		fPosIndex++;
	}
	
	cout << "-I- MpdEtof: " << cl2->GetEntriesFast() << " merged entries."  << endl;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdEtof::ConstructGeometry() 
{  
        FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
        FairGeoInterface* geoFace = geoLoad->getGeoInterface();
	MpdEtofGeo*       tofGeo  = new MpdEtofGeo();
	tofGeo->setGeomFile(GetGeometryFileName());
	geoFace->addGeoModule(tofGeo);

	Bool_t rc = geoFace->readSet(tofGeo);
	if(rc) tofGeo->create(geoLoad->getGeoBuilder());
	TList* volList = tofGeo->getListOfVolumes();

	// store geo parameter
        FairRun *fRun = FairRun::Instance();
        FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
	MpdEtofGeoPar* par =(MpdEtofGeoPar*)(rtdb->getContainer("MpdEtofGeoPar"));
	TObjArray *fSensNodes = par->GetGeoSensitiveNodes();
	TObjArray *fPassNodes = par->GetGeoPassiveNodes();

        FairGeoNode *node   = NULL;
        FairGeoVolume *aVol = NULL;
	TListIter iter(volList);
	
        while((node = (FairGeoNode*)iter.Next()))
	{
                aVol = dynamic_cast<FairGeoVolume*> (node);
		if(node->isSensitive()) 	fSensNodes->AddLast(aVol); 
		else           		 	fPassNodes->AddLast(aVol);      
  	}
	
	par->setChanged();
	par->setInputVersion(fRun->GetRunId(), 1);  
	ProcessNodes(volList);	
}
//------------------------------------------------------------------------------------------------------------------------
MpdEtofPoint* MpdEtof::AddPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t time, Double_t length, Double_t eLoss) 
{
	return new((*aTofHits)[aTofHits->GetEntriesFast()]) MpdEtofPoint(trackID, detID, pos, mom, time, length, eLoss);
}
//------------------------------------------------------------------------------------------------------------------------
void			MpdEtof::ParseTGeoManager(TH2D *h2TestStrips, bool forced)
{
static const double degree180 = 3.14159265359; // 180 degree to rad

assert(gGeoManager);

	if( !forced &&  !mStrips.empty()) return; // already parsed and filled

	mStrips.clear();

	TString pathTOF = "/cave_1/etof1_1"; 
	Int_t regionID = 1;
	int nModules = 0, nStrips = 0, nGases = 0;

	vector<MpdTof::intervalType> vDetectorsR, vDetectorsPhi;

goNextSide:	
	
{	 // REGIONS
	gGeoManager->cd(pathTOF);
	
	TGeoNode *moduleNode, *stripNode, *gasNode;
	TGeoMatrix *matrix;
	TGeoBBox *box;
	LStrip	*pStrip;
	
	Int_t volumeUID, moduleID, stripID, gasID; // module[1,...,30], pad [1,...,131]

	Double_t *X0Y0Z0 = new Double_t[3]; X0Y0Z0[0] = X0Y0Z0[1] = X0Y0Z0[2] = 0.; // center of sensetive detector
	Double_t  *local = new Double_t[3], master[3],  dX, dY, dZ;
	
  	TObjArray *array = gGeoManager->GetCurrentVolume()->GetNodes();
  	TIterator *it1 = array->MakeIterator(); 	
  		  	
  	while( (moduleNode = (TGeoNode*) it1->Next()) )		// MODULES	
    	{
    		TString PATH1 = pathTOF + "/" + moduleNode->GetName(); moduleID = moduleNode->GetNumber(); nModules++;
		///cout<<"\n MODULE: "<<moduleNode->GetName()<<", copy# "<<moduleID<<" path= "<<PATH1.Data();
    		
    		gGeoManager->cd(PATH1);	
    		
    		TGeoMatrix *matrix = gGeoManager->GetCurrentMatrix();	// calculate global TGeoHMatrix for current node
		matrix->LocalToMaster(X0Y0Z0, master);			// 0.0.0. --> MRS
 
 		TGeoBBox *box = (TGeoBBox*) gGeoManager->GetCurrentNode()->GetVolume()->GetShape(); 		
		dX = box->GetDX(); dY = box->GetDY(); dZ = box->GetDZ();
				   		
    		TVector3 A,B,C,D; double Rmin = 1.e+10, Rmax = -1.e+10, Phimin = 1.e+10, Phimax = -1.e+10;
			
		// point A
		local[0] = -dX;	local[1] = -dY; local[2] = +dZ;
		matrix->LocalToMaster(local, master);
		A.SetXYZ(master[0], master[1], master[2]);
		MpdTof::CalcMinMax(A.Perp(), A.Phi(), Rmin, Rmax, Phimin, Phimax);						
				
		// point B
		local[0] = +dX;	local[1] = -dY; local[2] = +dZ;
		matrix->LocalToMaster(local, master);
		B.SetXYZ(master[0], master[1], master[2]);
		MpdTof::CalcMinMax(B.Perp(), B.Phi(), Rmin, Rmax, Phimin, Phimax);	
				
		// point C
		local[0] = +dX;	local[1] = -dY; local[2] = -dZ;
		matrix->LocalToMaster(local, master);
		C.SetXYZ(master[0], master[1], master[2]);	
		MpdTof::CalcMinMax(C.Perp(), C.Phi(), Rmin, Rmax, Phimin, Phimax);	
								
		// point D
		local[0] = -dX;	local[1] = -dY; local[2] = -dZ;
		matrix->LocalToMaster(local, master);
		D.SetXYZ(master[0], master[1], master[2]);
		MpdTof::CalcMinMax(D.Perp(), D.Phi(), Rmin, Rmax, Phimin, Phimax);	
		
		if(h2TestStrips)
		{
			h2TestStrips->Fill(A.Perp(), A.Phi(), 33.);
			h2TestStrips->Fill(B.Perp(), B.Phi(), 44.);	
			h2TestStrips->Fill(C.Perp(), C.Phi(), 55.);
			h2TestStrips->Fill(D.Perp(), D.Phi(), 66.);
		}
		
		// Add Detector location interval to intervalTree
		volumeUID = MpdEtofPoint::GetVolumeUID(regionID, moduleID, 0); // volumeUID for 0th strip of the selected detector
		LRectangle * det = new LRectangle(volumeUID, A, B, C, D);
		det->InitCenterPerp();
				
		vDetectorsR.push_back(MpdTof::intervalType(Rmin, Rmax, det));
				
		if(Phimax - Phimin > degree180) // segment overlap the boundary [-180.,+180[
		{
			vDetectorsPhi.push_back(MpdTof::intervalType(-degree180, Phimin, det));
			vDetectorsPhi.push_back(MpdTof::intervalType(Phimax,  degree180, det));
		 	///cout<<"\n OVERLAPED   phimin="<<Phimin<<", phimax="<<Phimax;			
		}
		else 	vDetectorsPhi.push_back(MpdTof::intervalType(Phimin, Phimax, det));	
    		
    	    	TIterator *it2 = moduleNode->GetNodes()->MakeIterator(); 		
      		while( (stripNode = (TGeoNode*) it2->Next()) )		// STRIPS
		{
	 		TString stripName = stripNode->GetName(); 		
	 		TString PATH2 = PATH1 + "/" + stripName; nStrips++;	 		
	 		TString stripid = stripName; stripid.Remove(0, 7); stripid.Remove(stripid.Last('_')); stripID = stripid.Atoi();	 		
	  		///cout<<"\n\t PAD: "<<stripName.Data()<<", copy# "<<stripID<<" path= "<<PATH2.Data();
	 		
	  		TIterator *it3 = stripNode->GetNodes()->MakeIterator();
	  		while( (gasNode = (TGeoNode*) it3->Next()) )	// GASES
	    		{
	    			TString detName = gasNode->GetName();
	    			if(!detName.Contains("gas")) continue;
	    			
	    			TString PATH3 = PATH2  + "/" + detName;  nGases++;
				TString gasid = stripName; gasid.Remove(0, 7); gasid.Remove(gasid.Last('_')); gasID = gasid.Atoi();	 		    			
				///cout<<"\n\t\t GAS: "<<detName.Data()<<", copy# "<<gasID<<" path= "<<PATH3.Data();   
				
				gGeoManager->cd(PATH3);	
				
				TGeoMatrix *matrix = gGeoManager->GetCurrentMatrix();	// calculate global TGeoHMatrix for current node
				matrix->LocalToMaster(X0Y0Z0, master);			// 0.0.0. --> MRS			
															
				TGeoBBox *box = (TGeoBBox*) gGeoManager->GetCurrentNode()->GetVolume()->GetShape(); 		
				dX = box->GetDX(); dY = box->GetDY(); dZ = box->GetDZ();
					
				volumeUID = MpdEtofPoint::GetVolumeUID(regionID, moduleID, stripID);
				///cout<<"\n uid="<<volumeUID<<", center= "<<master[0]<<", "<<master[1]<<", "<<master[2]<<", dXYZ= "<<dX<<", "<<dY<<", "<<dZ;
				
				LStrip stripData(volumeUID, regionID, moduleID, stripID, 0);
				stripData.center.SetXYZ(master[0], master[1], master[2]);
								
				double Rmin = 1.e+10, Rmax = -1.e+10, Phimin = 1.e+10, Phimax = -1.e+10;
			
				// point A
				local[0] = -dX;	local[1] = -dY; local[2] = +dZ;
				matrix->LocalToMaster(local, master);
				stripData.A.SetXYZ(master[0], master[1], master[2]);
				MpdTof::CalcMinMax(stripData.A.Perp(), stripData.A.Phi(), Rmin, Rmax, Phimin, Phimax);						
				
				// point B
				local[0] = +dX;	local[1] = -dY; local[2] = +dZ;
				matrix->LocalToMaster(local, master);
				stripData.B.SetXYZ(master[0], master[1], master[2]);
				MpdTof::CalcMinMax(stripData.B.Perp(), stripData.B.Phi(), Rmin, Rmax, Phimin, Phimax);	
				
				// point C
				local[0] = +dX;	local[1] = -dY; local[2] = -dZ;
				matrix->LocalToMaster(local, master);
				stripData.C.SetXYZ(master[0], master[1], master[2]);	
				MpdTof::CalcMinMax(stripData.C.Perp(), stripData.C.Phi(), Rmin, Rmax, Phimin, Phimax);	
								
				// point D
				local[0] = -dX;	local[1] = -dY; local[2] = -dZ;
				matrix->LocalToMaster(local, master);
				stripData.D.SetXYZ(master[0], master[1], master[2]);
				MpdTof::CalcMinMax(stripData.D.Perp(), stripData.D.Phi(), Rmin, Rmax, Phimin, Phimax);					
				
				stripData.InitCenterPerp();
				
				if(h2TestStrips)
				{
					h2TestStrips->Fill(stripData.A.Perp(), stripData.A.Phi(), 1.);
					h2TestStrips->Fill(stripData.B.Perp(), stripData.B.Phi(), 4.);	
					h2TestStrips->Fill(stripData.C.Perp(), stripData.C.Phi(), 7.);
					h2TestStrips->Fill(stripData.D.Perp(), stripData.D.Phi(), 9.);					
				}													
///stripData.Dump("\n ETOFstrip:");	
				bool IsUniqueUID = mStrips.insert(make_pair(volumeUID, stripData)).second;
assert(IsUniqueUID);		
	    		} // GASES  		
	  		
	  	} // STRIPS 	
	  	
    	} // MODULES
 
} // REGIONS

 	if(pathTOF.EqualTo("/cave_1/etof1_1"))
 	{
 		pathTOF = "/cave_1/etof1_2";
 		regionID = 2;
 		goto goNextSide;
 	}
    	
    	mDetectorsR = MpdTof::intervalTreeType(vDetectorsR);
    	mDetectorsPhi = MpdTof::intervalTreeType(vDetectorsPhi);
    	  	
    	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdEtof::ParseTGeoManager] Regions= %d, modules= %d, strips= %d. ", 2, nModules, nStrips);
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdEtof::FindNeighborStrips(TH1D *h1, TH2D *h2, bool doTest)
{ 
	const LStrip *strip2; double  distance;
	for(MpdTof::MStripIT it = mStrips.begin(), itEnd = mStrips.end(); it != itEnd ; it++) // cycle1 by strips
	{
		LStrip *strip1 = &(it->second);
		
		for(MpdTof::MStripCIT it = mStrips.begin(), itEnd = mStrips.end(); it != itEnd ; it++) // cycle2 by strips
		{
			strip2 = &(it->second);
	
			// CATION: Ckeck  only left and right sides(one row strips NOW) 
			distance = strip1->Distance(LStrip::kRight, *strip2); if(doTest)  h1->Fill(distance);		
			if(distance < 0.8) // CAUTION: constant depends on the geometry layout(see h1TestDistance histo)
			{
			 	strip1->neighboring[LStrip::kRight] = strip2->volumeUID;
			 	if(doTest) h2->Fill(strip1->stripID, strip2->stripID);
			}
//////	cout<<"\n  RIGHT distance= "<<	distance;	MAYBE  Neighbor Strips located up at down( NOT left&right)

			distance = strip1->Distance(LStrip::kLeft, *strip2); if(doTest)  h1->Fill(distance);
			if(distance < 0.8) // CAUTION: constant depends on the geometry layout(see h1TestDistance histo)
			{
				strip1->neighboring[LStrip::kLeft] = strip2->volumeUID;
				if(doTest) h2->Fill( strip2->stripID, strip1->stripID);	
			}			
//////cout<<"\n  LEFT distance= "<<	distance;
		}// cycle2 by strips	
	}// cycle1 by strips
}
//------------------------------------------------------------------------------------------------------------------------
const LStrip*		MpdEtof::FindStrip(Int_t UID) 
{
	MpdTof::MStripCIT cit = mStrips.find(UID);

assert(cit != mStrips.end());

return &(cit->second);
}
//------------------------------------------------------------------------------------------------------------------------



