
//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTof, LRectangle, LStrip
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include <iostream>
#include <algorithm>
#include <limits>

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

#include "MpdTofGeo.h"
#include "MpdTofGeoPar.h"
#include "MpdTofPoint.h"

#include "MpdTof.h"

ClassImp(MpdTof)

MpdTof::MStripType		MpdTof::mStrips;
MpdTof::intervalTreeType		MpdTof::mDetectorsZ;
MpdTof::intervalTreeType		MpdTof::mDetectorsPhi;
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
MpdTof::MpdTof(const char* name, Bool_t active)
 : FairDetector(name, active), nan(std::numeric_limits<double>::quiet_NaN())
{  
	aTofHits = new TClonesArray("MpdTofPoint");
	fPosIndex = 0;
	fVerboseLevel = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdTof::~MpdTof() 
{
	if(aTofHits){ aTofHits->Delete(); delete aTofHits; }
}
//------------------------------------------------------------------------------------------------------------------------
Bool_t  MpdTof::ProcessHits(FairVolume* vol)
{
	Int_t  strip, detector, box, sector; 

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
		
		gMC->CurrentVolOffID(1, strip);
		gMC->CurrentVolOffID(2, detector);
		gMC->CurrentVolOffID(3, box);
		gMC->CurrentVolOffID(4, sector);		
					
		fVolumeID = MpdTofPoint::GetVolumeUID(sector, box, detector, strip);

		AddPoint(fTrackID, fVolumeID, TVector3(fPos.X(),  fPos.Y(),  fPos.Z()), TVector3(fMom.Px(), fMom.Py(), fMom.Pz()), fTime, fLength, fELoss);

		((FairStack*)gMC->GetStack())->AddPoint(kTOF);

    		ResetParameters();
  	}

return kTRUE;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::EndOfEvent() 
{
	if(fVerboseLevel) Print();
  	aTofHits->Delete();
  	fPosIndex = 0;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::Register(){ FairRootManager::Instance()->Register("TOFPoint", "Tof", aTofHits, kTRUE); }
//------------------------------------------------------------------------------------------------------------------------
TClonesArray* MpdTof::GetCollection(Int_t iColl) const 
{
	if(iColl == 0) 	return aTofHits;
	
return nullptr;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::Print() const 
{
	Int_t nHits = aTofHits->GetEntriesFast();
	cout << "-I- MpdTof: " << nHits << " points registered in this event." << endl;
	
	if(fVerboseLevel > 1)
    		for(Int_t i=0; i<nHits; i++) (*aTofHits)[i]->Print();
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::Reset(){ aTofHits->Delete(); ResetParameters(); }
//------------------------------------------------------------------------------------------------------------------------
void MpdTof::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
	Int_t nEntries = cl1->GetEntriesFast();
	cout << "-I- MpdTof: " << nEntries << " entries to add." << endl;
	TClonesArray& clref = *cl2;
	MpdTofPoint* oldpoint = nullptr;
	
	for(Int_t i=0; i<nEntries; i++) 
	{
		oldpoint = (MpdTofPoint*) cl1->At(i);
		Int_t index = oldpoint->GetTrackID() + offset;
		oldpoint->SetTrackID(index);
		new (clref[fPosIndex]) MpdTofPoint(*oldpoint);
		fPosIndex++;
	}
	
	cout << "-I- MpdTof: " << cl2->GetEntriesFast() << " merged entries."  << endl;
}
//--------------------------------------------------------------------------------------------------------------------------------------
void 			MpdTof::ConstructGeometry() 
{
	TString fileName = GetGeometryFileName();
	if(fileName.EndsWith(".root")) 
	{
		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "Constructing TOF geometry from ROOT file %s", fileName.Data());
		ConstructRootGeometry();
	}
	else if ( fileName.EndsWith(".geo") ) 
	{
		FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "Constructing TOF geometry from ASCII file %s", fileName.Data());
		ConstructAsciiGeometry();
	}
	else	FairLogger::GetLogger()->Fatal(MESSAGE_ORIGIN, "Geometry format of TOF file %S not supported.", fileName.Data());    
}
//------------------------------------------------------------------------------------------------------------------------
void 	MpdTof::ConstructAsciiGeometry() 
{
        FairGeoLoader*    geoLoad = FairGeoLoader::Instance();
        FairGeoInterface* geoFace = geoLoad->getGeoInterface();
	MpdTofGeo*       tofGeo  = new MpdTofGeo();
	tofGeo->setGeomFile(GetGeometryFileName());
	geoFace->addGeoModule(tofGeo);

	Bool_t rc = geoFace->readSet(tofGeo);
	if(rc) tofGeo->create(geoLoad->getGeoBuilder());
	TList* volList = tofGeo->getListOfVolumes();

	// store geo parameter

        FairRuntimeDb *rtdb = FairRun::Instance()->GetRuntimeDb();
	MpdTofGeoPar* par =(MpdTofGeoPar*)(rtdb->getContainer("MpdTofGeoPar"));
	TObjArray *fSensNodes = par->GetGeoSensitiveNodes();
	TObjArray *fPassNodes = par->GetGeoPassiveNodes();

        FairGeoNode *node   = nullptr;
        FairGeoVolume *aVol = nullptr;
	TListIter iter(volList);			
	
        while((node = (FairGeoNode*)iter.Next()))
	{
                aVol = dynamic_cast<FairGeoVolume*> (node);       
                
		if(node->isSensitive()) 	fSensNodes->AddLast(aVol); 
		else           		 	fPassNodes->AddLast(aVol);	
  	}

	par->setChanged();
	par->setInputVersion(FairRun::Instance()->GetRunId(), 1);  
	ProcessNodes(volList);
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofPoint* 		MpdTof::AddPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t time, Double_t length, Double_t eLoss) 
{	
	return new((*aTofHits)[aTofHits->GetEntriesFast()]) MpdTofPoint(trackID, detID, pos, mom, time, length, eLoss);
}
//------------------------------------------------------------------------------------------------------------------------
void			MpdTof::CalcMinMax(double A, double B, double& minA, double& maxA, double& minB, double& maxB) 
{
	///cout<<"\n\t A="<<A<<", B="<<B<<", minA="<<minA<<", maxA="<<maxA<<", minB="<<minB<<", maxB="<<maxB;
	minA = (A < minA) ? A : minA;
	maxA = (A > maxA) ? A : maxA;
	minB = (B < minB) ? B : minB;
	maxB = (B > maxB) ? B : maxB;
}
//------------------------------------------------------------------------------------------------------------------------
void			MpdTof::ParseTGeoManager(TH2D *h2TestStrips, bool forced)
{
static const double degree180 = 3.14159265359; // 180 degree to rad

assert(gGeoManager);

	if( !forced &&  !mStrips.empty()) return; // already parsed and filled

	mStrips.clear();

	TString pathTOF = "/cave_1/tof1_0", stripName = "/tof1StripActiveGasV_1";
	gGeoManager->cd(pathTOF);
	
	TGeoNode *sectorNode, *boxNode, *detectorNode, *stripBoxNode, *stripNode;
	TGeoMatrix *matrix;
	TGeoBBox *box;
	Int_t volumeUID, sectorID, boxID, detectorID, stripID; // strip[1,...,24], detector [1,...,6], box [1,...,2], sector [1,...,24]

	Double_t *X0Y0Z0 = new Double_t[3]; X0Y0Z0[0] = X0Y0Z0[1] = X0Y0Z0[2] = 0.; // center of sensetive detector
	Double_t  *local = new Double_t[3], master[3],  dX, dY, dZ;
	
  	TObjArray *array = gGeoManager->GetCurrentVolume()->GetNodes();
  	TIterator *it1 = array->MakeIterator(); int nSectors = 0, nBoxs = 0, nDetectors = 0, nStrips = 0;	
  	
  	vector<intervalType> vDetectorsZ, vDetectorsPhi;
  	
  	while( (sectorNode = (TGeoNode*) it1->Next()) )			// SECTORS vSector_#, vZBox_#, DetectorBoxV_# +, StripBoxV_#, StripActiveGasV_# +
    	{
    		TString PATH1 = pathTOF + "/" + sectorNode->GetName(); sectorID = sectorNode->GetNumber(); nSectors++;
		///cout<<"\n SECTOR: "<<sectorNode->GetName()<<", copy# "<<sectorID<<" path= "<<PATH1.Data();
    	
    	    	TIterator *it2 = sectorNode->GetNodes()->MakeIterator(); 		
      		while( (boxNode = (TGeoNode*) it2->Next()) )		// BOXES
		{
	 		TString boxName = boxNode->GetName();
	 		if(!boxName.Contains("Box")) continue;
	 		
	 		TString PATH2 = PATH1 + "/" + boxName; boxID = boxNode->GetNumber(); nBoxs++;
	  		///cout<<"\n\t BOX: "<<boxName.Data()<<", copy# "<<boxID<<" path= "<<PATH2.Data();
	  		
	  		TIterator *it3 = boxNode->GetNodes()->MakeIterator();
	  		while( (detectorNode = (TGeoNode*) it3->Next()) )	// DETECTORS
	    		{
	    			TString detName = detectorNode->GetName();
	    			if(!detName.Contains("Detector")) continue;
	    			
	    			TString PATH3 = PATH2  + "/" + detName; detectorID = detectorNode->GetNumber(); nDetectors++;
				///cout<<"\n\t\t DETECTOR: "<<detName.Data()<<", copy# "<<detectorID<<" path= "<<PATH3.Data();   
				
				gGeoManager->cd(PATH3);	
				
				TGeoMatrix *matrix = gGeoManager->GetCurrentMatrix();	// calculate global TGeoHMatrix for current node
				matrix->LocalToMaster(X0Y0Z0, master);			// 0.0.0. --> MRS			
															
				TGeoBBox *box = (TGeoBBox*) gGeoManager->GetCurrentNode()->GetVolume()->GetShape(); 		
				dX = box->GetDX(); dY = box->GetDY(); dZ = box->GetDZ();	
				
				TVector3 A,B,C,D; double Zmin = 1.e+10, Zmax = -1.e+10, Phimin = 1.e+10, Phimax = -1.e+10;
			
				// point A
				local[0] = -dX;	local[1] = -dY +1.3; local[2] = +dZ;
				matrix->LocalToMaster(local, master);
				A.SetXYZ(master[0], master[1], master[2]);
				CalcMinMax(master[2], A.Phi(), Zmin, Zmax, Phimin, Phimax);						
				
				// point B
				local[0] = +dX;	local[1] = -dY +1.3; local[2] = +dZ;
				matrix->LocalToMaster(local, master);
				B.SetXYZ(master[0], master[1], master[2]);
				CalcMinMax(master[2], B.Phi(), Zmin, Zmax, Phimin, Phimax);	
				
				// point C
				local[0] = +dX;	local[1] = -dY +1.3; local[2] = -dZ;
				matrix->LocalToMaster(local, master);
				C.SetXYZ(master[0], master[1], master[2]);	
				CalcMinMax(master[2], C.Phi(), Zmin, Zmax, Phimin, Phimax);	
								
				// point D
				local[0] = -dX;	local[1] = -dY +1.3; local[2] = -dZ;
				matrix->LocalToMaster(local, master);
				D.SetXYZ(master[0], master[1], master[2]);
				CalcMinMax(master[2], D.Phi(), Zmin, Zmax, Phimin, Phimax);					
					
				if(h2TestStrips)
				{
					h2TestStrips->Fill(A.Z(), A.Phi(), 33.);
					h2TestStrips->Fill(B.Z(), B.Phi(), 44.);	
					h2TestStrips->Fill(C.Z(), C.Phi(), 55.);
					h2TestStrips->Fill(D.Z(), D.Phi(), 66.);
				}
							
				// Add Detector location interval to intervalTree
				volumeUID = MpdTofPoint::GetVolumeUID(sectorID, boxID, detectorID, 0); // volumeUID for 0th strip of the selected detector
				LRectangle * det = new LRectangle(volumeUID, A, B, C, D);
				det->InitCenterPerp();
				
				vDetectorsZ.push_back(intervalType(Zmin, Zmax, det));
				
				if(Phimax - Phimin > degree180) // segment overlap the boundary [-180.,+180[
				{
					vDetectorsPhi.push_back(intervalType(-degree180, Phimin, det));
				 	vDetectorsPhi.push_back(intervalType(Phimax,  degree180, det));
				 	///cout<<"\n OVERLAPED   phimin="<<Phimin<<", phimax="<<Phimax;			
				}
				else vDetectorsPhi.push_back(intervalType(Phimin, Phimax, det));	
								
				TIterator *it4 = detectorNode->GetNodes()->MakeIterator();
	  			while( (stripBoxNode = (TGeoNode*) it4->Next()) )	// STRIPS
	    			{
	    				TString stripBoxName = stripBoxNode->GetName();
	    				
	    				TString PATH4 = PATH3  + "/" + stripBoxName; stripID = stripBoxNode->GetNumber(); nStrips++;
					///cout<<"\n\t\t\t STRIP: "<<stripBoxName.Data()<<", copy# "<<stripID<<" path= "<<PATH4.Data();   
					
					TString PATHtoStrip = PATH4  + stripName;
					gGeoManager->cd(PATHtoStrip);				// cd to strip sensitive gas node
					
		    			TGeoMatrix *matrix = gGeoManager->GetCurrentMatrix();	// calculate global TGeoHMatrix for current node
					matrix->LocalToMaster(X0Y0Z0, master);			// 0.0.0. --> MRS			
															
					TGeoBBox *box = (TGeoBBox*) gGeoManager->GetCurrentNode()->GetVolume()->GetShape(); 		
					dX = box->GetDX(); dY = box->GetDY(); dZ = box->GetDZ();				
					
					volumeUID = MpdTofPoint::GetVolumeUID(sectorID, boxID, detectorID, stripID);
					///cout<<"\n uid="<<volumeUID<<", center= "<<master[0]<<", "<<master[1]<<", "<<master[2]<<", dXYZ= "<<dX<<", "<<dY<<", "<<dZ;	
					///cout<<"\n sectorID= "<<sectorID<<", boxID= "<<boxID<<", detectorID= "<<detectorID<<", stripID= "<<stripID;
									
							
					LStrip stripData(volumeUID, sectorID, boxID, detectorID, stripID);
					stripData.center.SetXYZ(master[0], master[1], master[2]);
					
					// edges on the front plate of the strips. perp along Y.
					local[0] = -dX;	local[1] = -dY; local[2] = +dZ;
					matrix->LocalToMaster(local, master);
					stripData.A.SetXYZ(master[0], master[1], master[2]);
					
					local[0] = +dX;	local[1] = -dY; local[2] = +dZ;
					matrix->LocalToMaster(local, master);
					stripData.B.SetXYZ(master[0], master[1], master[2]);	
					
					local[0] = +dX;	local[1] = -dY; local[2] = -dZ;
					matrix->LocalToMaster(local, master);
					stripData.C.SetXYZ(master[0], master[1], master[2]);				
									
					local[0] = -dX;	local[1] = -dY; local[2] = -dZ;
					matrix->LocalToMaster(local, master);
					stripData.D.SetXYZ(master[0], master[1], master[2]);
					
					stripData.InitCenterPerp();
					
					if(h2TestStrips)
					{
						h2TestStrips->Fill(stripData.A.Z(), stripData.A.Phi(), 1.);
						h2TestStrips->Fill(stripData.B.Z(), stripData.B.Phi(), 4.);	
						h2TestStrips->Fill(stripData.C.Z(), stripData.C.Phi(), 7.);
						h2TestStrips->Fill(stripData.D.Z(), stripData.D.Phi(), 9.);
					}
///stripData.Dump("\n strip:");		
					bool IsUniqueUID = mStrips.insert(make_pair(volumeUID, stripData)).second;
assert(IsUniqueUID);			
											
	    			} // STRIPS
	    				
	    		} // DETECTORS  		
	  		
	  	} // BOXES 	
	  	
    	} // SECTORS

    	mDetectorsZ = intervalTreeType(vDetectorsZ); 
    	mDetectorsPhi = intervalTreeType(vDetectorsPhi);
    	
    	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdTof::ParseTGeoManager] Sectors= %d, boxes= %d, detectors= %d, strips= %d. ", nSectors, nBoxs, nDetectors, nStrips);
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdTof::FindNeighborStrips(TH1D *h1, TH2D *h2, bool doTest)
{ 
	const LStrip *strip2; double  distance;
	for(MStripIT it = mStrips.begin(), itEnd = mStrips.end(); it != itEnd ; it++) // cycle1 by strips
	{
		LStrip *strip1 = &(it->second);
		
		for(MStripCIT it = mStrips.begin(), itEnd = mStrips.end(); it != itEnd ; it++) // cycle2 by strips
		{
			strip2 = &(it->second);
	
			// CATION: Ckeck  only left and right sides(one row strips NOW) 
			distance = strip1->Distance(LStrip::kRight, *strip2); if(doTest)  h1->Fill(distance);		
			if(distance < 0.8) // CAUTION: constant depends on the geometry layout(see h1TestDistance histo)
			{
			 	strip1->neighboring[LStrip::kRight] = strip2->volumeUID;
			 	if(doTest) h2->Fill(strip1->stripID, strip2->stripID);
			}
			
			distance = strip1->Distance(LStrip::kLeft, *strip2); if(doTest)  h1->Fill(distance);
			if(distance < 0.8) // CAUTION: constant depends on the geometry layout(see h1TestDistance histo)
			{
				strip1->neighboring[LStrip::kLeft] = strip2->volumeUID;
				if(doTest) h2->Fill( strip2->stripID, strip1->stripID);	
			}			

		}// cycle2 by strips	
	}// cycle1 by strips
}
//------------------------------------------------------------------------------------------------------------------------
const LStrip*		MpdTof::FindStrip(Int_t UID) 
{
	MStripCIT cit = mStrips.find(UID);

assert(cit != mStrips.end());

return &(cit->second);
}
//--------------------------------------------------------------------------------------------------------------------------------------
Bool_t 			MpdTof::CheckIfSensitive(std::string name)
{
  TString tsname = name;
  if (tsname.Contains("Active")) return kTRUE;
  
return kFALSE;
}
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
LRectangle::LRectangle(Int_t uid, const TVector3& a, const TVector3& b, const TVector3& c, const TVector3& d, bool check) 
: IsInvalid(false), volumeUID(uid), A(a), B(b), C(c), D(d)
{
	if(check) CheckInValid();

}
//------------------------------------------------------------------------------------------------------------------------
Double_t 	LRectangle::DistanceFromPointToLine(const TVector3* pos, const TVector3& P1,const TVector3& P2)const
{
assert(P1 != P2);

return   (  (*pos - P1).Cross(*pos - P2)   ).Mag() / (P2 - P1).Mag();
}
//------------------------------------------------------------------------------------------------------------------------
Double_t 	LRectangle::DistanceFromPointToLineSegment(const TVector3* pos, const TVector3& P1,const TVector3& P2)const
{
assert(P1 != P2);

	TVector3 v = P2 - P1;
	TVector3 w = (*pos) - P1;
	
	double c1 = w.Dot(v);
	if( c1 <= 0 )	return w.Mag();

	double c2 = v.Dot(v);
    	if( c2 <= c1 ) return ((*pos) - P2).Mag();
    	    	
    	TVector3 Pb = P1 + (c1/c2) * v;
return ((*pos) - Pb).Mag();
}
//------------------------------------------------------------------------------------------------------------------------
Double_t		LRectangle::MinDistanceToEdge(const TVector3* pos, Side_t& side) const
{	
	double right 	= DistanceFromPointToLineSegment(pos, A, B);
	double left 	= DistanceFromPointToLineSegment(pos, C, D);
	
	// sorting & return minimal value
	if( right <= left )
	{ 
		side =  LStrip::kRight; 
		return right;
	}		
	 
	side =  LStrip::kLeft;  
return left;
}
//------------------------------------------------------------------------------------------------------------------------
void 		LRectangle::Print(ostream &out, const TVector3 &point, const char* comment)const
{
	if(comment) out<<comment; 
	out<<" ("<< point.X()<<","<<point.Y()<<","<<point.Z()<<") "; 
}
//------------------------------------------------------------------------------------------------------------------------
void 		LRectangle::Dump(const char* comment, ostream& out) const 
{ 
	if(comment) out<<comment; out<<" uid="<<volumeUID<<" IsInvalid="<<IsInvalid;
	Print(out, A, " A:"); Print(out, B, " B:"); Print(out, C, " C:"); Print(out, D, " D:");
}
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
LStrip::LStrip() 
: LRectangle(), sectorID(kInvalid), boxID(kInvalid),  detectorID(kInvalid), stripID(kInvalid) 
{ 
	neighboring[kRight] = kInvalid; 
	neighboring[kLeft] = kInvalid; 
}
//------------------------------------------------------------------------------------------------------------------------
LStrip::LStrip(Int_t uid, Int_t sector, Int_t box, Int_t detector, Int_t strip) 
 : LRectangle(), sectorID(sector), boxID(box),  detectorID(detector), stripID(strip) 
{ 
	volumeUID = uid;
	neighboring[kRight] = kInvalid; 
	neighboring[kLeft] = kInvalid; 
}	
//------------------------------------------------------------------------------------------------------------------------
void		LStrip::Dump(const char* comment, ostream& out) const 
{ 	
	if(comment) out<<comment; 
	out<<"  ids: "<<sectorID<<", "<<boxID<<", "<<detectorID<<", "<<stripID; 
	
	LRectangle::Dump(nullptr, out);
}
//------------------------------------------------------------------------------------------------------------------------	
Double_t 	LStrip::Distance(Side_t side, const LStrip& strip)
{
	Double_t value, min1 = 1.e+10, min2 = 1.e+10; // big value
	
	if((*this) == strip) 		return min1+min2; // same strip
	if(!IsSameDetector(strip)) 	return min1+min2; // different modules
		
	TVector3 *p1, *p2;
	switch(side)
	{
		case kRight: 	p1 = &A; p2 = &B; break;	
		case kLeft: 	p1 = &C; p2 = &D; break;				
	};

	value 	= fabs((*p1 - strip.A).Mag());	min1 = (value < min1) ? value : min1; 
	value 	= fabs((*p1 - strip.B).Mag());	min1 = (value < min1) ? value : min1; 	
	value 	= fabs((*p1 - strip.C).Mag());	min1 = (value < min1) ? value : min1; 		
	value 	= fabs((*p1 - strip.D).Mag());	min1 = (value < min1) ? value : min1; 
	
	value 	= fabs((*p2 - strip.A).Mag());	min2 = (value < min2) ? value : min2;		 
	value 	= fabs((*p2 - strip.B).Mag());	min2 = (value < min2) ? value : min2; 
	value 	= fabs((*p2 - strip.C).Mag());	min2 = (value < min2) ? value : min2;	
	value 	= fabs((*p2 - strip.D).Mag());	min2 = (value < min2) ? value : min2;
	
return min1 + min2;
}	
//------------------------------------------------------------------------------------------------------------------------


