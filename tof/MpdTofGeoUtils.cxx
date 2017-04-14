//------------------------------------------------------------------------------------------------------------------------
#include <assert.h>
#include <iostream>

#include <TGeoManager.h>
#include "TClonesArray.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TGeoMatrix.h"
#include "TGeoBBox.h"
#include "TGeoNode.h"

#include "FairRootManager.h"
#include "FairLogger.h"

#include "MpdTofPoint.h"
#include "MpdTofGeoUtils.h"

MpdTofGeoUtils*	MpdTofGeoUtils::instance = nullptr;
//------------------------------------------------------------------------------------------------------------------------
MpdTofGeoUtils::MpdTofGeoUtils()
{

}
//------------------------------------------------------------------------------------------------------------------------
void		MpdTofGeoUtils::FindNeighborStrips(Double_t thresh, TH1D* h1, TH2D* h2, bool forced)
{
	static bool founded = false;	
	if( !forced &&  founded) return; // neighbor strips already founded and filled
	
	size_t NR = 0, NL= 0;
	const LStrip *strip2; double  distance;
	
	for(MStripIT it1 = mStrips.begin(), itEnd1 = mStrips.end(); it1 != itEnd1; it1++) // cycle1 by strips
	{
		LStrip *strip1 = &(it1->second);
		
		for(MStripCIT it2 = mStrips.begin(), itEnd2 = mStrips.end(); it2 != itEnd2; it2++) // cycle2 by strips
		{
			strip2 = &(it2->second);
	
			// CATION: Ckeck  only left and right sides(one row strips NOW) 
			distance = strip1->Distance(LStrip::kRight, *strip2); 
			if(h1)  h1->Fill(distance);		
			if(distance < thresh) 
			{
			 	strip1->neighboring[LStrip::kRight] = strip2->volumeUID; NR++;
			 	if(h2) h2->Fill(strip1->stripID, strip2->stripID);
			}
			
			distance = strip1->Distance(LStrip::kLeft, *strip2); 
			if(h1)  h1->Fill(distance);
			if(distance < thresh) 
			{
				strip1->neighboring[LStrip::kLeft] = strip2->volumeUID; NL++;
				if(h2) h2->Fill( strip2->stripID, strip1->stripID);	
			}			

		}// cycle2 by strips	
		
	}// cycle1 by strips
	
	founded = true;
	
	cout<<" [MpdTofGeoUtils::FindNeighborStrips] Neighbor strips: left = "<<NL<<", right = "<<NR<<endl;
}
//------------------------------------------------------------------------------------------------------------------------	
void		MpdTofGeoUtils::ParseTGeoManager(bool useMCinput, TH2D* h2, bool forced)
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
	  		
	  		TIterator *it3 = boxNode->GetDaughter(0)->GetNodes()->MakeIterator();
	  		while( (detectorNode = (TGeoNode*) it3->Next()) )	// DETECTORS
	    		{
	    			TString detName = detectorNode->GetName();
	    			if(!detName.Contains("Detector")) continue;
	    			
	    			TString PATH3 = PATH2  + "/vSectorAir_1/" + detName; detectorID = detectorNode->GetNumber(); nDetectors++;
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
					
				if(h2)
				{
					h2->Fill(A.Z(), A.Phi());
					h2->Fill(B.Z(), B.Phi());	
					h2->Fill(C.Z(), C.Phi());
					h2->Fill(D.Z(), D.Phi());
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
	    				if(!stripBoxName.Contains("Strip")) continue;
	    				
	    				TString PATH4 = PATH3  + "/" + stripBoxName; stripID = stripBoxNode->GetNumber(); nStrips++;
					///cout<<"\n\t\t\t STRIP: "<<stripBoxName.Data()<<", copy# "<<stripID<<" path= "<<PATH4.Data();   
					
					gGeoManager->cd(PATH4);				// cd to strip sensitive gas node
					
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
					
					if(h2)
					{
						h2->Fill(stripData.A.Z(), stripData.A.Phi());
						h2->Fill(stripData.B.Z(), stripData.B.Phi());	
						h2->Fill(stripData.C.Z(), stripData.C.Phi());
						h2->Fill(stripData.D.Z(), stripData.D.Phi());
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
const LStrip*		MpdTofGeoUtils::FindStrip(Int_t UID) 
{
	MStripCIT cit = mStrips.find(UID);
assert(cit != mStrips.end());
return &(cit->second);
}
//------------------------------------------------------------------------------------------------------------------------
void			MpdTofGeoUtils::CalcMinMax(double A, double B, double& minA, double& maxA, double& minB, double& maxB) 
{
	///cout<<"\n\t A="<<A<<", B="<<B<<", minA="<<minA<<", maxA="<<maxA<<", minB="<<minB<<", maxB="<<maxB;
	minA = (A < minA) ? A : minA;
	maxA = (A > maxA) ? A : maxA;
	minB = (B < minB) ? B : minB;
	maxB = (B > maxB) ? B : maxB;
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
void		LRectangle::GetRPhiRanges(Double_t& Rmin, Double_t& Rmax, Double_t& PhiMin, Double_t& PhiMax)
{
	Rmin = PhiMin = 1.e+10;
	Rmax = PhiMax = -1.e+10;

	getRPhi(A, Rmin, Rmax, PhiMin, PhiMax); // edge
	getRPhi(B, Rmin, Rmax, PhiMin, PhiMax);
	getRPhi(C, Rmin, Rmax, PhiMin, PhiMax);	
	getRPhi(D, Rmin, Rmax, PhiMin, PhiMax);	
	
	getRPhi((A+B)*0.5, Rmin, Rmax, PhiMin, PhiMax); // middle of side
	getRPhi((B+C)*0.5, Rmin, Rmax, PhiMin, PhiMax);
	getRPhi((C+D)*0.5, Rmin, Rmax, PhiMin, PhiMax);	
	getRPhi((D+A)*0.5, Rmin, Rmax, PhiMin, PhiMax);	
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


