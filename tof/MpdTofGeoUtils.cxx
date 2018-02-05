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
	static bool found = false;	
	if( !forced &&  found) return; // neighbor strips already found and filled
	
	size_t NR = 0, NL= 0;
	
	for(auto& it1 : mStrips) // cycle1 by strips
	{
		LStrip *strip1 = &(it1.second);
		
		for(const auto& it2 : mStrips) // cycle2 by strips
		{
			const LStrip *strip2 = &(it2.second);

			if(strip1 == strip2) continue; // skip cross-reference
	
			// CATION: Ckeck  only left and right sides(one row strips NOW) 
			double distance = strip1->Distance(LStrip::kRight, *strip2); 
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
	
	found = true;
	
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
	Int_t volumeUID, sectorID, boxID, detectorID, stripID; // strip[1,...,24], detector [1,...,6], box [1,...,2], sector [1,...,24]

	Double_t *X0Y0Z0 = new Double_t[3]; X0Y0Z0[0] = X0Y0Z0[1] = X0Y0Z0[2] = 0.; // center of sensetive detector
	Double_t  *local = new Double_t[3], master[3],  dX, dY, dZ;
	
  	TObjArray *array = gGeoManager->GetCurrentVolume()->GetNodes();
  	auto  it1 = array->MakeIterator(); int nSectors = 0, nBoxs = 0, nDetectors = 0, nStrips = 0;	
  	
  	vector<intervalType> vDetectorsZ, vDetectorsPhi;
   	vector<intervalType> vStripsZ, vStripsPhi; 	
  	
  	while( (sectorNode = (TGeoNode*) it1->Next()) )			// SECTORS vSector_#, vZBox_#, DetectorBoxV_# +, StripBoxV_#, StripActiveGasV_# +
    	{
    		TString PATH1 = pathTOF + "/" + sectorNode->GetName(); sectorID = sectorNode->GetNumber(); nSectors++;
		///cout<<"\n SECTOR: "<<sectorNode->GetName()<<", copy# "<<sectorID<<" path= "<<PATH1.Data();
    	
    	    	auto it2 = sectorNode->GetNodes()->MakeIterator(); 		
      		while( (boxNode = (TGeoNode*) it2->Next()) )		// BOXES
		{
	 		TString boxName = boxNode->GetName();
	 		if(!boxName.Contains("Box")) continue;
	 		
	 		TString PATH2 = PATH1 + "/" + boxName; boxID = boxNode->GetNumber(); nBoxs++;
	  		///cout<<"\n\t BOX: "<<boxName.Data()<<", copy# "<<boxID<<" path= "<<PATH2.Data();
	  		
	  		auto it3 = boxNode->GetDaughter(0)->GetNodes()->MakeIterator();
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
				
				TVector3 A,B,C,D; 
				auto Z_range = make_pair(1.e+10, -1.e+10), Phi_range(Z_range);
			
				local[0] = -dX;	local[1] = -dY +1.3; local[2] = +dZ;
				localToMaster(matrix, local, A, Z_range, Phi_range);

				local[0] = +dX;	local[1] = -dY +1.3; local[2] = +dZ;
				localToMaster(matrix, local, B, Z_range, Phi_range);	

				local[0] = +dX;	local[1] = -dY +1.3; local[2] = -dZ;
				localToMaster(matrix, local, C, Z_range, Phi_range);	

				local[0] = -dX;	local[1] = -dY +1.3; local[2] = -dZ;
				localToMaster(matrix, local, D, Z_range, Phi_range);					
					
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
				
				vDetectorsZ.push_back(intervalType(Z_range.first, Z_range.second, det));
				
				if(Phi_range.second - Phi_range.first > degree180) // segment overlap the boundary [-180.,+180[
				{
					vDetectorsPhi.push_back(intervalType(-degree180,  Phi_range.first, det));
				 	vDetectorsPhi.push_back(intervalType(Phi_range.second,  degree180, det));
				 	cout<<"\n OVERLAPED   phimin="<<Phi_range.first<<", phimax="<<Phi_range.second;			
				}
				else vDetectorsPhi.push_back(intervalType(Phi_range.first, Phi_range.second, det));	
								
				auto it4 = detectorNode->GetNodes()->MakeIterator();
	  			while( (stripBoxNode = (TGeoNode*) it4->Next()) )	// STRIPS
	    			{
	    				TString stripBoxName = stripBoxNode->GetName();
	    				if(!stripBoxName.Contains("Strip")) continue;
	    				
	    				TString PATH4 = PATH3  + "/" + stripBoxName; stripID = stripBoxNode->GetNumber(); nStrips++;
					///cout<<"\n\t\t\t STRIP: "<<stripBoxName.Data()<<", copy# "<<stripID<<" path= "<<PATH4.Data();   
					
					gGeoManager->cd(PATH4);				// cd to strip sensitive gas node
					
		    			TGeoMatrix *matrix4 = gGeoManager->GetCurrentMatrix();	// calculate global TGeoHMatrix for current node
					matrix4->LocalToMaster(X0Y0Z0, master);			// 0.0.0. --> MRS			
															
					TGeoBBox *box4 = (TGeoBBox*) gGeoManager->GetCurrentNode()->GetVolume()->GetShape(); 		
					dX = box4->GetDX(); dY = box4->GetDY(); dZ = box4->GetDZ();				
					
					volumeUID = MpdTofPoint::GetVolumeUID(sectorID, boxID, detectorID, stripID);
					///cout<<"\n uid="<<volumeUID<<", center= "<<master[0]<<", "<<master[1]<<", "<<master[2]<<", dXYZ= "<<dX<<", "<<dY<<", "<<dZ;	
					///cout<<"\n sectorID= "<<sectorID<<", boxID= "<<boxID<<", detectorID= "<<detectorID<<", stripID= "<<stripID;
														
					LStrip stripData(volumeUID, sectorID, boxID, detectorID, stripID);
					stripData.center.SetXYZ(master[0], master[1], master[2]);

					auto sZ_range = make_pair(1.e+10, -1.e+10), sPhi_range(sZ_range);
					
					// edges on the front plate of the strips. perp along Y.
					local[0] = -dX;	local[1] = -dY; local[2] = +dZ;
					localToMaster(matrix4, local, stripData.A, sZ_range, sPhi_range);

					local[0] = +dX;	local[1] = -dY; local[2] = +dZ;
					localToMaster(matrix4, local, stripData.B, sZ_range, sPhi_range);	
					
					local[0] = +dX;	local[1] = -dY; local[2] = -dZ;
					localToMaster(matrix4, local, stripData.C, sZ_range, sPhi_range);				
									
					local[0] = -dX;	local[1] = -dY; local[2] = -dZ;
					localToMaster(matrix4, local, stripData.D, sZ_range, sPhi_range);
					
					stripData.InitCenterPerp();
					
					if(h2)
					{
						h2->Fill(stripData.A.Z(), stripData.A.Phi());
						h2->Fill(stripData.B.Z(), stripData.B.Phi());	
						h2->Fill(stripData.C.Z(), stripData.C.Phi());
						h2->Fill(stripData.D.Z(), stripData.D.Phi());
					}
					auto pStrip = new LStrip(stripData);
					
					vStripsZ.push_back(intervalType(sZ_range.first, sZ_range.second, pStrip));
				
					if(sPhi_range.second - sPhi_range.first > degree180) // segment overlap the boundary [-180.,+180[
					{
						vStripsPhi.push_back(intervalType(-degree180, sPhi_range.first, pStrip));
				 		vStripsPhi.push_back(intervalType(sPhi_range.second,  degree180, pStrip));
				 		cout<<"\n OVERLAPED strip  phimin="<<sPhi_range.first<<", phimax="<<sPhi_range.second;			
					}
					else vStripsPhi.push_back(intervalType(sPhi_range.first, sPhi_range.second, pStrip));
									
///stripData.Dump("\n strip:");		
					bool IsUniqueUID = mStrips.insert(make_pair(volumeUID, stripData)).second;
assert(IsUniqueUID);			
											
	    			} // STRIPS
	    				
	    		} // DETECTORS  		
	  		
	  	} // BOXES 	
	  	
    	} // SECTORS

    	mStripsZ = intervalTreeType(vStripsZ); 
    	mStripsPhi = intervalTreeType(vStripsPhi);
    	
      	mDetectorsZ = intervalTreeType(vDetectorsZ); 
    	mDetectorsPhi = intervalTreeType(vDetectorsPhi);  	
    	
    	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, "[MpdTof::ParseTGeoManager] Sectors= %d, boxes= %d, detectors= %d, strips= %d. ", nSectors, nBoxs, nDetectors, nStrips);
}
//------------------------------------------------------------------------------------------------------------------------
void 	MpdTofGeoUtils::localToMaster(const TGeoMatrix *matrix, Double_t* local, TVector3& position, pair<double, double>& Z, pair<double,double>& Phi)const
{
	Double_t  master[3];	
	matrix->LocalToMaster(local, master);
	position.SetXYZ(master[0], master[1], master[2]);

	Z.first  = std::min(position.Z(), Z.first);
	Z.second = std::max(position.Z(), Z.second);

	Phi.first  = std::min(position.Phi(), Phi.first);
	Phi.second = std::max(position.Phi(), Phi.second);
}
//------------------------------------------------------------------------------------------------------------------------
const LStrip*		MpdTofGeoUtils::FindStrip(Int_t UID) 
{
	auto cit = mStrips.find(UID);
assert(cit != mStrips.end());
return &(cit->second);
}
//------------------------------------------------------------------------------------------------------------------------
bool			MpdTofGeoUtils::GetStripListForDetector(Int_t detUID, MStripCIT& first, MStripCIT& last)
{
	first = mStrips.find(detUID | 1);  // first strip for detector
	last  = mStrips.find(detUID | 24); // last strip for detector	
	
return ((first != mStrips.end()) && (last != mStrips.end()));
}
//------------------------------------------------------------------------------------------------------------------------
bool			MpdTofGeoUtils::IsPointInsideStrips(const TVector3& position, vector<intervalType>& intersect)
{
	const double Z = position.Z();
	const double Phi = position.Phi();	
	
	vector<intervalType> 	insideZ, insidePhi;		
	mStripsZ.findOverlapping(Z, Z, insideZ); 	
	mStripsPhi.findOverlapping(Phi, Phi, insidePhi);

//cout<<"\n  IsPointInsideStrips Z= "<<position.Z()<<", phi="<<position.Phi()<<"  sizeZ="<<insideZ.size()<<" sizePhi="<<insidePhi.size();
	
	if(!insideZ.empty() && !insidePhi.empty()) // have overlaped segments both Z and Phi 
	{
		intersect.clear();
	
		// calc. intersection
		sort(insideZ.begin(), insideZ.end(), IntervalValueSorter<LRectangle*>());
    		sort(insidePhi.begin(), insidePhi.end(), IntervalValueSorter<LRectangle*>());  
	 	set_intersection(insideZ.begin(), insideZ.end(), insidePhi.begin(), insidePhi.end(), std::back_inserter(intersect), IntervalValueSorter<LRectangle*>()); 
 	 		
//cout<<"\n  intersect size="<<intersect.size();	 
//cout<<"\n  insideZ ==== ";	
//for(int i=0;i<	insideZ.size(); i++) cout<<"  "<<insideZ[i];
//cout<<"\n  insidePhi ==== ";
//for(int i=0;i<	insidePhi.size(); i++) cout<<"  "<<insidePhi[i];	
		
	 	return true;
	}
	
return false;	
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

	value 	= fabs((*p1 - strip.A).Mag());	min1 = std::min(value, min1); 
	value 	= fabs((*p1 - strip.B).Mag());	min1 = std::min(value, min1);	
	value 	= fabs((*p1 - strip.C).Mag());	min1 = std::min(value, min1);		
	value 	= fabs((*p1 - strip.D).Mag());	min1 = std::min(value, min1); 
	
	value 	= fabs((*p2 - strip.A).Mag());	min2 = std::min(value, min2);		 
	value 	= fabs((*p2 - strip.B).Mag());	min2 = std::min(value, min2);
	value 	= fabs((*p2 - strip.C).Mag());	min2 = std::min(value, min2);	
	value 	= fabs((*p2 - strip.D).Mag());	min2 = std::min(value, min2);
	
return min1 + min2;
}	
//------------------------------------------------------------------------------------------------------------------------


