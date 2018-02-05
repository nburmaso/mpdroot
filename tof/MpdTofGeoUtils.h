//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_GEO_UTILS_H
#define __MPD_TOF_GEO_UTILS_H 1

#include <TVector3.h>
#include <TList.h>

#include "IntervalTree.h"
//------------------------------------------------------------------------------------------------------------------------
class LRectangle	// convex quadrangle
{ 
	bool		IsInvalid;
	
	inline void		getRPhi(const TVector3& point, Double_t& Rmin, Double_t& Rmax, Double_t& Phimin, Double_t& Phimax)
	{
		const Double_t R = point.Perp(), Phi = point.Phi();
		if(R < Rmin) Rmin = R;
		if(R > Rmax) Rmax = R;	
		if(Phi < Phimin) Phimin = Phi;
		if(Phi > Phimax) Phimax = Phi;				   
	}
	
public:
	enum Side_t { kRight=0,  kLeft=1, kInvalid= -1 }; 
	
	Int_t 		volumeUID;
	TVector3 	A, B, C, D, center, perp;  // [cm]

	LRectangle() : IsInvalid(true), volumeUID(kInvalid) {};
	LRectangle(Int_t uid, const TVector3& a, const TVector3& b, const TVector3& c, const TVector3& d, bool check = false);
	
	double	 	GetInteriorAngle(int vertexIndex) const {;}; // FIXME
	TVector3	GetCenter() const{ return (A+B+C+D) * 0.25;}
	bool		isInvalid() const{ return IsInvalid;}

	void		GetRPhiRanges(Double_t& Rmin, Double_t& Rmax, Double_t& Phimin, Double_t& Phimax);
	Double_t 	DistanceFromPointToLineSegment(const TVector3* pos, const TVector3& P1,const TVector3& P2)const;
	Double_t 	DistanceFromPointToLine(const TVector3* pos, const TVector3& P1,const TVector3& P2)const;	
	Double_t	MinDistanceToEdge(const TVector3* pos, Side_t& side) const;
		
	void 		Print(ostream &out, const TVector3 &point, const char* comment = nullptr)const;	
	void 		Dump(const char* comment, ostream& out = std::cout) const;

	void		Shift(const TVector3& shift){ A +=shift; B +=shift; C +=shift; D +=shift; }; 

	inline void 	InitCenterPerp()
	{	
		center = (A+B+C+D) * 0.25;
		perp = (B-A).Cross(D-A).Unit();
	}
	
	inline void 	CheckInValid()
	{
		IsInvalid = false;

		// Convex Polygon Definition: A polygon that has all interior angles less than 180°
		;
		// Sum of Interior Angles, sum = 180*(n-2) degree, where n is the number of sides 
		// A square has 4 sides, 	so interior angles sum = 360°
		;
		// Rectangle check - all angles == 90 degree	
		TVector3 ab = A-B, bc = B-C, cd = C-D, da = D-A;
		if( ab.Dot(bc) != 0. || bc.Dot(cd) != 0. || cd.Dot(da) != 0. || da.Dot(ab) != 0.) 
		{
			std::cerr<<"\n ---> ERROR: invalid Rectangle."; Dump("", std::cerr);
			IsInvalid = true;
		}	
	}

	TVector3 ProjectionPointToPlane(const TVector3& point) const
	{
		Double_t d = perp * (A - point); // d = DotProduct ( N, A - point )  		    
	return point + perp*d;
	}

	bool	IsPointInside(const TVector3& point)const
	{
 		TVector3 v1 = A - B;TVector3 v2 = C - B;TVector3 v3 = point - B;
 		double mag = v1.Mag(), proj = v3*v1/mag;
 		if(proj < 0. || proj > mag) return false;
  
 		mag = v2.Mag(), proj = v3*v2/mag;
 		if(proj < 0. || proj > mag) return false;
	return true;	
	}
};
class TGeoMatrix;
//------------------------------------------------------------------------------------------------------------------------
class LStrip : public LRectangle
{
public:
 	Int_t 		sectorID, boxID, detectorID, stripID; 
 	Int_t 		neighboring[2]; // dim same as  Side_t enum
 	
 	LStrip();
  	LStrip(Int_t uid, Int_t sector, Int_t box, Int_t detector, Int_t strip);
 	
	void 		SetIDs(Int_t uid, Int_t sector, Int_t box, Int_t  detector, Int_t  strip){ volumeUID = uid; sectorID = sector; boxID = box; detectorID = detector; stripID = strip;}	
	
	inline bool	IsSameDetector(const LStrip& strip)const{ return ( sectorID == strip.sectorID && boxID == strip.boxID && detectorID == strip.detectorID);}
	inline bool 	operator==(const LStrip& rhs){ return ( sectorID == rhs.sectorID && boxID == rhs.boxID && detectorID == rhs.detectorID && stripID == rhs.stripID);}
	inline bool 	operator!=(const LStrip& rhs){ return !((*this) == rhs);}
	

	void		Dump(const char* comment = nullptr, ostream& out = std::cout) const;
	Double_t 	Distance(Side_t side, const LStrip& strip);
};
//------------------------------------------------------------------------------------------------------------------------
class TH1D;
class TH2D;

typedef std::map<Int_t, LStrip> 	MStripType; // pair<stripUID, Strip parameters>
typedef MStripType::const_iterator	MStripCIT;
typedef MStripType::iterator		MStripIT;
typedef std::pair<MStripIT, bool>	MStripResult;

typedef Interval<LRectangle*> 		intervalType;
typedef IntervalTree<LRectangle*> 	intervalTreeType;

class MpdTofGeoUtils 
{
	MStripType			mStrips; 	//! indexing strips by UID			
	intervalTreeType		mDetectorsZ, mStripsZ; 		//! detectors Z[cm] location interval tree
	intervalTreeType		mDetectorsPhi, mStripsPhi;	//! detectors Phi[rads] location interval tree	
	
	MpdTofGeoUtils(); 
	static	MpdTofGeoUtils *instance;


	void 	localToMaster(const TGeoMatrix *matrix, Double_t* local, TVector3& position, std::pair<double, double>& Z, std::pair<double,double>& Phi)const;
	
public:
	static MpdTofGeoUtils*  Instance(){ if(instance == nullptr) instance = new MpdTofGeoUtils; return instance;}
	
	void			FindNeighborStrips(Double_t thresh, TH1D* h1 = nullptr, TH2D* h2 = nullptr, bool forced = false); // thresh, [cm] <--- thresh. distance between neighbor strips, (see h1TestDistance histo)
	void			ParseTGeoManager(bool useMCinput, TH2D* h1 = nullptr, bool forced = false);
	const LStrip*		FindStrip(Int_t UID);
	
	bool			GetStripListForDetector(Int_t detUID, MStripCIT& first, MStripCIT& last);	
	bool			IsPointInsideStrips(const TVector3& position, vector<intervalType>& intersect);
	
	const intervalTreeType*	GetDetZ() { return &mDetectorsZ; };
	const intervalTreeType*	GetDetPhi() { return &mDetectorsPhi; };	
	
	const intervalTreeType*	GetStripZ() { return &mStripsZ; };
	const intervalTreeType*	GetStripPhi() { return &mStripsPhi; };		
};
//------------------------------------------------------------------------------------------------------------------------
#endif
