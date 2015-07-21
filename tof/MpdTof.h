//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_TOF_H
#define __MPD_TOF_H 1

//------------------------------------------------------------------------------------------------------------------------
/// \class MpdTof, LRectangle, LStrip
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#include <TLorentzVector.h>
#include <TVector3.h>

#include "IntervalTree.h"


#include "FairDetector.h"

#include "MpdTofUtils.h"
//------------------------------------------------------------------------------------------------------------------------
class LRectangle	// convex quadrangle
{ 
	bool		IsInvalid;
public:
	enum Side_t { kRight=0,  kLeft=1, kInvalid= -1 }; 
	
	Int_t 		volumeUID;
	TVector3 	A, B, C, D, center, perp;  // [cm] 
	
	LRectangle() : IsInvalid(true), volumeUID(kInvalid){};
	LRectangle(Int_t uid, const TVector3& a, const TVector3& b, const TVector3& c, const TVector3& d, bool check = false);
	
	double	 	GetInteriorAngle(int vertexIndex) const {;}; // FIXME
	TVector3	GetCenter() const{ return (A+B+C+D) * 0.25;}
	bool		isInvalid() const{ return IsInvalid;}

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
};
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
class MpdTofPoint;
class FairVolume;
class TClonesArray;
//------------------------------------------------------------------------------------------------------------------------
class MpdTof : public FairDetector
{
	// Track information to be stored until the track leaves the active volume.
  	Int_t		fTrackID;           //!  track index
  	Int_t		fVolumeID;          //!  volume id
  	TLorentzVector	fPos;               //!  position
  	TLorentzVector	fMom;               //!  momentum
  	Double32_t	fTime;              //!  time
  	Double32_t	fLength;            //!  length
  	Double32_t	fELoss;             //!  energy loss

  	Int_t 		fPosIndex;		//!
  	TClonesArray	*aTofHits;		//! Hit collection
  	const double	nan;			//!
 
public:
typedef std::map<Int_t, LStrip> 	MStripType; // pair<detectorUID, Strip parameters>
typedef MStripType::const_iterator	MStripCIT;
typedef MStripType::iterator		MStripIT;
typedef std::pair<MStripIT, bool>	MStripResult;

typedef Interval<LRectangle*> 		intervalType;
typedef IntervalTree<LRectangle*> 	intervalTreeType;
 
private:
	static intervalTreeType	mDetectorsZ; 	//! detectors Z[cm] location interval tree
	static intervalTreeType	mDetectorsPhi;	//! detectors Phi[rads] location interval tree
		
	static MStripType	mStrips; 	//! indexing strips by detectorUID
	
	void		ConstructAsciiGeometry();
  	MpdTofPoint* 	AddPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t time, Double_t length, Double_t eLoss); 
  	void 		ResetParameters();
  	
public:
  	MpdTof(const char* name = "TOF", Bool_t active = kTRUE);
	virtual ~MpdTof();

	static void		FindNeighborStrips(TH1D* h1 = nullptr, TH2D* h2 = nullptr, bool doTest = false);
	static void		ParseTGeoManager(TH2D* h1 = nullptr, bool forced = false);	
	
	static const LStrip*		FindStrip(Int_t UID);
	static const intervalTreeType*	GetDetZ() { return &mDetectorsZ; };
	static const intervalTreeType*	GetDetPhi() { return &mDetectorsPhi; };	
	static const MStripType*	GetStripMap() { return &mStrips; };		
	
	
	static void		CalcMinMax(double A, double B, double& minA, double& maxA, double& minB, double& maxB);
	
        virtual Bool_t  	ProcessHits(FairVolume* vol = nullptr);
  	virtual void 		EndOfEvent();
  	virtual void 		Register();
  	virtual TClonesArray* 	GetCollection(Int_t iColl) const;
	virtual void 		Print() const;
	virtual void 		Reset();
 	virtual void 		CopyClones(TClonesArray* from, TClonesArray* to, Int_t offset);
 	virtual void 		ConstructGeometry();
	virtual Bool_t 		CheckIfSensitive(std::string name);

ClassDef(MpdTof,2) 
};

//------------------------------------------------------------------------------------------------------------------------
inline void MpdTof::ResetParameters() 
{
	fTrackID = fVolumeID = 0;
	fPos.SetXYZM(nan, nan, nan, nan);
	fMom.SetXYZM(nan, nan, nan, nan);
	fTime = fLength = fELoss = nan;
	fPosIndex = 0;
};
//------------------------------------------------------------------------------------------------------------------------
#endif
