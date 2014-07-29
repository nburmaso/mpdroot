//--------------------------------------------------------------------------------------------------------------
#ifndef __HH_MPDTOFUTILS
#define __HH_MPDTOFUTILS 1
 
#include <stdio.h>
#include <iostream>
#include <vector>
#include <map>
#include <assert.h>
  
#include "TSystem.h"
#include "TMath.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TList.h"
#include "TVector3.h"
#include "TString.h"

//#define BIT(n)       (1 << (n))

using namespace std;
class TXMLNode;
class TXMLEngine;
//--------------------------------------------------------------------------------------------------------------
class MpdTofUtils
{
public:
 enum k_side { Up=0, Right, Down, Left, Absent= -1 };
 enum k_HitType { IsSingle = BIT(0), IsDouble = BIT(1), IsTriple = BIT(2), HaveTail = BIT(3), InCluster = BIT(4), McAbsent = BIT(5), IsSelected = BIT(12)	};
 enum k_LinkType { IsTofPointIndex = 1, IsMCTrackIndex = 2, IsVolumeUID = 3 }; 
 enum k_DetType { IsBarrel = 1, IsEndcapLeft = 2, IsEndcapRight = 3 }; 

 struct padPar{ TVector3 center, point[4]; Int_t region, module, pad; Int_t neighboring[4]; }; 
 struct modPar{ TVector3 center, perp, point[4]; Int_t  region, module;};
 struct regionData{ TVector3 center, point[4]; Int_t ID; double minPhi, maxPhi;};

 struct LLine{ TVector3 p1, p2; };

 	typedef map<Int_t, padPar>   	PadMap;
 	typedef PadMap::iterator  	PadIter;

 	typedef multimap<Int_t, modPar> ModMMap;
 	typedef ModMMap::iterator  	ModIter;

 	typedef vector<regionData>    	RegVec;
	typedef RegVec::iterator	RegIter;

	typedef map<Double_t, k_side> 	deltaMap;
	typedef deltaMap::iterator 	deltaIter;
	
private:

static	TVector3	_charToVector(const char*);
static	void		_setRegionPar(TXMLNode*, TXMLEngine*, RegVec&);
static	void		_setModulePar(TXMLNode*, TXMLEngine*, ModMMap&);
static	void		_setPadPar(TXMLNode*, TXMLEngine*, PadMap&);

public:

static TString	_vectorToChar(const TVector3&);
static TString 	GetTofParFlnm(const TString&);
static TString 	GetEtofParFlnm(const TString&);

static Bool_t	Find(PadMap&, padPar**, Int_t UID);
static void 	GetClosestSide(TVector3 point, padPar*, deltaMap*);
static Int_t	GetNeighboringPadUID(PadMap&, Int_t UID, k_side);
static TVector3	GetPadCenter(PadMap&, Int_t UID);
static void	CheckAllSide(k_side side, padPar *par, padPar *curPar, Double_t& min, Int_t& uid, Int_t curUid);
static LLine	GetLine(padPar*, k_side);
static void	FindCrossPads(PadMap&, TH2D*);

static void	WriteParamToXML(const char* flnm, const char* rootNodeName, const char* comment, RegVec&, ModMMap&, PadMap&);
static bool	ReadParamFromXML(const char* flnm, RegVec&, ModMMap&, PadMap&);

static	Double_t	Delta(TVector3 point, LLine line){ return  Delta(point, line.p1, line.p2); };
	
static	Double_t	Delta(LLine line1, LLine line2)
	{ 
		return    min( (line1.p1 - line2.p1).Mag(),  (line1.p1 - line2.p2).Mag() )
			+ min( (line1.p2 - line2.p2).Mag(),  (line1.p2 - line2.p1).Mag() ); 
	};

static	Double_t Delta(TVector3 point, TVector3 A0, TVector3 A1)
	{ 
		TVector3 v = A1 - A0;
		return  ((v.Cross(point - A0)).Mag()/v.Mag());
	};

static TVector3 _zerkalo(TVector3 zerkalo, TVector3 source){ return  zerkalo + zerkalo - source; };	

// return value: distance of a point X to a plane{by perp - perpendicular to plane & p0 - belongs to a plane}.  
static Double_t DistancePointToPlane(const TVector3& X, const TVector3& p0, const TVector3& perp);

// return value: projection of a point X to a plane{by perp - perpendicular to plane & p0 - belongs to a plane}. 
static TVector3 ProjectionPointToPlane(const TVector3& X, const TVector3& p0, const TVector3& perp);

// return value = true, if a point X is inside rectangle{by vector(A-B) & vector(C-B)}.
static bool IsPointInsidePar(const TVector3& X, const TVector3& A, const TVector3& B, const TVector3& C);


static TH1D* Make1D(const char*, Int_t, Double_t, Double_t, TList* list=NULL, const char* title="");
static TH2D* Make2D(const char*, Int_t, Double_t, Double_t, Int_t, Double_t, Double_t,  TList* list=NULL, const char* title="");
// make  clone with new name, title
static TH1D* Make1D(const char* name, const TH1D* src, TList* list=NULL, const char* title="");
static TH2D* Make2D(const char* name, const TH2D* src, TList* list=NULL, const char* title="");

};
//------------------------------------------------------------------------------------------------------------------------
inline TString 	MpdTofUtils::GetTofParFlnm(const TString& flnm)
{ 
	TString path = gSystem->Getenv("VMCWORKDIR"); path += "/macro/tof/";		
	if(flnm.IsNull()) path += "tof_default.geom.par.xml"; else path += flnm;
	return path;
};
//------------------------------------------------------------------------------------------------------------------------
inline TString 	MpdTofUtils::GetEtofParFlnm(const TString& flnm)
{ 
	TString path = gSystem->Getenv("VMCWORKDIR"); path += "/macro/etof/";		
	if(flnm.IsNull()) path += "etof_default.geom.par.xml"; else path += flnm;
	return path;
};
//------------------------------------------------------------------------------------------------------------------------
inline MpdTofUtils::LLine	MpdTofUtils::GetLine(padPar *par, k_side side)
{ 
	LLine line; 
	switch(side)
	{ 
		case Up: 	line.p1 = par->point[0]; line.p2 = par->point[1]; break;
		case Right: 	line.p1 = par->point[1]; line.p2 = par->point[2]; break;	
		case Down: 	line.p1 = par->point[2]; line.p2 = par->point[3]; break;
		case Left: 	line.p1 = par->point[3]; line.p2 = par->point[0]; break;
		case Absent: 	assert(side != Absent); break;
	}

return line;
};
//------------------------------------------------------------------------------------------------------------------------
inline void 	MpdTofUtils::GetClosestSide(TVector3 point, padPar *par, deltaMap *map)
{
	map->clear();
	map->insert(deltaMap::value_type( Delta(point, GetLine(par, Up)),	Up));
	map->insert(deltaMap::value_type( Delta(point, GetLine(par, Right)), 	Right));
	map->insert(deltaMap::value_type( Delta(point, GetLine(par, Down)), 	Down));
	map->insert(deltaMap::value_type( Delta(point, GetLine(par, Left)),	Left));		
}	
//------------------------------------------------------------------------------------------------------------------------
inline void	MpdTofUtils::CheckAllSide(k_side side, padPar *par, padPar *curPar, Double_t& min, Int_t& uid, Int_t curUid)
{
	Double_t val;
	for(Int_t s = Up; s <= Left; s++)
	{
		val = Delta(GetLine(par, side), GetLine(curPar, (k_side)s));
		if(val < min){ min = val; uid = curUid;}
	}
};
//------------------------------------------------------------------------------------------------------------------------
inline TString	MpdTofUtils::_vectorToChar(const TVector3& vec)
{
	return TString::Format("%.10g %.10g %.10g", vec.X(), vec.Y(), vec.Z()); 
}
//--------------------------------------------------------------------------------------------------------------
inline Double_t	MpdTofUtils::DistancePointToPlane(const TVector3& point, const TVector3& p0, const TVector3& perp) 
{ 	
	Double_t A = perp.X(), B = perp.Y(), C = perp.Z();
	Double_t D = -1.*( A*p0.X() + B*p0.Y() + C*p0.Z());
	return ((A*point.X() + B*point.Y() + C*point.Z() + D) / (sqrt(A*A+B*B+C*C)));
};
//--------------------------------------------------------------------------------------------------------------
inline TVector3	MpdTofUtils::ProjectionPointToPlane(const TVector3& point, const TVector3& p0, const TVector3& perp) 
{
	Double_t d = perp * (p0 - point); // d = DotProduct ( N, p0 - point )  		    
	return point + perp*d;
}
//--------------------------------------------------------------------------------------------------------------
inline	bool	MpdTofUtils::IsPointInsidePar(const TVector3& point, const TVector3& A, const TVector3& B, const TVector3& C)
{
 TVector3 v1 = A - B;TVector3 v2 = C - B;TVector3 v3 = point - B;
 double mag = v1.Mag(), proj = v3*v1/mag;
 if(proj < 0. || proj > mag) return false;
  
 mag = v2.Mag(), proj = v3*v2/mag;
 if(proj < 0. || proj > mag) return false;
return true; 
}
//--------------------------------------------------------------------------------------------------------------
inline TH2D* MpdTofUtils::Make2D(const char* name,  Int_t nbinx, Double_t xmin, Double_t xmax, Int_t nbiny, Double_t ymin, Double_t ymax, TList *list, const char* title)
{
 	TH2D* ptr = new TH2D(name, title, nbinx, xmin, xmax, nbiny, ymin, ymax); 
	if(list){ list->Add(ptr); ptr->SetDirectory(0); }
return ptr;
}
//------------------------------------------------------------------------------------------------------------------------
inline TH1D* MpdTofUtils::Make1D(const char* name, Int_t nbinx, Double_t xmin, Double_t xmax , TList *list, const char* title)
{
 	TH1D* ptr = new TH1D(name, title, nbinx, xmin, xmax); 
 	if(list){ list->Add(ptr); ptr->SetDirectory(0); }   
return ptr;
}
//------------------------------------------------------------------------------------------------------------------------
inline TH2D* MpdTofUtils::Make2D(const char* name, const TH2D *src, TList *list, const char* title)
{  
 	TH2D *ptr = (TH2D*) src->Clone(name);  ptr->SetTitle(title);
 	if(list){ list->Add(ptr); ptr->SetDirectory(0); }
return ptr; 
} 
//------------------------------------------------------------------------------------------------------------------------
inline TH1D* MpdTofUtils::Make1D(const char* name, const TH1D *src, TList *list, const char* title)
{  
	TH1D *ptr = (TH1D*) src->Clone(name);  ptr->SetTitle(title);
	if(list){ list->Add(ptr); ptr->SetDirectory(0); }
return ptr; 
}
//--------------------------------------------------------------------------------------------------------------
#endif
//--------------------------------------------------------------------------------------------------------------
