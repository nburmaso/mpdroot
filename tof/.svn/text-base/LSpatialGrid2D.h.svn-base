//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_LSPATIALGRID2D_H
#define __MPD_LSPATIALGRID2D_H 1

#include <map>
#include <vector>
#include <iostream>
#include <limits>
#include <algorithm>

#include <TFile.h>
#include <TVector3.h>
#include <TVector2.h>
#include<TGeoMatrix.h>
#include <TRandom2.h>
#include<TStopwatch.h>
#include <TH2F.h>
#include <TList.h>
//----------------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
struct LQuadrangle	// convex quadrangle
{ 
	T A, B, C, D; 

	LQuadrangle(const T& a, const T& b, const T& c, const T& d, bool check=false) : A(a), B(b), C(c), D(d)
	{
		if(check)
		{
			// Convex Polygon Definition: A polygon that has all interior angles less than 180°
			;
			// Sum of Interior Angles, sum = 180*(n-2) degree, where n is the number of sides 
			// A square has 4 sides, 	so interior angles sum = 360°
			;
			// Rectangle check - all angles == 90 degree	
			T ab = A-B, bc = B-C, cd = C-D, da = D-A;
			if( ( ab *= bc) !=0. || ( bc *= cd) !=0. || ( cd *= da) !=0. || ( da *= ab) !=0.) 
			{
				std::cerr<<"\n ---> ERROR: [LQuadrangle] NOT Rectangle."; Dump("", std::cerr);
			}
		}
	};
	double	 	GetInteriorAngle(int vertexIndex) {;}; // FIXME

	void inline	Dump(const char* title, ostream& out = std::cout)
		{ out<<title<<"{("<<A.X()<<","<<A.Y()<<")("<<B.X()<<","<<B.Y()<<")("<<C.X()<<","<<C.Y()<<")("<<D.X()<<","<<D.Y()<<")}"; }

	T 		Center() const{ return (A+B+C+D) * 0.25;}
	void	Shift(const T& shift){ A +=shift; B +=shift; C +=shift; D +=shift; }; 
};
//----------------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
class LSpatialGrid2D
{
public:
	// Mapping rectangles to active objects (TofHit)
	// relation: one to one
	typedef typename  std::map<long, T>		linksMAP;	// pair<rectangle UID, active object pointer> 
	typedef typename  linksMAP::iterator		linksIter;


	// Mapping rectangles to pixel
	// relation: one pixel <-> a few volumes (volumes MAY BE overlapped!)
	typedef std::multimap<long, long>	pixelMAP; // pair<pixel UID, rectangle UID>,  pixel - node on grid  
	typedef pixelMAP::iterator			pixelIter;


private:
public:
	// member data --->
	TString 	fName;
	TRandom2 *fRandom;
	int		verboseLevel;		// 0 -silent, 1- info, 2 - errors, 3 - warnings
	bool 	fDoTest;	
	bool	fDoShift;			// = true, if rectangles centering (shift on 2D) 
	bool	fIs2PiGrid;		// = true, if Y grid size = 2 pi
	double	fXShift, fYShift;	// shift vector(on 2D)
	double 	fXstep, fYstep;

	int 		nX, nY;
	double 	Xmin, Xmax, Ymin, Ymax, Xmean, Ymean;
	long 	fUID;
	
	Double_t *fMaster, *fLocal;
	TList	fWriteList;
	TH2F	*h2RestMap, *h2TestGrid2D, *h2TestGrid3D;

	// Mapping rectangleUID to rectangle data
	// relation: one to 1-2 (maybe rectange NEED to split)
	typedef std::multimap<long, LQuadrangle<TVector2> >	restangleMMAP;	// pair<rectangle UID, rectangle data> 
	typedef restangleMMAP::iterator						restangleIter;
	typedef restangleMMAP::const_iterator				restConstIter;

	restangleMMAP		mapRest;		// rectangle data
	linksMAP				mapLinks;	// links for current event active objects (hits, points, ...)
	pixelMAP				mapPixels;	// mapping rectangles to grid pixels

	enum kState { kPreInit, kInit};
	kState		fState;

	// member methods --->
	void inline	PrintVector(const char* title, const TVector3& point, ostream& out = std::cout){ out<<title<<"("<<point.X()<<", "<<point.Y()<<", "<<point.Z()<<")";}
	void inline	PrintVector(const char* title, const TVector2& point, ostream& out = std::cout){ out<<title<<"("<<point.X()<<", "<<point.Y()<<")";}
	void		Dump(const char* title, const TGeoHMatrix* geoTrans, ostream& out = std::cout) const;

	bool		CheckState(kState state, const char* message=NULL, ostream& out = std::cout)
					{if(state != fState){ if(verboseLevel>1) if(message)out<<"\n ---> ERROR: [CheckState] - "<<message; return false;} return true;}

	bool inline	IsInsideRectangle(const TVector2& point, LQuadrangle<TVector2>* data, bool boundaryInside = true);		// point inside convex quadrangle
	bool inline	IsInsideTriangle(const TVector2& point, const TVector2& A, const TVector2& B, const TVector2& C, bool boundaryInside);
	float inline	Sign(const TVector2& p1, const TVector2 p2, const TVector2& p3){ return (p1.X() - p3.X()) * (p2.Y() - p3.Y()) - (p2.X() - p3.X()) * (p1.Y() - p3.Y());}

	bool		_FindRectangle(double X, double Y, pixelMAP*  mapPixels); // Slow,  brute force method
	long 		_SplitRectangle(const TVector2& a, const TVector2& b, const TVector2& c, const TVector2& d, long UID);

	double		GetGridBinXCenter(int index){ return Xmin + fXstep * (index + 0.5); }
	double		GetGridBinYCenter(int index){ return Ymin + fYstep * (index + 0.5); }
	double		AlignmentByStep(double& x, double xstep){ if(x > 0.) x = (((int) (x / xstep)) + 1 ) * xstep; else x = (((int) (x / xstep)) - 1 ) * xstep; }

	void		CenteringGrid(); 
	long		GenerateUID(){ return ++fUID; };

	bool inline	IsLess(TVector2 a, TVector2 b)
	{ if(a.Y() < b.Y()) 	return true; if(a.Y() > b.Y()) 	return false; if(a.X() < b.X()) return true; return false;}

	void _sort(TVector2 array[], int size)   
	{ TVector2  t; for (int i = 0; i < size - 1; i++) for (int j = size - 1; j > i; j--)  if ( IsLess(array[j], array[j-1])) { t = array[j]; array[j] = array[j-1]; array[j-1] = t;  }}

	void inline	_UnHash(unsigned int pixelUID, int& x, int& y)
	{
		// hash:   unsigned int 4 bytes
		//  0xFFFF0000 -- x index, (64k max)
		//  0x0000FFFF -- y index, (64k max)
		x =(pixelUID & 0xFFFF0000) >>16; 
		y =(pixelUID & 0x0000FFFF); 
	}

	unsigned int inline _Hash(int X, int Y)
	{
#ifdef DEBUG
		unsigned int pixelUID =  ( (unsigned int) X <<16) | Y; int x, y; _UnHash(pixelUID, x, y); 

		if(y!=Y) std::cout<<" \n _Hash ERROR: Y="<<Y<<" y="<<y<<" pixelUID="<<pixelUID<<"  Ymin="<<Ymin<<", Ymax="<<Ymax;
		if(x!=X) std::cout<<" \n _Hash ERROR: X="<<X<<" x="<<x<<" pixelUID="<<pixelUID<<"  Xmin="<<Xmin<<", Xmax="<<Xmax;

assert(y==Y); assert(X==x);  return pixelUID;
#else
	return   ( (unsigned int) X <<16) | Y; 
#endif
	}


public:
	LSpatialGrid2D(const char* name, double xstep, double ystep, int verbose=0, bool dotest = false);
	~LSpatialGrid2D();

	void		MasterToLocal(const TVector3& master, TVector3& local, TGeoHMatrix* geoTrans);
	void		LocalToMaster(const  TVector3& local, TVector3& master, TGeoHMatrix* geoTrans);

	TVector3		RectangeRandomPoint(const TVector3& A, const TVector3& B, const TVector3& C, const TVector3& D, Double_t xmin=0., Double_t xmax=1.);

	void		Init(bool doShift = false);
	void		SetSeed(int seed){fRandom->SetSeed(seed);};
	void		Write(const char* flnm);
	void		ActivateRectangle(long rectUID, T ptr); 
	void		ResetActivation(){mapLinks.clear();};

	void		Transform(TGeoHMatrix* matrix, const TVector3& translationVector, const TVector3& perpVector);

	const T		GetLinkData(long rectUID) const {typename  linksMAP::const_iterator it = mapLinks.find(rectUID); if(it != mapLinks.end()) return it->second;   return NULL;}
	const LQuadrangle<TVector2>*	GetRectData(long rectUID) const { restConstIter it = mapRest.find(rectUID); if(it != mapRest.end()) return &(it->second);   return NULL;}

	// MUST be call before Init 
	long		InstallRectangle(const TVector2& A, const TVector2& B, const TVector2& C, const TVector2& D, long UID); // [mm]
	long		InstallRectangle(const TVector3& A, const TVector3& B, const TVector3& C, const TVector3& D, long UID, 
					void (*funcCallback)(const TVector3&,TVector2&), bool (*checkSingularity)(const TVector3&, const TVector3&));
	long		InstallRectangle(const TVector3& A, const TVector3& B, const TVector3& C, const TVector3& D,  void (*convert3D22D)(const TVector3&,TVector2&),  
					bool (*checkSingularity)(const TVector3&, const TVector3&))	{return  InstallRectangle(A, B, C, D, GenerateUID(), (*convert3D22D), (*checkSingularity));};

	void		SetYsizeTo2Pi(double R){ if(!CheckState(kPreInit, "SetYsizeTo2Pi() MUST call before Init().")) return; 
					Ymin = - TMath::Pi() * R; Ymax = + TMath::Pi() * R; fIs2PiGrid = true;	AlignmentByStep(Ymax, fYstep);	AlignmentByStep(Ymin, fYstep);}

	// MUST be call after Init
	long		 	Hash(double X, double Y); // [mm]
	bool 			Hash(double X, double Y, int& xIndex, int& yIndex, long& hash); // [mm], return true, if X&Y inside grid boundary

					// Fast,  grid method; return true, if point inside rectangle, return iter to selected volume (volumes MUST BE  DON'T overlaped)
	bool			FindRectangle(double X, double Y, linksIter& iter); 
	bool			FindRectangle(const TVector3& A, linksIter& iter,  void (*convert3D22D)(const TVector3&,TVector2& ))
						{TVector2 a; convert3D22D(A, a); return FindRectangle(a.X(), a.Y(), iter);}; 

					// Fast,  grid method; return true, if point inside rectangles, return iter vector to selected volumes (volumes MAY BE overlaped)
					// searching at area: X=[A-dX, A+dX], Y=[A-dY, A+dY]
	bool			FindRectangle(double X, double Y, linksMAP* candMap, long UIDshift = 0, int dX =0, int Xstep = 1, int dY =0, int Ystep = 1); 
	bool			FindRectangle(const TVector3& A, linksMAP* candMap,  void (*convert3D22D)(const TVector3&,TVector2& ), long UIDshift = 0, int dX =0, int Xstep = 1, 
						int dY =0, int Ystep = 1) {TVector2 a; convert3D22D(A, a); return FindRectangle(a.X(), a.Y(), candMap, UIDshift, dX, Xstep, dY, Ystep);}; 

	void			TestGrid4Pi(double R, void (*convert3D22D)(const TVector3&,TVector2&), int nEvents = 1000000);
};
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
LSpatialGrid2D<T>::LSpatialGrid2D(const char* name, double xstep, double ystep, int verbose, bool dotest)
: fName(name), verboseLevel(verbose), fDoTest(dotest), fIs2PiGrid(false), fDoShift(false), fXstep(xstep), fYstep(ystep), fState(kPreInit), fUID(0) 
{
	fRandom = new TRandom2(0); // seed = 0, from timer
	fMaster = new Double_t[3];
	fLocal = new Double_t[3];

	Xmax = Ymax = - std::numeric_limits<double>::max();
	Xmin = Ymin =  + std::numeric_limits<double>::max();

	assert(fXstep > 0.);
	assert(fYstep > 0.);

	if(fDoTest){	}
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
LSpatialGrid2D<T>::~LSpatialGrid2D(){ delete fRandom; delete fMaster; delete fLocal; }
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
void		LSpatialGrid2D<T>::Init(bool doShift)
{
	if(verboseLevel>0) std::cout<<" \n -I- [LSpatialGrid2D:Init] ("<<fName.Data()<<")  Installed "<<mapRest.size()<<" rectangles."<<std::flush;
	assert(!mapRest.empty());

	if(doShift) CenteringGrid();

	// boundary alignment by step
	AlignmentByStep(Xmax, fXstep);
	AlignmentByStep(Xmin, fXstep);
	if(!fIs2PiGrid)
	{
		AlignmentByStep(Ymax, fYstep);
		AlignmentByStep(Ymin, fYstep);
	}

	// update grid parameters
	nX = ceil(fabs((Xmax - Xmin) / fXstep));
	nY = ceil(fabs((Ymax - Ymin) / fYstep));
	assert(nX < 65535); // Update hash function from int to long
	assert(nY < 65535); // Update hash function from int to long

	if(fDoTest)
	{
		h2RestMap = new TH2F("RestMap", "Restangle centers map; X, mm; Y, mm", 1000, Xmin, Xmax, 1000, Ymin, Ymax);  
		fWriteList.Add(h2RestMap); h2RestMap->SetDirectory(0); 
		h2TestGrid2D = new TH2F("TestGrid2D", "2D grid test; X, mm; Y, mm", 1000, Xmin, Xmax, 1000, Ymin, Ymax); 
		fWriteList.Add(h2TestGrid2D); h2TestGrid2D->SetDirectory(0); 
		h2TestGrid3D = new TH2F("TestGrid3D", "3D grid test;#theta, degree; #phi, degree", 1000, 0., 180., 1000, -180., 180.);  
		fWriteList.Add(h2TestGrid3D); h2TestGrid3D->SetDirectory(0); 

		TVector2 pos;
		for(restangleIter it = mapRest.begin(), itEnd = mapRest.end(); it != itEnd; it++) // Cycle by restangles
		{
			pos = it->second.Center();	h2RestMap->Fill(pos.X(), pos.Y(), it->first);
			pos = it->second.A;		h2RestMap->Fill(pos.X(), pos.Y(), 1.);
			pos = it->second.B;		h2RestMap->Fill(pos.X(), pos.Y(), 1.);
			pos = it->second.C;		h2RestMap->Fill(pos.X(), pos.Y(), 1.);
			pos = it->second.D;		h2RestMap->Fill(pos.X(), pos.Y(), 1.);
		}
	}

	// update grid parameters
	Xmean = h2RestMap->GetMean(1);
	Ymean = h2RestMap->GetMean(2);

	fXstep = (Xmax - Xmin) / nX;
	fYstep = (Ymax - Ymin) / nY;	

	if(verboseLevel>0) std::cout<<" \n -I- [LSpatialGrid2D] ("<<fName.Data()<<") #nodes : "<<nX*nY<<"("<<nX<<"x"<<nY<<"), mean X,Y ("<<Xmean<<", "<<Ymean
		<<"), step X,Y ("<<fXstep<<", "<<fYstep<<"),  X= ["<<Xmin<<", "<<Xmax<<"], Y= ["<<Ymin<<", "<<Ymax<<"]"<<std::flush;

	fState = kInit;

	if(verboseLevel>0) std::cout<<" \n -I- Create grid ..."; 
	// ------------   Create grid --------------------------------------------
	double X, Y; 
	for(int xindex = 0; xindex < nX; xindex++)
	{
		if(verboseLevel>0) if(xindex%10 == 0) std::cout<<"."<<std::flush;
		for(int yindex = 0; yindex < nY; yindex++)	// cycle by pixels
		{
			X = GetGridBinXCenter(xindex);
			Y = GetGridBinYCenter(yindex);

			assert(Hash(X, Y) == _Hash(xindex, yindex));

			_FindRectangle(X, Y, &mapPixels);	// Slow,  brute force method
		}
	}

	if(verboseLevel>0) std::cout<<" \n -I- [LSpatialGrid2D:Init] ("<<fName.Data()<<") Grid filled on "<<( 100.*mapPixels.size()/ (nX*nY) )<<" %. ("
			<<mapPixels.size()<<"/"<<(nX*nY)<<")"<<std::flush;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
long 	LSpatialGrid2D<T>::Hash(double X, double Y)
{
	if(!CheckState(kInit, "Hash() MUST call after Init().")) return 0; 

	if((X < Xmin) ||  (X > Xmax) || (Y < Ymin)    ||  (Y > Ymax)) return  0xFFFFFFFFFFFFFFFF;	// out of grid sign
	
	int xIndex = (X - Xmin) / fXstep;	
	int yIndex = (Y - Ymin) / fYstep;	

return (long) _Hash(xIndex, yIndex);
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
bool 	LSpatialGrid2D<T>::Hash(double X, double Y, int& xIndex, int& yIndex, long& hash)
{
	if(!CheckState(kInit, "Hash() MUST call after Init().")) return 0; 

	if((X < Xmin) ||  (X > Xmax) || (Y < Ymin)    ||  (Y > Ymax))
	{
		hash = 0xFFFFFFFFFFFFFFFF;	// out of grid sign
	 	return  false;
	}
	
	xIndex = (X - Xmin) / fXstep;	
	yIndex = (Y - Ymin) / fYstep;	
	hash = _Hash(xIndex, yIndex);

return true;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
bool		LSpatialGrid2D<T>::FindRectangle(double X, double Y, linksIter& iter)
{
	if(!CheckState(kInit, "FindRectangle() MUST call after Init().")) return false; 

	if(fDoShift){  X -= fXShift; Y -= fYShift; }

	// looking for pixel
	pixelIter it = mapPixels.find( Hash(X, Y) );
	if(it == mapPixels.end()) return false;	// blank pixel == outside all rectangles

	// looking for linked object
	iter = mapLinks.find(it->second);
	if(iter == mapLinks.end()) return false; 	// absent active volume for selected rectangle(don't have hit inside pad)  

return true;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
bool		LSpatialGrid2D<T>::FindRectangle(double X, double Y, linksMAP* candMap, long UIDshift, int dX, int Xstep, int dY, int Ystep)
{
	if(!CheckState(kInit, "FindRectangle() MUST call after Init().")) return false; 

	if(fDoShift){  X -= fXShift; Y -= fYShift; }

	candMap->clear();

	long rectUID, pixelUID;
	if(dX == 0 && dY == 0)	// searching at point
	{
		// looking for pixel
		pixelUID = Hash(X, Y);
		pixelIter it = mapPixels.find(pixelUID);
		if(it == mapPixels.end()) return false;	// blank pixel == outside all rectangles

		int count = mapPixels.count(pixelUID); linksIter iter;
		for(int i = 0; i < count; i++, it++)
		{
			rectUID = it->second + UIDshift; // UID = padID + moduleID
			iter = mapLinks.find(rectUID);
			if(iter != mapLinks.end()) // active volume for selected rectangle exist.
			{
				// insert candidate with same rectangleUID ONLY one time
				if(candMap->find(rectUID) == candMap->end())	candMap->insert(typename LSpatialGrid2D<T>::linksMAP::value_type(rectUID, iter->second)); 
			}
		}
	}
	else 	// searching at area around point
	{
		int xIndex, yIndex; pixelIter it;
		if(Hash(X, Y, xIndex, yIndex, pixelUID)) // XY inside grid boundary --> xIndex, yIndex is valid
		{
			int xmin = std::max(xIndex - dX, 0), xmax = std::min(xIndex + dX, nX - 1);
			int ymin = std::max(yIndex - dY, 0), ymax = std::min(yIndex + dY, nY - 1);

			for(int ix = xmin; ix <= xmax; ix+=Xstep)
			for(int iy = ymin; iy <= ymax; iy+=Ystep) // cycle by area around point
			{
				// looking for pixel
				pixelUID = _Hash(ix, iy);
				it = mapPixels.find(pixelUID);
				if(it == mapPixels.end()) continue;	// blank pixel == outside all rectangles

				int count = mapPixels.count(pixelUID); linksIter iter;
				for(int i = 0; i < count; i++, it++)
				{
					rectUID = it->second + UIDshift; // UID = padID + moduleID
					iter = mapLinks.find(rectUID);
					if(iter != mapLinks.end()) // active volume for selected rectangle exist.
					{
						// insert candidate with same rectangleUID ONLY one time
						if(candMap->find(rectUID) == candMap->end())	candMap->insert(typename LSpatialGrid2D<T>::linksMAP::value_type(rectUID, iter->second)); 
					}
				}

			} // cycle by area around point
		}
	}

return (!candMap->empty());
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
void		LSpatialGrid2D<T>::CenteringGrid()
{
	// calc. means
	double Xsum = 0., Ysum = 0.; TVector2 vec;
	for(restangleIter it = mapRest.begin(), itEnd = mapRest.end(); it != itEnd; it++) 
	{
		vec = it->second.Center();	
		Xsum += vec.X();
		Ysum += vec.Y();
	}

	fXShift = Xsum / mapRest.size();
	fYShift = Ysum / mapRest.size();

	fDoShift = true; 
	TVector2 vecShift( - fXShift, - fYShift); // shift vector

	// shift rectangles
	for(restangleIter it = mapRest.begin(), itEnd = mapRest.end(); it != itEnd; it++) it->second.Shift(vecShift);

	// shift boundary&mean
	Xmin -= fXShift; Xmax -= fXShift; Ymin -= fYShift; Ymax -= fYShift;
	Xmean = Ymean = 0.;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
long		LSpatialGrid2D<T>::InstallRectangle(const TVector2& A, const TVector2& B, const TVector2& C, const TVector2& D, long UID)
{
	if(!CheckState(kPreInit, "InstallRectangle() MUST call before Init().")) return UID; 

	restangleIter it = mapRest.find(UID);
	if(it != mapRest.end()) 
	{
		if(verboseLevel>2) std::cout<<"\n ---> WARNING: [LSpatialGrid2D::InstallRectangle]("<<fName.Data()<<") - Rectangle with "<<UID<<" UID already installed."; 
	}

	// Install rectangle
	mapRest.insert(restangleMMAP::value_type(UID, LQuadrangle<TVector2>(A, B, C, D, true))); // true - check rectangle

	// Update Xmin, Xmax, Ymin, Ymax
	Xmin = std::min(A.X(), std::min(B.X(), std::min(C.X(), std::min(D.X(), Xmin))));
	Xmax = std::max(A.X(), std::max(B.X(), std::max(C.X(), std::max(D.X(), Xmax))));

	Ymin = std::min(A.Y(), std::min(B.Y(), std::min(C.Y(), std::min(D.Y(), Ymin))));
	Ymax = std::max(A.Y(), std::max(B.Y(), std::max(C.Y(), std::max(D.Y(), Ymax))));
/*
std::cout<<"\n ---> [LSpatialGrid2D::InstallRectangle] ("<<fName.Data()<<") - Xmin,max("<<Xmin<<","<<Xmax<<")    Ymin,max("<<Ymin<<","<<Ymax<<") UID="<<UID; 
PrintVector(" A ", A);
PrintVector(" B ", B);
PrintVector(" C ", C);
PrintVector(" D ", D);
PrintVector(" CENTER ", (A+B+C+D) * 0.25);
*/
return UID;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
long		LSpatialGrid2D<T>::InstallRectangle(const TVector3& A, const TVector3& B, const TVector3& C, const TVector3& D, 
									long UID, void (*convert3D22D)(const TVector3& vec3, TVector2& vec2), bool (*checkSingularity)(const TVector3&, const TVector3&))
{
	 // Projection 3D to 2D
	TVector2 a, b, c, d;
	convert3D22D(A, a);
	convert3D22D(B, b);
	convert3D22D(C, c);
	convert3D22D(D, d);

	// check singilarity line crossing
	if(checkSingularity(A,B) || checkSingularity(B,C) || checkSingularity(C,D) || checkSingularity(D,A))
	{
		if(verboseLevel>2)
		{
			std::cout<<"\n\n ---> WARNING: [LSpatialGrid2D::InstallRectangle] ("<<fName.Data()<<") - Rectangle overlap grid singularity.";
		 	PrintVector("\n 3D Rectangle:  A=", A); PrintVector(", B=", B); PrintVector(", C=", C); PrintVector(", D=", D);
		}
		return _SplitRectangle(a, b, c, d, UID);
	}

return InstallRectangle(a, b, c, d, UID);
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
long 	LSpatialGrid2D<T>::_SplitRectangle(const TVector2& a, const TVector2& b, const TVector2& c, const TVector2& d, long UID)
{
	if(verboseLevel>2)
	{
		PrintVector("\n ---> WARNING: [LSpatialGrid2D::SplitRectangle]\n 2D Origin:  a=", a); PrintVector(", b=", b); PrintVector(", c=", c); PrintVector(", d=", d);
	}

	TVector2 rest[4] = {a,b,c,d};
	_sort(rest, 4);

	TVector2 a1, b1, c1, d1, a2,b2, c2, d2;
	a1=rest[0]; b1=rest[1]; c1.Set(b1.X(), Ymin); d1.Set(a1.X(), Ymin);
	a2=rest[2]; b2=rest[3]; c2.Set(b2.X(), Ymax); d2.Set(a2.X(), Ymax);

	if(verboseLevel>2)
	{
		PrintVector("\n 2D after split:  a1=", a1); PrintVector(", b1=", b1); PrintVector(", c1=", c1); PrintVector(", d1=", d1);
		PrintVector("\n 2D after split:  a2=", a2); PrintVector(", b2=", b2); PrintVector(", c2=", c2); PrintVector(", d2=", d2);
	}

	InstallRectangle(a1, b1, c1, d1, UID);
return InstallRectangle(a2, b2, c2, d2, UID);
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
bool 	LSpatialGrid2D<T>::IsInsideRectangle(const TVector2& point, LQuadrangle<TVector2>* data, bool boundaryInside)
{
	if(IsInsideTriangle(point, data->A, data->B, data->C, boundaryInside)) return true; 	// inside first triangle,  ABC
 	if(IsInsideTriangle(point, data->A, data->C, data->D, boundaryInside)) return true;	// inside second triangle, ACD

return false;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
bool 	LSpatialGrid2D<T>::IsInsideTriangle(const TVector2& point, const TVector2& A, const TVector2& B, const TVector2& C, bool boundaryInside)
{
	bool b1, b2, b3;
	if(boundaryInside) // Boundary points aссepted  as inside
	{
			b1 = Sign(point, A, B) <= 0.0f; 
			b2 = Sign(point, B, C) <= 0.0f; 
			b3 = Sign(point, C, A) <= 0.0f; 
	}
	else	// Boundary points aссepted  as outside
	{
			b1 = Sign(point, A, B) < 0.0f; 
			b2 = Sign(point, B, C) < 0.0f; 
			b3 = Sign(point, C, A) < 0.0f;	
	}

return ((b1 == b2) && (b2 == b3)); 
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
void		LSpatialGrid2D<T>::TestGrid4Pi(double R, void (*convert3D22D)(const TVector3&,TVector2&), int nEvents)
{
	if(!CheckState(kInit, "TestGrid() MUST call after Init().")) return; 

	if(fDoTest)
    	{	
		TVector3 pos3; TVector2 pos2;
		Double_t x, y, z, radToDeg = TMath::RadToDeg(); linksIter Iter;

  		TStopwatch timer;
  		timer.Start();

		int nInside = 0;
		for(int i =0; i < nEvents; i++)
		{
			fRandom->Sphere(x, y, z, R);
			pos3.SetXYZ(x,y,z);

			if(FindRectangle(pos3, Iter, convert3D22D))
			{
				convert3D22D(pos3, pos2); nInside++;
				h2TestGrid2D->Fill(pos2.X(), pos2.Y());
				h2TestGrid3D->Fill(pos3.Theta() * radToDeg, pos3.Phi() * radToDeg);
			}
		}

 		timer.Stop();
		if(verboseLevel>0) std::cout<<" \n -I- [LSpatialGrid2D:TestGrid]  ("<<fName.Data()<<") calls = "<<nEvents<<", inside = "<<nInside<<"("<<100.*nInside / nEvents
				<<"%), RealTime="<<timer.RealTime()<<" seconds, CpuTime="<<timer.CpuTime()<<" seconds.\n";	
	}
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
void		LSpatialGrid2D<T>::Write(const char* flnm)
{
	if(fDoTest)
    	{	
		if(verboseLevel>0) std::cout<<" \n -I- [LSpatialGrid2D:Write]  ("<<fName.Data()<<") Update "<<flnm<<" file.";			
		TFile file(flnm, "RECREATE");
      		fWriteList.Write(); 
      		file.Close();
    	}	
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
void		LSpatialGrid2D<T>::ActivateRectangle(long rectUID, T data)
{
	mapLinks.insert(typename linksMAP::value_type(rectUID, data));
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T> 
bool		LSpatialGrid2D<T>::_FindRectangle(double X, double Y,  pixelMAP*  mapPixels) // Slow,  brute force method
{
	TVector2 pos(X, Y); long pixelUID, volumeUID;
	LQuadrangle<TVector2>* data; 
	for(restangleIter iter = mapRest.begin(), itEnd = mapRest.end(); iter != itEnd; iter++) // cycle by rectangles
	{
		data = &(iter->second);

		if(IsInsideRectangle(pos, data))
		{
			volumeUID = iter->first;
			pixelUID = Hash(X, Y);
			mapPixels->insert(pixelMAP::value_type(pixelUID, volumeUID)); 
		}
	}

return false;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T>
void		LSpatialGrid2D<T>::MasterToLocal(const TVector3& master, TVector3& local, TGeoHMatrix* geoTrans)
{
	master.GetXYZ(fMaster);
	geoTrans->MasterToLocal(fMaster, fLocal);
	local.SetXYZ(fLocal[0], fLocal[1], fLocal[2]);

//	PrintVector("\n MASTER: ", master); PrintVector(" to  LOCAL: ", local); 
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T>
void		LSpatialGrid2D<T>::LocalToMaster(const  TVector3& local, TVector3& master, TGeoHMatrix* geoTrans)
{
	local.GetXYZ(fLocal);
	geoTrans->LocalToMaster(fLocal, fMaster);
	master.SetXYZ(fMaster[0], fMaster[1], fMaster[2]);

//	PrintVector("\n LOCAL: ", local); PrintVector(" to MASTER: ", master); 
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T>
TVector3		LSpatialGrid2D<T>::RectangeRandomPoint(const TVector3& A, const TVector3& B, const TVector3& C, const TVector3& D, Double_t xmin, Double_t xmax)
{
	double rndm1 = fRandom->Uniform(xmin, xmax), rndm2 =  fRandom->Uniform(xmin, xmax);
	TVector3 point = A + (B - A) * rndm1 + (C- B) * rndm2;

//	PrintVector("\n RectangeRandomPoint point: ", point); PrintVector(" A: ", A);	PrintVector(" B: ", B); PrintVector(" C: ", C); PrintVector(" D: ", D);  
return point;
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T>
void		LSpatialGrid2D<T>::Transform(TGeoHMatrix* matrix, const TVector3& translationVector, const TVector3& perpVector)
{
	TGeoRotation geoRot;	geoRot.RotateZ(90. + perpVector.Phi() * TMath::RadToDeg());
	TGeoTranslation geoTran(translationVector.X(), translationVector.Y(), translationVector.Z());

	matrix->Multiply(&geoTran);     matrix->Multiply(&geoRot); // matrix = translation * rotation
}
//----------------------------------------------------------------------------------------------------------------------------------------
template<typename T>
void		LSpatialGrid2D<T>::Dump( const char* title, const TGeoHMatrix* geoTrans, ostream& out) const
{
	Double_t* hmat = new Double_t[16];
	geoTrans->GetHomogenousMatrix(hmat);
	
	out<<"\n ---- TGeoHMatrix dump:  "<<title;
	out	<<"\n"<<hmat[0]<<"\t"<<hmat[4]<<"\t"<<hmat[8]<<"\t"<<hmat[12]
		<<"\n"<<hmat[1]<<"\t"<<hmat[5]<<"\t"<<hmat[9]<<"\t"<<hmat[13]
		<<"\n"<<hmat[2]<<"\t"<<hmat[6]<<"\t"<<hmat[10]<<"\t"<<hmat[14]
		<<"\n"<<hmat[3]<<"\t"<<hmat[7]<<"\t"<<hmat[11]<<"\t"<<hmat[15]<<std::endl;

	delete hmat;
}
//----------------------------------------------------------------------------------------------------------------------------------------
#endif 


