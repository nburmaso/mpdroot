//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_ETOF_POINT_H
#define __MPD_ETOF_POINT_H 1


#include "TObject.h"
#include "TVector3.h"
#include "FairMCPoint.h"

using namespace std;

//------------------------------------------------------------------------------------------------------------------------
class MpdEtofPoint : public FairMCPoint
{

public:

  /** Constructor with arguments
   *@param trackID  Index of MCTrack
   *@param detID    Detector ID
   *@param pos      Ccoordinates at entrance to active volume [cm]
   *@param mom      Momentum of track at entrance [GeV]
   *@param tof      Time since event start [ns]
   *@param length   Track length since creation [cm]
   *@param eLoss    Energy deposit [GeV]
   **/
  	MpdEtofPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t tof, Double_t length, Double_t eLoss);
  	MpdEtofPoint(const MpdEtofPoint& point) { *this = point; }; //<<<<< ??????????????????????????? ADD BASE ctor ?????

  	MpdEtofPoint();	
  	virtual ~MpdEtofPoint();

	// Output to screen 
  	virtual void Print(const Option_t* opt) const;

  	// CATION:  MAX_VALUE = 255(0xFF)
  	// strip 	[1,...,131],	0x000000FF
  	// box		[1,...,30],	0x00FF0000
  	// sector 	[1,...,2],	0xFF000000
  	
  	//Int_t GetGap() const    {return (fDetectorID & 15);};
  	Int_t GetRegion() const   {		return ((fDetectorID & 0xFF000000) >> 24);};
  	Int_t GetModule() const {		return ((fDetectorID & 0x00FF0000) >> 16);};
   	Int_t GetStrip() const {		return ( fDetectorID & 0x000000FF);}; 	
   	
  	Int_t GetDetectorID() const {return fDetectorID;};
  
    	static Int_t GetRegion(Int_t uid){ 	return ((uid & 0xFF000000) >> 24);};
   	static Int_t GetModule(Int_t uid){ 	return ((uid & 0x00FF0000) >> 16);};
 	static Int_t GetStrip(Int_t uid){ 	return ( uid & 0x000000FF);};

  	static Int_t GetVolumeUID(Int_t region, Int_t module, Int_t strip) 
  	{ 
#ifdef DEBUG  	
 	Int_t uid =  (region<<24) + (module<<16) + strip; 
 	Int_t regionID = GetRegion(uid);
  	Int_t moduleID = GetModule(uid);	
  	Int_t stripID =	GetStrip(uid);	
assert(region == regionID);  
assert(module == moduleID);
assert(strip == stripID); 	
	return uid;
#else 
  	return (region<<24) + (module<<16) +  strip; 
#endif  	
  	};
 
 
 								
ClassDef(MpdEtofPoint,1)
};

//------------------------------------------------------------------------------------------------------------------------
#endif





