//--------------------------------------------------------------

#ifndef __MPD_TOF_POINT_H
#define __MPD_TOF_POINT_H 1


#include "TObject.h"
#include "TVector3.h"

#include "FairMCPoint.h"

using namespace std;

//------------------------------------------------------------------------------------------------------------------------
class MpdTofPoint : public FairMCPoint
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
  	MpdTofPoint(Int_t mcTrackIndex, Int_t detUID, TVector3 pos, TVector3 mom, Double_t tof, Double_t length, Double_t eLoss);
  //	MpdTofPoint(const MpdTofPoint& point) { *this = point; };

  	MpdTofPoint();	
  	virtual ~MpdTofPoint();

	// Output to screen 
  	virtual void Print(const Option_t* opt) const;

 	// CATION:  MAX_VALUE = 255(0xFF)
 	
 	// MpdTof --------------------------------
  	// strip 	[1,...,24],	0x000000FF
  	// detector 	[1,...,6],	0x0000FF00
  	// box		[1,...,2],	0x00FF0000
  	// sector 	[1,...,24],	0xFF000000
 
 	// MpdEtof -------------------------------- 
   	// strip 	[1,...,131],	0x000000FF
    	// detector 	[1,...,1],	0x0000FF00 	
  	// box		[1,...,30],	0x00FF0000
  	// sector 	[1,...,2],	0xFF000000
  	
  	//Int_t GetGap() const    {return (fDetectorID & 15);};
  	Int_t GetSector() const   {		return ((fDetectorID & 0xFF000000) >> 24);};
  	Int_t GetBox() const {			return ((fDetectorID & 0x00FF0000) >> 16);};
  	Int_t GetDetector() const {		return ((fDetectorID & 0x0000FF00) >> 8);};
   	Int_t GetStrip() const {		return ( fDetectorID & 0x000000FF);}; 	
   	
  	Int_t GetVolumeUID() const {return fDetectorID;};
  
    	static Int_t GetSector(Int_t uid){ 	return ((uid & 0xFF000000) >> 24);};
   	static Int_t GetBox(Int_t uid){ 	return ((uid & 0x00FF0000) >> 16);};
    	static Int_t GetDetector(Int_t uid){ 	return ((uid & 0x0000FF00) >> 8);};  	
 	static Int_t GetStrip(Int_t uid){ 	return ( uid & 0x000000FF);};

  	static Int_t GetVolumeUID(Int_t sector, Int_t box, Int_t detector, Int_t strip) 
  	{ 
#ifdef DEBUG  	
 	Int_t uid =  (sector<<24) + (box<<16) + (detector<<8) + strip; 
 	Int_t sectorID = GetSector(uid);
  	Int_t boxID = GetBox(uid);	
 	Int_t detectorID = GetDetector(uid);
  	Int_t stripID =	GetStrip(uid);	
assert(sector == sectorID);  
assert(box == boxID);
assert(detector == detID); 
assert(strip == stripID); 	
	return uid;
#else 
  	return (sector<<24) + (box<<16) + (detector<<8) + strip; 
#endif  	
  	};
  
  	
ClassDef(MpdTofPoint,1)
};

//------------------------------------------------------------------------------------------------------------------------
#endif





