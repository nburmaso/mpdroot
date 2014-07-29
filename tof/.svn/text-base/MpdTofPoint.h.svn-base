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
  	MpdTofPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t tof, Double_t length, Double_t eLoss);
  	MpdTofPoint(const MpdTofPoint& point) { *this = point; };

  	MpdTofPoint();	
  	virtual ~MpdTofPoint();

	// Output to screen 
  	virtual void Print(const Option_t* opt) const;

	// get  
  	Int_t GetGap() const    {return (fDetectorID & 15);};
  	Int_t GetPad() const   {return ((fDetectorID>>4) & 1023);};
  	Int_t GetModule() const {return ((fDetectorID>>14) & 1023);};
  	Int_t GetRegion() const {return fDetectorID>>24;};
  	Int_t GetDetectorID() const {return fDetectorID;};
  
    	static Int_t GetRegion(Int_t uid){ return uid>>24; };
   	static Int_t GetModule(Int_t uid){ return ((uid>>14) & 1023); };
 	static Int_t GetPad(Int_t uid){ return ((uid>>4) & 1023); };
  	static Int_t GetVolumeUID(Int_t regID, Int_t modID, Int_t cellID) { return (regID<<24) + (modID<<14) + (cellID<<4); };
  
  
ClassDef(MpdTofPoint,1)
};

//------------------------------------------------------------------------------------------------------------------------
#endif





