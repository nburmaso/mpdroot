//------------------------------------------------------------------------------------------------------------------------

#ifndef __MPD_NDET_POINT_H
#define __MPD_NDET_POINT_H 1


#include "TObject.h"
#include "TVector3.h"

#include "FairMCPoint.h"

using namespace std;

//------------------------------------------------------------------------------------------------------------------------
class MpdNDetPoint : public FairMCPoint
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
  	MpdNDetPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t tof, Double_t length, Double_t eLoss);
  	MpdNDetPoint(const MpdNDetPoint& point) { *this = point; };

  	MpdNDetPoint();	
  	virtual ~MpdNDetPoint();

	// Output to screen 
  	virtual void Print(const Option_t* opt) const;

        Int_t DetectorID() const {return fDetectorID;};
  Double_t Time() const {return fTime;};
  Double_t ELoss() const {return fELoss;};
  Double_t Length() const {return fLength;};
  Int_t TrackID() const {return fTrackID;};
        Double_t X() const {return GetX();};
        Double_t Y() const {return GetY();};
        Double_t Z() const {return GetZ();};
        Double_t Px() const {return GetPx();};
        Double_t Py() const {return GetPy();};
        Double_t Pz() const {return GetPz();};
  
ClassDef(MpdNDetPoint,1)
};

//------------------------------------------------------------------------------------------------------------------------
#endif





