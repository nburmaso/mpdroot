//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                     MpdBbcPoint header file                   -----
// -------------------------------------------------------------------------

#ifndef MPDBBCPOINT_H
#define MPDBBCPOINT_H


#include "TObject.h"
#include "TVector3.h"
#include "FairMCPoint.h"

using namespace std;

//------------------------------------------------------------------------------------------------------------------------
class MpdFsaPoint : public FairMCPoint
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
  	MpdFsaPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t tof, Double_t length, Double_t eLoss);
  	MpdFsaPoint(const MpdFsaPoint& point) { *this = point; };

  	MpdFsaPoint();	
  	virtual ~MpdFsaPoint();

	// Output to screen 
  	virtual void Print(const Option_t* opt) const;

  ClassDef(MpdFsaPoint,1)

};

//------------------------------------------------------------------------------------------------------------------------
#endif





