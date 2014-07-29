//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                     MpdCpcPoint header file                   -----
// -------------------------------------------------------------------------

#ifndef MPDCPCPOINT_H
#define MPDCPCPOINT_H


#include "TObject.h"
#include "TVector3.h"
#include "FairMCPoint.h"

using namespace std;

//------------------------------------------------------------------------------------------------------------------------
class MpdCpcPoint : public FairMCPoint
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
  	MpdCpcPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t tof, Double_t length, Double_t eLoss);
  	MpdCpcPoint(const MpdCpcPoint& point) { *this = point; };

  	MpdCpcPoint();	
  	virtual ~MpdCpcPoint();

	// Output to screen 
  	virtual void Print(const Option_t* opt) const;

  ClassDef(MpdCpcPoint,1)

};

//------------------------------------------------------------------------------------------------------------------------
#endif





