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
class MpdCpcPoint : public FairMCPoint {

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
  MpdCpcPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t tof, Double_t length, Double_t eLoss, Int_t CpcID, Int_t RingID, Int_t CellID);
 //MpdCpcPoint(const MpdCpcPoint& point):FairMCPoint(point) { *this = point; }

  Double_t GetDetectorID(){return fDetectorID;}

  	MpdCpcPoint();
  	virtual ~MpdCpcPoint();

	// Output to screen
  	virtual void Print(const Option_t* opt) const;
	Int_t GetCpcID(){return fCpcID;}
	Int_t GetRingID(){return fRingID;}
	Int_t GetCellID(){return fCellID;}
	Double_t GeteLoss(){return feLoss;}


  ClassDef(MpdCpcPoint,1)
  double fDetectorID;

  Int_t fCpcID;
  Int_t fRingID;
  Int_t fCellID;
  Double_t feLoss;
};

//------------------------------------------------------------------------------------------------------------------------
#endif





