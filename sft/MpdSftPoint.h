//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                     MpdSftPoint header file                   -----
// -----                 Created 28/07/04  by V. Friese                -----
// -------------------------------------------------------------------------

/**  MpdSftPoint.h
 *@author V. Friese
 **
 ** Interception of MC track with a TOF detector.
 **/


#ifndef MPDSFTPOINT_H
#define MPDSFTPOINT_H 1


#include "TObject.h"
#include "TVector3.h"
#include "FairMCPoint.h"

using namespace std;

//------------------------------------------------------------------------------------------------------------------------
class MpdSftPoint : public FairMCPoint
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
  MpdSftPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t tof, Double_t length, Double_t eLoss);
  MpdSftPoint(const MpdSftPoint& point) { *this = point; };
  
  MpdSftPoint();	
  virtual ~MpdSftPoint();

  // Output to screen 
  virtual void Print(const Option_t* opt) const;

  // Methods for getting the geometric coordinates (gap/cell/module/region)  
  Int_t GetGap() const    {return (fDetectorID & 15);};
  Int_t GetCell() const   {return ((fDetectorID>>4) & 1023);};
  Int_t GetModule() const {return ((fDetectorID>>14) & 1023);};
  Int_t GetRegion() const {return fDetectorID>>24;};
  Int_t GetStrip() const {return fStrip;};
  Int_t GetRad() const {return fRad;};
  void SetStrip(Int_t strip) { fStrip = strip; };
  void SetRad(Int_t rad) { fRad = rad; };

private:
  Int_t fStrip; // strip number
  Int_t fRad; // radial module number

  ClassDef(MpdSftPoint,1)
};

//------------------------------------------------------------------------------------------------------------------------
#endif





