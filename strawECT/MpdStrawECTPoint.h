//------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -----                     MpdStrawECTPoint header file                   -----
// -------------------------------------------------------------------------

#ifndef MPDStrawECTPOINT_H
#define MPDStrawECTPOINT_H


#include "TObject.h"
#include "TVector3.h"
#include "FairMCPoint.h"

using namespace std;

//------------------------------------------------------------------------------------------------------------------------
class MpdStrawECTPoint : public FairMCPoint
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
  	MpdStrawECTPoint(Int_t trackID, Int_t detID, TVector3 pos, TVector3 mom, Double_t tof,
            Double_t length, Double_t eLoss);
  	MpdStrawECTPoint(const MpdStrawECTPoint& point) { *this = point; };

  	MpdStrawECTPoint();
  	virtual ~MpdStrawECTPoint();

	// Output to screen
  	virtual void Print(const Option_t* opt) const;

        //Setters
        void SetModule(Int_t module) { fModule = module; }
        void SetSubmodule(Int_t submodule) { fSubmodule = submodule; }
        void SetLayer(Int_t layer) { fLayer = layer; }
        void SetLayerType(TString layer_type) { fLayerType = layer_type; }
        void SetStraw(Int_t straw) { fStraw = straw; }

        //Getters
        Int_t GetModule() { return fModule; }
        Int_t GetSubmodule() { return fSubmodule; }
        Int_t GetLayer() { return fLayer; }
        TString GetLayerType() { return fLayerType; }
        Int_t GetStraw() { return fStraw; }

protected:
    Double_t fModule;
    Double_t fSubmodule;
    Double_t fLayer;
    TString  fLayerType;
    Double_t fStraw;

  ClassDef(MpdStrawECTPoint,2)

};

//------------------------------------------------------------------------------------------------------------------------
#endif





