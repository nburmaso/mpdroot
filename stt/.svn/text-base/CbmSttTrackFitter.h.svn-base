// -------------------------------------------------------------------------
// -----                   CbmSttTrackFitter header file               -----
// -----                  Created 18/02/05  by V. Friese               -----
// -------------------------------------------------------------------------


/** CbmSttTrackFitter
 *@author V.Friese <v.friese@gsi.de>
 **
 ** Abstract base class for concrete STT track fitting algorithm.
 ** Each derived class must implement the method DoFit. This has
 ** to operate on an object of type CbmSttTrack and fill the
 ** parameters fPidHypo, fParamFirst, fParamLast, fFlag and fChi2.
 **/

#ifndef CBMSTTTRACKFITTER
#define CBMSTTTRACKFITTER 1

#include "TObject.h"
#include "FairTrackParam.h"

class CbmSttTrack;
class TClonesArray;

class CbmSttTrackFitter : public TObject
{

 public:
  virtual void AddHitCollection(TClonesArray* mHitArray) { };

  /** Default constructor **/
  CbmSttTrackFitter() { };


  /** Destructor **/
  virtual ~CbmSttTrackFitter() { };


  /** Virtual method Init. If needed, to be implemented in the
   ** concrete class. Else no action.
   **/
  virtual void Init() { };


  /** Abstract method DoFit. To be implemented in the concrete class.
   ** Task: Make a fit to the hits attached to the track by the track
   ** finder. Fill the track parameter member variables.
   **
   *@param pTrack      Pointer to CbmSttTrack
   *@param pidHypo     PID hypothesis for the fit. Default is pion.
   **/
  virtual Int_t DoFit(CbmSttTrack* pTrack, Int_t pidHypo = 211) = 0;


  /** Abstract method Extrapolate. Gives track parameters at a given r
   ** position.
   **
   *@param track  Pointer to SttTrack
   *@param r      r position
   *@param param  (return value) SttTrackParam at r
   **/
  virtual void Extrapolate( CbmSttTrack* track, Double_t r, 
                            FairTrackParam *param ) = 0;


  ClassDef(CbmSttTrackFitter,1);

};

#endif
