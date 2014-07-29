/** CbmstsMapsHitInfo source file
 *@author Michael Deveaux <m.deveaux@gsi.de>
 ** Acknowledgements to M. Al-Turany, D. Bertini, G. Gaycken
 ** Version beta 0.1 (02.02.2005)
 **/


#include "CbmSttHitInfo.h"


/** Constructor **/
CbmSttHitInfo::CbmSttHitInfo() {
  fFileNumber  = -1;
  fEventNumber = -1;
  fTrackID     = -1;
  fPointID     = -1;
  fNMerged     =  0;
  fIsFake      = kFALSE;
}


/** Constructor with all parameters **/
CbmSttHitInfo::CbmSttHitInfo(Int_t fileNumber, Int_t eventNumber,
			     Int_t trackID, Int_t pointID,
			     Int_t nMerged, Bool_t isFake) {
  fFileNumber  = fileNumber;
  fEventNumber = eventNumber;
  fTrackID     = trackID;
  fPointID     = pointID;
  fNMerged     = nMerged;
  fIsFake      = isFake;
}


/** Destructor **/
CbmSttHitInfo::~CbmSttHitInfo() { }


/** Public method Clear **/
void CbmSttHitInfo::Clear() {
  fFileNumber  = -1;
  fEventNumber = -1;
  fTrackID     = -1;
  fPointID     = -1;
  fNMerged     =  0;
  fIsFake      = kFALSE;
}



ClassImp(CbmSttHitInfo);
