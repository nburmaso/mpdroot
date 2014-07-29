
#ifndef MPD_VERTEXZFINDER_H
#define MPD_VERTEXZFINDER_H

#include "FairTask.h"

#include "TH1.h"
#include "TF1.h"
#include "TClonesArray.h"

class MpdVertexZfinder :public FairTask
{
 public:

  /** Constructor **/
  MpdVertexZfinder(const char *name="MpdVertexZfinder", Int_t iVerbose = 1);
  
  /** Destructor **/
  virtual ~MpdVertexZfinder();
  
  /// * FairTask methods
  
  /** Intialisation at begin of run. To be implemented in the derived class.
   *@value  Success   If not kSUCCESS, task will be set inactive.
   **/
  InitStatus Init();
  
  /** Reinitialisation.
   *@value  Success   If not kSUCCESS, task will be set inactive.
   **/
  InitStatus ReInit();

  /** Intialise parameter containers.
   **/
  void SetParContainers();

  void Exec(Option_t * option);

  /** Action after each event. **/
  void Finish();
  void Reset();
  void SetHits(const TClonesArray *hits, const TH1F *hLays); // set hits container and histo
  Double_t FindZ(const Int_t *layPointers, Int_t &flag); // evaluate vertex Z-position

 private:

  TH1F *fhZ;                    // histogram of Z-positions
  const TH1F *fhLays;           // histogram of layer occupancy
  const TClonesArray *fKHits;   // array of Kalman hits
  TF1 *fUnc;                    // fitting function

 private:
  // Some constants
  //static const Double_t fgkChi2Cut; // max accepted Chi2 of hit for track

  ClassDef(MpdVertexZfinder,1);
};
#endif
