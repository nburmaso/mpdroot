// --------------------------------------------------------------------//
//                   MpdNDetAnalysis header file                       //
//                   ............................                      // 
//                          Created May 2009                           //
//                   ............................                      //
// --------------------------------------------------------------------//

#ifndef MPDNDETANALYSIS_H
#define MPDNDETANALYSIS_H

#include "FairTask.h"   // keep CBM
#include "FairStack.h"  // names

class FairStack;
class TArrayD;
class TH1F;
class TH2F;

class TH1D;
class TH2D;
class TFile;

class MpdNDetAnalysis : public FairTask {

public:

  MpdNDetAnalysis();
  MpdNDetAnalysis(const char *name, const char *title="NDET analysis");
  virtual ~MpdNDetAnalysis();

  virtual InitStatus Init();
  void Exec(Option_t* option);
  void Finish();
  void SetDebug(Option_t *debug="") {fDebug = debug;}
  void ReadMCTrack();//KM test

  void OneEventAnalysis();
  void Acceptance();
  void WriteOutput();

  //---------------------
  void AnalyzePrimary();
  void AnalyzeMCPointsEdep();
  void AnalyzeMCPointsWall();
  /* void AnalyzeHits(); */
  /* void AnalyzeRecParticles(); */

private:

  TClonesArray* fMCtrack;      // List of particles in a stack
  TClonesArray* fNDetPointLite;// NDET MC points inside NDET
  TClonesArray* fNDetPoint;// NDET MC points on entrance to NDET
  TClonesArray* fListNDEThits;   // NDET hits
  TClonesArray* fListNDETrp;     // NDET rec.particles

  Int_t    fEvent;     // current event number
  Int_t    fNprimary;  // number of primary tracks

  //Test putput root file
  TFile *ftest;

  TH1F *fhNevents;//Number of processed events 
  // All Neutrons on the entrance of NDet
  TH1F *fhNeut0Pabs;  // Abs momentum distribution
  TH1F *fhNeut0Pt;    // Transverse momentum distribution
  TH1F *fhNeut0Eta;   // Pseudorapidity distribution
  TH1F *fhNeut0Theta; // Polar angle distribution
  TH1F *fhNeut0Phi;   // Azimuthal angle distribution
  TH2F *fhNeut0PtEta; // Transverse momentum vs Eta
  TH1F *fhNeut0Rad;
  TH2F *fhNeut0RadZ;

  TH1F *fhNeut1Pabs;  // Abs momentum distribution
  TH1F *fhNeut1Pt;    // Transverse momentum distribution
  TH1F *fhNeut1Eta;   // Pseudorapidity distribution
  TH1F *fhNeut1Theta; // Polar angle distribution
  TH1F *fhNeut1Phi;   // Azimuthal angle distribution
  TH2F *fhNeut1PtEta; // Transverse momentum vs Eta
  TH1F *fhNeut1Rad;
  TH2F *fhNeut1RadZ;
    TH2F *fhNeut1RadE;
   // All Not Neutrons on the entrance of NDet
  TH1F *fhProt0Pabs;  // Abs momentum distribution
  TH1F *fhProt0Pt;    // Transverse momentum distribution
  TH1F *fhProt0Eta;   // Pseudorapidity distribution
  TH1F *fhProt0Theta; // Polar angle distribution
  TH1F *fhProt0Phi;   // Azimuthal angle distribution
  TH2F *fhProt0PtEta; // Transverse momentum vs Eta
  TH1F *fhProt0Rad;

  TH1F *fhProt1Pabs;  // Abs momentum distribution
  TH1F *fhProt1Pt;    // Transverse momentum distribution
  TH1F *fhProt1Eta;   // Pseudorapidity distribution
  TH1F *fhProt1Theta; // Polar angle distribution
  TH1F *fhProt1Phi;   // Azimuthal angle distribution
  TH2F *fhProt1PtEta; // Transverse momentum vs Eta
  TH1F *fhProt1Rad;

  // MC points

  TH1F *fhMcpPabs;   // Abs momentum distribution of detected
  TH1F *fhMcpPt;     // Transverse momentum distribution of detected
  TH1F *fhMcpEta;    // Pseudorapidity distribution of detected
  TH1F *fhMcpTheta;  // Polar angle distribution of detected
  TH1F *fhMcpPhi;    // Azimuthal angle distribution of detected
  TH2F *fhMcpPtEta;  // Transverse momentum vs Eta of detected
  TH2F *fhMcpXY;        // (X,Y) coordinates of MC points
  
  TH1D *fhMcpPdgNdet;//MC points PDG code 
  // Hits

  TH2F *fhHitXY;     // x,y coordinates of NDET hits
  TH1F *fhHitE;      // NDET hit energy

  // Reconstructed particles

  TH1F *fhRecPabs;   // reconstructed absolute momentum
  TH1F *fhRecPt;     // reconstructed pt
  TH1F *fhRecYrap;   // reconstructed rapidity
  TH1F *fhRecPhi;    // reconstructed azimuthal angle

  // Acceptance

  TH2F* fhPtYrap;       // Pt and Y rapidity of primary particles
  TH2F* fhPtYrapSignal; // Pt and Y rapidity of signal particles
  TH2F* fhPtYrapAccept; // acceptance

  Option_t *fDebug;    //! debug flag

  ClassDef(MpdNDetAnalysis,2)

}; 

#endif
