#ifndef MPD_PID_AZ_H
#define MPD_PID_AZ_H

#define MASS_MU 0.1057
#define MASS_PI 0.1396
#define MASS_PI2 0.0195
#define MASS_KA 0.4937
#define MASS_PR 0.9383
#define MASS_DE 1.876
#define MASS_TR 2.8094
#define MASS_HE3 1.4047
#define MASS_HE4 1.863

#define PDG_DEUTERON 1000010020
#define PDG_TRITON 1000010030
#define PDG_HE3 1000020030
#define PDG_HE4 1000020040

#include "MpdTrack.h"
#include "MpdEvent.h"
#include "MpdTpcKalmanTrack.h"
#include "FairMCEventHeader.h"
#include "FairMCTrack.h"
#include "FairTask.h"
#include "FairRootManager.h"
#include "FairRunAna.h"
#include "FairRuntimeDb.h"
#include <TClonesArray.h>
#include <TCollection.h>
#include <TFriendElement.h>
#include <TList.h>
#include <TChain.h>
#include <TTree.h>
#include <TSystem.h>
#include <TGraphErrors.h>
#include <TCanvas.h>
#include <TF1.h>
#include <TF2.h>
#include <TH1D.h>
#include <TH2D.h>
#include <TMath.h>
#include <TObject.h>
#include <TROOT.h>
#include <TFitter.h>
#include <TLegend.h>
#include "TString.h"

using namespace std;

class MpdPid_AZ : public TObject
{
	public:
	
   //void Pidcheck(TString, Int_t);
   
   TF1 *parElBB;
   TF1 *parMuBB;
   TF1 *parPiBB1;
   TF1 *parPiBB2;
   TF1 *parPiBB3;
   TF1 *parPiBB4;
   TF1 *parKaBB1;
   TF1 *parKaBB2;
   TF1 *parKaBB3;
   TF1 *parKaBB4;
   TF1 *parPrBB1;
   TF1 *parPrBB2;
   TF1 *parPrBB3;
   TF1 *parPrBB4;
   TF1 *parDeBB;
   TF1 *parTrBB;
   TF1 *parHe3BB;
   TF1 *parHe4BB;
   
   TF1 *parElNegMom;
   TF1 *parElPosMom;
   TF1 *parMuNegMom;
   TF1 *parMuPosMom;
   TF1 *parPiNegMom;
   TF1 *parPiPosMom;
   TF1 *parKaNegMom;
   TF1 *parKaPosMom;
   TF1 *parPrPosMom;
   TF1 *parPrNegMom;
   TF1 *parDeMom;
   TF1 *parTrMom;
   TF1 *parHe3Mom;
   TF1 *parHe4Mom;
   
   MpdPid_AZ();
   
   MpdPid_AZ(Double_t sigmaTof, Double_t sigmaEloss, Double_t sqrts, Double_t koef = 1., TString Generator = "DEFAULT", TString Tracking = "CF");
   // generators: "URQMD", "LAQGSM" ("QGSM"), "DEFAULT"
   // tracking: "HP" (Hit Producer), "CF" (Cluster Finder)
   
   virtual ~MpdPid_AZ(){}
   Bool_t FillProbs(MpdTrack*);
   Bool_t FillProbs(MpdTrack*, Double_t);
   Bool_t FillProbs(Double_t, Double_t, Double_t, Int_t); 
   // variables: full momentum, eta, dE/dx, charge
   Bool_t FillProbs(Double_t, Double_t, Double_t, Double_t, Int_t); 
   // variables: full momentum, eta, dE/dx, mass squared, charge
   Double_t GetProbPi(void){return fProbPi;}
   Double_t GetProbMu(void){return fProbMu;}
   Double_t GetProbPr(void){return fProbPr;}
   Double_t GetProbKa(void){return fProbKa;}
   Double_t GetProbEl(void){return fProbEl;}
   Double_t GetProbDe(void){return fProbDe;}
   Double_t GetProbTr(void){return fProbTr;}
   Double_t GetProbHe3(void){return fProbHe3;}
   Double_t GetProbHe4(void){return fProbHe4;}
   Long_t GetMaxProb();
   
   Double_t GetDedxPiParam(Double_t);
   Double_t GetDedxPiParam(Double_t, Double_t);
   Double_t GetDedxMuParam(Double_t);
   Double_t GetDedxPrParam(Double_t);
   Double_t GetDedxPrParam(Double_t, Double_t);
   Double_t GetDedxKaParam(Double_t);
   Double_t GetDedxKaParam(Double_t, Double_t);
   Double_t GetDedxElParam(Double_t);
   Double_t GetDedxDeParam(Double_t);
   Double_t GetDedxTrParam(Double_t);
   Double_t GetDedxHe3Param(Double_t);
   Double_t GetDedxHe4Param(Double_t);

   Double_t GetPrRat() {return prrat;}
   void SetPrRat(Double_t PrRat) {prrat=PrRat; parPrNegMom->SetParameter(0, (parPrPosMom->GetParameter(0) / prrat));}
   
   Double_t MomPi(Double_t *x, Double_t *par);
   Double_t MomPr(Double_t *x, Double_t *par);
   
   private:
   
   TF1 *parElM2;
   TF1 *parMuM2;
   TF1 *parPiLowPM2;
   TF1 *parPiHighPM2;
   TF1 *parKaM2;
   TF1 *parPrLowPM2;
   TF1 *parPrHighPM2;
   TF1 *parDeM2;
   TF1 *parTrM2;
   TF1 *parHe3M2;
   TF1 *parHe4M2;
   
   TF1 *fgaus;
   TF2 *fgaus2;
   
   Double_t fProbEl;
   Double_t fProbMu;
   Double_t fProbPi;
   Double_t fProbKa;
   Double_t fProbPr;
   Double_t fProbDe;
   Double_t fProbTr;
   Double_t fProbHe3;
   Double_t fProbHe4;
   
   Double_t prrat; // rat is pos./neg.
   Double_t fSigmaDedx_1, fSigmaDedx_2, fSigmaDedx_3;
   Double_t fKoef; // scale of dedx
   
   Double_t kSigmaTof;
   Double_t kSigmaEloss;
   
   Int_t fCharge;
   
   Double_t fEnergy;
   
   void Init(TString, TString);
   Double_t GetDedxProb(Double_t, Double_t, Double_t, Double_t, Double_t);
   Double_t GetCombProb(Double_t, Double_t, Double_t, Double_t, Double_t, Double_t, Double_t, Double_t);
   
   //void CheckMethodOne(TString, Int_t);
   //void CheckMethodTwo(TString, Int_t);
   TH1D* GetHist(Int_t, Int_t, TH2D*);
   TH1D* GetMass2Width(TH2D*, Int_t, Int_t, Double_t, Double_t, Double_t, Double_t);
   TH1D* GetPidNormAmpls(Int_t, Double_t, Int_t, Int_t);
   TGraphErrors* GetTGraphErrors (TH2D*, TH2D*, Double_t, TF1*, Int_t);
   TGraphErrors* GetTGraphErrors (TH2D*, Double_t, TF1*, Int_t);
   
   ClassDef(MpdPid_AZ,1);
};

#endif
