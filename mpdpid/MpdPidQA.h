#ifndef MPD_PID_QA_H
#define MPD_PID_QA_H

#define nQAHists 40

using namespace std;

#include "MpdPid.h"
#include "Rtypes.h"

class MpdPidQA : public MpdPid
{
		public:
	
	MpdPidQA();   /// default ctor
	
	MpdPidQA(Double_t sigmaTof, Double_t sigmaEloss, Double_t sqrts,   /// generators: "URQMD", "LAQGSM" ("QGSM"), "DEFAULT", "NSIG" (for n-sigma method)
		Double_t koef = 1., TString Generator = "DEFAULT", TString Tracking = "CF",   /// tracking: "HP" (Hit Producer), "CF" (Cluster Finder)
		TString NSigPart = "pikapr");   /// possible expressions: el, mu, pi, ka, pr, de, tr, he3, he4
	
	virtual ~MpdPidQA();   /// destructor
	
	void FillDedxHists(Double_t, Double_t, Int_t);
	void Fillm2Hists(Double_t, Double_t, Int_t);
	void FillAmplHists(Double_t, Int_t);
	Bool_t FillEffContHists(MpdTrack*, Int_t, Double_t fProbCut = 0.);
	Bool_t FillEffContHists(Double_t, Double_t, Int_t, Int_t, Double_t fProbCut = 0.);
	Bool_t FillEffContHists(Double_t, Double_t, Double_t, Int_t, Int_t, Double_t fProbCut = 0.);
	void GetDedxQA(TString);
	void Getm2QA(TString);
	void GetAmplQA(TString);
	void GetEffContQA(TString);
	
		private:
		
	Double_t Xlow[nQAHists]; 
	Double_t Xhigh[nQAHists];
	Double_t X[nQAHists];
	
	TH1D *Pi[nQAHists]; TH1D *Ka[nQAHists]; TH1D *Pr[nQAHists];
	TH2D *m2_pi; TH2D *m2_ka; TH2D *m2_pr;
	TH1D *AllPiPos; TH1D *AllKaPos; TH1D *AllPrPos; TH1D *AllPiNeg; TH1D *AllKaNeg; TH1D *AllPrNeg;
	TH1D *IdPiPos; TH1D *IdKaPos; TH1D *IdPrPos; TH1D *IdPiNeg; TH1D *IdKaNeg; TH1D *IdPrNeg;
	TH1D *IdRightPiPos; TH1D *IdRightKaPos; TH1D *IdRightPrPos; TH1D *IdRightPiNeg; TH1D *IdRightKaNeg; TH1D *IdRightPrNeg;
	TH1D *IdWrongPiPos; TH1D *IdWrongKaPos; TH1D *IdWrongPrPos; TH1D *IdWrongPiNeg; TH1D *IdWrongKaNeg; TH1D *IdWrongPrNeg;
	TH1D *AmplPiPlus; TH1D *AmplPiMinus; TH1D *AmplPrPlus; TH1D *AmplPrMinus; TH1D *AmplKaPlus; TH1D *AmplKaMinus;
	
	void Init(TString);
	void SaveDedxGraphs(TGraphErrors*, TGraphErrors*, TGraphErrors*, TString);
	void Savem2Hists(TH1D*, TH1D*, TH1D*, TString);
	void SaveEffContHists(TString);
	void SaveAmplHists(TString);
	void FillEffDenominator(Double_t, Int_t);
	Bool_t FillEffContHists(Double_t, Int_t, Int_t);
	
	ClassDef(MpdPidQA,1);
};

#endif
