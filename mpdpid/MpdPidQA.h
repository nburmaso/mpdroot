#ifndef MPD_PID_QA_H
#define MPD_PID_QA_H

#define nQAHists 40
#define nPIDparticles 9

using namespace std;

#include "MpdPid.h"
#include "Rtypes.h"
#include "TGraphAsymmErrors.h"
#include <vector>
#include <iterator>
#include <map>

class MpdPidQA : public MpdPid
{
		public:
	
	MpdPidQA();   /// default ctor
	
	MpdPidQA(Double_t sigmaTof, Double_t sigmaEloss, Double_t sqrts,   /// generators: "URQMD", "LAQGSM" ("QGSM"), "DEFAULT", "NSIG" (for n-sigma method)
		Double_t koef = 1., TString Generator = "DEFAULT", TString Tracking = "CF",   /// tracking: "HP" (Hit Producer), "CF" (Cluster Finder)
		TString NSigPart = "pikapr");   /// possible expressions: el, mu, pi, ka, pr, de, tr, he3, he4
	
	virtual ~MpdPidQA() {};   /// destructor
	
	struct dEdXStruct 
	{ 
		TH1D *dEdXPart[nQAHists];
		Int_t ibeg; Int_t iend;
	};
	struct EffContStruct
	{ 
		TH1D *EffContPart[4];
	};
	void FillDedxHists(Double_t, Double_t, Int_t);
	void Fillm2Hists(Double_t, Double_t, Int_t);
	void FillAmplHists(Double_t, Int_t);
	Bool_t FillEffContHists(MpdTrack*, Int_t, Double_t fProbCut = 0.);
	Bool_t FillEffContHists(Double_t, Double_t, Int_t, Int_t, Double_t fProbCut = 0.);
	Bool_t FillEffContHists(Double_t, Double_t, Double_t, Int_t, Int_t, Double_t fProbCut = 0.);
	void GetDedxQA(TString);
	void Getm2QA(TString);
	void GetAmplQA(TString);
	void GetEffContQA(TString dir, TString s1 = "", TString s2 = "", TString s3 = "");
	
		private:
	
	Double_t Xlow[nQAHists]; 
	Double_t Xhigh[nQAHists];
	Double_t X[nQAHists];
	
	map <Int_t,dEdXStruct> dEdX;
	map <Int_t,TH2D*> m2Hists;
	map <Int_t,TH1D*> ampls;
	map <Int_t,EffContStruct> effcont;
	
	TString nSigPart;
	
	void Init(TString, TString);
	void SaveDedxGraphs(map <Int_t,TGraphAsymmErrors*>, TString);
	void Savem2Hists(map <Int_t,TH1D*>, TString);
	void SaveEffContHists(TString, TString, TString, TString);
	void SaveAmplHists(map <Int_t,TF1*>, TString);
	void FillEffDenominator(Double_t, Int_t);
	Bool_t FillEffContHists(Double_t, Int_t, Int_t);
	Double_t Novosibirsk (Double_t*, Double_t*);
	Double_t (MpdPid::*GetDedxParam)(Double_t);
	
	ClassDef(MpdPidQA,2);
};

#endif
