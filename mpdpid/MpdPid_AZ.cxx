#include "MpdPid_AZ.h"
#include "FairMCTrack.h"

MpdPid_AZ::MpdPid_AZ() : TObject(){

      parElBB = 0;
      parPiBB1 = 0;
      parPiBB2 = 0;
      parPiBB3 = 0;
      parPiBB4 = 0;
      parKaBB1 = 0;
      parKaBB2 = 0;
      parKaBB3 = 0;
      parKaBB4 = 0;
      parPrBB1 = 0;
      parPrBB2 = 0;
      parPrBB3 = 0;
      parPrBB4 = 0;
      parDeBB = 0;
      parTrBB = 0;
      parHe3BB = 0;
      parHe4BB = 0;
      fProbEl = 0.;
      fProbPi = 0.;
      fProbKa = 0.;
      fProbPr = 0.;
      fProbDe = 0.;
      fProbTr = 0.;
      fProbHe3 = 0.;
      fProbHe4 = 0.;
      fCharge = 1;
      fEnergy = 4.;
}


MpdPid_AZ::MpdPid_AZ(Double_t sigmaTof, Double_t sigmaEloss, Double_t sqrts, Double_t koef, TString Generator, TString Tracking)
   : TObject(), kSigmaTof(sigmaTof), kSigmaEloss(sigmaEloss), fCharge(1), fKoef(koef),
     fEnergy(sqrts) 
{
   Init(Generator, Tracking);

}

Double_t MpdPid_AZ::GetDedxProb(Double_t dedx, Double_t cut, Double_t n, Double_t emean, Double_t sige)
{
	Double_t fProb = 0;
	if (TMath::Abs((dedx/emean-1.)/sige) < cut)
	{
		fgaus->SetParameters(n,1.,sige);
		fProb=fgaus->Eval(dedx/emean);
	} 
	return fProb;
}

Double_t MpdPid_AZ::GetCombProb(Double_t dedx, Double_t m2, Double_t cut, Double_t n, Double_t emean, Double_t mmean, Double_t sige, Double_t sigm)
{
	Double_t xx, yy, distance, fProb = 0;
      
    xx = (dedx/emean-1.)/sige;
    yy = (m2-mmean)/sigm;
    distance = TMath::Sqrt(xx*xx+yy*yy);
    if (distance < cut)
    {
       fgaus2->SetParameters(n,mmean,sigm,1.,sige);
       fProb=fgaus2->Eval(m2,dedx/emean);
    }
    return fProb;
}

// Sum of two exponents in total momentum (pions and electrons)
Double_t MpdPid_AZ::MomPi(Double_t *x, Double_t *par)
{
  Double_t p = x[0], xx, x1, x2;
  xx=sqrt(p*p+par[4]*par[4])-par[4];
  x1=(1+par[1])/(par[2]*(par[4]+par[2]))/exp(xx/par[2]);
  x2=par[1]/(par[3]*(par[4]+par[3]))/exp(xx/par[3]);
  
  return (par[0]*p*(x1+x2));    
}

// Difference of 2 exponents in total momentum (All specie, except pi's and e+/-)
Double_t MpdPid_AZ::MomPr(Double_t *x, Double_t *par)
{
  Double_t p = x[0], xx, x1, x2;
  xx=sqrt(p*p+par[4]*par[4])-par[4];
  x1=(1+par[1])/(par[2]*(par[4]+par[2]))/exp(xx/par[2]);
  x2=par[1]/(par[3]*(par[4]+par[3]))/exp(xx/par[3]);
  
  return (par[0]*p*(x1-x2));
}

TH1D* MpdPid_AZ::GetMass2Width(TH2D* prototype, Int_t merge, Int_t nbinsx, Double_t l_border, Double_t r_border, Double_t l_gaus_border, Double_t r_gaus_border)
{
	TString oldName = prototype->GetName();
	prototype->SetName("prototype");
	TF1 *mygaus = new TF1 ("mygaus", "gaus", l_gaus_border, r_gaus_border);
	Double_t pminSlice = 0., pmaxSlice = 0.;
	Int_t minbin = 0, maxbin = 0;
	for (Int_t i=1; i<=nbinsx; i++)
	{
		TH1D* proj = GetHist(i, merge, prototype);
		if (prototype->GetXaxis()->GetBinCenter(i) < l_border) continue;
		minbin = i; break;
	}
	maxbin = nbinsx;
	for (Int_t i=minbin; i<=nbinsx; i++)
	{
		TH1D* proj = GetHist(i, merge, prototype);
		if (prototype->GetXaxis()->GetBinCenter(i) < r_border) continue;
		maxbin = i-1; break;
	}
	
	prototype->FitSlicesY(mygaus, minbin, maxbin, 0, "QNR G5", 0);
	TH1D* prototype_2 = (TH1D*)gDirectory->Get("prototype_2");
	pminSlice = prototype_2->GetBinLowEdge(minbin); pmaxSlice = prototype_2->GetBinLowEdge(maxbin+1); 
	prototype_2->SetAxisRange(pminSlice, pmaxSlice);
	
	prototype->SetName(oldName);
	return prototype_2;
}

TH1D* MpdPid_AZ::GetHist(Int_t bin, Int_t mergeInt, TH2D* prototype)
{
	static Int_t GetStringInt = 0;
	GetStringInt++;
	TString namehist(Form("hist%05d", GetStringInt));
	TH1D* projectionY = prototype->ProjectionY(namehist, bin, bin+mergeInt, "");
	return projectionY;
}

TGraphErrors* MpdPid_AZ::GetTGraphErrors (TH2D* dEdXmpdP, Double_t PMIN, TF1* parBB, Int_t part)
{
	TString oldNameTF1 = parBB->GetName(), oldName = dEdXmpdP->GetName();
	Double_t PIDparam; 
	Double_t dEdXmin = 0., dEdXmax = fKoef*18.e-06;
	TF1* xfunc = new TF1("xfunc","x", dEdXmin, dEdXmax);
	parBB->SetName("parBB"); dEdXmpdP->SetName("dEdXmpdP");
	TF1* parBBMultX = new TF1("parBBMultX","xfunc * parBB", dEdXmin, dEdXmax);
	TF1* GraphGaus = new TF1("GraphGaus", "gaus", dEdXmin, dEdXmax);
	Double_t a, b; // TF1-limits of integral
	Double_t mpdP;
	Int_t nbinsGraphmax = 100; Int_t mergeGraph = (Int_t) (500/nbinsGraphmax); // NBINSX dEdXfrommpdP... = 500
	Int_t nbinsGraph = nbinsGraphmax; Int_t GraphBinCounter = 0;
	
	for (Int_t j=1; j<nbinsGraphmax*mergeGraph; j+=mergeGraph)
	{
		if ((dEdXmpdP->GetXaxis()->GetBinLowEdge(j) <= PMIN) && (dEdXmpdP->GetXaxis()->GetBinLowEdge(j+mergeGraph) >= PMIN))
		{
			nbinsGraph--;
			break;
		}
		else nbinsGraph--;
	}
	
	Double_t* GrPointX = new Double_t[nbinsGraph]; Double_t* GrPointY = new Double_t[nbinsGraph]; 
	Double_t* GrPointError = new Double_t[nbinsGraph]; 
	for (Int_t i=0; i<nbinsGraph; i++) {GrPointX[i] = 0; GrPointY[i] = 0; GrPointError[i] = 0;}
	Double_t *zeros = new Double_t[nbinsGraph]; for (Int_t i=0; i<nbinsGraph; i++) zeros[i] = 0;
	GraphBinCounter = 0;
	
	for (Int_t i=1+mergeGraph*(nbinsGraphmax-nbinsGraph); i<mergeGraph*nbinsGraphmax; i+=mergeGraph)
	{
		GrPointX[GraphBinCounter] = dEdXmpdP->GetXaxis()->GetBinLowEdge(i+mergeGraph/2);
		TH1D* proj = GetHist(i, mergeGraph-1, dEdXmpdP);
		proj->SetName("proj");
		proj->Fit(GraphGaus, "Q0R");
		a = dEdXmpdP->GetXaxis()->GetBinLowEdge(i); b = dEdXmpdP->GetXaxis()->GetBinUpEdge(i+mergeGraph-1);
		mpdP = parBBMultX->Integral(a,b)/(parBB->Integral(a,b));
		switch (part)
		{
			case 0: PIDparam = GetDedxElParam(mpdP); break;
			case 1: PIDparam = GetDedxMuParam(mpdP); break;
			case 2: PIDparam = GetDedxPiParam(mpdP); break;
			case 3: PIDparam = GetDedxKaParam(mpdP); break;
			case 4: PIDparam = GetDedxPrParam(mpdP); break;
			case 5: PIDparam = GetDedxDeParam(mpdP); break;
			case 6: PIDparam = GetDedxTrParam(mpdP); break;
			case 7: PIDparam = GetDedxHe3Param(mpdP); break;
			case 8: PIDparam = GetDedxHe4Param(mpdP); break;
		}
		GrPointY[GraphBinCounter]=GraphGaus->GetParameter(1)/(PIDparam);
		GrPointError[GraphBinCounter]=GraphGaus->GetParameter(2)/(PIDparam);
		GraphBinCounter++;
	}
	
	parBB->SetName(oldNameTF1); dEdXmpdP->SetName(oldName);
	TGraphErrors *gr = new TGraphErrors(nbinsGraph,GrPointX,GrPointY,zeros,GrPointError);
	return gr;
}

TGraphErrors* MpdPid_AZ::GetTGraphErrors (TH2D* dEdXmpdP, TH2D* dEdXmpdPUp, Double_t PMIN, TF1* parBB, Int_t part)
{
	TString oldNameTF1 = parBB->GetName(), oldNameLow = dEdXmpdP->GetName(), oldNameUp = dEdXmpdPUp->GetName();
	Double_t PIDparam; 
	Double_t dEdXmin = 0., dEdXmax = fKoef*18.e-06;
	TF1* xfunc = new TF1("xfunc","x", dEdXmin, dEdXmax);
	parBB->SetName("parBB"); dEdXmpdP->SetName("dEdXmpdP"); dEdXmpdPUp->SetName("dEdXmpdPUp");
	TF1* parBBMultX = new TF1("parBBMultX","xfunc * parBB", dEdXmin, dEdXmax);
	TF1* GraphGaus = new TF1("GraphGaus", "gaus", dEdXmin, dEdXmax);
	Double_t a, b; // TF1-limits of integral
	Double_t mpdP;
	Int_t nbinsGraphmax = 100; Int_t mergeGraph = (Int_t) (500/nbinsGraphmax); // NBINSX dEdXfrommpdP... = 500
	Int_t nbinsGraph = nbinsGraphmax; Int_t GraphBinCounter = 0;
	
	for (Int_t j=1; j<nbinsGraphmax*mergeGraph; j+=mergeGraph)
	{
		if ((dEdXmpdP->GetXaxis()->GetBinLowEdge(j) <= PMIN) && (dEdXmpdP->GetXaxis()->GetBinLowEdge(j+mergeGraph) >= PMIN))
		{
			nbinsGraph--;
			break;
		}
		else nbinsGraph--;
	}
	
	Double_t* GrPointX = new Double_t[nbinsGraph]; Double_t* GrPointY = new Double_t[nbinsGraph]; 
	Double_t* GrPointError = new Double_t[nbinsGraph]; 
	for (Int_t i=0; i<nbinsGraph; i++) {GrPointX[i] = 0; GrPointY[i] = 0; GrPointError[i] = 0;}
	Double_t *zeros = new Double_t[nbinsGraph]; for (Int_t i=0; i<nbinsGraph; i++) zeros[i] = 0;
	GraphBinCounter = 0;
	
	for (Int_t i=1+mergeGraph*(nbinsGraphmax-nbinsGraph); i<mergeGraph*nbinsGraphmax; i+=mergeGraph)
	{
		GrPointX[GraphBinCounter] = dEdXmpdP->GetXaxis()->GetBinLowEdge(i+mergeGraph/2);
		TH1D* proj = GetHist(i, mergeGraph-1, dEdXmpdP);
		proj->SetName("proj");
		proj->Fit(GraphGaus, "Q0R");
		a = dEdXmpdP->GetXaxis()->GetBinLowEdge(i); b = dEdXmpdP->GetXaxis()->GetBinUpEdge(i+mergeGraph-1);
		mpdP = parBBMultX->Integral(a,b)/(parBB->Integral(a,b));
		switch (part)
		{
			case 0: PIDparam = GetDedxElParam(mpdP); break;
			case 1: PIDparam = GetDedxMuParam(mpdP); break;
			case 2: PIDparam = GetDedxPiParam(mpdP); break;
			case 3: PIDparam = GetDedxKaParam(mpdP); break;
			case 4: PIDparam = GetDedxPrParam(mpdP); break;
			case 5: PIDparam = GetDedxDeParam(mpdP); break;
			case 6: PIDparam = GetDedxTrParam(mpdP); break;
			case 7: PIDparam = GetDedxHe3Param(mpdP); break;
			case 8: PIDparam = GetDedxHe4Param(mpdP); break;
		}
		GrPointY[GraphBinCounter]=GraphGaus->GetParameter(1)/(PIDparam);
		GrPointError[GraphBinCounter]=GraphGaus->GetParameter(2)/(PIDparam);
		GraphBinCounter++;
		if ((dEdXmpdP->GetXaxis()->GetBinUpEdge(i+mergeGraph-1)) >= 1.5) break;
	}
	
	mergeGraph *= 1.2; // mergeGraph *= (nKEVall/nKEVup) * (nBINSup/nBINSall)
	for (Int_t i=1; i < 300; i+=mergeGraph)
	{
		GrPointX[GraphBinCounter] = dEdXmpdPUp->GetXaxis()->GetBinLowEdge(i+mergeGraph/2);
		TH1D* proj = GetHist(i, mergeGraph-1, dEdXmpdPUp);
		proj->Fit(GraphGaus, "Q0R");
		a = dEdXmpdPUp->GetXaxis()->GetBinLowEdge(i); b = dEdXmpdPUp->GetXaxis()->GetBinUpEdge(i+mergeGraph-1);
		mpdP = parBBMultX->Integral(a,b)/(parBB->Integral(a,b));
		switch (part)
		{
			case 0: PIDparam = GetDedxElParam(mpdP); break;
			case 1: PIDparam = GetDedxMuParam(mpdP); break;
			case 2: PIDparam = GetDedxPiParam(mpdP); break;
			case 3: PIDparam = GetDedxKaParam(mpdP); break;
			case 4: PIDparam = GetDedxPrParam(mpdP); break;
			case 5: PIDparam = GetDedxDeParam(mpdP); break;
			case 6: PIDparam = GetDedxTrParam(mpdP); break;
			case 7: PIDparam = GetDedxHe3Param(mpdP); break;
			case 8: PIDparam = GetDedxHe4Param(mpdP); break;
		}
		GrPointY[GraphBinCounter]=GraphGaus->GetParameter(1)/(PIDparam);
		GrPointError[GraphBinCounter]=GraphGaus->GetParameter(2)/(PIDparam);
		GraphBinCounter++;
	}
	
	parBB->SetName(oldNameTF1); dEdXmpdP->SetName(oldNameLow); dEdXmpdPUp->SetName(oldNameUp);
	TGraphErrors *gr = new TGraphErrors(nbinsGraph,GrPointX,GrPointY,zeros,GrPointError);
	return gr;
}

TH1D* MpdPid_AZ::GetPidNormAmpls(Int_t nSplit, Double_t PMIN, Int_t part, Int_t charge)
{
	TH1D *PidNormAmpl = new TH1D("PidNormAmpl", "", nSplit, PMIN, 3.);
	Double_t val_l, val_r, fEval, Integral1, Integral2, lEval, rEval;
	for (Int_t i=0; i<nSplit; i++)
	{
		val_l = ((3.-PMIN)/nSplit)*i + PMIN; val_r = ((3.-PMIN)/nSplit)*(i+1) + PMIN;
		if (charge > 0) {
			switch (part)
			{
				case 0: Integral1=parElPosMom->Integral(val_l,val_r); Integral2=parElPosMom->Integral(PMIN,3.); 
				lEval=parElPosMom->Eval(val_l); rEval=parElPosMom->Eval(val_r); break;
				case 1: Integral1=parMuPosMom->Integral(val_l,val_r); Integral2=parMuPosMom->Integral(PMIN,3.);
				lEval=parMuPosMom->Eval(val_l); rEval=parMuPosMom->Eval(val_r); break;
				case 2: Integral1=parPiPosMom->Integral(val_l,val_r); Integral2=parPiPosMom->Integral(PMIN,3.);
				lEval=parPiPosMom->Eval(val_l); rEval=parPiPosMom->Eval(val_r); break;
				case 3: Integral1=parKaPosMom->Integral(val_l,val_r); Integral2=parKaPosMom->Integral(PMIN,3.);
				lEval=parKaPosMom->Eval(val_l); rEval=parKaPosMom->Eval(val_r); break;
				case 4: Integral1=parPrPosMom->Integral(val_l,val_r); Integral2=parPrPosMom->Integral(PMIN,3.);
				lEval=parPrPosMom->Eval(val_l); rEval=parPrPosMom->Eval(val_r); break;
				case 5: Integral1=parDeMom->Integral(val_l,val_r); Integral2=parDeMom->Integral(PMIN,3.);
				lEval=parDeMom->Eval(val_l); rEval=parDeMom->Eval(val_r); break;
				case 6: Integral1=parTrMom->Integral(val_l,val_r); Integral2=parTrMom->Integral(PMIN,3.);
				lEval=parTrMom->Eval(val_l); rEval=parTrMom->Eval(val_r); break;
				case 7: Integral1=parHe3Mom->Integral(val_l,val_r); Integral2=parHe3Mom->Integral(PMIN,3.);
				lEval=parHe3Mom->Eval(val_l); rEval=parHe3Mom->Eval(val_r); break;
				case 8: Integral1=parHe4Mom->Integral(val_l,val_r); Integral2=parHe4Mom->Integral(PMIN,3.);
				lEval=parHe4Mom->Eval(val_l); rEval=parHe4Mom->Eval(val_r); break;
			}
		}
		else if (charge < 0) {
			switch (part)
			{
				case 0: Integral1=parElNegMom->Integral(val_l,val_r); Integral2=parElNegMom->Integral(PMIN,3.); 
				lEval=parElNegMom->Eval(val_l); rEval=parElNegMom->Eval(val_r); break;
				case 1: Integral1=parMuNegMom->Integral(val_l,val_r); Integral2=parMuNegMom->Integral(PMIN,3.);
				lEval=parMuNegMom->Eval(val_l); rEval=parMuNegMom->Eval(val_r); break;
				case 2: Integral1=parPiNegMom->Integral(val_l,val_r); Integral2=parPiNegMom->Integral(PMIN,3.);
				lEval=parPiNegMom->Eval(val_l); rEval=parPiNegMom->Eval(val_r); break;
				case 3: Integral1=parKaNegMom->Integral(val_l,val_r); Integral2=parKaNegMom->Integral(PMIN,3.);
				lEval=parKaNegMom->Eval(val_l); rEval=parKaNegMom->Eval(val_r); break;
				case 4: Integral1=parPrNegMom->Integral(val_l,val_r); Integral2=parPrNegMom->Integral(PMIN,3.);
				lEval=parPrNegMom->Eval(val_l); rEval=parPrNegMom->Eval(val_r); break;
				default: lEval = -1.; rEval = -1.; break;
			}
		}
		if ((lEval < 0.) || (rEval < 0.)) fEval = 0.;
		else fEval = Integral1 / Integral2; 
		PidNormAmpl->SetBinContent(i+1, fEval); 
	}
	return PidNormAmpl;
}

/*
void MpdPid::CheckMethodOne(TString inname, Int_t nevents)
{
	TChain chain("cbmsim");
	chain.Add(inname.Data());
	
	MpdEvent *event = NULL;
	MpdTrack *mpdTrack = NULL;
	FairMCTrack *mcTrack = NULL;
	TClonesArray *mpdTracks = NULL;
	TClonesArray *mcTracks = NULL;
	TClonesArray *kalmanTracks = NULL;
	chain.LoadTree(0);
	if(chain.GetListOfBranches()->FindObject("MPDEvent."))
	chain.SetBranchAddress("MPDEvent.", &event);
	else {cout << "ERROR! Branch \"MPDEvent.\" doesn't exist! \nEnd of MpdPid::Pidcheck" << endl;}
	
	if(chain.GetListOfBranches()->FindObject("MCTrack"))
	chain.SetBranchAddress("MCTrack", &mcTracks);
	else {cout << "ERROR! Branch \"MCTrack\" doesn't exist! \nEnd of MpdPid::Pidcheck" << endl;}
	
	if(chain.GetListOfBranches()->FindObject("TpcKalmanTrack"))
	chain.SetBranchAddress("TpcKalmanTrack", &kalmanTracks);
	else {cout << "ERROR! Branch \"TpcKalmanTrack\" doesn't exist! \nEnd of MpdPid::Pidcheck" << endl;}
	
	Double_t PMINPI=0.08, PMINKA=0.15, PMINPR=0.25, PMINEL=0.1, PMINMU=0.1, PMINDE=1.2, PMINTR=1.8, PMINHE3=1.1, PMINHE4=1.4;
	Int_t ID, nbinsx_m2 = 900, SplitAmplFunc = 600;
	Double_t dedxMIN = fKoef*0.1e-06, dedxMAX = fKoef*18.e-06, dedxMAXUp = fKoef*5.e-06;
	Double_t fEval;
	
	// dedx-hists
	TH2D *dEdXmpdPEl = new TH2D("dEdXmpdPEl", "e", 500, 0., 3., 1000, dedxMIN, dedxMAX);
	TH2D *dEdXmpdPMu = new TH2D("dEdXmpdPMu", "#mu", 500, 0., 3., 1000, dedxMIN, dedxMAX);
	TH2D *dEdXmpdPPi = new TH2D("dEdXmpdPPi", "#pi", 500, 0., 3., 1000, dedxMIN, dedxMAX);
	TH2D *dEdXmpdPPr = new TH2D("dEdXmpdPPr", "p", 500, 0., 3., 1000, dedxMIN, dedxMAX);
	TH2D *dEdXmpdPKa = new TH2D("dEdXmpdPKa", "ka", 500, 0., 3., 1000, dedxMIN, dedxMAX);
	TH2D *dEdXmpdPPiUp = new TH2D("dEdXmpdPPiUp", "#pi", 300, 1.5, 3., 500, dedxMIN, dedxMAXUp);
	TH2D *dEdXmpdPPrUp = new TH2D("dEdXmpdPPrUp", "p", 300, 1.5, 3., 500, dedxMIN, dedxMAXUp);
	TH2D *dEdXmpdPKaUp = new TH2D("dEdXmpdPKaUp", "ka", 300, 1.5, 3., 500, dedxMIN, dedxMAXUp);
	TH2D *dEdXmpdPDe = new TH2D("dEdXmpdPDe", "De", 500, 0., 3., 1000, dedxMIN, dedxMAX);
	TH2D *dEdXmpdPTr = new TH2D("dEdXmpdPTr", "Tr", 500, 0., 3., 1000, dedxMIN, dedxMAX);
	TH2D *dEdXmpdPHe3 = new TH2D("dEdXmpdPHe3", "He3", 500, 0., 3., 1000, dedxMIN, dedxMAX);
	TH2D *dEdXmpdPHe4 = new TH2D("dEdXmpdPHe4", "He4", 500, 0., 3., 1000, dedxMIN, dedxMAX);
	
	// m2-hists
	TH2D *Mass2El = new TH2D("Mass2El", "e", nbinsx_m2, 0., 3., 600, -0.5, 0.5);
	TH2D *Mass2Mu = new TH2D("Mass2Mu", "#mu", nbinsx_m2, 0., 3., 600, -0.5, 0.5);
	TH2D *Mass2Pi = new TH2D("Mass2Pi", "#pi", nbinsx_m2, 0., 3., 600, -0.5, 1.5);
	TH2D *Mass2Pr = new TH2D("Mass2Pr", "p", nbinsx_m2, 0., 3., 600, -0.5, 1.5);
	TH2D *Mass2Ka = new TH2D("Mass2Ka", "ka", nbinsx_m2, 0., 3., 600, -0.5, 0.7);
	TH2D *Mass2De = new TH2D("Mass2De", "De", nbinsx_m2, 0., 3., 600, -0.2, 15); 
	TH2D *Mass2Tr = new TH2D("Mass2Tr", "Tr", nbinsx_m2, 0., 3., 600, -0.2, 15);
	TH2D *Mass2He3 = new TH2D("Mass2He3", "He3", nbinsx_m2, 0., 3., 600, -0.2, 15);
	TH2D *Mass2He4 = new TH2D("Mass2He4", "He4", nbinsx_m2, 0., 3., 600, -0.2, 15);
	
	// ampl-hists
	TH1D *AmpRealPosEl = new TH1D("AmpRealPosEl", "e+", SplitAmplFunc, 0.25, 3.);
	TH1D *AmpRealNegEl = new TH1D("AmpRealNegEl", "e-", SplitAmplFunc, 0.25, 3.);
	TH1D *AmpRealPosMu = new TH1D("AmpRealPosMu", "#mu+", SplitAmplFunc, 0.25, 3.);
	TH1D *AmpRealNegMu = new TH1D("AmpRealNegMu", "#mu-", SplitAmplFunc, 0.25, 3.);
	TH1D *AmpRealPosPi = new TH1D("AmpRealPosPi", "#pi+", SplitAmplFunc, 0.25, 3.);
	TH1D *AmpRealNegPi = new TH1D("AmpRealNegPi", "#pi-", SplitAmplFunc, 0.25, 3.);
	TH1D *AmpRealPosPr = new TH1D("AmpRealPosPr", "p+", SplitAmplFunc, 0.3, 3.);
	TH1D *AmpRealPosKa = new TH1D("AmpRealPosKa", "ka+", SplitAmplFunc, 0., 3.);
	TH1D *AmpRealNegKa = new TH1D("AmpRealNegKa", "ka-", SplitAmplFunc, 0., 3.);
	TH1D *AmpRealDe = new TH1D("AmpRealDe", "De", SplitAmplFunc, 0., 3.);
	TH1D *AmpRealTr = new TH1D("AmpRealTr", "Tr", SplitAmplFunc, 0., 3.);
	TH1D *AmpRealHe3 = new TH1D("AmpRealHe3", "He3", SplitAmplFunc, 0., 3.);
	TH1D *AmpRealHe4 = new TH1D("AmpRealHe4", "He4", SplitAmplFunc, 0., 3.);
	
	TF1* polEl = new TF1("polEl", "pol3(0)", 0., 3.);
	TF1* polMu = new TF1("polMu", "pol3(0)", 0., 3.);
	TF1* polPi = new TF1("polPi", "pol3(0)", 0., 3.);
	TF1* polPr = new TF1("polPr", "pol3(0)", 0., 3.);
	TF1* polKa = new TF1("polKa", "pol3(0)", 0., 3.);
	TF1* polDe = new TF1("polDe", "pol3(0)", 0., 3.);
	TF1* polTr = new TF1("polTr", "pol3(0)", 0., 3.);
	TF1* polHe3 = new TF1("polHe3", "pol3(0)", 0., 3.);
	TF1* polHe4 = new TF1("polHe4", "pol3(0)", 0., 3.);
	
	cout << " Number of events in DST file = " << nevents << endl;
	time_t now = time(0); // current date/time based on current system
	char* dt = ctime(&now); // convert now to string form
	cout << " Loop is starting at time = " << dt <<endl;
	time_t initTime=time(0);
	
	for (Int_t i = 0; i < nevents; i++)
	{
		chain.GetEntry(i);
		mpdTracks = (TClonesArray*) event->GetGlobalTracks();
		Int_t fNtracks = mpdTracks->GetEntriesFast();
		Int_t mcNtracks = mcTracks->GetEntries();
		
		//time stuff
		
		float elapsedTime = floor(float(difftime(time(0),initTime))/60); //m
		float remainingTime = floor((nevents-i) * float(elapsedTime/(i+1)));
		
		// arrays of mc flags for reduce mpd matching overhead
		Int_t *flag_array = new Int_t[mcNtracks+1];
		for(int nt = 0; nt <= mcNtracks; nt++) flag_array[nt] = 0; 
		
		cout<<"Event = "<<i+1<<"/"<<nevents<<" *** Elapsed = "<< elapsedTime <<" min, Remaining = " << remainingTime <<" min                   \r"<<flush;
		if (fNtracks == 0) continue;
		
		for (Int_t j = 0; j < fNtracks; j++)
		{
			MpdTrack* mpdTrack = (MpdTrack*) mpdTracks->UncheckedAt(j);
			ID = mpdTrack->GetID();
			if ( flag_array[ID] == 1 ){
				continue;
			}
			flag_array[ID] = 1;
               
            MpdTpcKalmanTrack* kftrack = (MpdTpcKalmanTrack*)kalmanTracks->UncheckedAt(j);
            FairMCTrack* mcTrack = (FairMCTrack*) mcTracks->UncheckedAt(ID);
            Int_t mother = mcTrack->GetMotherId();
            Int_t tofFlag = mpdTrack->GetTofFlag(), pidFlag = 0;
            Int_t pdg = mcTrack->GetPdgCode(), pdgc = TMath::Abs(pdg);
            Double_t mpdPx = mpdTrack->GetPx(); Double_t mpdPy = mpdTrack->GetPy(); Double_t mpdPz = mpdTrack->GetPz();
            Double_t mom = TMath::Sqrt(mpdPx*mpdPx+mpdPy*mpdPy+mpdPz*mpdPz);
            Int_t charge = 1;
            if (mpdTrack->GetPt() > 0) charge = -1;
            
            // Setting cuts
            if (TMath::Abs(mpdTrack->GetEta()) > 1.) 
            continue; // only tracks with eta<1.
            
            if (!(mcTrack->GetNPoints(kTPC)))
            continue; // MC-points in TPC
				
            if (mother != -1)
            continue; // primary
               
            if (kftrack->GetChi2() / (kftrack->GetNofTrHits() * 2 - 5) > 3.0)
            continue;
               
            if ((tofFlag == 4) || (tofFlag == 6))
            pidFlag+=4; // You may use dedx-param
               
            if ((mcTrack->GetNPoints(kTOF)) && ((tofFlag == 2) || (tofFlag == 6)))
            pidFlag+=2; // You may use m2-param
            
            // Fill AmpReal-hists
            if ((pidFlag == 4) || (pidFlag == 6)) {
				switch (pdg)
				{
					case 11: // electrons
					AmpRealNegEl->Fill(mom); break;
					case -11: // positrons
					AmpRealPosEl->Fill(mom); break;
					case 13: // muons
					AmpRealNegMu->Fill(mom); break;
					case -13: // muons
					AmpRealPosMu->Fill(mom); break;
					case 211: // pions
					AmpRealPosPi->Fill(mom); break;
					case -211: // pions
					AmpRealNegPi->Fill(mom); break;
					case 2212: // protons
					AmpRealPosPr->Fill(mom); break;
					case 321: // kaons
					AmpRealPosKa->Fill(mom); break;
					case -321: // kaons
					AmpRealNegKa->Fill(mom); break;
					case PDG_DEUTERON:
					AmpRealDe->Fill(mom); break;
					case PDG_TRITON:
					AmpRealTr->Fill(mom); break;
					case PDG_HE3:
					AmpRealHe3->Fill(mom); break;
					case PDG_HE4:
					AmpRealHe4->Fill(mom); break;
					default: break;
				}
			}
            
            if (mpdTrack->GetNofHits() < 10) 
            continue; //tracks with more than 10 tpc points
            
            Double_t dedx = mpdTrack->GetdEdXTPC(), m2 = mpdTrack->GetTofMass2();
            
            if ((pidFlag == 4) || (pidFlag == 6))
            {
            switch (pdgc)
			{
				case 11: // electrons
				dEdXmpdPEl->Fill(mom,dedx); break;
				case 13: // muons
				dEdXmpdPMu->Fill(mom,dedx); break;
				case 211: // pions
				dEdXmpdPPi->Fill(mom,dedx); dEdXmpdPPiUp->Fill(mom,dedx); break;
				case 2212: // protons
				dEdXmpdPPr->Fill(mom,dedx); dEdXmpdPPrUp->Fill(mom,dedx); break;
				case 321: // kaons
				dEdXmpdPKa->Fill(mom,dedx); dEdXmpdPKaUp->Fill(mom,dedx); break;
				case PDG_DEUTERON:
				dEdXmpdPDe->Fill(mom,dedx); break;
				case PDG_TRITON:
				dEdXmpdPTr->Fill(mom,dedx); break;
				case PDG_HE3:
				dEdXmpdPHe3->Fill(mom,dedx); break;
				case PDG_HE4:
				dEdXmpdPHe4->Fill(mom,dedx); break;
				default: break;
			};
			
			}
			if ((pidFlag == 2) || (pidFlag == 6))
            {
            switch (pdgc)
			{
				case 11: // electrons
				Mass2El->Fill(mom,m2); break;
				case 13: // muons
				Mass2Mu->Fill(mom,m2); break;
				case 211: // pions
				Mass2Pi->Fill(mom,m2); break;
				case 2212: // protons
				Mass2Pr->Fill(mom,m2); break;
				case 321: // kaons
				Mass2Ka->Fill(mom,m2); break;
				case PDG_DEUTERON:
				Mass2De->Fill(mom,m2); break;
				case PDG_TRITON:
				Mass2Tr->Fill(mom,m2); break;
				case PDG_HE3:
				Mass2He3->Fill(mom,m2); break;
				case PDG_HE4:
				Mass2He4->Fill(mom,m2); break;
				default: break;
			};
			}
            
		} // end of tracks
		
	} // end of events
	cout << endl;
	
	// check existance of particles
	Int_t IsEmpty;
	Bool_t positrons=1, muonsPos=1, pionsPos=1, kaonsPos=1, protons=1, deuterons=1, tritons=1, he3=1, he4=1;
	Bool_t electrons=1, muonsNeg=1, pionsNeg=1, kaonsNeg=1;
	IsEmpty = AmpRealNegEl->GetEntries();
	if (!IsEmpty) {delete AmpRealNegEl; electrons=0;}
	IsEmpty = AmpRealPosEl->GetEntries();
	if (!IsEmpty) {delete AmpRealPosEl; positrons=0;}
	IsEmpty = AmpRealNegMu->GetEntries();
	if (!IsEmpty) {delete AmpRealNegMu; muonsNeg=0;}
	IsEmpty = AmpRealPosMu->GetEntries();
	if (!IsEmpty) {delete AmpRealPosMu; muonsPos=0;}
	IsEmpty = AmpRealNegPi->GetEntries();
	if (!IsEmpty) {delete AmpRealNegPi; pionsNeg=0;}
	IsEmpty = AmpRealPosPi->GetEntries();
	if (!IsEmpty) {delete AmpRealPosPi; pionsPos=0;}
	IsEmpty = AmpRealNegKa->GetEntries();
	if (!IsEmpty) {delete AmpRealNegKa; kaonsNeg=0;}
	IsEmpty = AmpRealPosKa->GetEntries();
	if (!IsEmpty) {delete AmpRealPosKa; kaonsPos=0;}
	IsEmpty = AmpRealPosPr->GetEntries();
	if (!IsEmpty) {delete AmpRealPosPr; protons=0;}
	IsEmpty = AmpRealDe->GetEntries();
	if (!IsEmpty) {delete AmpRealDe; delete dEdXmpdPDe; delete Mass2De; deuterons=0;}
	IsEmpty = AmpRealTr->GetEntries();
	if (!IsEmpty) {delete AmpRealTr; delete dEdXmpdPTr; delete Mass2Tr; tritons=0;}
	IsEmpty = AmpRealHe3->GetEntries();
	if (!IsEmpty) {delete AmpRealHe3; delete dEdXmpdPHe3; delete Mass2He3; he3=0;}
	IsEmpty = AmpRealHe4->GetEntries();
	if (!IsEmpty) {delete AmpRealHe4; delete dEdXmpdPHe4; delete Mass2He4; he4=0;}
	
	// Making TGraphErrors for each particle (dedx)
	TGraphErrors *grElectrons, *grMuons, *grPions, *grProtons, *grKaons, *grDeuterons, *grTritons, *grHe3, *grHe4;
	if ((electrons) || (positrons)) {grElectrons = GetTGraphErrors(dEdXmpdPEl, PMINEL, parElBB, 0);}
	if ((muonsNeg) || (muonsPos)) {grMuons = GetTGraphErrors(dEdXmpdPMu, PMINMU, parMuBB, 1);}
	if ((pionsNeg) || (pionsPos)) {grPions = GetTGraphErrors(dEdXmpdPPi, dEdXmpdPPiUp, PMINPI, parPiBB, 2);}
	if (protons) {grProtons = GetTGraphErrors(dEdXmpdPPr, dEdXmpdPPrUp, PMINPR, parPrBB, 4);}
	if ((kaonsNeg) || (kaonsPos)) {grKaons = GetTGraphErrors(dEdXmpdPKa, dEdXmpdPKaUp, PMINKA, parKaBB, 3);}
	if (deuterons) {grDeuterons = GetTGraphErrors(dEdXmpdPDe, PMINDE, parDeBB, 5);}
	if (tritons) {grTritons = GetTGraphErrors(dEdXmpdPTr, PMINTR, parTrBB, 6);}
	if (he3) {grHe3 = GetTGraphErrors(dEdXmpdPHe3, PMINHE3, parHe3BB, 7);}
	if (he4) {grHe4 = GetTGraphErrors(dEdXmpdPHe4, PMINHE4, parHe4BB, 8);}
	
	// m2-check
	TH1D *Mass2El_2, *Mass2Mu_2, *Mass2Pi_2, *Mass2Pr_2, *Mass2Ka_2, *Mass2De_2, *Mass2Tr_2, *Mass2He3_2, *Mass2He4_2;
	if ((electrons) || (positrons)) {Mass2El_2 = GetMass2Width(Mass2El, 4, nbinsx_m2, 0.2, 1.4, -0.05, 0.05);
	Mass2El_2->SetName("Mass2El_2"); Mass2El_2->Fit(polEl, "Q0R");}
	if ((muonsNeg) || (muonsPos)) {Mass2Mu_2 = GetMass2Width(Mass2Mu, 0, nbinsx_m2, 0.09, 1.55, -0.05, 0.05);
	Mass2Mu_2->SetName("Mass2Mu_2"); Mass2Mu_2->Fit(polMu, "Q0R");}
	if ((pionsNeg) || (pionsPos)) {Mass2Pi_2 = GetMass2Width(Mass2Pi, 0, nbinsx_m2, 0.2, 1.5, -0.3, 0.3);
	Mass2Pi_2->SetName("Mass2Pi_2"); Mass2Pi_2->Fit(polPi, "Q0R");}
	if (protons) {Mass2Pr_2 = GetMass2Width(Mass2Pr, 4, nbinsx_m2, 0.3, 2.2, 0.1, 1.5);
	Mass2Pr_2->SetName("Mass2Pr_2"); Mass2Pr_2->Fit(polPr, "Q0", "", 0.4, 2.2);}
	if ((kaonsNeg) || (kaonsPos)) {Mass2Ka_2 = GetMass2Width(Mass2Ka, 4, nbinsx_m2, 0.2, 1.3, 0.05, 0.5);
	Mass2Ka_2->SetName("Mass2Ka_2"); Mass2Ka_2->Fit(polKa, "Q0", "", 0.3, 1.3);}
	if (deuterons) {Mass2De_2 = GetMass2Width(Mass2De, 0, nbinsx_m2, 0.3, 6., 0., 7.);
	Mass2De_2->SetName("Mass2De_2"); Mass2De_2->Fit(polDe, "Q0R");}
	if (tritons) {Mass2Tr_2 = GetMass2Width(Mass2Tr, 0, nbinsx_m2, 0.8, 5.8, 4.5, 11.);
	Mass2Tr_2->SetName("Mass2Tr_2"); Mass2Tr_2->Fit(polTr, "Q0R");}
	if (he3) {Mass2He3_2 = GetMass2Width(Mass2He3, 0, nbinsx_m2, 0.3, 3., 0., 4.);
	Mass2He3_2->SetName("Mass2He3_2"); Mass2He3_2->Fit(polHe3, "Q0R");}
	if (he4) {Mass2He4_2 = GetMass2Width(Mass2He4, 0, nbinsx_m2, 0.3, 3., 1., 6.);
	Mass2He4_2->SetName("Mass2He4_2"); Mass2He4_2->Fit(polHe4, "Q0R");}
	
	// ampls-check
	Double_t nEntries; Int_t bin_l;
	TH1D *AmpPidPosEl, *AmpPidPosMu, *AmpPidPosPi, *AmpPidPosPr, *AmpPidPosKa, *AmpPidDe, *AmpPidTr, *AmpPidHe3, *AmpPidHe4;
	TH1D *AmpPidNegEl, *AmpPidNegMu, *AmpPidNegPi, *AmpPidNegKa;
	
	if (electrons) {AmpPidNegEl = GetPidNormAmpls(SplitAmplFunc, 0., 0, -1); AmpPidNegEl->SetName("AmpPidNegEl");
		nEntries = AmpRealNegEl->Integral(1, SplitAmplFunc); AmpRealNegEl->Scale(1./nEntries);}
	if (positrons) {AmpPidPosEl = GetPidNormAmpls(SplitAmplFunc, 0., 0, 1); AmpPidPosEl->SetName("AmpPidPosEl");
		nEntries = AmpRealPosEl->Integral(1, SplitAmplFunc); AmpRealPosEl->Scale(1./nEntries);}
	if (muonsNeg) {AmpPidNegMu = GetPidNormAmpls(SplitAmplFunc, 0., 1, -1); AmpPidNegMu->SetName("AmpPidNegMu");
		nEntries = AmpRealNegMu->Integral(1, SplitAmplFunc); AmpRealNegMu->Scale(1./nEntries);}
	if (muonsPos) {AmpPidPosMu = GetPidNormAmpls(SplitAmplFunc, 0., 1, 1); AmpPidPosMu->SetName("AmpPidPosMu");
		nEntries = AmpRealPosMu->Integral(1, SplitAmplFunc); AmpRealPosMu->Scale(1./nEntries);}
	if (pionsNeg) {AmpPidNegPi = GetPidNormAmpls(SplitAmplFunc, 0.25, 2, -1); AmpPidNegPi->SetName("AmpPidNegPi");
		bin_l = AmpRealNegPi->GetXaxis()->FindBin(0.25); nEntries = AmpRealNegPi->Integral(bin_l, SplitAmplFunc); AmpRealNegPi->Scale(1./nEntries);}
	if (pionsPos) {AmpPidPosPi = GetPidNormAmpls(SplitAmplFunc, 0.25, 2, 1); AmpPidPosPi->SetName("AmpPidPosPi");
		bin_l = AmpRealPosPi->GetXaxis()->FindBin(0.25); nEntries = AmpRealPosPi->Integral(bin_l, SplitAmplFunc); AmpRealPosPi->Scale(1./nEntries);}
	if (protons) {AmpPidPosPr = GetPidNormAmpls(SplitAmplFunc, 0.3, 4, 1); AmpPidPosPr->SetName("AmpPidPosPr");
		bin_l = AmpRealPosPr->GetXaxis()->FindBin(0.3); nEntries = AmpRealPosPr->Integral(bin_l, SplitAmplFunc); AmpRealPosPr->Scale(1./nEntries);}
	if (kaonsNeg) {AmpPidNegKa = GetPidNormAmpls(SplitAmplFunc, 0., 3, -1); AmpPidNegKa->SetName("AmpPidNegKa");
		nEntries = AmpRealNegKa->Integral(1, SplitAmplFunc); AmpRealNegKa->Scale(1./nEntries);}
	if (kaonsPos) {AmpPidPosKa = GetPidNormAmpls(SplitAmplFunc, 0., 3, 1); AmpPidPosKa->SetName("AmpPidPosKa");
		nEntries = AmpRealPosKa->Integral(1, SplitAmplFunc); AmpRealPosKa->Scale(1./nEntries);}
	if (deuterons) {AmpPidDe = GetPidNormAmpls(SplitAmplFunc, 0., 5, 1); AmpPidDe->SetName("AmpPidDe");
		nEntries = AmpRealDe->Integral(1, SplitAmplFunc); AmpRealDe->Scale(1./nEntries);}
	if (tritons) {AmpPidTr = GetPidNormAmpls(SplitAmplFunc, 0., 6, 1); AmpPidTr->SetName("AmpPidTr");
		nEntries = AmpRealTr->Integral(1, SplitAmplFunc); AmpRealTr->Scale(1./nEntries);}
	if (he3) {AmpPidHe3 = GetPidNormAmpls(SplitAmplFunc, 0., 7, 1); AmpPidHe3->SetName("AmpPidHe3");
		nEntries = AmpRealHe3->Integral(1, SplitAmplFunc); AmpRealHe3->Scale(1./nEntries);}
	if (he4) {AmpPidHe4 = GetPidNormAmpls(SplitAmplFunc, 0., 8, 1); AmpPidHe4->SetName("AmpPidHe4");
		nEntries = AmpRealHe4->Integral(1, SplitAmplFunc); AmpRealHe4->Scale(1./nEntries);}
	
	// DRAWING
	TCanvas *can1 = new TCanvas ("can1", "(dE/dX) / (PidFunc) Graph", 1200, 1200);
	can1->Divide(3,3);
	if((pionsNeg) || (pionsPos)) {
		grPions->SetTitle("Ratio Pions"); 
		grPions->GetYaxis()->SetTitle("dedx / Pid Bethe-Bloch function (parametrization)");
		grPions->GetXaxis()->SetTitle("Momentum, GeV/c");
		grPions->SetMarkerStyle(21);
		grPions->GetYaxis()->SetRangeUser(0.8,1.2);
		TPad* can1_1=(TPad*)(can1->GetPrimitive("can1_1"));
		can1_1->cd(); can1_1->SetGrid(); grPions->Draw("AP");
	}
	if(protons) {
		grProtons->SetTitle("Ratio Protons");
		grProtons->GetYaxis()->SetTitle("dedx / Pid Bethe-Bloch function (parametrization)");
		grProtons->GetXaxis()->SetTitle("Momentum, GeV/c");
		grProtons->SetMarkerStyle(21);
		grProtons->GetYaxis()->SetRangeUser(0.8,1.2);
		TPad* can1_2=(TPad*)(can1->GetPrimitive("can1_2"));
		can1_2->cd(); can1_2->SetGrid(); grProtons->Draw("AP");
	}
	if((kaonsNeg) || (kaonsPos)) {
		grKaons->SetTitle("Ratio Kaons");
		grKaons->GetYaxis()->SetTitle("dedx / Pid Bethe-Bloch function (parametrization)");
		grKaons->GetXaxis()->SetTitle("Momentum, GeV/c");
		grKaons->SetMarkerStyle(21);
		grKaons->GetYaxis()->SetRangeUser(0.8,1.2);
		TPad* can1_3=(TPad*)(can1->GetPrimitive("can1_3"));
		can1_3->cd(); can1_3->SetGrid(); grKaons->Draw("AP");
	}
	if((electrons) || (positrons)) {
		grElectrons->SetTitle("Ratio Electrons"); 
		grElectrons->GetYaxis()->SetTitle("dedx / Pid Bethe-Bloch function (parametrization)");
		grElectrons->GetXaxis()->SetTitle("Momentum, GeV/c");
		grElectrons->SetMarkerStyle(21);
		grElectrons->GetYaxis()->SetRangeUser(0.8,1.2);
		TPad* can1_4=(TPad*)(can1->GetPrimitive("can1_4"));
		can1_4->cd(); can1_4->SetGrid(); grElectrons->Draw("AP");
	}
	if((muonsNeg) || (muonsPos)) {
		grMuons->SetTitle("Ratio Muons"); 
		grMuons->GetYaxis()->SetTitle("dedx / Pid Bethe-Bloch function (parametrization)");
		grMuons->GetXaxis()->SetTitle("Momentum, GeV/c");
		grMuons->SetMarkerStyle(21);
		grMuons->GetYaxis()->SetRangeUser(0.8,1.2);
		TPad* can1_5=(TPad*)(can1->GetPrimitive("can1_5"));
		can1_5->cd(); can1_5->SetGrid(); grMuons->Draw("AP");
	}
	if(deuterons) {
		grDeuterons->SetTitle("Ratio Deuterons"); 
		grDeuterons->GetYaxis()->SetTitle("dedx / Pid Bethe-Bloch function (parametrization)");
		grDeuterons->GetXaxis()->SetTitle("Momentum, GeV/c");
		grDeuterons->SetMarkerStyle(21);
		grDeuterons->GetYaxis()->SetRangeUser(0.8,1.2);
		TPad* can1_6=(TPad*)(can1->GetPrimitive("can1_6"));
		can1_6->cd(); can1_6->SetGrid(); grDeuterons->Draw("AP");
	}
	if(tritons) {
		grTritons->SetTitle("Ratio Tritons"); 
		grTritons->GetYaxis()->SetTitle("dedx / Pid Bethe-Bloch function (parametrization)");
		grTritons->GetXaxis()->SetTitle("Momentum, GeV/c");
		grTritons->SetMarkerStyle(21);
		grTritons->GetYaxis()->SetRangeUser(0.8,1.2);
		TPad* can1_7=(TPad*)(can1->GetPrimitive("can1_7"));
		can1_7->cd(); can1_7->SetGrid(); grTritons->Draw("AP");
	}
	if(he3) {
		grHe3->SetTitle("Ratio Tritons"); 
		grHe3->GetYaxis()->SetTitle("dedx / Pid Bethe-Bloch function (parametrization)");
		grHe3->GetXaxis()->SetTitle("Momentum, GeV/c");
		grHe3->SetMarkerStyle(21);
		grHe3->GetYaxis()->SetRangeUser(0.8,1.2);
		TPad* can1_8=(TPad*)(can1->GetPrimitive("can1_8"));
		can1_8->cd(); can1_8->SetGrid(); grHe3->Draw("AP");
	}
	if(he4) {
		grHe4->SetTitle("Ratio Tritons"); 
		grHe4->GetYaxis()->SetTitle("dedx / Pid Bethe-Bloch function (parametrization)");
		grHe4->GetXaxis()->SetTitle("Momentum, GeV/c");
		grHe4->SetMarkerStyle(21);
		grHe4->GetYaxis()->SetRangeUser(0.8,1.2);
		TPad* can1_9=(TPad*)(can1->GetPrimitive("can1_9"));
		can1_9->cd(); can1_9->SetGrid(); grHe4->Draw("AP");
	}
	
	TCanvas *can2 = new TCanvas ("can2", "m2-width fit", 1200, 1200);
	can2->Divide(3,3);
	if((pionsNeg) || (pionsPos)){
		Mass2Pi_2->SetMarkerStyle(20);
		Mass2Pi_2->SetMarkerSize(1);
		Mass2Pi_2->SetStats(kFALSE);
		Mass2Pi_2->SetTitle("#sigma(p) pions");
		Mass2Pi_2->GetXaxis()->SetTitle("Momentum, GeV/c");
		Mass2Pi_2->GetYaxis()->SetTitle("#sigma, GeV/c^{2}");
		TPad* can2_1=(TPad*)(can2->GetPrimitive("can2_1"));
		can2_1->cd(); can2_1->SetGrid(); 
		Mass2Pi_2->Draw("P"); polPi->Draw("SAME");
	}
	if(protons){
		Mass2Pr_2->SetMarkerStyle(20);
		Mass2Pr_2->SetMarkerSize(1);
		Mass2Pr_2->SetStats(kFALSE);
		Mass2Pr_2->SetTitle("#sigma(p) protons");
		Mass2Pr_2->GetXaxis()->SetTitle("Momentum, GeV/c");
		Mass2Pr_2->GetYaxis()->SetTitle("#sigma, GeV/c^{2}");
		TPad* can2_2=(TPad*)(can2->GetPrimitive("can2_2"));
		can2_2->cd(); can2_2->SetGrid(); 
		Mass2Pr_2->Draw("P"); polPr->Draw("SAME");
	}
	if((kaonsNeg) || (kaonsPos)){
		Mass2Ka_2->SetMarkerStyle(20);
		Mass2Ka_2->SetMarkerSize(1);
		Mass2Ka_2->SetStats(kFALSE);
		Mass2Ka_2->SetTitle("#sigma(p) kaons");
		Mass2Ka_2->GetXaxis()->SetTitle("Momentum, GeV/c");
		Mass2Ka_2->GetYaxis()->SetTitle("#sigma, GeV/c^{2}");
		TPad* can2_3=(TPad*)(can2->GetPrimitive("can2_3"));
		can2_3->cd(); can2_3->SetGrid(); 
		Mass2Ka_2->Draw("P"); polKa->Draw("SAME");
	}
	if((electrons) || (positrons)){
		Mass2El_2->SetMarkerStyle(20);
		Mass2El_2->SetMarkerSize(1);
		Mass2El_2->SetStats(kFALSE);
		Mass2El_2->SetTitle("#sigma(p) kaons");
		Mass2El_2->GetXaxis()->SetTitle("Momentum, GeV/c");
		Mass2El_2->GetYaxis()->SetTitle("#sigma, GeV/c^{2}");
		TPad* can2_4=(TPad*)(can2->GetPrimitive("can2_4"));
		can2_4->cd(); can2_4->SetGrid(); 
		Mass2El_2->Draw("P"); polEl->Draw("SAME");
	}
	if((muonsNeg) || (muonsPos)){
		Mass2Mu_2->SetMarkerStyle(20);
		Mass2Mu_2->SetMarkerSize(1);
		Mass2Mu_2->SetStats(kFALSE);
		Mass2Mu_2->SetTitle("#sigma(p) kaons");
		Mass2Mu_2->GetXaxis()->SetTitle("Momentum, GeV/c");
		Mass2Mu_2->GetYaxis()->SetTitle("#sigma, GeV/c^{2}");
		TPad* can2_5=(TPad*)(can2->GetPrimitive("can2_5"));
		can2_5->cd(); can2_5->SetGrid(); 
		Mass2Mu_2->Draw("P"); polMu->Draw("SAME");
	}
	if(deuterons) {
		Mass2De_2->SetMarkerStyle(20);
		Mass2De_2->SetMarkerSize(1);
		Mass2De_2->SetStats(kFALSE);
		Mass2De_2->SetTitle("#sigma(p) kaons");
		Mass2De_2->GetXaxis()->SetTitle("Momentum, GeV/c");
		Mass2De_2->GetYaxis()->SetTitle("#sigma, GeV/c^{2}");
		TPad* can2_6=(TPad*)(can2->GetPrimitive("can2_6"));
		can2_6->cd(); can2_6->SetGrid(); 
		Mass2De_2->Draw("P"); polDe->Draw("SAME");
	}
	if(tritons) {
		Mass2Tr_2->SetMarkerStyle(20);
		Mass2Tr_2->SetMarkerSize(1);
		Mass2Tr_2->SetStats(kFALSE);
		Mass2Tr_2->SetTitle("#sigma(p) kaons");
		Mass2Tr_2->GetXaxis()->SetTitle("Momentum, GeV/c");
		Mass2Tr_2->GetYaxis()->SetTitle("#sigma, GeV/c^{2}");
		TPad* can2_7=(TPad*)(can2->GetPrimitive("can2_7"));
		can2_7->cd(); can2_7->SetGrid(); 
		Mass2Tr_2->Draw("P"); polTr->Draw("SAME");
	}
	if(he3) {
		Mass2He3_2->SetMarkerStyle(20);
		Mass2He3_2->SetMarkerSize(1);
		Mass2He3_2->SetStats(kFALSE);
		Mass2He3_2->SetTitle("#sigma(p) kaons");
		Mass2He3_2->GetXaxis()->SetTitle("Momentum, GeV/c");
		Mass2He3_2->GetYaxis()->SetTitle("#sigma, GeV/c^{2}");
		TPad* can2_8=(TPad*)(can2->GetPrimitive("can2_8"));
		can2_8->cd(); can2_8->SetGrid(); 
		Mass2He3_2->Draw("P"); polHe3->Draw("SAME");
	}
	if(he4) {
		Mass2He4_2->SetMarkerStyle(20);
		Mass2He4_2->SetMarkerSize(1);
		Mass2He4_2->SetStats(kFALSE);
		Mass2He4_2->SetTitle("#sigma(p) kaons");
		Mass2He4_2->GetXaxis()->SetTitle("Momentum, GeV/c");
		Mass2He4_2->GetYaxis()->SetTitle("#sigma, GeV/c^{2}");
		TPad* can2_9=(TPad*)(can2->GetPrimitive("can2_9"));
		can2_9->cd(); can2_9->SetGrid(); 
		Mass2He4_2->Draw("P"); polHe4->Draw("SAME");
	}
	
	TCanvas *can3 = new TCanvas ("can3", "Normalized count of positive particles", 1200, 800);
	can3->Divide(3,3);
	if(pionsPos) {
		AmpRealPosPi->SetStats(kFALSE);
		AmpRealPosPi->SetLineWidth(3); AmpPidPosPi->SetLineWidth(2);
		AmpRealPosPi->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealPosPi->GetXaxis()->CenterTitle(kTRUE);
		AmpRealPosPi->GetYaxis()->SetTitle("Normalized count of particle"); AmpRealPosPi->GetYaxis()->CenterTitle(kTRUE);
		AmpRealPosPi->SetLineColor(kGreen); AmpPidPosPi->SetLineColor(kRed);
		TPad* can3_1=(TPad*)(can3->GetPrimitive("can3_1"));
		can3_1->cd(); can3_1->SetGrid(); can3_1->SetBorderMode(0); can3_1->SetBorderSize(0); can3_1->SetLogy();
		AmpRealPosPi->Draw(); AmpPidPosPi->Draw("SAME");
		TLegend *leg1 = new TLegend(2.0,0.008,2.8,0.01,NULL,"brNDC");
		leg1->AddEntry(AmpRealPosPi, "REAL DISTRIBUTION", "l");
		leg1->AddEntry(AmpPidPosPi, "PID DISTRIBUTION", "l");
		leg1->Draw();
	}
	if(protons) {
		AmpRealPosPr->SetStats(kFALSE);
		AmpRealPosPr->SetLineWidth(3); AmpPidPosPr->SetLineWidth(2);
		AmpRealPosPr->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealPosPr->GetXaxis()->CenterTitle(kTRUE);
		AmpRealPosPr->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealPosPr->GetYaxis()->CenterTitle(kTRUE);
		AmpRealPosPr->SetLineColor(kGreen); AmpPidPosPr->SetLineColor(kRed);
		TPad* can3_2=(TPad*)(can3->GetPrimitive("can3_2"));
		can3_2->cd(); can3_2->SetGrid(); can3_2->SetBorderMode(0); can3_2->SetBorderSize(0); can3_2->SetLogy();
		AmpRealPosPr->Draw(); AmpPidPosPr->Draw("SAME");
		TLegend *leg2 = new TLegend(2.0,0.003,2.8,0.004,NULL,"brNDC");
		leg2->AddEntry(AmpRealPosPr, "REAL DISTRIBUTION", "l");
		leg2->AddEntry(AmpPidPosPr, "PID DISTRIBUTION", "l");
		leg2->Draw();
	}
	if(kaonsPos) {
		AmpRealPosKa->SetStats(kFALSE);
		AmpRealPosKa->SetLineWidth(3);
		AmpPidPosKa->SetLineWidth(2);
		AmpRealPosKa->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealPosKa->GetXaxis()->CenterTitle(kTRUE);
		AmpRealPosKa->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealPosKa->GetYaxis()->CenterTitle(kTRUE);
		AmpRealPosKa->SetLineColor(kGreen); AmpPidPosKa->SetLineColor(kRed); 
		TPad* can3_3=(TPad*)(can3->GetPrimitive("can3_3"));
		can3_3->cd(); can3_3->SetGrid(); can3_3->SetBorderMode(0); can3_3->SetBorderSize(0); can3_3->SetLogy();
		AmpRealPosKa->Draw(); AmpPidPosKa->Draw("SAME");
		TLegend *leg3 = new TLegend(2.0,0.004,2.8,0.0055,NULL,"brNDC");
		leg3->AddEntry(AmpRealPosKa, "REAL DISTRIBUTION", "l");
		leg3->AddEntry(AmpPidPosKa, "PID DISTRIBUTION", "l");
		leg3->Draw();
	}
	if(positrons) {
		AmpRealPosEl->SetStats(kFALSE);
		AmpRealPosEl->SetLineWidth(3);
		AmpPidPosEl->SetLineWidth(2);
		AmpRealPosEl->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealPosEl->GetXaxis()->CenterTitle(kTRUE);
		AmpRealPosEl->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealPosEl->GetYaxis()->CenterTitle(kTRUE);
		AmpRealPosEl->SetLineColor(kGreen); AmpPidPosEl->SetLineColor(kRed); 
		TPad* can3_4=(TPad*)(can3->GetPrimitive("can3_4"));
		can3_4->cd(); can3_4->SetGrid(); can3_4->SetBorderMode(0); can3_4->SetBorderSize(0); can3_4->SetLogy();
		AmpRealPosEl->Draw(); AmpPidPosEl->Draw("SAME");
	}
	if(muonsPos) {
		AmpRealPosMu->SetStats(kFALSE);
		AmpRealPosMu->SetLineWidth(3);
		AmpPidPosMu->SetLineWidth(2);
		AmpRealPosMu->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealPosMu->GetXaxis()->CenterTitle(kTRUE);
		AmpRealPosMu->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealPosMu->GetYaxis()->CenterTitle(kTRUE);
		AmpRealPosMu->SetLineColor(kGreen); AmpPidPosMu->SetLineColor(kRed); 
		TPad* can3_5=(TPad*)(can3->GetPrimitive("can3_5"));
		can3_5->cd(); can3_5->SetGrid(); can3_5->SetBorderMode(0); can3_5->SetBorderSize(0); can3_5->SetLogy();
		AmpRealPosMu->Draw(); AmpPidPosMu->Draw("SAME");
	}
	if(deuterons) {
		AmpRealDe->SetStats(kFALSE);
		AmpRealDe->SetLineWidth(3);
		AmpPidDe->SetLineWidth(2);
		AmpRealDe->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealDe->GetXaxis()->CenterTitle(kTRUE);
		AmpRealDe->GetYaxis()->SetTitle("The proportion of total count of particles"); AmpRealDe->GetYaxis()->CenterTitle(kTRUE);
		AmpRealDe->SetLineColor(kGreen); AmpPidDe->SetLineColor(kRed); 
		TPad* can3_6=(TPad*)(can3->GetPrimitive("can3_6"));
		can3_6->cd(); can3_6->SetGrid(); can3_6->SetBorderMode(0); can3_6->SetBorderSize(0); can3_6->SetLogy();
		AmpRealDe->Draw(); AmpPidDe->Draw("SAME");
	}
	if(tritons) {
		AmpRealTr->SetStats(kFALSE);
		AmpRealTr->SetLineWidth(3);
		AmpPidTr->SetLineWidth(2);
		AmpRealTr->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealTr->GetXaxis()->CenterTitle(kTRUE);
		AmpRealTr->GetYaxis()->SetTitle("The proportion of total count of particles"); AmpRealTr->GetYaxis()->CenterTitle(kTRUE);
		AmpRealTr->SetLineColor(kGreen); AmpPidTr->SetLineColor(kRed); 
		TPad* can3_7=(TPad*)(can3->GetPrimitive("can3_7"));
		can3_7->cd(); can3_7->SetGrid(); can3_7->SetBorderMode(0); can3_7->SetBorderSize(0); can3_7->SetLogy();
		AmpRealTr->Draw(); AmpPidTr->Draw("SAME");
	}
	if(he3) {
		AmpRealHe3->SetStats(kFALSE);
		AmpRealHe3->SetLineWidth(3);
		AmpPidHe3->SetLineWidth(2);
		AmpRealHe3->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealHe3->GetXaxis()->CenterTitle(kTRUE);
		AmpRealHe3->GetYaxis()->SetTitle("The proportion of total count of particles"); AmpRealHe3->GetYaxis()->CenterTitle(kTRUE);
		AmpRealHe3->SetLineColor(kGreen); AmpPidHe3->SetLineColor(kRed); 
		TPad* can3_8=(TPad*)(can3->GetPrimitive("can3_8"));
		can3_8->cd(); can3_8->SetGrid(); can3_8->SetBorderMode(0); can3_8->SetBorderSize(0); can3_8->SetLogy();
		AmpRealHe3->Draw(); AmpPidHe3->Draw("SAME");
	}
	if(he4) {
		AmpRealHe4->SetStats(kFALSE);
		AmpRealHe4->SetLineWidth(3);
		AmpPidHe4->SetLineWidth(2);
		AmpRealHe4->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealHe4->GetXaxis()->CenterTitle(kTRUE);
		AmpRealHe4->GetYaxis()->SetTitle("The proportion of total count of particles"); AmpRealHe4->GetYaxis()->CenterTitle(kTRUE);
		AmpRealHe4->SetLineColor(kGreen); AmpPidHe4->SetLineColor(kRed); 
		TPad* can3_9=(TPad*)(can3->GetPrimitive("can3_9"));
		can3_9->cd(); can3_9->SetGrid(); can3_9->SetBorderMode(0); can3_9->SetBorderSize(0); can3_9->SetLogy();
		AmpRealHe4->Draw(); AmpPidHe4->Draw("SAME");
	}
	
	TCanvas *can4 = new TCanvas ("can4", "Normalized count of negative particles", 1200, 800);
	can4->Divide(2,2);
	if(pionsNeg) {
		AmpRealNegPi->SetStats(kFALSE);
		AmpRealNegPi->SetLineWidth(3); AmpPidNegPi->SetLineWidth(2);
		AmpRealNegPi->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealNegPi->GetXaxis()->CenterTitle(kTRUE);
		AmpRealNegPi->GetYaxis()->SetTitle("Normalized count of particle"); AmpRealNegPi->GetYaxis()->CenterTitle(kTRUE);
		AmpRealNegPi->SetLineColor(kGreen); AmpPidNegPi->SetLineColor(kRed);
		TPad* can4_1=(TPad*)(can4->GetPrimitive("can4_1"));
		can4_1->cd(); can4_1->SetGrid(); can4_1->SetBorderMode(0); can4_1->SetBorderSize(0); can4_1->SetLogy();
		AmpRealNegPi->Draw(); AmpPidNegPi->Draw("SAME");
		TLegend *leg4 = new TLegend(2.0,0.008,2.8,0.01,NULL,"brNDC");
		leg4->AddEntry(AmpRealNegPi, "REAL DISTRIBUTION", "l");
		leg4->AddEntry(AmpPidNegPi, "PID DISTRIBUTION", "l");
		leg4->Draw();
	}
	if(kaonsNeg) {
		AmpRealNegKa->SetStats(kFALSE);
		AmpRealNegKa->SetLineWidth(3);
		AmpPidNegKa->SetLineWidth(2);
		AmpRealNegKa->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealNegKa->GetXaxis()->CenterTitle(kTRUE);
		AmpRealNegKa->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealNegKa->GetYaxis()->CenterTitle(kTRUE);
		AmpRealNegKa->SetLineColor(kGreen); AmpPidNegKa->SetLineColor(kRed); 
		TPad* can4_2=(TPad*)(can4->GetPrimitive("can4_2"));
		can4_2->cd(); can4_2->SetGrid(); can4_2->SetBorderMode(0); can4_2->SetBorderSize(0); can4_2->SetLogy();
		AmpRealNegKa->Draw(); AmpPidNegKa->Draw("SAME");
		TLegend *leg6 = new TLegend(2.0,0.004,2.8,0.0055,NULL,"brNDC");
		leg6->AddEntry(AmpRealNegKa, "REAL DISTRIBUTION", "l");
		leg6->AddEntry(AmpPidNegKa, "PID DISTRIBUTION", "l");
		leg6->Draw();
	}
	if(electrons) {
		AmpRealNegEl->SetStats(kFALSE);
		AmpRealNegEl->SetLineWidth(3);
		AmpPidNegEl->SetLineWidth(2);
		AmpRealNegEl->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealNegEl->GetXaxis()->CenterTitle(kTRUE);
		AmpRealNegEl->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealNegEl->GetYaxis()->CenterTitle(kTRUE);
		AmpRealNegEl->SetLineColor(kGreen); AmpPidNegEl->SetLineColor(kRed); 
		TPad* can4_3=(TPad*)(can4->GetPrimitive("can4_3"));
		can4_3->cd(); can4_3->SetGrid(); can4_3->SetBorderMode(0); can4_3->SetBorderSize(0); can4_3->SetLogy();
		AmpRealNegEl->Draw(); AmpPidNegEl->Draw("SAME");
	}
	if(muonsNeg) {
		AmpRealNegMu->SetStats(kFALSE);
		AmpRealNegMu->SetLineWidth(3);
		AmpPidNegMu->SetLineWidth(2);
		AmpRealNegMu->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealNegMu->GetXaxis()->CenterTitle(kTRUE);
		AmpRealNegMu->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealNegMu->GetYaxis()->CenterTitle(kTRUE);
		AmpRealNegMu->SetLineColor(kGreen); AmpPidNegMu->SetLineColor(kRed); 
		TPad* can4_4=(TPad*)(can4->GetPrimitive("can4_4"));
		can4_4->cd(); can4_4->SetGrid(); can4_4->SetBorderMode(0); can4_4->SetBorderSize(0); can4_4->SetLogy();
		AmpRealNegMu->Draw(); AmpPidNegMu->Draw("SAME");
	}
	
	cout << "MpdPid::CheckMethodOne finished successfully" << endl;
}
*/
/*
void MpdPid::CheckMethodTwo(TString inname, Int_t nevents)
{
	TChain chain("cbmsim");
	chain.Add(inname.Data());
	
	MpdEvent *event = NULL;
	MpdTrack *mpdTrack = NULL;
	FairMCTrack *mcTrack = NULL;
	TClonesArray *mpdTracks = NULL, *mcTracks = NULL, *kalmanTracks = NULL;
	chain.LoadTree(0);
	
	if(chain.GetListOfBranches()->FindObject("MPDEvent."))
	chain.SetBranchAddress("MPDEvent.", &event);
	else {cout << "ERROR! Branch \"MPDEvent.\" doesn't exist! \nEnd of MpdPid::Pidcheck" << endl;}
	
	if(chain.GetListOfBranches()->FindObject("MCTrack"))
	chain.SetBranchAddress("MCTrack", &mcTracks);
	else {cout << "ERROR! Branch \"MCTrack\" doesn't exist! \nEnd of MpdPid::Pidcheck" << endl;}
	
	if(chain.GetListOfBranches()->FindObject("TpcKalmanTrack"))
	chain.SetBranchAddress("TpcKalmanTrack", &kalmanTracks);
	else {cout << "ERROR! Branch \"TpcKalmanTrack\" doesn't exist! \nEnd of MpdPid::Pidcheck" << endl;}
	
	Int_t ID, SplitAmplFunc = 100; Double_t fEval;
	Int_t d_el_inside = 0, d_el_all = 0, d_mu_inside = 0, d_mu_all = 0, d_de_inside = 0, d_de_all = 0;
	Int_t d_pi_inside = 0, d_pi_all = 0, d_pr_inside = 0, d_pr_all = 0, d_ka_inside = 0, d_ka_all = 0;
	Int_t d_tr_inside = 0, d_tr_all = 0, d_he3_inside = 0, d_he3_all = 0, d_he4_inside = 0, d_he4_all = 0;
	
	Int_t m_el_inside = 0, m_el_all = 0, m_mu_inside = 0, m_mu_all = 0, m_de_inside = 0, m_de_all = 0;
	Int_t m_pi_inside = 0, m_pi_all = 0, m_pr_inside = 0, m_pr_all = 0, m_ka_inside = 0, m_ka_all = 0;
	Int_t m_tr_inside = 0, m_tr_all = 0, m_he3_inside = 0, m_he3_all = 0, m_he4_inside = 0, m_he4_all = 0;
	
	// ampl-hists
	TH1D *AmpRealPosEl = new TH1D("AmpRealPosEl", "e+", SplitAmplFunc, 0.25, 3.);
	TH1D *AmpRealNegEl = new TH1D("AmpRealNegEl", "e-", SplitAmplFunc, 0.25, 3.);
	TH1D *AmpRealPosMu = new TH1D("AmpRealPosMu", "#mu+", SplitAmplFunc, 0.25, 3.);
	TH1D *AmpRealNegMu = new TH1D("AmpRealNegMu", "#mu-", SplitAmplFunc, 0.25, 3.);
	TH1D *AmpRealPosPi = new TH1D("AmpRealPosPi", "#pi+", SplitAmplFunc, 0.25, 3.);
	TH1D *AmpRealNegPi = new TH1D("AmpRealNegPi", "#pi-", SplitAmplFunc, 0.25, 3.);
	TH1D *AmpRealPosPr = new TH1D("AmpRealPosPr", "p+", SplitAmplFunc, 0.3, 3.);
	TH1D *AmpRealNegPr = new TH1D("AmpRealNegPr", "p-", SplitAmplFunc, 0.3, 3.);
	TH1D *AmpRealPosKa = new TH1D("AmpRealPosKa", "ka+", SplitAmplFunc, 0., 3.);
	TH1D *AmpRealNegKa = new TH1D("AmpRealNegKa", "ka-", SplitAmplFunc, 0., 3.);
	TH1D *AmpRealDe = new TH1D("AmpRealDe", "De", SplitAmplFunc, 0., 3.);
	TH1D *AmpRealTr = new TH1D("AmpRealTr", "Tr", SplitAmplFunc, 0., 3.);
	TH1D *AmpRealHe3 = new TH1D("AmpRealHe3", "He3", SplitAmplFunc, 0., 3.);
	TH1D *AmpRealHe4 = new TH1D("AmpRealHe4", "He4", SplitAmplFunc, 0., 3.);
	
	TF1 *polElPlus = new TF1 ("polElPlus", "0.002+3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 3.); 
	polElPlus->SetParameters(parElM2->GetParameters());
	TF1 *polElMinus = new TF1 ("polElMinus", "0.002-3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 3.); 
	polElMinus->SetParameters(parElM2->GetParameters());
	TF1 *polMuPlus = new TF1 ("polMuPlus", "0.011+3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 3.); 
	polMuPlus->SetParameters(parMuM2->GetParameters());
	TF1 *polMuMinus = new TF1 ("polMuMinus", "0.011-3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 3.); 
	polMuMinus->SetParameters(parMuM2->GetParameters());
	TF1 *polPiPlus = new TF1 ("polPiPlus", "0.019+3.*([0]+[1]*x+[2]*x*x)", 0., 3.); 
	polPiPlus->SetParameters(parPiM2->GetParameters());
	TF1 *polPiMinus = new TF1 ("polPiMinus", "0.019-3.*([0]+[1]*x+[2]*x*x)", 0., 3.); 
	polPiMinus->SetParameters(parPiM2->GetParameters());
	TF1 *polPrPlusLow = new TF1 ("polPrPlusLow", "0.887+3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 1.4); 
	polPrPlusLow->SetParameters(parPrLowPM2->GetParameters());
	TF1 *polPrMinusLow = new TF1 ("polPrMinusLow", "0.887-3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 1.4); 
	polPrMinusLow->SetParameters(parPrLowPM2->GetParameters());
	TF1 *polPrPlusHigh = new TF1 ("polPrPlusHigh", "0.887+3.*([0]+[1]*x+[2]*x*x)", 1.4, 3.); 
	polPrPlusHigh->SetParameters(parPrHighPM2->GetParameters());
	TF1 *polPrMinusHigh = new TF1 ("polPrMinusHigh", "0.887-3.*([0]+[1]*x+[2]*x*x)", 1.4, 3.); 
	polPrMinusHigh->SetParameters(parPrHighPM2->GetParameters());
	TF1 *polKaPlus = new TF1 ("polKaPlus", "0.24+3.*([0]+[1]*x+[2]*x*x)", 0., 3.); 
	polKaPlus->SetParameters(parKaM2->GetParameters());
	TF1 *polKaMinus = new TF1 ("polKaMinus", "0.24-3.*([0]+[1]*x+[2]*x*x)", 0., 3.);
	polKaMinus->SetParameters(parKaM2->GetParameters());
	TF1 *polDePlus = new TF1 ("polDePlus", "3.54+3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 3.); 
	polDePlus->SetParameters(parDeM2->GetParameters());
	TF1 *polDeMinus = new TF1 ("polDeMinus", "3.54-3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 3.);
	polDeMinus->SetParameters(parDeM2->GetParameters());
	TF1 *polTrPlus = new TF1 ("polTrPlus", "7.87+3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 3.); 
	polTrPlus->SetParameters(parTrM2->GetParameters());
	TF1 *polTrMinus = new TF1 ("polTrMinus", "7.87-3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 3.);
	polTrMinus->SetParameters(parTrM2->GetParameters());
	TF1 *polHe3Plus = new TF1 ("polHe3Plus", "1.983+3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 3.); 
	polHe3Plus->SetParameters(parHe3M2->GetParameters());
	TF1 *polHe3Minus = new TF1 ("polHe3Minus", "1.983-3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 3.);
	polHe3Minus->SetParameters(parHe3M2->GetParameters());
	TF1 *polHe4Plus = new TF1 ("polHe4Plus", "3.51+3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 3.); 
	polHe4Plus->SetParameters(parHe4M2->GetParameters());
	TF1 *polHe4Minus = new TF1 ("polHe4Minus", "3.51-3.*([0]+[1]*x+[2]*x*x+[3]*x*x*x)", 0., 3.);
	polHe4Minus->SetParameters(parHe4M2->GetParameters());
	
	cout << " Number of events in DST file = " << nevents << endl;
	time_t now = time(0); // current date/time based on current system
	char* dt = ctime(&now); // convert now to string form
	cout << " Loop is starting at time = " << dt <<endl;
	time_t initTime=time(0);
	
	for (Int_t i = 0; i < nevents; i++)
	{
		chain.GetEntry(i);
		mpdTracks = (TClonesArray*) event->GetGlobalTracks();
		Int_t fNtracks = mpdTracks->GetEntriesFast();
		Int_t mcNtracks = mcTracks->GetEntries();
		//time stuff
		float elapsedTime = floor(float(difftime(time(0),initTime))/60); //m
		float remainingTime = floor((nevents-i) * float(elapsedTime/(i+1)));
		
		// arrays of mc flags for reduce mpd matching overhead
		Int_t *flag_array = new Int_t[mcNtracks+1];
		for(int nt = 0; nt <= mcNtracks; nt++) flag_array[nt] = 0; 
		
		cout<<"Event = "<<i<<" Tracks = "<<fNtracks<<" *** Elapsed = "<< elapsedTime <<" min, Remaining = " << remainingTime <<" min                  \r"<<flush;
		if (fNtracks == 0) continue;    
		for (Int_t j = 0; j < fNtracks; j++)
		{
			mpdTrack = (MpdTrack*) mpdTracks->UncheckedAt(j);
			ID = mpdTrack->GetID();
			if ( flag_array[ID] == 1 ){
				continue;
			}
			flag_array[ID] = 1;
               
            MpdTpcKalmanTrack* kftrack = (MpdTpcKalmanTrack*)kalmanTracks->UncheckedAt(j);
            mcTrack = (FairMCTrack*) mcTracks->UncheckedAt(mpdTrack->GetID());
            Int_t mother = mcTrack->GetMotherId();
            Int_t tofFlag = mpdTrack->GetTofFlag(), pidFlag = 0;
            Int_t pdg = mcTrack->GetPdgCode(), pdgc = TMath::Abs(pdg);
            Double_t mpdPx = mpdTrack->GetPx(); Double_t mpdPy = mpdTrack->GetPy(); Double_t mpdPz = mpdTrack->GetPz();
            Double_t mom = TMath::Sqrt(mpdPx*mpdPx+mpdPy*mpdPy+mpdPz*mpdPz);
            Double_t eta = mpdTrack->GetEta();
            // Setting cuts
               
            if (TMath::Abs(eta) > 1.) 
            continue; // only tracks with eta<1.
               
            if (!(mcTrack->GetNPoints(kTPC)))
            continue; // MC-points in TPC
				
            if (mother != -1)
            continue; // primary
               
            if (kftrack->GetChi2() / (kftrack->GetNofTrHits() * 2 - 5) > 3.0)
            continue;
               
            if ((tofFlag == 4) || (tofFlag == 6))
            pidFlag+=4; // You may use dedx-param
               
            if ((mcTrack->GetNPoints(kTOF)) && ((tofFlag == 2) || (tofFlag == 6)))
            pidFlag+=2; // You may use m2-param
            
            // Fill AmpReal-hists
            if ((pidFlag == 4) || (pidFlag == 6)) {
				switch (pdg)
				{
					case 11: // electrons
					AmpRealNegEl->Fill(mom); break;
					case -11: // positrons
					AmpRealPosEl->Fill(mom); break;
					case 13: // muons-
					AmpRealNegMu->Fill(mom); break;
					case -13: // muons+
					AmpRealPosMu->Fill(mom); break;
					case 211: // pions+
					AmpRealPosPi->Fill(mom); break;
					case -211: // pions-
					AmpRealNegPi->Fill(mom); break;
					case 2212: // protons
					AmpRealPosPr->Fill(mom); break;
					case -2212: // antiprotons
					AmpRealNegPr->Fill(mom); break;
					case 321: // kaons
					AmpRealPosKa->Fill(mom); break;
					case -321: // kaons
					AmpRealNegKa->Fill(mom); break;
					case PDG_DEUTERON:
					AmpRealDe->Fill(mom); break;
					case PDG_TRITON:
					AmpRealTr->Fill(mom); break;
					case PDG_HE3:
					AmpRealHe3->Fill(mom); break;
					case PDG_HE4:
					AmpRealHe4->Fill(mom); break;
					default: break;
				}
			}
            
            if (mpdTrack->GetNofHits() < 10) 
            continue; //tracks with more than 10 tpc points
            
            Double_t dedx = mpdTrack->GetdEdXTPC(), m2 = mpdTrack->GetTofMass2();
               
            if (!((pidFlag == 2) || (pidFlag == 4) || (pidFlag == 6))) continue;
            
            if (pdgc == 11) {
				if (pidFlag != 4) { fEval = parElM2->Eval(mom);
				if(TMath::Abs((m2-0.002)/fEval) < 3.0) m_el_inside++;}
				d_el_all++; m_el_all++;
				fEval = GetDedxElParam(mom);
				if (TMath::Abs((dedx/fEval-1.)/fSigmaDedx_1) < 3.0) d_el_inside++;
			}
			if (pdgc == 13) {
				if (pidFlag != 4) { fEval = parMuM2->Eval(mom);
				if(TMath::Abs((m2-0.011)/fEval) < 3.0) m_mu_inside++; }
				d_mu_all++; m_mu_all++;
				fEval = GetDedxMuParam(mom);
				if (TMath::Abs((dedx/fEval-1.)/fSigmaDedx_1) < 3.0) d_mu_inside++;
			}
            if (pdgc == 211) {
				if (pidFlag != 4) { fEval = parPiM2->Eval(mom);
				if(TMath::Abs((m2-0.019)/fEval) < 3.0) m_pi_inside++; }
				
				d_pi_all++; m_pi_all++;
				fEval = GetDedxPiParam(mom);
				if (TMath::Abs((dedx/fEval-1.)/fSigmaDedx_1) < 3.0) d_pi_inside++;
			}
			if (pdgc == 2212) {
				if (pidFlag != 4) { if(mom<=1.4) fEval = parPrLowPM2->Eval(mom); else fEval = parPrHighPM2->Eval(mom);
				if(TMath::Abs((m2-0.887)/fEval) < 3.0) m_pr_inside++; }
				d_pr_all++; m_pr_all++;
				fEval = GetDedxPrParam(mom);
				if (TMath::Abs((dedx/fEval-1.)/fSigmaDedx_1) < 3.0) d_pr_inside++;
			}
			if (pdgc == 321) {
				if (pidFlag != 4) { fEval = parKaM2->Eval(mom);
				if(TMath::Abs((m2-0.24)/fEval) < 3.0) m_ka_inside++; }
				d_ka_all++; m_ka_all++;
				fEval = GetDedxKaParam(mom);
				if (TMath::Abs((dedx/fEval-1.)/fSigmaDedx_1) < 3.0) d_ka_inside++;
			}
			if (pdgc == PDG_DEUTERON) {
				if (pidFlag != 4) { fEval = parDeM2->Eval(mom);
				if(TMath::Abs((m2-3.54)/fEval) < 3.0) m_de_inside++; }
				d_de_all++; m_de_all++;
				fEval = GetDedxDeParam(mom);
				if (TMath::Abs((dedx/fEval-1.)/fSigmaDedx_1) < 3.0) d_de_inside++;
			}
			if (pdgc == PDG_TRITON) {
				if (pidFlag != 4) { fEval = parTrM2->Eval(mom);
				if(TMath::Abs((m2-7.87)/fEval) < 3.0) m_tr_inside++; }
				d_tr_all++; m_tr_all++;
				fEval = GetDedxTrParam(mom);
				if (TMath::Abs((dedx/fEval-1.)/fSigmaDedx_1) < 3.0) d_tr_inside++;
			}
			if (pdgc == PDG_HE3) {
				if (pidFlag != 4) { fEval = parHe3M2->Eval(mom);
				if(TMath::Abs((m2-1.983)/fEval) < 3.0) m_he3_inside++; }
				d_he3_all++; m_he3_all++;
				fEval = GetDedxHe3Param(mom);
				if (TMath::Abs((dedx/fEval-1.)/fSigmaDedx_1) < 3.0) d_he3_inside++;
			}
			if (pdgc == PDG_HE4) {
				if (pidFlag != 4) { fEval = parHe4M2->Eval(mom);
				if(TMath::Abs((m2-3.51)/fEval) < 3.0) m_he4_inside++; }
				d_he4_all++; m_he4_all++;
				fEval = GetDedxHe4Param(mom);
				if (TMath::Abs((dedx/fEval-1.)/fSigmaDedx_1) < 3.0) d_he4_inside++;
			}
		} // end of track
	} // end of event
	
	cout << endl;
	
	Int_t IsEmpty;
	Double_t nEntries; Int_t bin_l;
	TH1D *AmpPidPosEl, *AmpPidPosMu, *AmpPidPosPi, *AmpPidPosPr, *AmpPidPosKa, *AmpPidDe, *AmpPidTr, *AmpPidHe3, *AmpPidHe4;
	TH1D *AmpPidNegEl, *AmpPidNegMu, *AmpPidNegPi, *AmpPidNegKa;
	
	Bool_t positrons=0, muonsPos=0, pionsPos=0, kaonsPos=0, protons=0, deuterons=0, tritons=0, he3=0, he4=0;
	Bool_t electrons=0, muonsNeg=0, pionsNeg=0, kaonsNeg=0;
	
	IsEmpty = AmpRealNegEl->GetEntries();
	if (IsEmpty)
	{AmpPidNegEl = GetPidNormAmpls(SplitAmplFunc, 0., 0, -1); AmpPidNegEl->SetName("AmpPidNegEl");
	nEntries = AmpRealNegEl->Integral(1, SplitAmplFunc); AmpRealNegEl->Scale(1./nEntries); electrons=1;}
	
	IsEmpty = AmpRealPosEl->GetEntries();
	if (IsEmpty)
	{AmpPidPosEl = GetPidNormAmpls(SplitAmplFunc, 0., 0, 1); AmpPidPosEl->SetName("AmpPidPosEl");
	nEntries = AmpRealPosEl->Integral(1, SplitAmplFunc); AmpRealPosEl->Scale(1./nEntries); positrons=1;}
	
	IsEmpty = AmpRealNegMu->GetEntries();
	if (IsEmpty)
	{AmpPidNegMu = GetPidNormAmpls(SplitAmplFunc, 0., 1, -1); AmpPidNegMu->SetName("AmpPidNegMu");
	nEntries = AmpRealNegMu->Integral(1, SplitAmplFunc); AmpRealNegMu->Scale(1./nEntries); muonsNeg=1;}
	
	IsEmpty = AmpRealPosMu->GetEntries();
	if (IsEmpty)
	{AmpPidPosMu = GetPidNormAmpls(SplitAmplFunc, 0., 1, 1); AmpPidPosMu->SetName("AmpPidPosMu");
	nEntries = AmpRealPosMu->Integral(1, SplitAmplFunc); AmpRealPosMu->Scale(1./nEntries); muonsPos=1;}
	
	IsEmpty = AmpRealNegPi->GetEntries();
	if (IsEmpty)
	{AmpPidNegPi = GetPidNormAmpls(SplitAmplFunc, 0.25, 2, -1); AmpPidNegPi->SetName("AmpPidNegPi");
	bin_l = AmpRealNegPi->GetXaxis()->FindBin(0.25);
	nEntries = AmpRealNegPi->Integral(bin_l, SplitAmplFunc); AmpRealNegPi->Scale(1./nEntries); pionsNeg=1;}
	
	IsEmpty = AmpRealPosPi->GetEntries();
	if (IsEmpty)
	{AmpPidPosPi = GetPidNormAmpls(SplitAmplFunc, 0.25, 2, 1); AmpPidPosPi->SetName("AmpPidPosPi");
	bin_l = AmpRealPosPi->GetXaxis()->FindBin(0.25);
	nEntries = AmpRealPosPi->Integral(bin_l, SplitAmplFunc); AmpRealPosPi->Scale(1./nEntries); pionsPos=1;}
	
	IsEmpty = AmpRealNegKa->GetEntries();
	if (IsEmpty)
	{AmpPidNegKa = GetPidNormAmpls(SplitAmplFunc, 0., 3, -1); AmpPidNegKa->SetName("AmpPidNegKa");
	nEntries = AmpRealNegKa->Integral(1, SplitAmplFunc); AmpRealNegKa->Scale(1./nEntries); kaonsNeg=1;}
	
	IsEmpty = AmpRealPosKa->GetEntries();
	if (IsEmpty)
	{AmpPidPosKa = GetPidNormAmpls(SplitAmplFunc, 0., 3, 1); AmpPidPosKa->SetName("AmpPidPosKa");
	nEntries = AmpRealPosKa->Integral(1, SplitAmplFunc); AmpRealPosKa->Scale(1./nEntries); kaonsPos=1;}
	
	IsEmpty = AmpRealPosPr->GetEntries();
	if (IsEmpty)
	{AmpPidPosPr = GetPidNormAmpls(SplitAmplFunc, 0.3, 4, 1); AmpPidPosPr->SetName("AmpPidPosPr");
	bin_l = AmpRealPosPr->GetXaxis()->FindBin(0.3);
	nEntries = AmpRealPosPr->Integral(bin_l, SplitAmplFunc); AmpRealPosPr->Scale(1./nEntries); protons=1;}
	
	IsEmpty = AmpRealDe->GetEntries();
	if (IsEmpty)
	{AmpPidDe = GetPidNormAmpls(SplitAmplFunc, 0., 5, 1); AmpPidDe->SetName("AmpPidDe");
	nEntries = AmpRealDe->Integral(1, SplitAmplFunc); AmpRealDe->Scale(1./nEntries); deuterons=1;}
	
	IsEmpty = AmpRealTr->GetEntries();
	if (IsEmpty)
	{AmpPidTr = GetPidNormAmpls(SplitAmplFunc, 0., 6, 1); AmpPidTr->SetName("AmpPidTr");
	nEntries = AmpRealTr->Integral(1, SplitAmplFunc); AmpRealTr->Scale(1./nEntries); tritons=1;}
	
	IsEmpty = AmpRealHe3->GetEntries();
	if (IsEmpty)
	{AmpPidHe3 = GetPidNormAmpls(SplitAmplFunc, 0., 7, 1); AmpPidHe3->SetName("AmpPidHe3");
	nEntries = AmpRealHe3->Integral(1, SplitAmplFunc); AmpRealHe3->Scale(1./nEntries); he3=1;}
	
	IsEmpty = AmpRealHe4->GetEntries();
	if (IsEmpty)
	{AmpPidHe4 = GetPidNormAmpls(SplitAmplFunc, 0., 8, 1); AmpPidHe4->SetName("AmpPidHe4");
	nEntries = AmpRealHe4->Integral(1, SplitAmplFunc); AmpRealHe4->Scale(1./nEntries); he4=1;}
	
	Double_t PartInside;
	cout << endl << "MpdPid::CheckMethodTwo dedx-checking : " << endl;
	if (d_el_all != 0) {
		PartInside = TMath::Nint(10000 * d_el_inside / (Double_t) d_el_all) / 100.;
		cout << PartInside << "% electrons are detected (dedx)" << endl;
		}
	if (d_mu_all != 0) {
		PartInside = TMath::Nint(10000 * d_mu_inside / (Double_t) d_mu_all) / 100.;
		cout << PartInside << "% muons are detected (dedx)" << endl;
	}
	if (d_pi_all != 0) {
		PartInside = TMath::Nint(10000 * d_pi_inside / (Double_t) d_pi_all) / 100.;
		cout << PartInside << "% pions are detected (dedx)" << endl;
	}
	if (d_ka_all != 0) {
		PartInside = TMath::Nint(10000 * d_ka_inside / (Double_t) d_ka_all) / 100.;
		cout << PartInside << "% kaons are detected (dedx)" << endl;
	}
	if (d_pr_all != 0) {
		PartInside = TMath::Nint(10000 * d_pr_inside / (Double_t) d_pr_all) / 100.;
		cout << PartInside << "% protons are detected (dedx)" << endl;
	}
	if (d_de_all != 0) {
		PartInside = TMath::Nint(10000 * d_de_inside / (Double_t) d_de_all) / 100.;
		cout << PartInside << "% deuterons are detected (dedx)" << endl;
	}
	if (d_tr_all != 0) {
		PartInside = TMath::Nint(10000 * d_tr_inside / (Double_t) d_tr_all) / 100.;
		cout << PartInside << "% tritons are detected (dedx)" << endl;
	}
	if (d_he3_all != 0) {
		PartInside = TMath::Nint(10000 * d_he3_inside / (Double_t) d_he3_all) / 100.;
		cout << PartInside << "% he3 are detected (dedx)" << endl;
	}
	if (d_he4_all != 0) {
		PartInside = TMath::Nint(10000 * d_he4_inside / (Double_t) d_he4_all) / 100.;
		cout << PartInside << "% he4 are detected (dedx)" << endl;
	}
	
	cout << endl << "MpdPid::CheckMethodTwo m2-checking : " << endl;
	if (m_el_all != 0) {
		PartInside = TMath::Nint(10000 * m_el_inside / (Double_t) m_el_all) / 100.;
		cout << PartInside << "% electrons are detected (m2)" << endl;
		}
	if (m_mu_all != 0) {
		PartInside = TMath::Nint(10000 * m_mu_inside / (Double_t) m_mu_all) / 100.;
		cout << PartInside << "% muons are detected (m2)" << endl;
	}
	if (m_pi_all != 0) {
		PartInside = TMath::Nint(10000 * m_pi_inside / (Double_t) m_pi_all) / 100.;
		cout << PartInside << "% pions are detected (m2)" << endl;
	}
	if (m_ka_all != 0) {
		PartInside = TMath::Nint(10000 * m_ka_inside / (Double_t) m_ka_all) / 100.;
		cout << PartInside << "% kaons are detected (m2)" << endl;
	}
	if (m_pr_all != 0) {
		PartInside = TMath::Nint(10000 * m_pr_inside / (Double_t) m_pr_all) / 100.;
		cout << PartInside << "% protons are detected (m2)" << endl;
	}
	if (m_de_all != 0) {
		PartInside = TMath::Nint(10000 * m_de_inside / (Double_t) m_de_all) / 100.;
		cout << PartInside << "% deuterons are detected (m2)" << endl;
	}
	if (m_tr_all != 0) {
		PartInside = TMath::Nint(10000 * m_tr_inside / (Double_t) m_tr_all) / 100.;
		cout << PartInside << "% tritons are detected (m2)" << endl;
	}
	if (m_he3_all != 0) {
		PartInside = TMath::Nint(10000 * m_he3_inside / (Double_t) m_he3_all) / 100.;
		cout << PartInside << "% he3 are detected (m2)" << endl;
	}
	if (m_he4_all != 0) {
		PartInside = TMath::Nint(10000 * m_he4_inside / (Double_t) m_he4_all) / 100.;
		cout << PartInside << "% he4 are detected (m2)" << endl;
	}
	
	// DRAWING
	TCanvas *can3 = new TCanvas ("can3", "Normalized count of positive particles", 1200, 800);
	can3->Divide(3,3);
	if(pionsPos) {
		AmpRealPosPi->SetStats(kFALSE);
		AmpRealPosPi->SetLineWidth(3); AmpPidPosPi->SetLineWidth(2);
		AmpRealPosPi->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealPosPi->GetXaxis()->CenterTitle(kTRUE);
		AmpRealPosPi->GetYaxis()->SetTitle("Normalized count of particle"); AmpRealPosPi->GetYaxis()->CenterTitle(kTRUE);
		AmpRealPosPi->SetLineColor(kGreen); AmpPidPosPi->SetLineColor(kRed);
		TPad* can3_1=(TPad*)(can3->GetPrimitive("can3_1"));
		can3_1->cd(); can3_1->SetGrid(); can3_1->SetBorderMode(0); can3_1->SetBorderSize(0); can3_1->SetLogy();
		AmpRealPosPi->Draw(); AmpPidPosPi->Draw("SAME");
	}
	if(protons) {
		AmpRealPosPr->SetStats(kFALSE);
		AmpRealPosPr->SetLineWidth(3); AmpPidPosPr->SetLineWidth(2);
		AmpRealPosPr->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealPosPr->GetXaxis()->CenterTitle(kTRUE);
		AmpRealPosPr->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealPosPr->GetYaxis()->CenterTitle(kTRUE);
		AmpRealPosPr->SetLineColor(kGreen); AmpPidPosPr->SetLineColor(kRed);
		TPad* can3_2=(TPad*)(can3->GetPrimitive("can3_2"));
		can3_2->cd(); can3_2->SetGrid(); can3_2->SetBorderMode(0); can3_2->SetBorderSize(0); can3_2->SetLogy();
		AmpRealPosPr->Draw(); AmpPidPosPr->Draw("SAME");
	}
	if(kaonsPos) {
		AmpRealPosKa->SetStats(kFALSE);
		AmpRealPosKa->SetLineWidth(3);
		AmpPidPosKa->SetLineWidth(2);
		AmpRealPosKa->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealPosKa->GetXaxis()->CenterTitle(kTRUE);
		AmpRealPosKa->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealPosKa->GetYaxis()->CenterTitle(kTRUE);
		AmpRealPosKa->SetLineColor(kGreen); AmpPidPosKa->SetLineColor(kRed); 
		TPad* can3_3=(TPad*)(can3->GetPrimitive("can3_3"));
		can3_3->cd(); can3_3->SetGrid(); can3_3->SetBorderMode(0); can3_3->SetBorderSize(0); can3_3->SetLogy();
		AmpRealPosKa->Draw(); AmpPidPosKa->Draw("SAME");
	}
	if(positrons) {
		AmpRealPosEl->SetStats(kFALSE);
		AmpRealPosEl->SetLineWidth(3);
		AmpPidPosEl->SetLineWidth(2);
		AmpRealPosEl->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealPosEl->GetXaxis()->CenterTitle(kTRUE);
		AmpRealPosEl->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealPosEl->GetYaxis()->CenterTitle(kTRUE);
		AmpRealPosEl->SetLineColor(kGreen); AmpPidPosEl->SetLineColor(kRed); 
		TPad* can3_4=(TPad*)(can3->GetPrimitive("can3_4"));
		can3_4->cd(); can3_4->SetGrid(); can3_4->SetBorderMode(0); can3_4->SetBorderSize(0); can3_4->SetLogy();
		AmpRealPosEl->Draw(); AmpPidPosEl->Draw("SAME");
	}
	if(muonsPos) {
		AmpRealPosMu->SetStats(kFALSE);
		AmpRealPosMu->SetLineWidth(3);
		AmpPidPosMu->SetLineWidth(2);
		AmpRealPosMu->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealPosMu->GetXaxis()->CenterTitle(kTRUE);
		AmpRealPosMu->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealPosMu->GetYaxis()->CenterTitle(kTRUE);
		AmpRealPosMu->SetLineColor(kGreen); AmpPidPosMu->SetLineColor(kRed); 
		TPad* can3_5=(TPad*)(can3->GetPrimitive("can3_5"));
		can3_5->cd(); can3_5->SetGrid(); can3_5->SetBorderMode(0); can3_5->SetBorderSize(0); can3_5->SetLogy();
		AmpRealPosMu->Draw(); AmpPidPosMu->Draw("SAME");
	}
	if(deuterons) {
		AmpRealDe->SetStats(kFALSE);
		AmpRealDe->SetLineWidth(3);
		AmpPidDe->SetLineWidth(2);
		AmpRealDe->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealDe->GetXaxis()->CenterTitle(kTRUE);
		AmpRealDe->GetYaxis()->SetTitle("The proportion of total count of particles"); AmpRealDe->GetYaxis()->CenterTitle(kTRUE);
		AmpRealDe->SetLineColor(kGreen); AmpPidDe->SetLineColor(kRed); 
		TPad* can3_6=(TPad*)(can3->GetPrimitive("can3_6"));
		can3_6->cd(); can3_6->SetGrid(); can3_6->SetBorderMode(0); can3_6->SetBorderSize(0); can3_6->SetLogy();
		AmpRealDe->Draw(); AmpPidDe->Draw("SAME");
	}
	if(tritons) {
		AmpRealTr->SetStats(kFALSE);
		AmpRealTr->SetLineWidth(3);
		AmpPidTr->SetLineWidth(2);
		AmpRealTr->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealTr->GetXaxis()->CenterTitle(kTRUE);
		AmpRealTr->GetYaxis()->SetTitle("The proportion of total count of particles"); AmpRealTr->GetYaxis()->CenterTitle(kTRUE);
		AmpRealTr->SetLineColor(kGreen); AmpPidTr->SetLineColor(kRed); 
		TPad* can3_7=(TPad*)(can3->GetPrimitive("can3_7"));
		can3_7->cd(); can3_7->SetGrid(); can3_7->SetBorderMode(0); can3_7->SetBorderSize(0); can3_7->SetLogy();
		AmpRealTr->Draw(); AmpPidTr->Draw("SAME");
	}
	if(he3) {
		AmpRealHe3->SetStats(kFALSE);
		AmpRealHe3->SetLineWidth(3);
		AmpPidHe3->SetLineWidth(2);
		AmpRealHe3->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealHe3->GetXaxis()->CenterTitle(kTRUE);
		AmpRealHe3->GetYaxis()->SetTitle("The proportion of total count of particles"); AmpRealHe3->GetYaxis()->CenterTitle(kTRUE);
		AmpRealHe3->SetLineColor(kGreen); AmpPidHe3->SetLineColor(kRed); 
		TPad* can3_8=(TPad*)(can3->GetPrimitive("can3_8"));
		can3_8->cd(); can3_8->SetGrid(); can3_8->SetBorderMode(0); can3_8->SetBorderSize(0); can3_8->SetLogy();
		AmpRealHe3->Draw(); AmpPidHe3->Draw("SAME");
	}
	if(he4) {
		AmpRealHe4->SetStats(kFALSE);
		AmpRealHe4->SetLineWidth(3);
		AmpPidHe4->SetLineWidth(2);
		AmpRealHe4->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealHe4->GetXaxis()->CenterTitle(kTRUE);
		AmpRealHe4->GetYaxis()->SetTitle("The proportion of total count of particles"); AmpRealHe4->GetYaxis()->CenterTitle(kTRUE);
		AmpRealHe4->SetLineColor(kGreen); AmpPidHe4->SetLineColor(kRed); 
		TPad* can3_9=(TPad*)(can3->GetPrimitive("can3_9"));
		can3_9->cd(); can3_9->SetGrid(); can3_9->SetBorderMode(0); can3_9->SetBorderSize(0); can3_9->SetLogy();
		AmpRealHe4->Draw(); AmpPidHe4->Draw("SAME");
	}
	
	TCanvas *can4 = new TCanvas ("can4", "Normalized count of negative particles", 1200, 800);
	can4->Divide(2,2);
	if(pionsNeg) {
		AmpRealNegPi->SetStats(kFALSE);
		AmpRealNegPi->SetLineWidth(3); AmpPidNegPi->SetLineWidth(2);
		AmpRealNegPi->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealNegPi->GetXaxis()->CenterTitle(kTRUE);
		AmpRealNegPi->GetYaxis()->SetTitle("Normalized count of particle"); AmpRealNegPi->GetYaxis()->CenterTitle(kTRUE);
		AmpRealNegPi->SetLineColor(kGreen); AmpPidNegPi->SetLineColor(kRed);
		TPad* can4_1=(TPad*)(can4->GetPrimitive("can4_1"));
		can4_1->cd(); can4_1->SetGrid(); can4_1->SetBorderMode(0); can4_1->SetBorderSize(0); can4_1->SetLogy();
		AmpRealNegPi->Draw(); AmpPidNegPi->Draw("SAME");
	}
	if(kaonsNeg) {
		AmpRealNegKa->SetStats(kFALSE);
		AmpRealNegKa->SetLineWidth(3);
		AmpPidNegKa->SetLineWidth(2);
		AmpRealNegKa->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealNegKa->GetXaxis()->CenterTitle(kTRUE);
		AmpRealNegKa->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealNegKa->GetYaxis()->CenterTitle(kTRUE);
		AmpRealNegKa->SetLineColor(kGreen); AmpPidNegKa->SetLineColor(kRed); 
		TPad* can4_2=(TPad*)(can4->GetPrimitive("can4_2"));
		can4_2->cd(); can4_2->SetGrid(); can4_2->SetBorderMode(0); can4_2->SetBorderSize(0); can4_2->SetLogy();
		AmpRealNegKa->Draw(); AmpPidNegKa->Draw("SAME");
	}
	if(electrons) {
		AmpRealNegEl->SetStats(kFALSE);
		AmpRealNegEl->SetLineWidth(3);
		AmpPidNegEl->SetLineWidth(2);
		AmpRealNegEl->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealNegEl->GetXaxis()->CenterTitle(kTRUE);
		AmpRealNegEl->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealNegEl->GetYaxis()->CenterTitle(kTRUE);
		AmpRealNegEl->SetLineColor(kGreen); AmpPidNegEl->SetLineColor(kRed); 
		TPad* can4_3=(TPad*)(can4->GetPrimitive("can4_3"));
		can4_3->cd(); can4_3->SetGrid(); can4_3->SetBorderMode(0); can4_3->SetBorderSize(0); can4_3->SetLogy();
		AmpRealNegEl->Draw(); AmpPidNegEl->Draw("SAME");
	}
	if(muonsNeg) {
		AmpRealNegMu->SetStats(kFALSE);
		AmpRealNegMu->SetLineWidth(3);
		AmpPidNegMu->SetLineWidth(2);
		AmpRealNegMu->GetXaxis()->SetTitle("Momentum, GeV/c"); AmpRealNegMu->GetXaxis()->CenterTitle(kTRUE);
		AmpRealNegMu->GetYaxis()->SetTitle("Normalized count of particles"); AmpRealNegMu->GetYaxis()->CenterTitle(kTRUE);
		AmpRealNegMu->SetLineColor(kGreen); AmpPidNegMu->SetLineColor(kRed); 
		TPad* can4_4=(TPad*)(can4->GetPrimitive("can4_4"));
		can4_4->cd(); can4_4->SetGrid(); can4_4->SetBorderMode(0); can4_4->SetBorderSize(0); can4_4->SetLogy();
		AmpRealNegMu->Draw(); AmpPidNegMu->Draw("SAME");
	}
	
	cout << "MpdPid::CheckMethodTwo finished successfully" << endl;
}
*/
/*
void MpdPid::Pidcheck(TString inname, Int_t nev = 0)
{
	Int_t nevents = nev;
	if (nevents == 0) {
		TChain chain("cbmsim");
		chain.Add(inname.Data());
		nevents = chain.GetEntries();
	}
	if (nevents >= 50000) CheckMethodOne(inname, nevents);
	else CheckMethodTwo(inname, nevents);
}
*/
void MpdPid_AZ::Init(TString Generator, TString Tracking)
{
	cout<<"MpdPid::Init().."<<endl;
	
	fSigmaDedx_2 = 0.11; fSigmaDedx_3 = 0.08;
	Double_t PMIN=0., PMAX=4.5;
	Double_t dedxParam;
	
	// Setting default ratio ('rat' is pos./neg.)
	prrat=43.43;
	if (fEnergy < 7.0) prrat=1000.;
	
	//
	// Bethe-Bloch versus p (MC with STRA=0!)
	//
	
	parElBB=new TF1("parElBB","pol3(0)",0.01,PMAX);
	parMuBB = new TF1("parMuBB","[0]/pow(x/sqrt(x*x+0.011),[3])*([1]-pow(x/sqrt(x*x+0.011),[3])-log([2]+pow(1./(x/0.1057),[4])) )",PMIN,PMAX);
	parPiBB1 = new TF1("parPiBB1","[0]/pow(x/sqrt(x*x+0.01949),[3])*([1]-pow(x/sqrt(x*x+0.01949),[3])-log([2]+pow(1./(x/0.1396),[4])) )",PMIN,PMAX);
	parPiBB2 = new TF1("parPiBB2","[0]/pow(x/sqrt(x*x+0.01949),[3])*([1]-pow(x/sqrt(x*x+0.01949),[3])-log([2]+pow(1./(x/0.1396),[4])) )",PMIN,PMAX);
	parPiBB3 = new TF1("parPiBB3","[0]/pow(x/sqrt(x*x+0.01949),[3])*([1]-pow(x/sqrt(x*x+0.01949),[3])-log([2]+pow(1./(x/0.1396),[4])) )",PMIN,PMAX);
	parPiBB4 = new TF1("parPiBB4","[0]/pow(x/sqrt(x*x+0.01949),[3])*([1]-pow(x/sqrt(x*x+0.01949),[3])-log([2]+pow(1./(x/0.1396),[4])) )",PMIN,PMAX);
	parKaBB1 = new TF1("parKaBB1","[0]/pow(x/sqrt(x*x+0.2437),[3])*([1]-pow(x/sqrt(x*x+0.2437),[3])-log([2]+pow(1./(x/0.4937),[4])) )",PMIN,PMAX);
	parKaBB2 = new TF1("parKaBB2","[0]/pow(x/sqrt(x*x+0.2437),[3])*([1]-pow(x/sqrt(x*x+0.2437),[3])-log([2]+pow(1./(x/0.4937),[4])) )",PMIN,PMAX);
	parKaBB3 = new TF1("parKaBB3","[0]/pow(x/sqrt(x*x+0.2437),[3])*([1]-pow(x/sqrt(x*x+0.2437),[3])-log([2]+pow(1./(x/0.4937),[4])) )",PMIN,PMAX);
	parKaBB4 = new TF1("parKaBB4","[0]/pow(x/sqrt(x*x+0.2437),[3])*([1]-pow(x/sqrt(x*x+0.2437),[3])-log([2]+pow(1./(x/0.4937),[4])) )",PMIN,PMAX);
	parPrBB1 = new TF1("parPrBB1","[0]/pow(x/sqrt(x*x+0.88),[3])*([1]-pow(x/sqrt(x*x+0.88),[3])-log([2]+pow(1./(x/0.9383),[4])) )",PMIN,PMAX);
	parPrBB2 = new TF1("parPrBB2","[0]/pow(x/sqrt(x*x+0.88),[3])*([1]-pow(x/sqrt(x*x+0.88),[3])-log([2]+pow(1./(x/0.9383),[4])) )",PMIN,PMAX);
	parPrBB3 = new TF1("parPrBB3","[0]/pow(x/sqrt(x*x+0.88),[3])*([1]-pow(x/sqrt(x*x+0.88),[3])-log([2]+pow(1./(x/0.9383),[4])) )",PMIN,PMAX);
	parPrBB4 = new TF1("parPrBB4","[0]/pow(x/sqrt(x*x+0.88),[3])*([1]-pow(x/sqrt(x*x+0.88),[3])-log([2]+pow(1./(x/0.9383),[4])) )",PMIN,PMAX);
	parDeBB = new TF1("parDeBB","[0]/pow(x/sqrt(x*x+3.52),[3])*([1]-pow(x/sqrt(x*x+3.52),[3])-log([2]+pow(1./(x/1.876),[4])) )",PMIN,PMAX);
	parTrBB = new TF1("parTrBB","[0]/pow(x/sqrt(x*x+7.89),[3])*([1]-pow(x/sqrt(x*x+7.89),[3])-log([2]+pow(1./(x/2.81),[4])) )",PMIN,PMAX);
	// Double charged have p_reco= p_MC/2
	parHe3BB = new TF1("parHe3BB","[0]*((1+(x/1.4047)**2)/pow(x/1.4047,[3])*([1]+[2]*log(1+pow(x/1.4047,2)))-1.)",PMIN,PMAX);
	parHe4BB = new TF1("parHe4BB","[0]*((1+(x/1.863)**2)/pow(x/1.863,[3])*([1]+[2]*log(1+pow(x/1.863,2)))-1.)",PMIN,PMAX);
	
	// DEFINE TRACKING
	if(Tracking=="HP")
	{
		fSigmaDedx_1 = 0.05; 
		
		// electrons
		dedxParam = fKoef*1.97124e-06; parElBB->SetParameter(0, dedxParam);
		dedxParam = fKoef*9.51716e-07; parElBB->SetParameter(1, dedxParam);
		dedxParam = fKoef*(-1.73101e-06); parElBB->SetParameter(2, dedxParam);
		dedxParam = fKoef*1.06394e-06; parElBB->SetParameter(3, dedxParam);
		// muons
		dedxParam = fKoef*3.31e-07;
		parMuBB->SetParameters(dedxParam,3.49,0.081,2.94,1.575);
		// pions
		dedxParam = fKoef*4.89e-08;
		parPiBB1->SetParameters(dedxParam,22.953,1.876e-05,2.59,4.153);
		parPiBB2->SetParameters(dedxParam,22.953,1.876e-05,2.59,4.153);
		parPiBB3->SetParameters(dedxParam,22.953,1.876e-05,2.59,4.153);
		parPiBB4->SetParameters(dedxParam,22.953,1.876e-05,2.59,4.153);
		// kaons
		dedxParam = fKoef*1.636e-07;
		parKaBB1->SetParameters(dedxParam,6.415,3.309e-02,2.716,2.754);
		parKaBB2->SetParameters(dedxParam,6.415,3.309e-02,2.716,2.754);
		parKaBB3->SetParameters(dedxParam,6.415,3.309e-02,2.716,2.754);
		parKaBB4->SetParameters(dedxParam,6.415,3.309e-02,2.716,2.754);
		// protons
		dedxParam = fKoef*3.428e-07;
		parPrBB1->SetParameters(dedxParam,3.768,-1.74e-02,2.284,0.963);
		parPrBB2->SetParameters(dedxParam,3.768,-1.74e-02,2.284,0.963);
		parPrBB3->SetParameters(dedxParam,3.768,-1.74e-02,2.284,0.963);
		parPrBB4->SetParameters(dedxParam,3.768,-1.74e-02,2.284,0.963);
		// deuterons
		dedxParam = fKoef*3.27e-07;
		parDeBB->SetParameters(dedxParam,3.74,-0.23,2.32,0.987);
		// tritons
		dedxParam = fKoef*2.59e-07;
		parTrBB->SetParameters(dedxParam,5.06,0.0001,2.2,1.056);
		// He3
		dedxParam = fKoef*2.86201e-06;
		parHe3BB->SetParameters(dedxParam,2.10168e+00,2.74807e-01,1.86774e+00);
		// He4
		dedxParam = fKoef*2.96e-06;
		parHe4BB->SetParameters(dedxParam,2.085,0.256,1.85);
	}
	else // Tracking == "CF" is default
	{
		if (Tracking != "CF") cout << "ERROR! Unknown tracking method! Switch to default (\"CF\")." << endl;
		
		fSigmaDedx_1 = 0.07; 
		
		// electrons
		dedxParam = fKoef*1.97124e-06; parElBB->SetParameter(0, dedxParam);
		dedxParam = fKoef*9.51716e-07; parElBB->SetParameter(1, dedxParam);
		dedxParam = fKoef*(-1.73101e-06); parElBB->SetParameter(2, dedxParam);
		dedxParam = fKoef*1.06394e-06; parElBB->SetParameter(3, dedxParam);
		// muons
		dedxParam = fKoef*3.31e-07;
		parMuBB->SetParameters(dedxParam,3.49,0.081,2.94,1.575);
		// pions
		dedxParam = fKoef*(-1414.98);
		parPiBB1->SetParameters(dedxParam,2.05564,16.9395,3.07208,-0.824895);
		dedxParam = fKoef*(-1602.51);
		parPiBB2->SetParameters(dedxParam,0.615104,2.18989,3.40611,-0.341706);
		dedxParam = fKoef*(-1986.35);
		parPiBB3->SetParameters(dedxParam,-0.0316029,0.137013,4.99144,-0.156954);
		dedxParam = fKoef*(-1791.57);
		parPiBB4->SetParameters(dedxParam,-0.995518,-0.516845,3.06142,-0.0771934);
		// kaons
		dedxParam = fKoef*(-3592.55);
		parKaBB1->SetParameters(dedxParam,0.640408,1.59293,0.395287,3.09712);
		dedxParam = fKoef*(-3363.36);
		parKaBB2->SetParameters(dedxParam,0.509816,1.46593,0.372388,3.06191);
		dedxParam = fKoef*(-2834.79);
		parKaBB3->SetParameters(dedxParam,0.222117,1.27029,0.282767,3.09152);
		dedxParam = fKoef*(-3537.74);
		parKaBB4->SetParameters(dedxParam,0.598669,1.4831,-0.263893,3.27131);
		// protons
		dedxParam = fKoef*(-7339.89);
		parPrBB1->SetParameters(dedxParam,1.76536,3.22294,0.0533274,2.90796);
		dedxParam = fKoef*(-7347.42);
		parPrBB2->SetParameters(dedxParam,1.76034,3.17649,-0.0118557,2.87759);
		dedxParam = fKoef*(-6788.54);
		parPrBB3->SetParameters(dedxParam,1.7389,3.18312,-0.198819,3.02804);
		dedxParam = fKoef*(-5703.8);
		parPrBB4->SetParameters(dedxParam,1.57262,2.88478,-0.58615,3.58431);
		// deuterons
		dedxParam = fKoef*3.27e-07;
		parDeBB->SetParameters(dedxParam,3.74,-0.23,2.32,0.987);
		// tritons
		dedxParam = fKoef*2.59e-07;
		parTrBB->SetParameters(dedxParam,5.06,0.0001,2.2,1.056);
		// He3
		dedxParam = fKoef*2.86201e-06;
		parHe3BB->SetParameters(dedxParam,2.10168e+00,2.74807e-01,1.86774e+00);
		// He4
		dedxParam = fKoef*2.96e-06;
		parHe4BB->SetParameters(dedxParam,2.085,0.256,1.85);
	}
	
	// Particle yields versus momentum (close to a thermal function).
	// The predicted number roughly speaking are not dN/dy or total yiels,
	// only their relative hights are of relevance!
	
	Double_t amplParam;
	parElPosMom = new TF1("parElPosMom",this,&MpdPid_AZ::MomPr,PMIN,PMAX,5,"MpdPid_AZ","MomPi");
	parElPosMom->SetParameters(17.6,-0.12,0.078,0.167,0.00); // QGSM 5-9 gev
	parElNegMom = new TF1("parElNegMom",this,&MpdPid_AZ::MomPr,PMIN,PMAX,5,"MpdPid_AZ","MomPi");
	parElNegMom->SetParameters(16.3,-0.12,0.078,0.167,0.00);
	parMuPosMom = new TF1("parMuPosMom",this,&MpdPid_AZ::MomPi,PMIN,PMAX,5,"MpdPid_AZ","MomPi");
	parMuPosMom->SetParameters(20.5,0.064,0.107,0.05,0.105); // QGSM 5-9 gev
	parMuNegMom = new TF1("parMuNegMom",this,&MpdPid_AZ::MomPi,PMIN,PMAX,5,"MpdPid_AZ","MomPi");
	parMuNegMom->SetParameters(20.5,0.064,0.107,0.05,0.105);
	parPiPosMom = new TF1("parPiPosMom",this,&MpdPid_AZ::MomPi,PMIN,PMAX,5,"MpdPid_AZ","MomPi");
	parPiNegMom = new TF1("parPiNegMom",this,&MpdPid_AZ::MomPi,PMIN,PMAX,5,"MpdPid_AZ","MomPi");
	parKaPosMom = new TF1("parKaPosMom",this,&MpdPid_AZ::MomPr,PMIN,PMAX,5,"MpdPid_AZ","MomPr");
	parKaNegMom = new TF1("parKaNegMom",this,&MpdPid_AZ::MomPr,PMIN,PMAX,5,"MpdPid_AZ","MomPr");
	parPrPosMom = new TF1("parPrPosMom",this,&MpdPid_AZ::MomPr,PMIN,PMAX,5,"MpdPid_AZ","MomPr");
	parPrNegMom = new TF1("parPrNegMom",this,&MpdPid_AZ::MomPr,PMIN,PMAX,5,"MpdPid_AZ","MomPr");
	parDeMom = new TF1("parDeMom",this,&MpdPid_AZ::MomPr,PMIN,PMAX,5,"MpdPid_AZ","MomPr");
	parDeMom->SetParameters(5.7,0.338,0.333,0.114,1.878); // QGSM 5 gev
	parDeMom->SetParameters(1.8,0.05,0.432,0.163,1.878); // QGSM 9 gev
	parTrMom = new TF1("parTrMom",this,&MpdPid_AZ::MomPr,PMIN,PMAX,5,"MpdPid_AZ","MomPr");
	parTrMom->SetParameters(0.2,-0.35,0.723,0.2,2.81);
	parHe3Mom = new TF1("parHe3Mom",this,&MpdPid_AZ::MomPr,PMIN,PMAX,5,"MpdPid_AZ","MomPr");
	parHe3Mom->SetParameters(0.36,-0.784,530.3,0.131,1.983);
	parHe4Mom = new TF1("parHe4Mom",this,&MpdPid_AZ::MomPr,PMIN,PMAX,5,"MpdPid_AZ","MomPr");
	parHe4Mom->SetParameters(6.6e-03,0.27,0.2,1.42,3.51);
	
	if (fEnergy < 7.0){
		parPiPosMom->SetParameters(307.0,0.035,0.175,0.127,0.139); // QGSM 5 gev
		parPiNegMom->SetParameters(325.6,0.035,0.175,0.127,0.139); // QGSM 5 gev
		parKaPosMom->SetParameters(15.3,0.236,0.203,0.056,0.494); // QGSM 5 gev
		parKaNegMom->SetParameters(8.88,0.236,0.203,0.056,0.494); // QGSM 5 gev
		amplParam = 104.0;
		parPrPosMom->SetParameters(amplParam,0.213,0.294,0.09,0.938); // QGSM 5 gev
		amplParam /= prrat;
		parPrNegMom->SetParameters(amplParam,0.213,0.294,0.09,0.938); // QGSM 5 gev
	} else {
		if( (Generator == "LAQGSM") || (Generator == "QGSM") ){
			parPiPosMom->SetParameters(473.,0.034,0.187,0.469,0.139); // QGSM 9 gev
			parPiNegMom->SetParameters(501.6,0.034,0.187,0.469,0.139); // QGSM 9 gev
			parKaPosMom->SetParameters(21.1,0.157,0.241,0.043,0.494); // QGSM 9 gev
			parKaNegMom->SetParameters(12.25,0.157,0.241,0.043,0.494); // QGSM 9 gev
			amplParam = 67.4;
			parPrPosMom->SetParameters(amplParam,.02,0.365,0.01,0.938); // QGSM 9 gev
			amplParam /= prrat;
			parPrNegMom->SetParameters(amplParam,.02,0.365,0.01,0.938); // QGSM 9 gev
		}
		if(Generator == "URQMD"){
			/*
			parPiPosMom->SetParameters(4.809e+03,0.702,0.164,0.296,0.179); // UrQMD 11 gev
			parPiNegMom->SetParameters(4.233e+03,0.935,0.155,0.288,0.214); // UrQMD 11 gev
			parKaPosMom->SetParameters(1.027e+03,0.316,0.286,0.1,0.292); // UrQMD 11 gev
			parKaNegMom->SetParameters(5.947e+02,0.352,0.263,0.0938,0.325); // UrQMD 11 gev
			amplParam = 2.54e+03;
			parPrPosMom->SetParameters(amplParam,0.934,0.311,0.182,0.694); // UrQMD 11 gev
			amplParam /= prrat;
			parPrNegMom->SetParameters(amplParam,0.934,0.311,0.182,0.694); // UrQMD 11 gev
			*/
			parPiPosMom->SetParameters(1.541e+04,0.618,0.167,0.302,0.163); // UrQMD 8 gev CF
			parPiNegMom->SetParameters(1.313e+04,0.896,0.152,0.29,0.237); // UrQMD 8 gev CF
			parKaPosMom->SetParameters(5.217e+03,0.0101,0.281,0.144,-0.143); // UrQMD 8 gev CF
			parKaNegMom->SetParameters(1.646e+03,0.38,0.27,0.097,0.336); // UrQMD 8 gev CF
			amplParam = 1.272e+04;
			parPrPosMom->SetParameters(amplParam,0.573,0.295,0.134,0.982); // UrQMD 8 gev CF
			amplParam /= prrat;
			parPrNegMom->SetParameters(amplParam,0.573,0.295,0.134,0.982); // UrQMD 8 gev CF
		}
		//if(DEFAULT){
		else{
			parPiPosMom->SetParameters(503.,0.035,0.203,0.668,0.139); // average 9 gev
			parPiNegMom->SetParameters(533.4,0.035,0.203,0.668,0.139); // average 9 gev
			parKaPosMom->SetParameters(29.3,0.17,0.27,0.06,0.494); // average 9 gev
			parKaNegMom->SetParameters(17.,0.17,0.27,0.06,0.494); // average 9 gev
			amplParam = 88.;
			parPrPosMom->SetParameters(amplParam,0.18,0.37,0.15,0.938); // average 9 gev
			amplParam /= prrat;
			parPrNegMom->SetParameters(amplParam,0.18,0.37,0.15,0.938); // average 9 gev
		}
	}
	
	//
	// Width of mass-squared versus total p
	//
	
	parElM2=new TF1("parElM2","pol3(0)",PMIN,PMAX);  
	//parElM2->SetParameters(0.011,-0.027,0.0569,-0.00783);
	parElM2->SetParameters(0.,0.,0.,0.);
	
	parMuM2=new TF1("parMuM2","pol3(0)",PMIN,PMAX);  
	//parMuM2->SetParameters(0.0201574,-0.0373204,0.0546577,-0.00743827);
	parMuM2->SetParameters(0.,0.,0.,0.);
	
	parPiLowPM2=new TF1("parPiLowPM2","pol2(0)",PMIN,PMAX);  
	parPiLowPM2->SetParameters(0.00261214, -0.00246658, 0.0299342);
	
	parPiHighPM2=new TF1("parPiHighPM2","pol2(0)",PMIN,PMAX);  
	parPiHighPM2->SetParameters(-0.029863, 0.0490093, 0.00885599);
	
	parKaM2=new TF1("parKaM2","pol2(0)",PMIN,PMAX);
	parKaM2->SetParameters(0.00060699, 0.0210267, 0.0188327);
	
	parPrLowPM2=new TF1("parPrLowPM2","pol3(0)",PMIN,PMAX);  
	parPrLowPM2->SetParameters(0.0687976, -0.0933061, 0.123855, -0.0284459);
	
	parPrHighPM2=new TF1("parPrHighPM2","pol2(0)",PMIN,PMAX);  
	parPrHighPM2->SetParameters(-0.00296477, 0.0584284, 0.0121032);
	
	parDeM2=new TF1("parDeM2","pol3(0)",PMIN,PMAX);  
	parDeM2->SetParameters(0.535691,-0.529882,0.293807,-0.0428139);
	
	parTrM2=new TF1("parTrM2","pol3(0)",PMIN,PMAX);  
	parTrM2->SetParameters(0.422,0.3,-0.202,0.0524);
	
	parHe3M2=new TF1("parHe3M2","pol3(0)",PMIN,PMAX);  
	parHe3M2->SetParameters(0.17,-0.0,0.0,-0.00);
	
	parHe4M2=new TF1("parHe4M2","pol3(0)",PMIN,PMAX);  
	parHe4M2->SetParameters(0.3,-0.0,0.0,-0.00);
	
	fgaus = new TF1("fgaus","gaus(0)",-1.,5.);
	fgaus2 = new TF2("fgaus2","[0]*TMath::Gaus(x,[1],[2])*TMath::Gaus(y,[3],[4])",-2,10,-1,5);
	  
}

Double_t MpdPid_AZ::GetDedxElParam(Double_t p){
   Double_t dedx=parElBB->Eval(p);
   if (p>0.8) dedx = 2.2E-06;
   return dedx;
}

Double_t MpdPid_AZ::GetDedxMuParam(Double_t p){
   Double_t dedx=parMuBB->Eval(p);
   if (p<0.2) dedx *= 1.06;
   return dedx;
}

Double_t MpdPid_AZ::GetDedxPiParam(Double_t p){
   Double_t dedx=parPiBB1->Eval(p);
   //dedx *= 1.01;
   //if (p>2.5) dedx *= 0.99;
   return dedx;
}

Double_t MpdPid_AZ::GetDedxPiParam(Double_t p, Double_t eta){
   Double_t dedx = -1.; 
   Double_t AbsEta = TMath::Abs(eta);
   if (AbsEta < 0.4) dedx = parPiBB1->Eval(p);
   else if ( (AbsEta >= 0.4) && (AbsEta < 0.8) ) 
   {
	   dedx = parPiBB2->Eval(p);
	   if (p>1.8) dedx *= 1.005;
   }
   else if ( (AbsEta >= 0.8) && (AbsEta < 1.2) ) 
   {
	   dedx = parPiBB3->Eval(p);
	   if (p>2.6) dedx *= 0.99;
   }
   else if ( (AbsEta >= 1.2) && (AbsEta < 1.6) ) dedx = parPiBB4->Eval(p);
   return dedx;
}

Double_t MpdPid_AZ::GetDedxKaParam(Double_t p){
   Double_t dedx=parKaBB1->Eval(p);
   //if (p>2.2) dedx *= 1.02;
   return dedx;
}

Double_t MpdPid_AZ::GetDedxKaParam(Double_t p, Double_t eta){
   Double_t dedx = -1.;
   Double_t AbsEta = TMath::Abs(eta);
   if (AbsEta < 0.4) 
   {
	   dedx = parKaBB1->Eval(p);
	   if (p>2.1) dedx *= 1.07;
   }
   else if ( (AbsEta >= 0.4) && (AbsEta < 0.8) ) 
   {
	   dedx = parKaBB2->Eval(p);
	   if (p>2.3) dedx *= 1.05;
   }
   else if ( (AbsEta >= 0.8) && (AbsEta < 1.2) ) 
   {
	   dedx = parKaBB3->Eval(p);
	   if (p>2.7) dedx *= 1.02;
   }
   else if ( (AbsEta >= 1.2) && (AbsEta < 1.6) ) dedx = parKaBB4->Eval(p);
   return dedx;
}

Double_t MpdPid_AZ::GetDedxPrParam(Double_t p){
   Double_t dedx=parPrBB1->Eval(p);
   //if (p>2.5) dedx *= 1.02;
   //if (p<0.3) dedx *= 0.92;
   return dedx;
}

Double_t MpdPid_AZ::GetDedxPrParam(Double_t p, Double_t eta){
   Double_t dedx = -1.;
   Double_t AbsEta = TMath::Abs(eta);
   if (AbsEta < 0.4) 
   {
	   dedx = parPrBB1->Eval(p);
	   if (p>2.3) dedx *= 1.03;
   }
   else if ( (AbsEta >= 0.4) && (AbsEta < 0.8) ) 
   {
	   dedx = parPrBB2->Eval(p);
	   if (p>2.5) dedx *= 1.02;
   }
   else if ( (AbsEta >= 0.8) && (AbsEta < 1.2) ) 
   dedx = parPrBB3->Eval(p);
   else if ( (AbsEta >= 1.2) && (AbsEta < 1.6) ) dedx = parPrBB4->Eval(p);
   return dedx;
}

Double_t MpdPid_AZ::GetDedxDeParam(Double_t p){
   Double_t dedx=parDeBB->Eval(p);
   if (p<0.25) dedx *= 0.83;
   return dedx;
}

Double_t MpdPid_AZ::GetDedxTrParam(Double_t p){
   Double_t dedx=parTrBB->Eval(p);
   if (p<0.5) dedx *= 0.91;
   return dedx;
}

Double_t MpdPid_AZ::GetDedxHe3Param(Double_t p){
   Double_t dedx=parHe3BB->Eval(p);
   if (p<0.35) dedx *= 0.88;
   return dedx;
}

Double_t MpdPid_AZ::GetDedxHe4Param(Double_t p){
   Double_t dedx=parHe4BB->Eval(p);
   if (p>0.0) dedx *= 1.12;
   return dedx;
}

Bool_t MpdPid_AZ::FillProbs(MpdTrack* track)
{
   if (track == 0) return (kFALSE);
   Double_t px=track->GetPx(),py=track->GetPy(),pz=track->GetPz();   
   Double_t p=TMath::Sqrt(px*px+py*py+pz*pz);       
//
// NO parameterisation beyond p=4.5 GeV/c
// ignore anti-d,-He3 and -He4 
//
   if (p > 4.5) return (kFALSE);
   Int_t charge = 1;
   if (track->GetPt() > 0) charge = -1;
   Int_t flag = track->GetTofFlag();
   Double_t dedx = track->GetdEdXTPC();
   Double_t eta = track->GetEta();
   
   if (flag == 2 || flag == 6){
      Double_t m2 = track->GetTofMass2();
      FillProbs(p,eta,dedx,m2,charge);
   }else{
      FillProbs(p,eta,dedx,charge);   
   }

   return kTRUE;
}

Bool_t MpdPid_AZ::FillProbs(MpdTrack* track, Double_t dedx){
   if (track == 0) return (kFALSE);
      
   Double_t px=track->GetPx(),py=track->GetPy(),pz=track->GetPz();   
   Double_t p=TMath::Sqrt(px*px+py*py+pz*pz);       
//
// NO parameterisation beyond p=4.5 GeV/c
// ignore anti-d,-He3 and -He4 
//
   if (p > 4.5) return (kFALSE);
   Int_t charge = 1;
   if (track->GetPt() > 0.) charge = -1;
      
   Int_t flag = track->GetTofFlag();
   Double_t eta = track->GetEta();
   
   if (flag == 2 || flag == 6){
      Double_t m2 = track->GetTofMass2();
      FillProbs(p,eta,dedx,m2,charge);
   }else{
      FillProbs(p,eta,dedx,charge);
   }

   return kTRUE;
}

Bool_t MpdPid_AZ::FillProbs(Double_t p, Double_t eta, Double_t dedx, Int_t charge){
	
	//
	// NO parameterisation beyond p=4.5 GeV/c
	// ignore anti-d,-He3 and -He4 
	//
	
	if (p > 4.5) return (kFALSE);
	
	fCharge = charge;
	
	Double_t nel=0.,nmu=0.,npi=0.,nka=0.,npr=0.,nde=0.,ntr=0.,nhe3=0.,nhe4=0.;
	
	if (fCharge > 0) {
		nel=parElPosMom->Eval(p);
		nmu=parMuPosMom->Eval(p);
		npi=parPiPosMom->Eval(p);
		nka=parKaPosMom->Eval(p);
		npr=parPrPosMom->Eval(p);
		nde=parDeMom->Eval(p);
		ntr=parTrMom->Eval(p);
		nhe3=parHe3Mom->Eval(p);
		nhe4=parHe4Mom->Eval(p);
	}
	else if (fCharge < 0) {
		nel=parElNegMom->Eval(p);
		nmu=parMuNegMom->Eval(p);
		npi=parPiNegMom->Eval(p);
		nka=parKaNegMom->Eval(p);
		npr=parPrNegMom->Eval(p);
	}
	else return (kFALSE);
	
	//
	// Params. for dE/dx sigma versus total momentum.
	// These are koef*<dE/dx> , koef=0.07
	// Deviations from Bethe-Bloch at low-p accounted by somewhat larger sigmas
	//
	
	Double_t emeanel=GetDedxElParam(p);
	Double_t emeanmu=GetDedxMuParam(p);
	Double_t emeanpi=GetDedxPiParam(p, eta);
	Double_t emeanpr=GetDedxPrParam(p, eta);
	Double_t emeanka=GetDedxKaParam(p, eta);
	Double_t emeande=GetDedxDeParam(p);
	Double_t emeantr=GetDedxTrParam(p);
	Double_t emeanhe3=GetDedxHe3Param(p);
	Double_t emeanhe4=GetDedxHe4Param(p);
	
	Double_t sigeel,sigemu,sigepi,sigepr,sigeka,sigede,sigetr,sigehe3,sigehe4;
	
	sigeel=fSigmaDedx_3;
	sigemu=fSigmaDedx_1;
	sigepi=fSigmaDedx_1;
	sigeka=fSigmaDedx_1;
	sigepr=fSigmaDedx_1;
	sigede=fSigmaDedx_1;
	sigetr=fSigmaDedx_1;
	sigehe3=fSigmaDedx_1;
	sigehe4=fSigmaDedx_1;
	
	if (p<0.15){
		sigemu=fSigmaDedx_2; sigepi=fSigmaDedx_2; sigepr=fSigmaDedx_2; sigeka=fSigmaDedx_2; sigede=fSigmaDedx_2;
	}
	if (p<0.45){
		sigede=fSigmaDedx_2; sigetr=fSigmaDedx_2; sigehe3=fSigmaDedx_2; sigehe4=fSigmaDedx_2;
	}
	Double_t fel=0.,fmu=0.,fpi=0.,fka=0.,fpr=0.,fde=0.,ftr=0.,fhe3=0.,fhe4=0.;
	Double_t fsum=0., cut=4.0;
	if (kSigmaEloss>0.1) cut = kSigmaEloss;
	
	//
	// Set prob=0. for differences greater than "n" of sigmas
	// otherwise evaluation..
	
	fel = GetDedxProb(dedx, cut, nel, emeanel, sigeel);
	fmu = GetDedxProb(dedx, cut, nmu, emeanmu, sigemu);
	fpi = GetDedxProb(dedx, cut, npi, emeanpi, sigepi);
	fka = GetDedxProb(dedx, cut, nka, emeanka, sigeka);
	fpr = GetDedxProb(dedx, cut, npr, emeanpr, sigepr);
	fde = GetDedxProb(dedx, cut, nde, emeande, sigede);
	ftr = GetDedxProb(dedx, cut, ntr, emeantr, sigetr);
	fhe3 = GetDedxProb(dedx, cut, nhe3, emeanhe3, sigehe3);
	fhe4 = GetDedxProb(dedx, cut, nhe4, emeanhe4, sigehe4);
	
	//
	// Normalization
	//
	
	fsum=fel+fmu+fpi+fka+fpr+fde+ftr+fhe3+fhe4;
	if (1.0e+8*fsum>0.){
		fProbEl = fel/fsum;
		fProbMu = fmu/fsum;
		fProbPi = fpi/fsum;
		fProbKa = fka/fsum;
		fProbPr = fpr/fsum;
		fProbDe = fde/fsum;
		fProbTr = ftr/fsum;
		fProbHe3 = fhe3/fsum;
		fProbHe4 = fhe4/fsum;
	} else {return kFALSE;}; // outliers!
	
	return kTRUE;         
}

Bool_t MpdPid_AZ::FillProbs(Double_t p, Double_t eta, Double_t dedx, Double_t m2, Int_t charge){
	
	//
	// NO parameterisation beyond p=4.5 GeV/c
	// ignore anti-d,-He3 and -He4 
	//
	
	if (p > 4.5) return (kFALSE);
	
	fCharge = charge;
	
	Double_t nel=0.,nmu=0.,npi=0.,nka=0.,npr=0.,nde=0.,ntr=0.,nhe3=0.,nhe4=0.;
	
	//
	// particle multiplicities for positives and negatives separately
	//
	
	if (fCharge > 0) {
		nel=parElPosMom->Eval(p);
		nmu=parMuPosMom->Eval(p);
		npi=parPiPosMom->Eval(p);
		nka=parKaPosMom->Eval(p);
		npr=parPrPosMom->Eval(p);
		nde=parDeMom->Eval(p);
		ntr=parTrMom->Eval(p);
		nhe3=parHe3Mom->Eval(p);
		nhe4=parHe4Mom->Eval(p);
	}
	else if (fCharge < 0) {
		nel=parElNegMom->Eval(p);
		nmu=parMuNegMom->Eval(p);
		npi=parPiNegMom->Eval(p);
		nka=parKaNegMom->Eval(p);
		npr=parPrNegMom->Eval(p);
	}
	else return (kFALSE);
	
	//
	// Params. for dE/dx sigma versus total momentum.
	// These are koef*<dE/dx> , koef=0.06
	// Deviations from Bethe-Bloch at low-p accounted by somewhat larger sigmas
	//
	
	Double_t emeanel=GetDedxElParam(p);
	Double_t emeanmu=GetDedxMuParam(p);
	Double_t emeanpi=GetDedxPiParam(p, eta);
	Double_t emeanpr=GetDedxPrParam(p, eta);
	Double_t emeanka=GetDedxKaParam(p, eta);
	Double_t emeande=GetDedxDeParam(p);
	Double_t emeantr=GetDedxTrParam(p);
	Double_t emeanhe3=GetDedxHe3Param(p);
	Double_t emeanhe4=GetDedxHe4Param(p);
	
	Double_t sigeel,sigemu,sigepi,sigepr,sigeka,sigede,sigetr,sigehe3,sigehe4;
	
	sigeel=fSigmaDedx_3;
	sigemu=fSigmaDedx_1;
	sigepi=fSigmaDedx_1;
	sigeka=fSigmaDedx_1;
	sigepr=fSigmaDedx_1;
	sigede=fSigmaDedx_1;
	sigetr=fSigmaDedx_1;
	sigehe3=fSigmaDedx_1;
	sigehe4=fSigmaDedx_1;
	
	if (p<0.15){
		sigemu=fSigmaDedx_2; sigepi=fSigmaDedx_2; sigepr=fSigmaDedx_2; sigeka=fSigmaDedx_2; sigede=fSigmaDedx_2;
	}
	if (p<0.45){
		sigede=fSigmaDedx_2; sigetr=fSigmaDedx_2; sigehe3=fSigmaDedx_2; sigehe4=fSigmaDedx_2;
	}
	
	Double_t p_calc = 3.5; // Above p=3.5 GeV/c param. for sig_M2 is bad!
	if (p < p_calc) p_calc = p;
	
	Double_t sigmel=parElM2->Eval(p_calc), sigmmu=parMuM2->Eval(p_calc);
	Double_t sigmpi;
	if(p<=1.4) sigmpi=parPiLowPM2->Eval(p_calc); else sigmpi=parPiHighPM2->Eval(p_calc);
	Double_t sigmka=parKaM2->Eval(p_calc);
	Double_t sigmpr;
	if(p<=1.4) sigmpr=parPrLowPM2->Eval(p_calc); else sigmpr=parPrHighPM2->Eval(p_calc);
	Double_t sigmde=parDeM2->Eval(p_calc), sigmtr=parTrM2->Eval(p_calc);
	Double_t sigmhe3=parHe3M2->Eval(p_calc), sigmhe4=parHe4M2->Eval(p_calc);
	
	Double_t fel=0.,fmu=0.,fpi=0.,fka=0.,fpr=0.,fde=0.,ftr=0.,fhe3=0.,fhe4=0.;
	Double_t fsum=0., cut=4.0;
	
	if (kSigmaEloss*kSigmaTof > 0.1) cut=TMath::Sqrt(kSigmaEloss*kSigmaEloss+kSigmaTof*kSigmaTof);
	
	//
	// Set prob=0. for differences greater than n-sigmas
	// otherwise evaluation..
	//
	
	Double_t xx, yy, distance;
	
	xx = (dedx/emeanel-1.)/sigeel;
	yy = (m2-0.0007)/sigmel;
	distance = TMath::Sqrt(xx*xx+yy*yy);
	if (distance < cut){
		fgaus2->SetParameters(nel,0.002,sigmel,1.,sigeel);
		fel=fgaus2->Eval(m2,dedx/emeanel);
	} else fel=0.;
	
	fmu = GetCombProb(dedx, m2, cut, nmu, emeanmu, 0.011, sigemu, sigmmu);
	fpi = GetCombProb(dedx, m2, cut, npi, emeanpi, 0.019, sigepi, sigmpi);
	fka = GetCombProb(dedx, m2, cut, nka, emeanka, 0.24, sigeka, sigmka);
	fpr = GetCombProb(dedx, m2, cut, npr, emeanpr, 0.887, sigepr, sigmpr);
	fde = GetCombProb(dedx, m2, cut, nde, emeande, 3.54, sigede, sigmde);
	ftr = GetCombProb(dedx, m2, cut, ntr, emeantr, 7.87, sigede, sigmde);
	fhe3 = GetCombProb(dedx, m2, cut, nhe3, emeanhe3, 1.983, sigehe3, sigmhe3);
	fhe4 = GetCombProb(dedx, m2, cut, nhe4, emeanhe4, 3.51, sigehe4, sigmhe4);
	
	//
	// Normalization
	//
	
	fsum=fel+fmu+fpi+fka+fpr+fde+ftr+fhe3+fhe4;
	if (1.0e+8*fsum>0.){
		fProbEl = fel/fsum;
		fProbMu = fmu/fsum;
		fProbPi = fpi/fsum;
		fProbKa = fka/fsum;
		fProbPr = fpr/fsum;
		fProbDe = fde/fsum;
		fProbTr = ftr/fsum;
		fProbHe3 = fhe3/fsum;
		fProbHe4 = fhe4/fsum;
	} else {return kFALSE;};// outliers!
	
	return kTRUE;
}

Long_t MpdPid_AZ::GetMaxProb() {
	Long_t pdg=211;
	Double_t pcut=0.501;
	const Int_t nCodes=9;
	Long_t codes[nCodes]={211,2212,321,13,11,1000010020,1000010030,1000020030,1000020040};
	Double_t probs[nCodes]={fProbPi,fProbPr,fProbKa,fProbMu,fProbEl,fProbDe,fProbTr,fProbHe3,fProbHe4};
	
	if (fProbPi > pcut)       pdg=211;
	else if (fProbPr > pcut)  pdg=2212;
	else if (fProbKa > pcut)  pdg=321;
	else if (fProbMu > pcut)  pdg=13;
	else if (fProbEl > pcut)  pdg=11;
	else if (fProbDe > pcut)  pdg=1000010020;
	else if (fProbTr > pcut)  pdg=1000010030;
	else if (fProbHe3 > pcut)  pdg=1000020030;
	else if (fProbHe4 > pcut) pdg=1000020040;
	else{
		Long_t tmp;
		for (Int_t i=1;i<nCodes;i++){
			if (probs[0]<probs[i]){
				tmp=codes[0];
				codes[0]=codes[i];
				codes[i]=tmp;
				tmp=probs[0];
				probs[0]=probs[i];
				probs[i]=tmp;
				}
			}
		pdg=codes[0];
   }
   
   return (fCharge*pdg);   
}

ClassImp(MpdPid_AZ);
