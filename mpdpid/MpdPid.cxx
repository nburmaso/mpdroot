#include "MpdPid.h"

MpdPid::MpdPid() : TObject(){

      parElBB = 0;
      parPiBB = 0;
      parKaBB = 0;
      parPrBB = 0;
      parPrBBMerged = 0;
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
      fTracking = kTRUE;
}


MpdPid::MpdPid(Double_t sigmaTof, Double_t sigmaEloss, Double_t sqrts, Double_t koef, TString Generator, TString Tracking, TString NSigPart)
   : TObject(), fKoef(koef), kSigmaTof(sigmaTof), kSigmaEloss(sigmaEloss),  fCharge(1), fEnergy(sqrts) 
{
	Init(Generator, Tracking, NSigPart);
}

Double_t MpdPid::mergedPrBB(Double_t *x, Double_t *par)
{
	Double_t ans = -999.;
	Double_t par1[5] = { par[0], par[1], par[2], par[3], par[4] };
    Double_t par2[5] = { par[5], par[6], par[7], par[8], par[9] };
	if (parPrBB) {
		if (x[0] < 0.211841) ans = parPrBB->EvalPar(x,par1);
		else ans = parPrBB->EvalPar(x,par2);
	}
	return ans;
}

Double_t MpdPid::GetDedxWidthValue(Double_t p, Int_t specie)
{
	/// Species: 1 - pions, 2 - kaons, 3 - protons, 4 - electrons, 5 - muons, 6 - deuterons, 7 - tritons, 8 - He3, 9 - He4
	Double_t WidthValue = 0.;
	if (specie == 1)
	{
		if (p < piSigmaLowP->GetXmax()) WidthValue = piSigmaLowP->Eval(p);
		if ((p >= piSigmaMid1P->GetXmin()) && (p < piSigmaMid1P->GetXmax())) WidthValue = piSigmaMid1P->Eval(p);
		if ((p >= piSigmaMid2P->GetXmin()) && (p < piSigmaMid2P->GetXmax())) WidthValue = piSigmaMid2P->Eval(p);
		if ((p >= piSigmaHighP->GetXmin()) && (p <= piSigmaHighP->GetXmax())) WidthValue = piSigmaHighP->Eval(p);
	}
	if (specie == 2)
	{
		if (p < kaSigmaLowP->GetXmax()) WidthValue = kaSigmaLowP->Eval(p);
		if ((p >= kaSigmaHighP->GetXmin()) && (p <= kaSigmaHighP->GetXmax())) WidthValue = kaSigmaHighP->Eval(p);
	}
	if (specie == 3)
	{
		if (p < prSigmaLowP->GetXmax()) WidthValue = prSigmaLowP->Eval(p);
		if ((p >= prSigmaHighP->GetXmin()) && (p <= prSigmaHighP->GetXmax())) WidthValue = prSigmaHighP->Eval(p);
	}
	if (specie == 4)
	{
		if (p < elSigmaLowP->GetXmax()) WidthValue = elSigmaLowP->Eval(p);
		if ((p >= elSigmaMidP->GetXmin()) && (p <= elSigmaMidP->GetXmax())) WidthValue = elSigmaMidP->Eval(p);
		if ((p >= elSigmaHighP->GetXmin()) && (p <= elSigmaHighP->GetXmax())) WidthValue = elSigmaHighP->Eval(p);
	}
	if (specie == 5)
	{
		if (p < muSigmaLowP->GetXmax()) WidthValue = muSigmaLowP->Eval(p);
		if ((p >= muSigmaMidP->GetXmin()) && (p <= muSigmaMidP->GetXmax())) WidthValue = muSigmaMidP->Eval(p);
		if ((p >= muSigmaHighP->GetXmin()) && (p <= muSigmaHighP->GetXmax())) WidthValue = muSigmaHighP->Eval(p);
	}
	if (specie == 6)
	{
		if ((p >= deSigmaLowP->GetXmin()) && (p <= deSigmaLowP->GetXmax())) WidthValue = deSigmaLowP->Eval(p);
		if ((p >= deSigmaHighP->GetXmin()) && (p <= deSigmaHighP->GetXmax())) WidthValue = deSigmaHighP->Eval(p);
	}
	if (specie == 7)
	{
		WidthValue = 0.05;
	}
	if (specie == 8)
	{
		WidthValue = 0.05;
	}
	if (specie == 9)
	{
		WidthValue = 0.05;
	}
	
	return WidthValue;
}

Double_t MpdPid::GetTailValue(Double_t p, Int_t specie)
{
	/// Species: 1 - pions, 2 - kaons, 3 - protons, 4 - electrons, 5 - muons, 6 - deuterons, 7 - tritons, 8 - He3, 9 - He4
	Double_t TailValue = 0.;
	
	if (specie == 1)
	{
		if (p < fAsymmetryPiLowP->GetXmax()) TailValue = fAsymmetryPiLowP->Eval(p);
		if ( (p >= fAsymmetryPiMidP->GetXmin()) && (p < fAsymmetryPiMidP->GetXmax()) ) TailValue = fAsymmetryPiMidP->Eval(p);
		if ( (p >= fAsymmetryPiHighP->GetXmin()) && (p <= fAsymmetryPiHighP->GetXmax()) ) TailValue = fAsymmetryPiHighP->Eval(p);
	}
	if (specie == 2)
	{
		if (p < fAsymmetryKaLowP->GetXmax()) TailValue = fAsymmetryKaLowP->Eval(p);
		if ( (p >= fAsymmetryKaMidP->GetXmin()) && (p < fAsymmetryKaMidP->GetXmax()) ) TailValue = fAsymmetryKaMidP->Eval(p);
		if ( (p >= fAsymmetryKaHighP->GetXmin()) && (p <= fAsymmetryKaHighP->GetXmax()) ) TailValue = fAsymmetryKaHighP->Eval(p);
	}
	if (specie == 3)
	{
		if (p < fAsymmetryPrLowP->GetXmax()) TailValue = fAsymmetryPrLowP->Eval(p);
		if ( (p >= fAsymmetryPrMidP->GetXmin()) && (p < fAsymmetryPrMidP->GetXmax()) ) TailValue = fAsymmetryPrMidP->Eval(p);
		if ( (p >= fAsymmetryPrHighP->GetXmin()) && (p <= fAsymmetryPrHighP->GetXmax()) ) TailValue = fAsymmetryPrHighP->Eval(p);
	}
	if (specie == 4)
	{
		if (p < fAsymmetryElLowP->GetXmax()) TailValue = fAsymmetryElLowP->Eval(p);
		if ( (p >= fAsymmetryElMidP->GetXmin()) && (p < fAsymmetryElMidP->GetXmax()) ) TailValue = fAsymmetryElMidP->Eval(p);
		if ( (p >= fAsymmetryElHighP->GetXmin()) && (p <= fAsymmetryElHighP->GetXmax()) ) TailValue = fAsymmetryElHighP->Eval(p);
	}
	if (specie == 5)
	{
		if (p < fAsymmetryMuLowP->GetXmax()) TailValue = fAsymmetryMuLowP->Eval(p);
		if ( (p >= fAsymmetryMuMidP->GetXmin()) && (p < fAsymmetryMuMidP->GetXmax()) ) TailValue = fAsymmetryMuMidP->Eval(p);
		if ( (p >= fAsymmetryMuHighP->GetXmin()) && (p <= fAsymmetryMuHighP->GetXmax()) ) TailValue = fAsymmetryMuHighP->Eval(p);
	}
	if (specie == 6)
	{
		if (p < fAsymmetryDeLowP->GetXmin()) TailValue = 1.5;
		if ( (p >= fAsymmetryDeLowP->GetXmin()) && (p < fAsymmetryDeLowP->GetXmax()) ) TailValue = fAsymmetryDeLowP->Eval(p);
		if ( (p >= fAsymmetryDeHighP->GetXmin()) && (p <= fAsymmetryDeHighP->GetXmax()) ) TailValue = fAsymmetryDeHighP->Eval(p);
	}
	if (specie == 7)
	{
		if (p < fAsymmetryTrLowP->GetXmin()) TailValue = 1.;
		if ( (p >= fAsymmetryTrLowP->GetXmin()) && (p < fAsymmetryTrLowP->GetXmax()) ) TailValue = fAsymmetryTrLowP->Eval(p);
		if ( (p >= fAsymmetryTrMidP->GetXmin()) && (p < fAsymmetryTrMidP->GetXmax()) ) TailValue = fAsymmetryTrMidP->Eval(p);
		if ( (p >= fAsymmetryTrHighP->GetXmin()) && (p <= fAsymmetryTrHighP->GetXmax()) ) TailValue = fAsymmetryTrHighP->Eval(p);
	}
	if (specie == 8)
	{
		if (p < fAsymmetryHe3LowP->GetXmin()) TailValue = 1.;
		if ( (p >= fAsymmetryHe3LowP->GetXmin()) && (p < fAsymmetryHe3LowP->GetXmax()) ) TailValue = fAsymmetryHe3LowP->Eval(p);
		if ( (p >= fAsymmetryHe3MidP->GetXmin()) && (p < fAsymmetryHe3MidP->GetXmax()) ) TailValue = fAsymmetryHe3MidP->Eval(p);
		if ( (p >= fAsymmetryHe3HighP->GetXmin()) && (p <= fAsymmetryHe3HighP->GetXmax()) ) TailValue = fAsymmetryHe3HighP->Eval(p);
	}
	if (specie == 9)
	{
		if (p < fAsymmetryHe4LowP->GetXmax()) TailValue = fAsymmetryHe4LowP->Eval(p);
		if ( (p >= fAsymmetryHe4MidP->GetXmin()) && (p < fAsymmetryHe4MidP->GetXmax()) ) TailValue = fAsymmetryHe4MidP->Eval(p);
		if ( (p >= fAsymmetryHe4HighP->GetXmin()) && (p <= fAsymmetryHe4HighP->GetXmax()) ) TailValue = fAsymmetryHe4HighP->Eval(p);
		if (p > fAsymmetryHe4HighP->GetXmax()) TailValue = -0.1;
	}
	
	return TailValue;
}

Double_t MpdPid::GetDedxProb_asym(Double_t cut, Double_t p, Double_t dedx, Double_t n, Double_t emean, Double_t sige, Int_t specie)
{
	Double_t fProb = 0.;
	Double_t delta = GetTailValue(p, specie);
	Double_t SideOfAsymmetry = delta * (dedx - emean);
	if (SideOfAsymmetry >= 0.) cut *= 1. + TMath::Abs(delta);
	if (TMath::Abs((dedx/emean-1.)/sige) < cut)
	{
		fasymgaus->SetParameters(n, 1., sige, delta);
		fProb = fasymgaus->Eval(dedx/emean);
	}
	
	dEdXSigmasArray[specie-1] = TMath::Abs((dedx/emean-1.)/sige);
	if (SideOfAsymmetry >= 0.) dEdXSigmasArray[specie-1] /= 1. + TMath::Abs(delta);
	m2SigmasArray[specie-1] = -1.0;
	return fProb;
}

Double_t MpdPid::GetCombProb_asym(Double_t cut_dedx, Double_t cut_m2, Double_t p, Double_t dedx, Double_t m2, Double_t n, Double_t emean, Double_t mmean, Double_t sige, Double_t sigm, Int_t specie)
{
	Double_t fProb = 0., xx, yy;
	xx = (dedx/emean-1.)/sige;
    yy = (m2-mmean)/sigm;
	Double_t delta = GetTailValue(p, specie);
	Double_t SideOfAsymmetry = delta * (dedx - emean);
	if (SideOfAsymmetry >= 0.) xx /= 1. + TMath::Abs(delta);
	if ( (TMath::Abs(xx) < cut_dedx) && (TMath::Abs(yy) < cut_m2) )
	{
		fasymgaus2->SetParameters(n, 1., sige, delta, mmean, sigm);
		fProb = fasymgaus2->Eval(dedx/emean, m2);
	}
	dEdXSigmasArray[specie-1] = TMath::Abs(xx);
	m2SigmasArray[specie-1] = TMath::Abs(yy);
	return fProb;
}

/// Sum of two exponents in total momentum (pions and electrons)
Double_t MpdPid::MomPi(Double_t *x, Double_t *par)
{
	Double_t p = x[0], xx, x1, x2;
	xx=sqrt(p*p+par[4]*par[4])-par[4];
	x1=(1+par[1])/(par[2]*(par[4]+par[2]))/exp(xx/par[2]);
	x2=par[1]/(par[3]*(par[4]+par[3]))/exp(xx/par[3]);
	return (par[0]*p*(x1+x2));    
}

/// Difference of 2 exponents in total momentum (All specie, except pi's and e+/-)
Double_t MpdPid::MomPr(Double_t *x, Double_t *par)
{
	Double_t p = x[0], xx, x1, x2;
	xx=sqrt(p*p+par[4]*par[4])-par[4];
	x1=(1+par[1])/(par[2]*(par[4]+par[2]))/exp(xx/par[2]);
	x2=par[1]/(par[3]*(par[4]+par[3]))/exp(xx/par[3]);
	return (par[0]*p*(x1-x2));
}

Double_t MpdPid::AsymGaus(Double_t *x, Double_t *par)
{
	Double_t peak = par[1];
	if (x[0] < peak)
	return par[0]*TMath::Exp( -0.5 * TMath::Power( ( (x[0] - par[1]) / par[2] ), 2 ));
	else
	return par[0]*TMath::Exp( -0.5 * TMath::Power( ( (x[0] - par[1]) / ((1. + par[3]) * par[2]) ), 2 ));
}

Double_t MpdPid::AsymGaus2(Double_t *x, Double_t *par)
{
	Double_t peak = par[1];
	if (x[0] < peak)
	return par[0]*TMath::Exp( -0.5 * TMath::Power( ( (x[0] - par[1]) / par[2] ), 2 )) * TMath::Exp( -0.5 * TMath::Power( ( (x[1] - par[4]) / par[5] ), 2 ));
	else
	return par[0]*TMath::Exp( -0.5 * TMath::Power( ( (x[0] - par[1]) / ((1. + par[3]) * par[2]) ), 2 )) * TMath::Exp( -0.5 * TMath::Power( ( (x[1] - par[4]) / par[5] ), 2 ));
}

void MpdPid::Init(TString Generator, TString Tracking, TString NSigPart)
{
	cout<<"MpdPid::Init().."<<endl;
	
	fSigmaDedx_2 = 0.11; fSigmaDedx_3 = 0.08;
	Double_t PMIN=0., PMAX=3.0;
	Double_t dedxParam;
	
	/// Setting default ratio ('rat' is pos./neg.)
	prrat=102.11;
	if (fEnergy < 7.0) prrat=1000.;
	
	///
	/// Bethe-Bloch versus p (MC with STRA=0!)
	///
	
	parElBB=new TF1("parElBB","[0]/x+[1]",PMIN,PMAX);
	parMuBB = new TF1("parMuBB","[0]/pow(x/sqrt(x*x+0.011),[3])*([1]-pow(x/sqrt(x*x+0.011),[3])-log([2]+pow(1./(x/0.1057),[4])) )",PMIN,PMAX);
	parPiBB = new TF1("parPiBB","[0]/pow(x/sqrt(x*x+0.01949),[3])*([1]-pow(x/sqrt(x*x+0.01949),[3])-log([2]+pow(1./(x/0.1396),[4])) )",PMIN,PMAX);
	parKaBB = new TF1("parKaBB","[0]/pow(x/sqrt(x*x+0.2437),[3])*([1]-pow(x/sqrt(x*x+0.2437),[3])-log([2]+pow(1./(x/0.4937),[4])) )",PMIN,PMAX);
	parPrBB = new TF1("parPrBB","[0]/pow(x/sqrt(x*x+0.88),[3])*([1]-pow(x/sqrt(x*x+0.88),[3])-log([2]+pow(1./(x/0.9383),[4])) )",PMIN,PMAX);
	parPrBBMerged = new TF1("parPrBBMerged",this,&MpdPid::mergedPrBB,PMIN,PMAX,10,"MpdPid","mergedPrBB");
	parDePol1 = new TF1("parDePol1", "pol1(0)", 0., 0.225); parDePol2 = new TF1("parDePol2", "pol1(0)", 0.225, 0.375);
	parDeBB = new TF1("parDeBB","[0]/pow(x/sqrt(x*x+3.52),[3])*([1]-pow(x/sqrt(x*x+3.52),[3])-log([2]+pow(1./(x/1.876),[4])) )",PMIN,PMAX);
	parTrPol1 = new TF1("parTrPol1", "pol1(0)", 0., 0.3); parTrPol2 = new TF1("parTrPol2", "pol1(0)", 0.3, 0.525);
	parTrBB = new TF1("parTrBB","[0]/pow(x/sqrt(x*x+7.89),[3])*([1]-pow(x/sqrt(x*x+7.89),[3])-log([2]+pow(1./(x/2.81),[4])) )",PMIN,PMAX);
	/// Double charged have p_reco= p_MC/2
	parHe3Pol1 = new TF1("parHe3Pol1", "pol1(0)", 0., 0.24); parHe3Pol2 = new TF1("parHe3Pol2", "pol1(0)", 0.24, 0.325); parHe3Pol3 = new TF1("parHe3Pol3", "pol1(0)", 0.325, 0.55);
	parHe3BB = new TF1("parHe3BB","[0]*((1+(x/1.4047)**2)/pow(x/1.4047,[3])*([1]+[2]*log(1+pow(x/1.4047,2)))-1.)",PMIN,PMAX);
	parHe4Pol1 = new TF1("parHe4Pol1", "pol1(0)", 0., 0.32); parHe4Pol2 = new TF1("parHe4Pol2", "pol1(0)", 0.32, 0.5);
	parHe4BB = new TF1("parHe4BB","[0]*((1+(x/1.863)**2)/pow(x/1.863,[3])*([1]+[2]*log(1+pow(x/1.863,2)))-1.)",PMIN,PMAX);
	
	///
	/// Width of mass-squared versus total p
	///
	
	parElM2=new TF1("parElM2","pol2(0)",PMIN,PMAX);  
	parMuM2=new TF1("parMuM2","pol2(0)",PMIN,PMAX);  
	parPiLowPM2=new TF1("parPiLowPM2","pol2(0)",PMIN,PMAX);  
	parPiHighPM2=new TF1("parPiHighPM2","pol2(0)",PMIN,PMAX);  
	parKaM2=new TF1("parKaM2","pol2(0)",PMIN,PMAX);
	parPrLowPM2=new TF1("parPrLowPM2","pol3(0)",PMIN,PMAX);  
	parPrHighPM2=new TF1("parPrHighPM2","pol2(0)",PMIN,PMAX);  
	parDeM2=new TF1("parDeM2","pol3(0)",PMIN,PMAX);    
	parTrM2=new TF1("parTrM2","pol3(0)",PMIN,PMAX);  
	parHe3M2=new TF1("parHe3M2","pol3(0)",PMIN,PMAX);  
	parHe4M2=new TF1("parHe4M2","pol3(0)",PMIN,PMAX);  
	
	/// DEFINE TRACKING
	if(Tracking=="HP")
	{
		fSigmaDedx_1 = 0.05;
		fTracking = kFALSE;
		
		/// electrons
		dedxParam = fKoef*(-8.27289e-09); parElBB->SetParameter(0, dedxParam);
		dedxParam = fKoef*(2.10438e-06); parElBB->SetParameter(1, dedxParam);
		/// muons
		dedxParam = fKoef*2.56183e-06;
		parMuBB->SetParameters(dedxParam,2.7034,2.52734,0.854563,0.330576);
		/// pions
		dedxParam = fKoef*(-1.19342e-07);
		parPiBB->SetParameters(dedxParam,-7.83114,8.17749,1.85775,-1.80695);
		/// kaons
		dedxParam = fKoef*6.50167e-07;
		parKaBB->SetParameters(dedxParam,1.01718,-0.795357,1.80916,0.0707667);
		/// protons
		dedxParam = fKoef*(4.40008e-07);
		parPrBBMerged->SetParameters(dedxParam,2.97563,-0.192657,2.16118,0.61451,dedxParam,2.97563,-0.192657,2.16118,0.61451);
		/// deuterons
		dedxParam = fKoef*3.27e-07;
		parDeBB->SetParameters(dedxParam,3.74,-0.23,2.32,0.987);
		/// tritons
		dedxParam = fKoef*2.59e-07;
		parTrBB->SetParameters(dedxParam,5.06,0.0001,2.2,1.056);
		/// He3
		dedxParam = fKoef*2.86201e-06;
		parHe3BB->SetParameters(dedxParam,2.10168e+00,2.74807e-01,1.86774e+00);
		/// He4
		dedxParam = fKoef*2.96e-06;
		parHe4BB->SetParameters(dedxParam,2.085,0.256,1.85);
		
		/// m2
		parElM2->SetParameters(0.00102552,-0.000243946,0.0307395);
		parMuM2->SetParameters(0.00155957,-0.000984273,0.0306857);
		parPiLowPM2->SetParameters(0.00259115, -0.00251021, 0.0287287);
		parPiHighPM2->SetParameters(-0.0393955, 0.0586315, 0.00516675);
		parKaM2->SetParameters(0.00144014, 0.0183536, 0.0161613);
		parPrLowPM2->SetParameters(0.0777042, -0.123828, 0.139278, -0.0338542);
		parPrHighPM2->SetParameters(0.0244298, 0.018534, 0.0174998);
		parDeM2->SetParameters(0.535691,-0.529882,0.293807,-0.0428139);
		parTrM2->SetParameters(0.422,0.3,-0.202,0.0524);
		parHe3M2->SetParameters(0.17,-0.0,0.0,-0.00);
		parHe4M2->SetParameters(0.3,-0.0,0.0,-0.00);
		
		/// dE/dx width
		elSigmaLowP = new TF1("elSigmaLowP", "pol1(0)", 0., 0.55); elSigmaLowP->SetParameters(0.0484732, -0.002846727);
		elSigmaMidP = new TF1("elSigmaMidP", "pol1(0)", 0.55, 2.0); elSigmaMidP->SetParameters(0.0470012, -0.000170422);
		elSigmaHighP = new TF1("elSigmaHighP", "pol1(0)", 2.0, 3.0); elSigmaHighP->SetParameters(0.0477092, -0.000503717);
		muSigmaLowP = new TF1("muSigmaLowP", "pol1(0)", 0., 0.3); muSigmaLowP->SetParameters(0.0668409, -0.0657125);
		muSigmaMidP = new TF1("muSigmaMidP", "pol1(0)", 0.3, 1.05); muSigmaMidP->SetParameters(0.0458461, 0.00145295);
		muSigmaHighP = new TF1("muSigmaHighP", "pol1(0)", 1.05, 3.0); muSigmaHighP->SetParameters(0.0450895, 0.00118004);
		piSigmaLowP = new TF1("piSigmaLowP", "pol1(0)", 0., 0.35); piSigmaLowP->SetParameters(0.0787807, -0.100463);
		piSigmaMid1P = new TF1("piSigmaMid1P", "pol1(0)", 0.35, 1.5); piSigmaMid1P->SetParameters(0.0487635, 0.000857375);
		piSigmaMid2P = new TF1("piSigmaMid2P", "pol1(0)", 0.35, 1.5); piSigmaMid2P->SetParameters(0.0487635, 0.000857375);
		piSigmaHighP = new TF1("piSigmaHighP", "pol1(0)", 1.5, 3.0); piSigmaHighP->SetParameters(0.0443209, 0.00524245);
		prSigmaLowP = new TF1("prSigmaLowP", "pol1(0)", 0., 0.2); prSigmaLowP->SetParameters(0.068647, -0.0426204);
		prSigmaHighP = new TF1("prSigmaHighP", "pol1(0)", 0.2, 3.0); prSigmaHighP->SetParameters(0.0476193, -0.000557916);
		kaSigmaLowP = new TF1("kaSigmaLowP", "pol1(0)", 0., 0.2); kaSigmaLowP->SetParameters(0.149889, -0.465214);
		kaSigmaHighP = new TF1("kaSigmaHighP", "pol1(0)", 0.2, 3.0); kaSigmaHighP->SetParameters(0.0486608, -0.000571203);
		deSigmaLowP = new TF1("deSigmaLowP", "pol1(0)", 0., 0.8); deSigmaLowP->SetParameters(0.05, 0.);
		deSigmaHighP = new TF1("deSigmaHighP", "pol1(0)", 0.8, 3.0); deSigmaHighP->SetParameters(0.05, 0.);
		trSigmaLowP = new TF1("trSigmaLowP", "pol1(0)", 0., 0.625); trSigmaLowP->SetParameters(0.05, 0.);
		trSigmaMidP = new TF1("trSigmaMidP", "pol1(0)", 0.625, 1.5); trSigmaMidP->SetParameters(0.05, 0.);
		trSigmaHighP = new TF1("trSigmaHighP", "pol1(0)", 1.5, 3.0); trSigmaHighP->SetParameters(0.05, 0.);
		he3SigmaLowP = new TF1("he3SigmaLowP", "pol1(0)", 0., 0.675); he3SigmaLowP->SetParameters(0.05, 0.);
		he3SigmaMidP = new TF1("he3SigmaMidP", "pol1(0)", 0.675, 1.475); he3SigmaMidP->SetParameters(0.05, 0.);
		he3SigmaHighP = new TF1("he3SigmaHighP", "pol1(0)", 1.475, 3.0); he3SigmaHighP->SetParameters(0.05, 0.);
		he4SigmaLowP = new TF1("he4SigmaLowP", "pol1(0)", 0., 0.475); he4SigmaLowP->SetParameters(0.05, 0.);
		he4SigmaMid1P = new TF1("he4SigmaMid1P", "pol1(0)", 0.475, 0.7); he4SigmaMid1P->SetParameters(0.05, 0.);
		he4SigmaMid2P = new TF1("he4SigmaMid2P", "pol1(0)", 0.7, 0.925); he4SigmaMid2P->SetParameters(0.05, 0.);
		he4SigmaHighP = new TF1("he4SigmaHighP", "pol1(0)", 0.925, 3.0); he4SigmaHighP->SetParameters(0.05, 0.);
		
		/// Asymmetry
		fAsymmetryElLowP = new TF1("fAsymmetryElLowP", "pol1(0)", 0., 0.5); fAsymmetryElLowP->SetParameters(0.456871, -0.0424708);
		fAsymmetryElMidP = new TF1("fAsymmetryElMidP", "pol1(0)", 0.5, 0.85); fAsymmetryElMidP->SetParameters(0.4418036, -0.0131662);
		fAsymmetryElHighP = new TF1("fAsymmetryElHighP", "pol1(0)", 0.85, 3.0); fAsymmetryElHighP->SetParameters(0.440012, -0.0113746);
		fAsymmetryMuLowP = new TF1("fAsymmetryMuLowP", "pol1(0)", 0., 0.175); fAsymmetryMuLowP->SetParameters(12.138, -66.6292);
		fAsymmetryMuMidP = new TF1("fAsymmetryMuMidP", "pol1(0)", 0.175, 0.65); fAsymmetryMuMidP->SetParameters(0.31412, 0.158834);
		fAsymmetryMuHighP = new TF1("fAsymmetryMuHighP", "pol1(0)", 0.65, 3.0); fAsymmetryMuHighP->SetParameters(0.409629, -0.0234975);
		fAsymmetryPiLowP = new TF1("fAsymmetryPiLowP", "pol1(0)", 0., 0.18); fAsymmetryPiLowP->SetParameters(11.3953, -58.1321);
		fAsymmetryPiMidP = new TF1("fAsymmetryPiMidP", "pol1(0)", 0.18, 0.9); fAsymmetryPiMidP->SetParameters(0.412467, -0.0289088);
		fAsymmetryPiHighP = new TF1("fAsymmetryPiHighP", "pol1(0)", 0.9, 3.); fAsymmetryPiHighP->SetParameters(0.408242, -0.0520603);
		fAsymmetryPrLowP = new TF1("fAsymmetryPrLowP", "pol1(0)", 0., 0.3); fAsymmetryPrLowP->SetParameters(6.40228, -19.5847);
		fAsymmetryPrMidP = new TF1("fAsymmetryPrMidP", "pol1(0)", 0.3, 1.0); fAsymmetryPrMidP->SetParameters(0.193311, 0.179621);
		fAsymmetryPrHighP = new TF1("fAsymmetryPrHighP", "pol1(0)", 1.0, 3.); fAsymmetryPrHighP->SetParameters(0.21947, 0.0936557);
		fAsymmetryKaLowP = new TF1("fAsymmetryKaLowP", "pol1(0)", 0., 0.45); fAsymmetryKaLowP->SetParameters(3.21698, -6.37965);
		fAsymmetryKaMidP = new TF1("fAsymmetryKaMidP", "pol1(0)", 0.45, 1.05); fAsymmetryKaMidP->SetParameters(0.237053, 0.143231);
		fAsymmetryKaHighP = new TF1("fAsymmetryKaHighP", "pol1(0)", 1.05, 3.); fAsymmetryKaHighP->SetParameters(0.252239, 0.0558685);
		fAsymmetryDeLowP = new TF1("fAsymmetryDeLowP", "pol1(0)", 0., 0.585); fAsymmetryDeLowP->SetParameters(0., 0.);
		fAsymmetryDeHighP = new TF1("fAsymmetryDeHighP", "pol1(0)", 0.585, 3.); fAsymmetryDeHighP->SetParameters(0., 0.);
		fAsymmetryTrLowP = new TF1("fAsymmetryTrLowP", "pol1(0)", 0., 0.575); fAsymmetryTrLowP->SetParameters(0., 0.);
		fAsymmetryTrMidP = new TF1("fAsymmetryTrMidP", "pol1(0)", 0.575, 0.84); fAsymmetryTrMidP->SetParameters(0., 0.);
		fAsymmetryTrHighP = new TF1("fAsymmetryTrHighP", "pol1(0)", 0.84, 3.); fAsymmetryTrHighP->SetParameters(0., 0.);
		fAsymmetryHe3LowP = new TF1("fAsymmetryHe3LowP", "pol1(0)", 0., 0.685); fAsymmetryHe3LowP->SetParameters(0., 0.);
		fAsymmetryHe3MidP = new TF1("fAsymmetryHe3MidP", "pol1(0)", 0.685, 0.99); fAsymmetryHe3MidP->SetParameters(0., 0.);
		fAsymmetryHe3HighP = new TF1("fAsymmetryHe3HighP", "pol1(0)", 0.99, 3.); fAsymmetryHe3HighP->SetParameters(0., 0.);
		fAsymmetryHe4LowP = new TF1("fAsymmetryHe4LowP", "pol1(0)", 0., 0.575); fAsymmetryHe4LowP->SetParameters(0., 0.);
		fAsymmetryHe4MidP = new TF1("fAsymmetryHe4MidP", "pol1(0)", 0.575, 0.875); fAsymmetryHe4MidP->SetParameters(0., 0.);
		fAsymmetryHe4HighP = new TF1("fAsymmetryHe4HighP", "pol1(0)", 0.875, 3.); fAsymmetryHe4HighP->SetParameters(0., 0.);
	}
	else /// Tracking == "CF" is default
	{
		if (Tracking != "CF") cout << "ERROR! Unknown tracking method! Switch to default (\"CF\")." << endl;
		
		fSigmaDedx_1 = 0.07;
		fTracking = kTRUE;
		
		/// electrons
		dedxParam = fKoef*(-65.7432); parElBB->SetParameter(0, dedxParam);
		dedxParam = fKoef*4007.68; parElBB->SetParameter(1, dedxParam);
		/// muons
		dedxParam = fKoef*(-54.6053);
		parMuBB->SetParameters(dedxParam,-41.38,109.594,1.30576,-4.66578);
		/// pions
		dedxParam = fKoef*(-371.355);
		parPiBB->SetParameters(dedxParam,-4.09623,3.29922,1.90921,-1.18982);
		/// kaons
		dedxParam = fKoef*(-640.087);
		parKaBB->SetParameters(dedxParam,-4.04746,0.468871,1.1251,3.80837);
		/// protons
		parPrBBMerged->SetParameters(fKoef*(-5906.51),0.227931,-0.102815,1.26866,0.707981,fKoef*(-14146.3),3.16193,10.5214,-0.262496,3.1826);
		/// deuterons
		dedxParam = fKoef*(112.e+04); parDePol1->SetParameter(0, dedxParam);
		dedxParam = fKoef*(-4466.666e+03); parDePol1->SetParameter(1, dedxParam);
		dedxParam = fKoef*(215507.); parDePol2->SetParameter(0, dedxParam);
		dedxParam = fKoef*(-474223.); parDePol2->SetParameter(1, dedxParam);
		dedxParam = fKoef*(-3591.57);
		parDeBB->SetParameters(dedxParam,0.656424,1.61966,0.585121,2.70803);
		/// tritons
		dedxParam = fKoef*(1.094542e+06); parTrPol1->SetParameter(0, dedxParam);
		dedxParam = fKoef*(-1.81963e+06); parTrPol1->SetParameter(1, dedxParam);
		dedxParam = fKoef*(226703.); parTrPol2->SetParameter(0, dedxParam);
		dedxParam = fKoef*(-362129.); parTrPol2->SetParameter(1, dedxParam);
		dedxParam = fKoef*(-6467.59);
		parTrBB->SetParameters(dedxParam,1.83428,3.94612,0.0819389,3.2898);
		/// He3
		dedxParam = fKoef*(3.09e+06); parHe3Pol1->SetParameter(0, dedxParam);
		dedxParam = fKoef*(-10.833333e+06); parHe3Pol1->SetParameter(1, dedxParam);
		dedxParam = fKoef*(1.403646e+06); parHe3Pol2->SetParameter(0, dedxParam);
		dedxParam = fKoef*(-3.806859e+06); parHe3Pol2->SetParameter(1, dedxParam);
		dedxParam = fKoef*(333495.); parHe3Pol3->SetParameter(0, dedxParam);
		dedxParam = fKoef*(-514085.); parHe3Pol3->SetParameter(1, dedxParam);
		dedxParam = fKoef*(-19120.8);
		parHe3BB->SetParameters(dedxParam,-0.183431,0.323092,2.41984);
		/// He4
		dedxParam = fKoef*(3.243636e+06); parHe4Pol1->SetParameter(0, dedxParam);
		dedxParam = fKoef*(-8.636364e+06); parHe4Pol1->SetParameter(1, dedxParam);
		dedxParam = fKoef*(1.100082e+06); parHe4Pol2->SetParameter(0, dedxParam);
		dedxParam = fKoef*(-1.937756e+06); parHe4Pol2->SetParameter(1, dedxParam);
		dedxParam = fKoef*(-16150.2);
		parHe4BB->SetParameters(dedxParam,-0.0645649,-0.0213786,3.61265);
		
		/// m2
		parElM2->SetParameters(0.001227,-0.000973509,0.0314155);
		parMuM2->SetParameters(0.00166279,-0.00131341,0.0311028);
		parPiLowPM2->SetParameters(0.00284467, -0.00307647, 0.0303474);
		parPiHighPM2->SetParameters(-0.0364446, 0.0569689, 0.00652391);
		parKaM2->SetParameters(-1.25162e-05, 0.0206642, 0.0187346);
		parPrLowPM2->SetParameters(0.0752714, -0.115864, 0.139697, -0.033123);
		parPrHighPM2->SetParameters(0.0130241, 0.0302837, 0.0207017);
		parDeM2->SetParameters(0.417362, -0.399356, 0.24154, -0.0339673);
		parTrM2->SetParameters(1.03599,-0.663541,0.199024,0.);
		parHe3M2->SetParameters(0.13,-0.0,0.0,-0.00);
		parHe4M2->SetParameters(0.24,-0.0,0.0,-0.00);
		
		/// dE/dx width
		elSigmaLowP = new TF1("elSigmaLowP", "pol1(0)", 0., 0.55); elSigmaLowP->SetParameters(0.0747217, -0.0308101);
		elSigmaMidP = new TF1("elSigmaMidP", "pol1(0)", 0.55, 2.0); elSigmaMidP->SetParameters(0.0653074, -0.00546004);
		elSigmaHighP = new TF1("elSigmaHighP", "pol1(0)", 2.0, 3.0); elSigmaHighP->SetParameters(0.0572145, -0.00104922);
		muSigmaLowP = new TF1("muSigmaLowP", "pol1(0)", 0., 0.3); muSigmaLowP->SetParameters(0.115611, -0.179933);
		muSigmaMidP = new TF1("muSigmaMidP", "pol1(0)", 0.3, 1.05); muSigmaMidP->SetParameters(0.0583018, 0.00140717);
		muSigmaHighP = new TF1("muSigmaHighP", "pol1(0)", 1.05, 3.0); muSigmaHighP->SetParameters(0.0570477, -8.02801e-05);
		piSigmaLowP = new TF1("piSigmaLowP", "pol1(0)", 0., 0.32); piSigmaLowP->SetParameters(0.103709,-0.142807);
		piSigmaMid1P = new TF1("piSigmaMid1P", "pol1(0)", 0.32, 0.81); piSigmaMid1P->SetParameters(0.0572968,0.00228388);
		piSigmaMid2P = new TF1("piSigmaMid2P", "pol1(0)", 0.81, 1.46); piSigmaMid2P->SetParameters(0.0614121,-0.00279866);
		piSigmaHighP = new TF1("piSigmaHighP", "pol1(0)", 1.46, 3.0); piSigmaHighP->SetParameters(0.0541414,0.00216918);
		prSigmaLowP = new TF1("prSigmaLowP", "pol1(0)", 0., 0.5); prSigmaLowP->SetParameters(0.134444, -0.148889);
		prSigmaHighP = new TF1("prSigmaHighP", "pol1(0)", 0.5, 3.0); prSigmaHighP->SetParameters(0.0623534, -0.00287991);
		kaSigmaLowP = new TF1("kaSigmaLowP", "pol1(0)", 0., 0.2); kaSigmaLowP->SetParameters(0.120409, -0.289855);
		kaSigmaHighP = new TF1("kaSigmaHighP", "pol1(0)", 0.2, 3.0); kaSigmaHighP->SetParameters(0.0630206, -0.00374603);
		deSigmaLowP = new TF1("deSigmaLowP", "pol1(0)", 0.6, 1.025); deSigmaLowP->SetParameters(0.158871, -0.0961071);
		deSigmaHighP = new TF1("deSigmaHighP", "pol1(0)", 1.025, 3.0); deSigmaHighP->SetParameters(0.054097, 0.00610817);
		trSigmaLowP = new TF1("trSigmaLowP", "pol1(0)", 0., 0.625); trSigmaLowP->SetParameters(0.50625, -0.65);
		trSigmaMidP = new TF1("trSigmaMidP", "pol1(0)", 0.625, 1.5); trSigmaMidP->SetParameters(0.137969, -0.05625);
		trSigmaHighP = new TF1("trSigmaHighP", "pol1(0)", 1.5, 3.0); trSigmaHighP->SetParameters(0.0510345, 0.0039953);
		he3SigmaLowP = new TF1("he3SigmaLowP", "pol1(0)", 0., 0.675); he3SigmaLowP->SetParameters(0.44225, -0.47);
		he3SigmaMidP = new TF1("he3SigmaMidP", "pol1(0)", 0.675, 1.475); he3SigmaMidP->SetParameters(0.17984375, -0.08125);
		he3SigmaHighP = new TF1("he3SigmaHighP", "pol1(0)", 1.475, 3.0); he3SigmaHighP->SetParameters(-0.261818, 0.21818);
		he4SigmaLowP = new TF1("he4SigmaLowP", "pol1(0)", 0., 0.475); he4SigmaLowP->SetParameters(0.37188, -0.225);
		he4SigmaMid1P = new TF1("he4SigmaMid1P", "pol1(0)", 0.475, 0.7); he4SigmaMid1P->SetParameters(0.19375, 0.15);
		he4SigmaMid2P = new TF1("he4SigmaMid2P", "pol1(0)", 0.7, 0.925); he4SigmaMid2P->SetParameters(0.72231, -0.6025);
		he4SigmaHighP = new TF1("he4SigmaHighP", "pol1(0)", 0.925, 3.0); he4SigmaHighP->SetParameters(0.32477, -0.17273);
		
		/// Asymmetry
		fAsymmetryElLowP = new TF1("fAsymmetryElLowP", "pol1(0)", 0., 0.5); fAsymmetryElLowP->SetParameters(-0.340074, 1.04407);
		fAsymmetryElMidP = new TF1("fAsymmetryElMidP", "pol1(0)", 0.5, 0.85); fAsymmetryElMidP->SetParameters(0.291559, -0.306138);
		fAsymmetryElHighP = new TF1("fAsymmetryElHighP", "pol1(0)", 0.85, 3.0); fAsymmetryElHighP->SetParameters(0.0915437, 0.0467649);
		fAsymmetryMuLowP = new TF1("fAsymmetryMuLowP", "pol1(0)", 0., 0.125); fAsymmetryMuLowP->SetParameters(2.5656, -14.391);
		fAsymmetryMuMidP = new TF1("fAsymmetryMuMidP", "pol1(0)", 0.125, 0.5); fAsymmetryMuMidP->SetParameters(-0.0467746, 0.496758);
		fAsymmetryMuHighP = new TF1("fAsymmetryMuHighP", "pol1(0)", 0.5, 3.0); fAsymmetryMuHighP->SetParameters(0.196422, 0.0117276);
		fAsymmetryPiLowP = new TF1("fAsymmetryPiLowP", "pol1(0)", 0., 0.1841); fAsymmetryPiLowP->SetParameters(4.51001, -23.116);
		fAsymmetryPiMidP = new TF1("fAsymmetryPiMidP", "pol1(0)", 0.1841, 0.6); fAsymmetryPiMidP->SetParameters(0.206221, 0.103783);
		fAsymmetryPiHighP = new TF1("fAsymmetryPiHighP", "pol1(0)", 0.6, 3.); fAsymmetryPiHighP->SetParameters(0.204105, 0.0544095);
		fAsymmetryPrLowP = new TF1("fAsymmetryPrLowP", "pol1(0)", 0., 0.25); fAsymmetryPrLowP->SetParameters(1.68519, -2.75816);
		fAsymmetryPrMidP = new TF1("fAsymmetryPrMidP", "pol1(0)", 0.25, 0.5); fAsymmetryPrMidP->SetParameters(1.68519, -2.75816);
		fAsymmetryPrHighP = new TF1("fAsymmetryPrHighP", "pol1(0)", 0.5, 3.); fAsymmetryPrHighP->SetParameters(-0.0761382, 0.186669);
		fAsymmetryKaLowP = new TF1("fAsymmetryKaLowP", "pol1(0)", 0., 0.5); fAsymmetryKaLowP->SetParameters(2.3588, -4.87114);
		fAsymmetryKaMidP = new TF1("fAsymmetryKaMidP", "pol1(0)", 0.5, 1.05); fAsymmetryKaMidP->SetParameters(0.00526557, 0.239378);
		fAsymmetryKaHighP = new TF1("fAsymmetryKaHighP", "pol1(0)", 1.05, 3.); fAsymmetryKaHighP->SetParameters(0.0892569, 0.119403);
		fAsymmetryDeLowP = new TF1("fAsymmetryDeLowP", "pol1(0)", 0.375, 0.585); fAsymmetryDeLowP->SetParameters(0., 0.); //fAsymmetryDeLowP->SetParameters(24.7375, -42.5);
		fAsymmetryDeHighP = new TF1("fAsymmetryDeHighP", "pol1(0)", 0.585, 3.); fAsymmetryDeHighP->SetParameters(0., 0.); //fAsymmetryDeHighP->SetParameters(-0.238221, 0.186621);
		fAsymmetryTrLowP = new TF1("fAsymmetryTrLowP", "pol1(0)", 0.425, 0.575); fAsymmetryTrLowP->SetParameters(0., 0.); //fAsymmetryTrLowP->SetParameters(-10.2906, 25.9162);
		fAsymmetryTrMidP = new TF1("fAsymmetryTrMidP", "pol1(0)", 0.575, 0.84); fAsymmetryTrMidP->SetParameters(0., 0.); //fAsymmetryTrMidP->SetParameters(15.51, -18.8);
		fAsymmetryTrHighP = new TF1("fAsymmetryTrHighP", "pol1(0)", 0.84, 3.); fAsymmetryTrHighP->SetParameters(0., 0.); //fAsymmetryTrHighP->SetParameters(0.0510345, 0.0039953);
		fAsymmetryHe3LowP = new TF1("fAsymmetryHe3LowP", "pol1(0)", 0.525, 0.685); fAsymmetryHe3LowP->SetParameters(0., 0.); //fAsymmetryHe3LowP->SetParameters(-5.60648, 13.1829);
		fAsymmetryHe3MidP = new TF1("fAsymmetryHe3MidP", "pol1(0)", 0.685, 0.99); fAsymmetryHe3MidP->SetParameters(0., 0.); //fAsymmetryHe3MidP->SetParameters(11.275, -11.666);
		fAsymmetryHe3HighP = new TF1("fAsymmetryHe3HighP", "pol1(0)", 0.99, 3.); fAsymmetryHe3HighP->SetParameters(0., 0.); //fAsymmetryHe3HighP->SetParameters(-0.231232, -0.0431765);
		fAsymmetryHe4LowP = new TF1("fAsymmetryHe4LowP", "pol1(0)", 0., 0.575); fAsymmetryHe4LowP->SetParameters(0., 0.); //fAsymmetryHe4LowP->SetParameters(1.45, -2.);
		fAsymmetryHe4MidP = new TF1("fAsymmetryHe4MidP", "pol1(0)", 0.575, 0.875); fAsymmetryHe4MidP->SetParameters(0., 0.); //fAsymmetryHe4MidP->SetParameters(-3.10941, 5.73622);
		fAsymmetryHe4HighP = new TF1("fAsymmetryHe4HighP", "pol1(0)", 0.875, 1.25); fAsymmetryHe4HighP->SetParameters(0., 0.); //fAsymmetryHe4HighP->SetParameters(7.47211, -6.09903);
	}
	
	/// Particle yields versus momentum (close to a thermal function).
	/// The predicted number roughly speaking are not dN/dy or total yiels,
	/// only their relative hights are of relevance!
	
	Double_t amplParam;
	if (Generator != "NSIG")
	{
		parElPosMom = new TF1("parElPosMom",this,&MpdPid::MomPr,PMIN,PMAX,5,"MpdPid","MomPi");
		parElNegMom = new TF1("parElNegMom",this,&MpdPid::MomPr,PMIN,PMAX,5,"MpdPid","MomPi");
		parMuPosMom = new TF1("parMuPosMom",this,&MpdPid::MomPi,PMIN,PMAX,5,"MpdPid","MomPi");
		parMuNegMom = new TF1("parMuNegMom",this,&MpdPid::MomPi,PMIN,PMAX,5,"MpdPid","MomPi");
		parPiPosMom = new TF1("parPiPosMom",this,&MpdPid::MomPi,PMIN,PMAX,5,"MpdPid","MomPi");
		parPiNegMom = new TF1("parPiNegMom",this,&MpdPid::MomPi,PMIN,PMAX,5,"MpdPid","MomPi");
		parKaPosMom = new TF1("parKaPosMom",this,&MpdPid::MomPr,PMIN,PMAX,5,"MpdPid","MomPr");
		parKaNegMom = new TF1("parKaNegMom",this,&MpdPid::MomPr,PMIN,PMAX,5,"MpdPid","MomPr");
		parPrPosMom = new TF1("parPrPosMom",this,&MpdPid::MomPr,PMIN,PMAX,5,"MpdPid","MomPr");
		parPrNegMom = new TF1("parPrNegMom",this,&MpdPid::MomPr,PMIN,PMAX,5,"MpdPid","MomPr");
		parDeMom = new TF1("parDeMom",this,&MpdPid::MomPr,PMIN,PMAX,5,"MpdPid","MomPr");
		parTrMom = new TF1("parTrMom",this,&MpdPid::MomPr,PMIN,PMAX,5,"MpdPid","MomPr");
		parHe3Mom = new TF1("parHe3Mom",this,&MpdPid::MomPr,PMIN,PMAX,5,"MpdPid","MomPr");
		parHe4Mom = new TF1("parHe4Mom",this,&MpdPid::MomPr,PMIN,PMAX,5,"MpdPid","MomPr");
	}
	else
	{
		if (NSigPart.Contains("el")) {parElPosMom = new TF1("parElPosMom","1.",PMIN,PMAX); parElNegMom = new TF1("parElNegMom","1.",PMIN,PMAX); cout << "electrons are included in n-sigma method..." << endl;}
		else {parElPosMom = new TF1("parElPosMom","0.",PMIN,PMAX); parElNegMom = new TF1("parElNegMom","0.",PMIN,PMAX);}
		if (NSigPart.Contains("mu")) {parMuPosMom = new TF1("parMuPosMom","1.",PMIN,PMAX); parMuNegMom = new TF1("parMuNegMom","1.",PMIN,PMAX); cout << "muons are included in n-sigma method..." << endl;}
		else {parMuPosMom = new TF1("parMuPosMom","0.",PMIN,PMAX); parMuNegMom = new TF1("parMuNegMom","0.",PMIN,PMAX);}
		if (NSigPart.Contains("pi")) {parPiPosMom = new TF1("parPiPosMom","1.",PMIN,PMAX); parPiNegMom = new TF1("parPiNegMom","1.",PMIN,PMAX); cout << "pions are included in n-sigma method..." << endl;}
		else {parPiPosMom = new TF1("parPiPosMom","0.",PMIN,PMAX); parPiNegMom = new TF1("parPiNegMom","0.",PMIN,PMAX);}
		if (NSigPart.Contains("ka")) {parKaPosMom = new TF1("parKaPosMom","1.",PMIN,PMAX); parKaNegMom = new TF1("parKaNegMom","1.",PMIN,PMAX); cout << "kaons are included in n-sigma method..." << endl;}
		else {parKaPosMom = new TF1("parKaPosMom","0.",PMIN,PMAX); parKaNegMom = new TF1("parKaNegMom","0.",PMIN,PMAX);}
		if (NSigPart.Contains("pr")) {parPrPosMom = new TF1("parPrPosMom","1.",PMIN,PMAX); parPrNegMom = new TF1("parPrNegMom","1.",PMIN,PMAX); cout << "protons are included in n-sigma method..." << endl;}
		else {parPrPosMom = new TF1("parPrPosMom","0.",PMIN,PMAX); parPrNegMom = new TF1("parPrNegMom","0.",PMIN,PMAX);}
		if (NSigPart.Contains("de")) {parDeMom = new TF1("parDeMom","1.",PMIN,PMAX); cout << "deuterons are included in n-sigma method..." << endl;}
		else parDeMom = new TF1("parDeMom","0.",PMIN,PMAX);
		if (NSigPart.Contains("tr")) {parTrMom = new TF1("parTrMom","1.",PMIN,PMAX); cout << "tritons are included in n-sigma method..." << endl;}
		else parTrMom = new TF1("parTrMom","0.",PMIN,PMAX);
		if (NSigPart.Contains("he3")) {parHe3Mom = new TF1("parHe3Mom","1.",PMIN,PMAX); cout << "he3 are included in n-sigma method..." << endl;}
		else parHe3Mom = new TF1("parHe3Mom","0.",PMIN,PMAX);
		if (NSigPart.Contains("he4")) {parHe4Mom = new TF1("parHe4Mom","1.",PMIN,PMAX); cout << "he4 are included in n-sigma method..." << endl;}
		else parHe4Mom = new TF1("parHe4Mom","0.",PMIN,PMAX);
	}
	
	for (Int_t itr = 0; itr < 14; itr++) Multiplicities[itr] = 0;
	if ( !( (Generator == "LAQGSM") || (Generator == "QGSM") || (Generator == "URQMD") || (Generator == "NSIG") || (Generator == "PHSD") || (Generator == "EPOS") ) )
	{
		cout << "Incorrect generator string! Switching to DEFAULT..." << endl;
		Generator = "DEFAULT";
	}
	
	if ( Generator == "EPOS" ) /// for p + p collisions
	{
		if (fEnergy < 7.0) /// sqrt(s) = 6 GeV, ~1M events
		{
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parPiPosMom->SetParameters(2853.8,0.623542,0.0537214,0.137323,1.4267); Multiplicities[4] = 1209767;
			parPiNegMom->SetParameters(1407.44,0.987421,0.0574761,0.131526,1.18167); Multiplicities[5] = 765846;
			parKaPosMom->SetParameters(228.358,7.59862,0.162463,0.148098,0.183593); Multiplicities[6] = 46342;
			parKaNegMom->SetParameters(57.3598,0.546853,0.154656,0.0593315,0.543194); Multiplicities[7] = 11794;
			parPrPosMom->SetParameters(1796.08,8.20092,0.272697,0.253002,0.181719); Multiplicities[8] = 360301;
			parPrNegMom->SetParameters(38.0024,0.0768751,0.151354,0.144691,-0.144209); Multiplicities[9] = 732;
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		else if (fEnergy < 9.0) /// sqrt(s) = 8.76 GeV, 1M events
		{
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parPiPosMom->SetParameters(3160.76,0.70523,0.080124,0.206086,0.974646); Multiplicities[4] = 1442819;
			parPiNegMom->SetParameters(1751.43,1.17603,0.0914196,0.206213,0.737364); Multiplicities[5] = 1083128;
			parKaPosMom->SetParameters(362.313,0.332122,0.232739,0.0763912,0.301778); Multiplicities[6] = 73099;
			parKaNegMom->SetParameters(161.454,0.483719,0.217565,0.0874472,0.345383); Multiplicities[7] = 32741;
			parPrPosMom->SetParameters(1254.12,0.409612,0.385691,0.171431,0.181715); Multiplicities[8] = 251783;
			parPrNegMom->SetParameters(89.4667,0.216063,0.139082,0.0149435,1.29752); Multiplicities[9] = 4716;
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		else if (fEnergy < 12.0) /// sqrt(s) = 10 GeV, 1M events
		{
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parPiPosMom->SetParameters(3320.92,0.698313,0.0892079,0.227947,0.875591); Multiplicities[4] = 1508888;
			parPiNegMom->SetParameters(2090.76,1.0203,0.100926,0.228817,0.673615); Multiplicities[5] = 1176549;
			parKaPosMom->SetParameters(400.101,4.6224,0.208173,0.182652,0.069632); Multiplicities[6] = 81475;
			parKaNegMom->SetParameters(197.395,0.355122,0.235916,0.0794332,0.358637); Multiplicities[7] = 39962;
			parPrPosMom->SetParameters(1089.56,0.242752,0.407993,0.13063,0.250824); Multiplicities[8] = 219002;
			parPrNegMom->SetParameters(136.99,1.09482,0.231846,0.127863,0.205208); Multiplicities[9] = 7120;
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		else if (fEnergy < 16.0) /// sqrt(s) = 15 GeV, 1M events
		{
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parPiPosMom->SetParameters(3750.67,0.657879,0.108281,0.279038,0.725381); Multiplicities[4] = 1651949;
			parPiNegMom->SetParameters(2984.16,0.769168,0.129349,0.287196,0.507861); Multiplicities[5] = 1406766;
			parKaPosMom->SetParameters(675.506,0.0596585,0.26442,0.14352,-0.138467); Multiplicities[6] = 97470;
			parKaNegMom->SetParameters(292.368,0.256861,0.275726,0.0776192,0.326165); Multiplicities[7] = 59204;
			parPrPosMom->SetParameters(890.967,0.00141238,0.416039,0.150785,-0.150519); Multiplicities[8] = 147891;
			parPrNegMom->SetParameters(518.922,0.00427706,0.2737,0.156507,-0.156194); Multiplicities[9] = 15895;
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		else if (fEnergy < 19.0) /// sqrt(s) = 17.3 GeV, 1M events
		{
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parPiPosMom->SetParameters(3817.06,0.664315,0.115249,0.292103,0.674843); Multiplicities[4] = 1691208;
			parPiNegMom->SetParameters(3440.51,0.65109,0.139433,0.3066,0.464094); Multiplicities[5] = 1471640;
			parKaPosMom->SetParameters(505.915,0.229333,0.289876,0.0773325,0.275986); Multiplicities[6] = 102339;
			parKaNegMom->SetParameters(321.28,0.255435,0.283842,0.0806211,0.315722); Multiplicities[7] = 65082;
			parPrPosMom->SetParameters(655.647,0.165912,0.410953,0.0981928,0.296843); Multiplicities[8] = 132908;
			parPrNegMom->SetParameters(368.293,0.275794,0.289646,0.066954,0.501408); Multiplicities[9] = 18897;
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		else if (fEnergy < 23.0) /// sqrt(s) = 20 GeV, 1M events
		{
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parPiPosMom->SetParameters(3908.67,0.663881,0.11984,0.303021,0.648025); Multiplicities[4] = 1732094;
			parPiNegMom->SetParameters(3652.92,0.635229,0.145063,0.317634,0.434774); Multiplicities[5] = 1539128;
			parKaPosMom->SetParameters(1504.68,-0.608577,0.300936,0.105427,-0.169068); Multiplicities[6] = 105966;
			parKaNegMom->SetParameters(352.373,0.234724,0.293469,0.0797531,0.303124); Multiplicities[7] = 71516;
			parPrPosMom->SetParameters(599.226,0.172265,0.406555,0.0998964,0.284495); Multiplicities[8] = 121639;
			parPrNegMom->SetParameters(441.899,0.187852,0.296744,0.0391455,0.591304); Multiplicities[9] = 22716;
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		else /// sqrt(s) = 25 GeV, 1M events
		{
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parPiPosMom->SetParameters(3982.71,0.68892,0.122378,0.312125,0.636517); Multiplicities[4] = 1804766;
			parPiNegMom->SetParameters(3980.97,0.60921,0.149336,0.331234,0.424032); Multiplicities[5] = 1642269;
			parKaPosMom->SetParameters(1370.93,-0.538074,0.313878,0.103715,-0.16116); Multiplicities[6] = 112638;
			parKaNegMom->SetParameters(396.416,0.206327,0.304933,0.0762121,0.305453); Multiplicities[7] = 80412;
			parPrPosMom->SetParameters(547.27,0.166229,0.401412,0.0955138,0.284058); Multiplicities[8] = 111148;
			parPrNegMom->SetParameters(546.003,0.664169,0.32501,0.154779,-0.0269533); Multiplicities[9] = 28556;
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
	}
	
	if ( Generator == "PHSD" )
	{
		if (Tracking == "HP")
		{
			if (fEnergy < 7.0) /// not ready
			{
				parElPosMom->SetParameters(17.6,-0.12,0.078,0.167,0.00); // QGSM 5-9 gev
				parElNegMom->SetParameters(16.3,-0.12,0.078,0.167,0.00);
				parMuPosMom->SetParameters(20.5,0.064,0.107,0.05,0.105); // QGSM 5-9 gev
				parMuNegMom->SetParameters(20.5,0.064,0.107,0.05,0.105);
				parPiPosMom->SetParameters(307.0,0.035,0.175,0.127,0.139); // QGSM 5 gev
				parPiNegMom->SetParameters(325.6,0.035,0.175,0.127,0.139); // QGSM 5 gev
				parKaPosMom->SetParameters(15.3,0.236,0.203,0.056,0.494); // QGSM 5 gev
				parKaNegMom->SetParameters(8.88,0.236,0.203,0.056,0.494); // QGSM 5 gev
				amplParam = 104.0;
				parPrPosMom->SetParameters(amplParam,0.213,0.294,0.09,0.938); // QGSM 5 gev
				amplParam /= prrat;
				parPrNegMom->SetParameters(amplParam,0.213,0.294,0.09,0.938); // QGSM 5 gev
				parDeMom->SetParameters(5.7,0.338,0.333,0.114,1.878); // QGSM 5 gev
				parTrMom->SetParameters(0.2,-0.35,0.723,0.2,2.81);
				parHe3Mom->SetParameters(0.36,-0.784,530.3,0.131,1.983);
				parHe4Mom->SetParameters(6.6e-03,0.27,0.2,1.42,3.51);
			}
			else /// fEnergy > 7.0 not ready
			{
				parPiPosMom->SetParameters(473.,0.034,0.187,0.469,0.139); // QGSM 9 gev
				parPiNegMom->SetParameters(501.6,0.034,0.187,0.469,0.139); // QGSM 9 gev
				parKaPosMom->SetParameters(21.1,0.157,0.241,0.043,0.494); // QGSM 9 gev
				parKaNegMom->SetParameters(12.25,0.157,0.241,0.043,0.494); // QGSM 9 gev
				amplParam = 67.4;
				parPrPosMom->SetParameters(amplParam,.02,0.365,0.01,0.938); // QGSM 9 gev
				amplParam /= prrat;
				parPrNegMom->SetParameters(amplParam,.02,0.365,0.01,0.938); // QGSM 9 gev
				parDeMom->SetParameters(1.8,0.05,0.432,0.163,1.878); // QGSM 9 gev
				parTrMom->SetParameters(0.2,-0.35,0.723,0.2,2.81);
				parHe3Mom->SetParameters(0.36,-0.784,530.3,0.131,1.983);
				parHe4Mom->SetParameters(6.6e-03,0.27,0.2,1.42,3.51);
			}
		}
		
		else /// Tracking == "CF"
		{
			if (fEnergy < 7.0) /// not ready
			{
				parElPosMom->SetParameters(0.,1.,1.,1.,1.);
				parElNegMom->SetParameters(0.,1.,1.,1.,1.);
				parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
				parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
				parPiPosMom->SetParameters(44660.6,1.4425,0.0821871,0.126819,-0.106069); Multiplicities[4] = 13565353;
				parPiNegMom->SetParameters(1929.85,24.5821,0.111797,0.066913,0.667266); Multiplicities[5] = 17666880;
				parKaPosMom->SetParameters(3089.81,9.2121,0.136135,0.125175,0.350274); Multiplicities[6] = 621942;
				parKaNegMom->SetParameters(237.81,-0.746975,0.0771305,0.0365433,2.27772); Multiplicities[7] = 39940;
				amplParam = 114392.;
				parPrPosMom->SetParameters(amplParam,6.35265,0.188466,0.166392,0.7605); Multiplicities[8] = 23124724;
				amplParam /= prrat;
				parPrNegMom->SetParameters(amplParam,6.35265,0.188466,0.166392,0.7605); Multiplicities[9] = 4;
				parDeMom->SetParameters(5024.67,0.129733,0.266767,0.00308559,39.0077); Multiplicities[10] = 1011296;
				parTrMom->SetParameters(938.334,0.368862,0.0161982,0.00680544,109.992); Multiplicities[11] = 34449;
				parHe3Mom->SetParameters(661.212,8.47325,0.150273,0.135588,1.57518); Multiplicities[12] = 22466;
				parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
			}
			else /// fEnergy = 11.0, 03/07/2018, 629K events
			{
				parElPosMom->SetParameters(0.,1.,1.,1.,1.);
				parElNegMom->SetParameters(0.,1.,1.,1.,1.);
				parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
				parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
				parPiPosMom->SetParameters(12708,7.25584,0.167878,0.303888,0.277294); Multiplicities[4] = 37433422;
				parPiNegMom->SetParameters(3319.17,14.7475,0.212685,0.341987,-0.190745); Multiplicities[5] = 40994164;
				parKaPosMom->SetParameters(24333.9,0.409004,0.283049,0.107653,0.396297); Multiplicities[6] = 4868113;
				parKaNegMom->SetParameters(12323.4,0.392905,0.267303,0.0952731,0.457674); Multiplicities[7] = 2471313;
				parPrPosMom->SetParameters(42750.4,1.92747,0.363864,0.273259,0.337891); Multiplicities[8] = 8579069;
				parPrNegMom->SetParameters(4636.8,1.42461,0.285596,0.181618,0.837135); Multiplicities[9] = 233640;
				parDeMom->SetParameters(0.,1.,1.,1.,1.);
				parTrMom->SetParameters(0.,1.,1.,1.,1.);
				parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
				parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
			}
		}
	}
	
	if ( Generator == "PHSD_CENT" ) /// PHSD central events, b < 3 fm, fEnergy = 11.0, 20/03/2019, 5482 events
	{
		parPiPosMom->SetParameters(2628.47,0.846834,0.203337,0.357357,0.147232); Multiplicities[0] = 1310378;
		parPiNegMom->SetParameters(1997.0,1.41096,0.196863,0.341347,0.18051); Multiplicities[1] = 1423581;
		parKaPosMom->SetParameters(872.775,0.346697,0.298306,0.102013,0.423512); Multiplicities[2] = 175908;
		parKaNegMom->SetParameters(437.025,0.368078,0.279169,0.0955438,0.48267); Multiplicities[3] = 88328;
		parPrPosMom->SetParameters(1705.66,0.663284,0.425112,0.220086,0.457981); Multiplicities[4] = 344194;
		parPrNegMom->SetParameters(191.997,6.39986,0.276029,0.244932,0.722006); Multiplicities[5] = 9803;
		parElPosMom->SetParameters(0.,1.,1.,1.,1.);
		parElNegMom->SetParameters(0.,1.,1.,1.,1.);
		parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
		parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
		parDeMom->SetParameters(0.,1.,1.,1.,1.);
		parTrMom->SetParameters(0.,1.,1.,1.,1.);
		parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
		parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
	}
	
	if ( Generator == "PHSD_CSR" )
	{
		if (fEnergy < 5.0) /// PHSD csr central events, b < 3 fm, fEnergy = 4.0 GeV, 10/10/2019, ~50k events
		{
			parPiPosMom->SetParameters(326.591,37.4694,0.0395906,0.145417,1.08328); Multiplicities[0] = 4637927;
			parPiNegMom->SetParameters(7834.26,1.46868,0.178981,0.0634056,0.520443); Multiplicities[1] = 5651992;
			parKaPosMom->SetParameters(1709.8,1.73908,0.20194,0.143219,0.290164); Multiplicities[2] = 343045;
			parKaNegMom->SetParameters(327.052,0.638412,0.183259,0.0800663,0.554539); Multiplicities[3] = 65986;
			parPrPosMom->SetParameters(29176.4,3.57695,0.238379,0.195002,0.670283); Multiplicities[4] = 5858655;
			parPrNegMom->SetParameters(4.18494,1.07077,0.165123,0.0865973,2.30082); Multiplicities[5] = 945;
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		
		else if (fEnergy < 7.0) /// PHSD csr central events, b < 3 fm, fEnergy = 6.2 GeV, 10/10/2019, ~50k events
		{
			parPiPosMom->SetParameters(112.48,188.057,0.183905,0.284109,-9.42658e-05); Multiplicities[0] = 7656887;
			parPiNegMom->SetParameters(369.468,63.2746,0.190742,0.278494,-0.0606429); Multiplicities[1] = 8715275;
			parKaPosMom->SetParameters(4727.89,0.411218,0.271326,0.10275,0.38878); Multiplicities[2] = 948753;
			parKaNegMom->SetParameters(1467.24,0.423877,0.247845,0.0908385,0.477971); Multiplicities[3] = 294951;
			parPrPosMom->SetParameters(22653.3,2.16885,0.335439,0.255945,0.403967); Multiplicities[4] = 4560388;
			parPrNegMom->SetParameters(85.4405,1.87681,0.13522,0.0885255,2.5824); Multiplicities[5] = 17742;
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		
		else if (fEnergy < 8.0) /// PHSD csr central events, b < 3 fm, fEnergy = 7.6 GeV, 10/10/2019, ~50k events
		{
			parPiPosMom->SetParameters(271.408,92.8688,0.184859,0.303397,0.0543418); Multiplicities[0] = 9235403;
			parPiNegMom->SetParameters(1489.49,17.1459,0.192214,0.302818,-0.0970675); Multiplicities[1] = 10319307;
			parKaPosMom->SetParameters(6031.92,0.356296,0.28695,0.0997535,0.406052); Multiplicities[2] = 1211986;
			parKaNegMom->SetParameters(2292.64,0.392553,0.26581,0.0946355,0.461694); Multiplicities[3] = 460846;
			parPrPosMom->SetParameters(20282.1,1.38552,0.373508,0.255721,0.370525); Multiplicities[4] = 4090897;
			parPrNegMom->SetParameters(178.133,1.51585,0.201521,0.125455,1.68405); Multiplicities[5] = 36475;
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		
		else if (fEnergy < 10.0) /// PHSD csr central events, b < 3 fm, fEnergy = 8.8 GeV, 10/10/2019, ~50k events
		{
			parPiPosMom->SetParameters(330.227,85.1517,0.179918,0.3141,0.121203); Multiplicities[0] = 10473715;
			parPiNegMom->SetParameters(21412.6,0.965329,0.210709,0.338147,0.08417); Multiplicities[1] = 11579444;
			parKaPosMom->SetParameters(6714.74,0.343555,0.297204,0.101954,0.396783); Multiplicities[2] = 1350509;
			parKaNegMom->SetParameters(2939.86,0.373118,0.276092,0.0958796,0.458351); Multiplicities[3] = 591237;
			parPrPosMom->SetParameters(18934.3,1.02918,0.397596,0.248315,0.378061); Multiplicities[4] = 3823045;
			parPrNegMom->SetParameters(271.395,1.29596,0.246056,0.146202,1.31564); Multiplicities[5] = 55217;
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		
		else /// PHSD csr central events, b < 3 fm, fEnergy = 12.3 GeV, 10/10/2019, ~50k events
		{
			parPiPosMom->SetParameters(523.376,66.8745,0.172619,0.330811,0.198081); Multiplicities[0] = 13262313;
			parPiNegMom->SetParameters(996.699,25.9407,0.202627,0.35925,-0.162493); Multiplicities[1] = 14426178;
			parKaPosMom->SetParameters(8208.95,0.325369,0.313731,0.105367,0.393535); Multiplicities[2] = 1653266;
			parKaNegMom->SetParameters(4565.48,0.344706,0.298358,0.100691,0.441146); Multiplicities[3] = 919173;
			parPrPosMom->SetParameters(15861.8,1.06114,0.420781,0.268906,0.331336); Multiplicities[4] = 3211971;
			parPrNegMom->SetParameters(545.455,5.15299,0.289769,0.250636,0.750215); Multiplicities[5] = 110793;
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
	}
	
	if ( Generator == "PHSD_NOCSR" )
	{
		if (fEnergy < 5.0) /// PHSD no csr central events, b < 3 fm, fEnergy = 4 GeV, 10/10/2019, ~50k events
		{
			parPiPosMom->SetParameters(396.282,32.7313,0.0382901,0.141021,1.12647); Multiplicities[0] = 4940651;
			parPiNegMom->SetParameters(3579.74,3.98371,0.11532,0.0306925,1.30859); Multiplicities[1] = 5997347;
			parKaPosMom->SetParameters(1081.74,3.09011,0.190816,0.154575,0.279164); Multiplicities[2] = 217190;
			parKaNegMom->SetParameters(253.536,0.589513,0.178808,0.0743182,0.612162); Multiplicities[3] = 51184;
			parPrPosMom->SetParameters(30200.6,3.29956,0.236862,0.190743,0.684265); Multiplicities[4] = 6064105;
			parPrNegMom->SetParameters(4.86563,1.59102,0.115168,0.0729325,3.3942); Multiplicities[5] = 1139;
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		
		else if (fEnergy < 7.0) /// PHSD no csr central events, b < 3 fm, fEnergy = 6.2 GeV, 10/10/2019, ~50k events
		{
			parPiPosMom->SetParameters(2907.1,7.34039,0.192883,0.283338,0.000931754); Multiplicities[0] = 8289950;
			parPiNegMom->SetParameters(434.643,57.0644,0.194478,0.275712,-0.0728979); Multiplicities[1] = 9398261;
			parKaPosMom->SetParameters(3030.57,0.431268,0.264635,0.102554,0.388718); Multiplicities[2] = 608249;
			parKaNegMom->SetParameters(1194.21,0.430582,0.239526,0.0874055,0.502341); Multiplicities[3] = 240120;
			parPrPosMom->SetParameters(24464.4,2.01624,0.334706,0.250743,0.412746); Multiplicities[4] = 4924589;
			parPrNegMom->SetParameters(104.891,2.0149,0.141495,0.0947837,2.53473); Multiplicities[5] = 21688;
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		
		else if (fEnergy < 8.0) /// PHSD no csr central events, b < 3 fm, fEnergy = 7.6 GeV, 10/10/2019, ~50k events
		{
			parPiPosMom->SetParameters(3463.66,7.24569,0.190893,0.304263,0.0839204); Multiplicities[0] = 9906781;
			parPiNegMom->SetParameters(638.648,40.2362,0.194759,0.301837,-0.1207); Multiplicities[1] = 11025250;
			parKaPosMom->SetParameters(4243.36,0.373601,0.279679,0.0997516,0.401217); Multiplicities[2] = 852728;
			parKaNegMom->SetParameters(1954.45,0.411682,0.258866,0.0941066,0.470275); Multiplicities[3] = 393033;
			parPrPosMom->SetParameters(21992.6,1.39463,0.370946,0.254295,0.371529); Multiplicities[4] = 4435915;
			parPrNegMom->SetParameters(210.959,1.4898,0.201572,0.123791,1.70536); Multiplicities[5] = 43113;
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		
		else if (fEnergy < 10.0) /// PHSD no csr central events, b < 3 fm, fEnergy = 8.8 GeV, 10/10/2019, ~50k events
		{
			parPiPosMom->SetParameters(6728.05,3.88388,0.190088,0.319898,0.124891); Multiplicities[0] = 10994546;
			parPiNegMom->SetParameters(3648.45,8.36849,0.304356,0.177689,0.177577); Multiplicities[1] = 12124932;
			parKaPosMom->SetParameters(5131.42,0.356848,0.289977,0.101518,0.39762); Multiplicities[2] = 1031859;
			parKaNegMom->SetParameters(2595.77,0.383127,0.270511,0.0949794,0.464502); Multiplicities[3] = 522263;
			parPrPosMom->SetParameters(20165.6,1.28612,0.388469,0.261465,0.353915); Multiplicities[4] = 4073272;
			parPrNegMom->SetParameters(305.183,1.28427,0.245721,0.144331,1.35441); Multiplicities[5] = 62210;
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
		
		else /// PHSD no csr central events, b < 3 fm, fEnergy = 12.3 GeV, 10/10/2019, ~50k events
		{
			parPiPosMom->SetParameters(25403.9,0.930567,0.206834,0.368397,0.134282); Multiplicities[0] = 13619538;
			parPiNegMom->SetParameters(1064.68,36.2707,0.326473,0.173008,0.228547); Multiplicities[1] = 14796114;
			parKaPosMom->SetParameters(7276.52,0.328204,0.309239,0.104224,0.392577); Multiplicities[2] = 1465260;
			parKaNegMom->SetParameters(4294.11,0.353445,0.293973,0.100527,0.443493); Multiplicities[3] = 864408;
			parPrPosMom->SetParameters(16438.3,1.07511,0.419069,0.268969,0.330482); Multiplicities[4] = 3328879;
			parPrNegMom->SetParameters(575.422,4.48476,0.290547,0.246113,0.770269); Multiplicities[5] = 117001;
			parElPosMom->SetParameters(0.,1.,1.,1.,1.);
			parElNegMom->SetParameters(0.,1.,1.,1.,1.);
			parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
			parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
			parDeMom->SetParameters(0.,1.,1.,1.,1.);
			parTrMom->SetParameters(0.,1.,1.,1.,1.);
			parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
			parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
		}
	}
	
	if ( (Generator == "LAQGSM") || (Generator == "QGSM") )
	{
		if (Tracking == "HP")
		{
			if (fEnergy < 7.0) // not ready
			{
				parElPosMom->SetParameters(17.6,-0.12,0.078,0.167,0.00); // QGSM 5-9 gev
				parElNegMom->SetParameters(16.3,-0.12,0.078,0.167,0.00);
				parMuPosMom->SetParameters(20.5,0.064,0.107,0.05,0.105); // QGSM 5-9 gev
				parMuNegMom->SetParameters(20.5,0.064,0.107,0.05,0.105);
				parPiPosMom->SetParameters(307.0,0.035,0.175,0.127,0.139); // QGSM 5 gev
				parPiNegMom->SetParameters(325.6,0.035,0.175,0.127,0.139); // QGSM 5 gev
				parKaPosMom->SetParameters(15.3,0.236,0.203,0.056,0.494); // QGSM 5 gev
				parKaNegMom->SetParameters(8.88,0.236,0.203,0.056,0.494); // QGSM 5 gev
				amplParam = 104.0;
				parPrPosMom->SetParameters(amplParam,0.213,0.294,0.09,0.938); // QGSM 5 gev
				amplParam /= prrat;
				parPrNegMom->SetParameters(amplParam,0.213,0.294,0.09,0.938); // QGSM 5 gev
				parDeMom->SetParameters(5.7,0.338,0.333,0.114,1.878); // QGSM 5 gev
				parTrMom->SetParameters(0.2,-0.35,0.723,0.2,2.81);
				parHe3Mom->SetParameters(0.36,-0.784,530.3,0.131,1.983);
				parHe4Mom->SetParameters(6.6e-03,0.27,0.2,1.42,3.51);
			}
			else /// fEnergy > 7.0 not ready
			{
				parPiPosMom->SetParameters(473.,0.034,0.187,0.469,0.139); // QGSM 9 gev
				parPiNegMom->SetParameters(501.6,0.034,0.187,0.469,0.139); // QGSM 9 gev
				parKaPosMom->SetParameters(21.1,0.157,0.241,0.043,0.494); // QGSM 9 gev
				parKaNegMom->SetParameters(12.25,0.157,0.241,0.043,0.494); // QGSM 9 gev
				amplParam = 67.4;
				parPrPosMom->SetParameters(amplParam,.02,0.365,0.01,0.938); // QGSM 9 gev
				amplParam /= prrat;
				parPrNegMom->SetParameters(amplParam,.02,0.365,0.01,0.938); // QGSM 9 gev
				parDeMom->SetParameters(1.8,0.05,0.432,0.163,1.878); // QGSM 9 gev
				parTrMom->SetParameters(0.2,-0.35,0.723,0.2,2.81);
				parHe3Mom->SetParameters(0.36,-0.784,530.3,0.131,1.983);
				parHe4Mom->SetParameters(6.6e-03,0.27,0.2,1.42,3.51);
			}
		}
		
		else /// Tracking == "CF"
		{
			if (fEnergy < 7.0)
			{
				parElPosMom->SetParameters(0.,1.,1.,1.,1.);
				parElNegMom->SetParameters(0.,1.,1.,1.,1.);
				parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
				parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
				parPiPosMom->SetParameters(44660.6,1.4425,0.0821871,0.126819,-0.106069); Multiplicities[4] = 13565353;
				parPiNegMom->SetParameters(1929.85,24.5821,0.111797,0.066913,0.667266); Multiplicities[5] = 17666880;
				parKaPosMom->SetParameters(3089.81,9.2121,0.136135,0.125175,0.350274); Multiplicities[6] = 621942;
				parKaNegMom->SetParameters(237.81,-0.746975,0.0771305,0.0365433,2.27772); Multiplicities[7] = 39940;
				amplParam = 114392.;
				parPrPosMom->SetParameters(amplParam,6.35265,0.188466,0.166392,0.7605); Multiplicities[8] = 23124724;
				amplParam /= prrat;
				parPrNegMom->SetParameters(amplParam,6.35265,0.188466,0.166392,0.7605); Multiplicities[9] = 4;
				parDeMom->SetParameters(5024.67,0.129733,0.266767,0.00308559,39.0077); Multiplicities[10] = 1011296;
				parTrMom->SetParameters(938.334,0.368862,0.0161982,0.00680544,109.992); Multiplicities[11] = 34449;
				parHe3Mom->SetParameters(661.212,8.47325,0.150273,0.135588,1.57518); Multiplicities[12] = 22466;
				parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
			}
			else /// fEnergy = 11.0 (mb, Au+Au), 12/12/2017, 394.5K events
			{
				parElPosMom->SetParameters(0.,1.,1.,1.,1.);
				parElNegMom->SetParameters(0.,1.,1.,1.,1.);
				parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
				parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
				parPiPosMom->SetParameters(1287.54,48.9678,0.253939,0.121823,0.371256); Multiplicities[4] = 23985146;
				parPiNegMom->SetParameters(3979.89,3.39065,0.194877,0.392421,-0.186646); Multiplicities[5] = 24982438;
				parKaPosMom->SetParameters(9300.04,0.252028,0.280878,0.0806716,0.30038); Multiplicities[6] = 1869970;
				parKaNegMom->SetParameters(4270.24,0.269129,0.276149,0.0813025,0.316033); Multiplicities[7] = 860059;
				amplParam = 19730.6;
				parPrPosMom->SetParameters(amplParam,0.327478,0.478771,0.188208,0.263743); Multiplicities[8] = 3982483;
				amplParam /= prrat;
				parPrNegMom->SetParameters(amplParam,0.327478,0.478771,0.188208,0.263743); Multiplicities[9] = 44015;
				parDeMom->SetParameters(303.123,0.144,0.580517,0.097639,1.31764); Multiplicities[10] = 61753;
				parTrMom->SetParameters(0.,1.,1.,1.,1.);
				parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
				parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
			}
		}
	}
	
	else if (Generator == "URQMD")
	{
		if (Tracking == "HP")
		{
			if (fEnergy < 7.0) // not ready
			{
				parElPosMom->SetParameters(0.,1.,1.,1.,1.);
				parElNegMom->SetParameters(0.,1.,1.,1.,1.);
				parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
				parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
				parPiPosMom->SetParameters(307.0,0.035,0.175,0.127,0.139); // QGSM 5 gev
				parPiNegMom->SetParameters(325.6,0.035,0.175,0.127,0.139); // QGSM 5 gev
				parKaPosMom->SetParameters(15.3,0.236,0.203,0.056,0.494); // QGSM 5 gev
				parKaNegMom->SetParameters(8.88,0.236,0.203,0.056,0.494); // QGSM 5 gev
				amplParam = 104.0;
				parPrPosMom->SetParameters(amplParam,0.213,0.294,0.09,0.938); // QGSM 5 gev
				amplParam /= prrat;
				parPrNegMom->SetParameters(amplParam,0.213,0.294,0.09,0.938); // QGSM 5 gev
				parDeMom->SetParameters(0.,1.,1.,1.,1.);
				parTrMom->SetParameters(0.,1.,1.,1.,1.);
				parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
				parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
			}
			else /// fEnergy > 7.0, parameterization 11 GeV (27/07/2017) 10K events
			{
				prrat = 52.4698;
				parElPosMom->SetParameters(0.,1.,1.,1.,1.);
				parElNegMom->SetParameters(0.,1.,1.,1.,1.);
				parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
				parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
				parPiPosMom->SetParameters(1050.82,1.07492,0.16913,0.354773,0.253396); Multiplicities[4] = 627950;
				parPiNegMom->SetParameters(1134.48,1.05525,0.170399,0.35685,0.241421); Multiplicities[5] = 668192;
				parKaPosMom->SetParameters(316.545,0.193559,0.359936,0.0945873,0.283851); Multiplicities[6] = 63758;
				parKaNegMom->SetParameters(174.667,0.280725,0.316105,0.101935,0.293752); Multiplicities[7] = 35483;
				amplParam = 872.525;
				parPrPosMom->SetParameters(amplParam,0.440378,0.475486,0.207794,0.440092); Multiplicities[8] = 170999;
				amplParam /= prrat;
				parPrNegMom->SetParameters(amplParam,0.440378,0.475486,0.207794,0.440092); Multiplicities[9] = 3259;
				parDeMom->SetParameters(0.,1.,1.,1.,1.);
				parTrMom->SetParameters(0.,1.,1.,1.,1.);
				parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
				parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
			}
		}
		
		else /// Tracking == "CF"
		{
			if (fEnergy < 7.0) // not ready
			{
				parElPosMom->SetParameters(0.,1.,1.,1.,1.);
				parElNegMom->SetParameters(0.,1.,1.,1.,1.);
				parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
				parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
				parPiPosMom->SetParameters(307.0,0.035,0.175,0.127,0.139); // QGSM 5 gev
				parPiNegMom->SetParameters(325.6,0.035,0.175,0.127,0.139); // QGSM 5 gev
				parKaPosMom->SetParameters(15.3,0.236,0.203,0.056,0.494); // QGSM 5 gev
				parKaNegMom->SetParameters(8.88,0.236,0.203,0.056,0.494); // QGSM 5 gev
				amplParam = 104.0;
				parPrPosMom->SetParameters(amplParam,0.213,0.294,0.09,0.938); // QGSM 5 gev
				amplParam /= prrat;
				parPrNegMom->SetParameters(amplParam,0.213,0.294,0.09,0.938); // QGSM 5 gev
				parDeMom->SetParameters(0.,1.,1.,1.,1.);
				parTrMom->SetParameters(0.,1.,1.,1.,1.);
				parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
				parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
			}
			else /// fEnergy > 7.0, parameterization 8 GeV (27/07/2017) 10K events
			{
				prrat = 282.25;
				parElPosMom->SetParameters(0.,1.,1.,1.,1.);
				parElNegMom->SetParameters(0.,1.,1.,1.,1.);
				parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
				parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
				parPiPosMom->SetParameters(3360.54,1.08086,0.151008,0.330264,0.351397); Multiplicities[4] = 2020338;
				parPiNegMom->SetParameters(3530.8,1.14233,0.147187,0.328013,0.358974); Multiplicities[5] = 2205162;
				parKaPosMom->SetParameters(1206.47,0.245233,0.333437,0.0996193,0.327909); Multiplicities[6] = 241612;
				parKaNegMom->SetParameters(529.028,0.288553,0.303136,0.0948728,0.376275); Multiplicities[7] = 106440;
				amplParam = 4408.45;
				parPrPosMom->SetParameters(amplParam,0.666617,0.416237,0.215083,0.487749); Multiplicities[8] = 870192;
				amplParam /= prrat;
				parPrNegMom->SetParameters(amplParam,0.666617,0.416237,0.215083,0.487749); Multiplicities[9] = 3083;
				parDeMom->SetParameters(0.,1.,1.,1.,1.);
				parTrMom->SetParameters(0.,1.,1.,1.,1.);
				parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
				parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
			}
		}
	}
	
	else if (Generator == "DEFAULT")
	{
		parElPosMom->SetParameters(0.,1.,1.,1.,1.);
		parElNegMom->SetParameters(0.,1.,1.,1.,1.);
		parMuPosMom->SetParameters(0.,1.,1.,1.,1.);
		parMuNegMom->SetParameters(0.,1.,1.,1.,1.);
		parPiPosMom->SetParameters(503.,0.035,0.203,0.668,0.139); // average 9 gev
		parPiNegMom->SetParameters(533.4,0.035,0.203,0.668,0.139); // average 9 gev
		parKaPosMom->SetParameters(29.3,0.17,0.27,0.06,0.494); // average 9 gev
		parKaNegMom->SetParameters(17.,0.17,0.27,0.06,0.494); // average 9 gev
		amplParam = 88.;
		parPrPosMom->SetParameters(amplParam,0.18,0.37,0.15,0.938); // average 9 gev
		amplParam /= prrat;
		parPrNegMom->SetParameters(amplParam,0.18,0.37,0.15,0.938); // average 9 gev
		parDeMom->SetParameters(0.,1.,1.,1.,1.);
		parTrMom->SetParameters(0.,1.,1.,1.,1.);
		parHe3Mom->SetParameters(0.,1.,1.,1.,1.);
		parHe4Mom->SetParameters(0.,1.,1.,1.,1.);
	}
	
	fgaus = new TF1("fgaus","gaus(0)",-1.,5.);
	fgaus2 = new TF2("fgaus2","[0]*TMath::Gaus(x,[1],[2])*TMath::Gaus(y,[3],[4])",-2.,10.,-1.,5.);
	fasymgaus = new TF1("fasymgaus", this, &MpdPid::AsymGaus, -1., 5., 4, "MpdPid", "AsymGaus");
	fasymgaus2 = new TF2("fasymgaus2", this, &MpdPid::AsymGaus2, -1., 5., -2., 10., 6, "MpdPid", "AsymGaus2");
}

Bool_t MpdPid::ReInitPartYield(Double_t *par, Int_t pdg)
{
	/// Number of parameters is always 5!
	Bool_t ans = kFALSE;
	switch (pdg)
	{
		case -11   : parElPosMom->SetParameters(par); ans = kTRUE; break;
		case  11   : parElNegMom->SetParameters(par); ans = kTRUE; break;
		case -13   : parMuPosMom->SetParameters(par); ans = kTRUE; break;
		case  13   : parMuNegMom->SetParameters(par); ans = kTRUE; break;
		case  211  : parPiPosMom->SetParameters(par); ans = kTRUE; break;
		case -211  : parPiNegMom->SetParameters(par); ans = kTRUE; break;
		case  321  : parKaPosMom->SetParameters(par); ans = kTRUE; break;
		case -321  : parKaNegMom->SetParameters(par); ans = kTRUE; break;
		case  2212 : parPrPosMom->SetParameters(par); ans = kTRUE; break;
		case -2212 : parPrNegMom->SetParameters(par); ans = kTRUE; break;
		case PDG_DEUTERON : parDeMom->SetParameters(par); ans = kTRUE; break;
		case PDG_TRITON   : parTrMom->SetParameters(par); ans = kTRUE; break;
		case PDG_HE3      : parHe3Mom->SetParameters(par); ans = kTRUE; break;
		case PDG_HE4      : parHe4Mom->SetParameters(par); ans = kTRUE; break;
		default: cout << "Incorrect PID species!" << endl;
	}
	return ans;
}

Double_t MpdPid::GetDedxElParam(Double_t p){
   Double_t dedx=parElBB->Eval(p);
   if (fTracking)
   {
	   if (p<0.15) dedx *= 0.98;
	   if (p<0.1) dedx *= 1.025;
   }
   return dedx;
}

Double_t MpdPid::GetDedxMuParam(Double_t p){
   Double_t dedx=parMuBB->Eval(p);
   if (fTracking)
   {
	   if (p<0.1) dedx *= 0.96;
	   if (p>1.25) dedx *= 1.005;
	   if (p>1.9) dedx *= 0.985;
   }
   else
   {
	   if (p<0.15) dedx *= 0.97;
	   if (p<0.2) dedx *= 1.015;
	   if (p>1.35) dedx *= 1.005;
	   if (p>1.6) dedx *= 1.005;
	   if (p>1.9) dedx *= 0.985;
   }
   return dedx;
}

Double_t MpdPid::GetDedxPiParam(Double_t p)
{
	Double_t dedx=parPiBB->Eval(p);
	if (fTracking)
	{
		if (p<0.3) dedx *= 0.99;
		if (p<0.25) dedx *= 1.01;
		if (p<0.2) dedx *= 1.01;
		if (p<0.15) dedx *= 0.965;
		if (p<0.1) dedx *= 0.845;
		if (p>2.3) dedx *= 0.99;
		if (p>2.7) dedx *= 0.975;
	}
	else
	{
		if (p<0.1) dedx *= 0.895;
		if (p<0.15) dedx *= 0.955;
		if (p<0.25) dedx *= 1.02;
		if (p<0.3) dedx *= 0.99;
		if (p>1.7) dedx *= 1.005;
		if (p>2.5) dedx *= 0.99;
	}
   return dedx;
}

Double_t MpdPid::GetDedxKaParam(Double_t p)
{
	Double_t dedx=parKaBB->Eval(p);
	if (fTracking)
	{
		if (p<0.15) dedx *= 0.89; 
		if (p<0.2) dedx *= 0.965; 
		if (p<0.25) dedx *= 1.02; 
		if (p<0.3) dedx *= 1.03; 
		if (p<0.45) dedx *= 0.99; 
		if (p>1.1) dedx *= 1.015; 
		if (p>1.7) dedx *= 1.01;
		if (p>2.1) dedx *= 1.015;
	}
	else
	{
		if (p<0.15) dedx *= 0.98;
		if (p<0.2) dedx *= 0.97;
		if (p<0.25) dedx *= 1.025;
		if (p<0.3) dedx *= 1.02;
		if (p<0.4) dedx *= 0.99;
		if (p<0.45) dedx *= 0.99;
		if (p<0.5) dedx *= 1.01;
		if (p>1.05) dedx *= 1.015;
		if (p>1.5) dedx *= 0.985;
		if (p>1.8) dedx *= 0.99;
		if (p>2.1) dedx *= 0.99;
		if (p>2.2) dedx *= 0.99;
		if (p>2.4) dedx *= 0.99;
		if (p>2.7) dedx *= 0.985;
	}
	return dedx;
}

Double_t MpdPid::GetDedxPrParam(Double_t p)
{
	Double_t dedx=parPrBBMerged->Eval(p);
	if (fTracking)
	{
		if ( (p>0.05) && (p<0.1) ) dedx *= 0.896244;
		if ( (p>0.1) && (p<0.15) ) dedx *= 0.938972;
		if ( (p>0.15) && (p<0.2) ) dedx *= 1.05937;
		if ( (p>0.2) && (p<0.25) ) dedx *= 1.08904;
		if (p<0.55) dedx *= 0.985;
		if (p<0.45) dedx *= 1.015;
		if (p>1.35) dedx *= 0.99;
		if (p>2.0) dedx *= 1.02;
		if (p>2.2) dedx *= 1.01;
	}
	else
	{
		if (p<0.1) dedx *= 0.69;
		if (p<0.15) dedx *= 0.94;
		if (p<0.2) dedx *= 0.955;
		if (p<0.25) dedx *= 0.965;
		if (p<0.3) dedx *= 0.975;
		if (p<0.45) dedx *= 1.03;
		if (p<0.55) dedx *= 0.97;
		if (p<0.6) dedx *= 1.03;
		if (p<0.65) dedx *= 0.975;
		if (p<0.7) dedx *= 1.01;
		if (p>2.0) dedx *= 1.01;
		if (p>2.4) dedx *= 0.99;
	}
	return dedx;
}

Double_t MpdPid::GetDedxDeParam(Double_t p)
{
	Double_t dedx;
	if (fTracking)
	{
		if (p<parDePol1->GetXmax()) dedx=parDePol1->Eval(p);
		else if ( (p>=parDePol2->GetXmin()) && (p<parDePol2->GetXmax()) ) dedx=parDePol2->Eval(p);
		else dedx=parDeBB->Eval(p);
	}
	else dedx=parDeBB->Eval(p);
	return dedx;
}

Double_t MpdPid::GetDedxTrParam(Double_t p)
{
	Double_t dedx;
	if (fTracking)
	{
		if (p<parTrPol1->GetXmax()) dedx=parTrPol1->Eval(p);
		else if ( (p>=parTrPol2->GetXmin()) && (p<parTrPol2->GetXmax()) ) dedx=parTrPol2->Eval(p);
		else dedx=parTrBB->Eval(p);
	}
	else dedx=parTrBB->Eval(p);
	return dedx;
}

Double_t MpdPid::GetDedxHe3Param(Double_t p)
{
	Double_t dedx;
	if (fTracking)
	{
		if (p<parHe3Pol1->GetXmax()) dedx=parHe3Pol1->Eval(p);
		else if ( (p>=parHe3Pol2->GetXmin()) && (p<parHe3Pol2->GetXmax()) ) dedx=parHe3Pol2->Eval(p);
		else if ( (p>=parHe3Pol3->GetXmin()) && (p<parHe3Pol3->GetXmax()) ) dedx=parHe3Pol3->Eval(p);
		else dedx=parHe3BB->Eval(p);
	}
	else dedx=parHe3BB->Eval(p);
	return dedx;
}

Double_t MpdPid::GetDedxHe4Param(Double_t p)
{
	Double_t dedx;
	if (fTracking)
	{
		if (p<parHe4Pol1->GetXmax()) dedx=parHe4Pol1->Eval(p);
		else if ( (p>=parHe4Pol2->GetXmin()) && (p<parHe4Pol2->GetXmax()) ) dedx=parHe4Pol2->Eval(p);
		else dedx=parHe4BB->Eval(p);
	}
	else dedx=parHe4BB->Eval(p);
	return dedx;
}

Bool_t MpdPid::FillProbs(MpdTrack* track)
{
   if (track == 0) return (kFALSE);
   Double_t px=track->GetPx(),py=track->GetPy(),pz=track->GetPz();   
   Double_t p=TMath::Sqrt(px*px+py*py+pz*pz);       
///
/// NO parameterisation beyond p=3.0 GeV/c
/// ignore anti-d,-He3 and -He4 
///
   if (p > 3.0) return (kFALSE);
   Int_t charge = 1;
   if (track->GetPt() > 0) charge = -1;
   Int_t flag = track->GetTofFlag();
   Double_t dedx = track->GetdEdXTPC();
   //Double_t eta = track->GetEta();
   
   if (flag == 2 || flag == 6){
      Double_t m2 = track->GetTofMass2();
      FillProbs(p,dedx,m2,charge);
   }else{
      FillProbs(p,dedx,charge);   
   }

   return kTRUE;
}

Bool_t MpdPid::FillProbs(MpdTrack* track, Double_t dedx){
   if (track == 0) return (kFALSE);
      
   Double_t px=track->GetPx(),py=track->GetPy(),pz=track->GetPz();   
   Double_t p=TMath::Sqrt(px*px+py*py+pz*pz);       
///
/// NO parameterisation beyond p=3.0 GeV/c
/// ignore anti-d,-He3 and -He4 
///
   if (p > 3.0) return (kFALSE);
   Int_t charge = 1;
   if (track->GetPt() > 0.) charge = -1;
      
   Int_t flag = track->GetTofFlag();
   //Double_t eta = track->GetEta();
   
   if (flag == 2 || flag == 6){
      Double_t m2 = track->GetTofMass2();
      FillProbs(p,dedx,m2,charge);
   }else{
      FillProbs(p,dedx,charge);
   }

   return kTRUE;
}

Bool_t MpdPid::FillProbs(Double_t p, Double_t dedx, Int_t charge){
	
	///
	/// NO parameterisation beyond p=3.0 GeV/c
	/// ignore anti-d,-He3 and -He4 
	///
	
	if (p > 3.0) return (kFALSE);
	
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
	
	///
	/// Params. for dE/dx sigma versus total momentum.
	/// These are koef*<dE/dx> , koef=0.07
	/// Deviations from Bethe-Bloch at low-p accounted by somewhat larger sigmas
	///
	
	Double_t emeanel=GetDedxElParam(p);
	Double_t emeanmu=GetDedxMuParam(p);
	Double_t emeanpi=GetDedxPiParam(p);
	Double_t emeanpr=GetDedxPrParam(p);
	Double_t emeanka=GetDedxKaParam(p);
	Double_t emeande=GetDedxDeParam(p);
	Double_t emeantr=GetDedxTrParam(p);
	Double_t emeanhe3=GetDedxHe3Param(p);
	Double_t emeanhe4=GetDedxHe4Param(p);
	
	Double_t sigeel,sigemu,sigepi,sigepr,sigeka,sigede,sigetr,sigehe3,sigehe4;
	Double_t fel=0.,fmu=0.,fpi=0.,fka=0.,fpr=0.,fde=0.,ftr=0.,fhe3=0.,fhe4=0.;
	Double_t fsum=0., cut=4.0;
	
	if (kSigmaEloss>0.1) cut = kSigmaEloss;
	else return kFALSE;
	
	if (!fTracking)
	{
		sigemu=fSigmaDedx_1; if (p<0.15) sigemu=fSigmaDedx_2;
		sigeel=fSigmaDedx_3;
		sigepi=fSigmaDedx_1;
		sigeka=fSigmaDedx_1;
		sigepr=fSigmaDedx_1;
		if (p<0.15){ sigepi=fSigmaDedx_2; sigepr=fSigmaDedx_2; sigeka=fSigmaDedx_2; }
		sigede=fSigmaDedx_1;
		if (p<0.15)sigede=fSigmaDedx_2;
		if (p<0.45){sigede=fSigmaDedx_2; sigetr=fSigmaDedx_2; sigehe3=fSigmaDedx_2; sigehe4=fSigmaDedx_2;}
		sigetr=fSigmaDedx_1;
		sigehe3=fSigmaDedx_1;
		sigehe4=fSigmaDedx_1;
	}
	else
	{
		sigepi=GetDedxWidthValue(p,1);
		sigeka=GetDedxWidthValue(p,2);
		sigepr=GetDedxWidthValue(p,3);
		sigeel=GetDedxWidthValue(p,4);
		sigemu=GetDedxWidthValue(p,5);
		sigede=GetDedxWidthValue(p,6);
		sigetr=GetDedxWidthValue(p,7);
		sigehe3=GetDedxWidthValue(p,8);
		sigehe4=GetDedxWidthValue(p,9);
	}
	
	///
	/// Set prob=0. for differences greater than "n" of sigmas
	/// otherwise evaluation..
	fpi = GetDedxProb_asym(cut, p, dedx, npi, emeanpi, sigepi, 1);
	fka = GetDedxProb_asym(cut, p, dedx, nka, emeanka, sigeka, 2);
	fpr = GetDedxProb_asym(cut, p, dedx, npr, emeanpr, sigepr, 3);
	fel = GetDedxProb_asym(cut, p, dedx, nel, emeanel, sigeel, 4);
	fmu = GetDedxProb_asym(cut, p, dedx, nmu, emeanmu, sigemu, 5);
	fde = GetDedxProb_asym(cut, p, dedx, nde, emeande, sigede, 6);
	ftr = GetDedxProb_asym(cut, p, dedx, ntr, emeantr, sigetr, 7);
	fhe3 = GetDedxProb_asym(cut, p, dedx, nhe3, emeanhe3, sigehe3, 8);
	fhe4 = GetDedxProb_asym(cut, p, dedx, nhe4, emeanhe4, sigehe4, 9);
	
	///
	/// Normalization
	///
	
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
	} else {return kFALSE;}; /// outliers!
	
	return kTRUE;         
}

Bool_t MpdPid::FillProbs(Double_t p, Double_t dedx, Double_t m2, Int_t charge){
	
	///
	/// NO parameterisation beyond p=3.0 GeV/c
	/// ignore anti-d,-He3 and -He4 
	///
	
	if (p > 3.0) return (kFALSE);
	
	fCharge = charge;
	
	Double_t nel=0.,nmu=0.,npi=0.,nka=0.,npr=0.,nde=0.,ntr=0.,nhe3=0.,nhe4=0.;
	
	///
	/// particle multiplicities for positives and negatives separately
	///
	
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
	
	///
	/// Params. for dE/dx sigma versus total momentum.
	/// These are koef*<dE/dx> , koef=0.06
	/// Deviations from Bethe-Bloch at low-p accounted by somewhat larger sigmas
	///
	
	Double_t emeanel=GetDedxElParam(p);
	Double_t emeanmu=GetDedxMuParam(p);
	Double_t emeanpi=GetDedxPiParam(p);
	Double_t emeanpr=GetDedxPrParam(p);
	Double_t emeanka=GetDedxKaParam(p);
	Double_t emeande=GetDedxDeParam(p);
	Double_t emeantr=GetDedxTrParam(p);
	Double_t emeanhe3=GetDedxHe3Param(p);
	Double_t emeanhe4=GetDedxHe4Param(p);
	
	Double_t sigeel,sigemu,sigepi,sigepr,sigeka,sigede,sigetr,sigehe3,sigehe4;
	
	if (!fTracking)
	{
		sigeel=fSigmaDedx_3;
		sigemu=fSigmaDedx_1; if (p<0.15) sigemu=fSigmaDedx_2;
		sigepi=fSigmaDedx_1;
		sigeka=fSigmaDedx_1;
		sigepr=fSigmaDedx_1;
		if (p<0.15) {sigepi=fSigmaDedx_2; sigepr=fSigmaDedx_2; sigeka=fSigmaDedx_2;}
		sigede=fSigmaDedx_1;
		sigetr=fSigmaDedx_1;
		sigehe3=fSigmaDedx_1;
		sigehe4=fSigmaDedx_1;
		if (p<0.15) sigede=fSigmaDedx_2;
		if (p<0.45){ sigede=fSigmaDedx_2; sigetr=fSigmaDedx_2; sigehe3=fSigmaDedx_2; sigehe4=fSigmaDedx_2; }
	}
	else
	{
		sigepi=GetDedxWidthValue(p,1);
		sigeka=GetDedxWidthValue(p,2);
		sigepr=GetDedxWidthValue(p,3);
		sigeel=GetDedxWidthValue(p,4);
		sigemu=GetDedxWidthValue(p,5);
		sigede=GetDedxWidthValue(p,6);
		sigetr=GetDedxWidthValue(p,7);
		sigehe3=GetDedxWidthValue(p,8);
		sigehe4=GetDedxWidthValue(p,9);
	}
	
	Double_t p_calc = 3.0; /// Above p=3.0 GeV/c param. for sig_M2 is bad!
	if (p < p_calc) p_calc = p;
	
	Double_t sigmel=parElM2->Eval(p_calc), sigmmu=parMuM2->Eval(p_calc);
	Double_t sigmpi;
	if(p<=1.4) sigmpi=parPiLowPM2->Eval(p_calc); else sigmpi=parPiHighPM2->Eval(p_calc);
	Double_t sigmka=parKaM2->Eval(p_calc);
	Double_t sigmpr;
	if(p<=1.4) sigmpr=parPrLowPM2->Eval(p_calc); else sigmpr=parPrHighPM2->Eval(p_calc);
	Double_t sigmde = parDeM2->Eval(p_calc);
	Double_t sigmtr=parTrM2->Eval(p_calc);
	Double_t sigmhe3=parHe3M2->Eval(p_calc), sigmhe4=parHe4M2->Eval(p_calc);
	
	Double_t fel=0.,fmu=0.,fpi=0.,fka=0.,fpr=0.,fde=0.,ftr=0.,fhe3=0.,fhe4=0.;
	Double_t fsum=0., cut_dedx, cut_m2;
	
	if ( (kSigmaEloss > 0.1) && (kSigmaTof > 0.1) )
	{ cut_dedx = kSigmaEloss; cut_m2 = kSigmaTof;}
	else return kFALSE;
	
	///
	/// Set prob=0. for differences greater than n-sigmas
	/// otherwise evaluation..
	///
	
	Double_t xx, yy;//, distance;
	
	xx = (dedx/emeanel-1.)/sigeel;
	yy = (m2-0.0007)/sigmel;
	if ( (TMath::Abs(xx) < cut_dedx) && (TMath::Abs(yy) < cut_m2) ){
		fgaus2->SetParameters(nel,0.002,sigmel,1.,sigeel);
		fel=fgaus2->Eval(m2,dedx/emeanel);
	} else fel=0.;
	
	fpi = GetCombProb_asym(cut_dedx, cut_m2, p, dedx, m2, npi, emeanpi, 0.019, sigepi, sigmpi, 1);
	fka = GetCombProb_asym(cut_dedx, cut_m2, p, dedx, m2, nka, emeanka, 0.24, sigeka, sigmka, 2);
	fpr = GetCombProb_asym(cut_dedx, cut_m2, p, dedx, m2, npr, emeanpr, 0.887, sigepr, sigmpr, 3);
	fmu = GetCombProb_asym(cut_dedx, cut_m2, p, dedx, m2, nmu, emeanmu, 0.011, sigemu, sigmmu, 5);
	fde = GetCombProb_asym(cut_dedx, cut_m2, p, dedx, m2, nde, emeande, 3.54, sigede, sigmde, 6);
	ftr = GetCombProb_asym(cut_dedx, cut_m2, p, dedx, m2, ntr, emeantr, 7.87, sigetr, sigmtr, 7);
	fhe3 = GetCombProb_asym(cut_dedx, cut_m2, p, dedx, m2, nhe3, emeanhe3, 1.983, sigehe3, sigmhe3, 8);
	fhe4 = GetCombProb_asym(cut_dedx, cut_m2, p, dedx, m2, nhe4, emeanhe4, 3.51, sigehe4, sigmhe4, 9);
	
	///
	/// Normalization
	///
	
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
	} else {return kFALSE;};/// outliers!
	
	return kTRUE;
}

Long_t MpdPid::GetMaxProb() 
{
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

Double_t MpdPid::GetNsigmaToBetheBloch(TString specie)
{
	Double_t ans = -1.0;
	if (specie.EqualTo("pi")) ans = dEdXSigmasArray[0];
	else if (specie.EqualTo("ka")) ans = dEdXSigmasArray[1];
	else if (specie.EqualTo("pr")) ans = dEdXSigmasArray[2];
	else if (specie.EqualTo("el")) ans = dEdXSigmasArray[3];
	else if (specie.EqualTo("mu")) ans = dEdXSigmasArray[4];
	else if (specie.EqualTo("de")) ans = dEdXSigmasArray[5];
	else if (specie.EqualTo("tr")) ans = dEdXSigmasArray[6];
	else if (specie.EqualTo("he3")) ans = dEdXSigmasArray[7];
	else if (specie.EqualTo("he4")) ans = dEdXSigmasArray[8];
	return ans;
}

Double_t MpdPid::GetNsigmaToAverageMass2(TString specie)
{
	Double_t ans = -1.0;
	if (specie.EqualTo("pi")) ans = m2SigmasArray[0];
	else if (specie.EqualTo("ka")) ans = m2SigmasArray[1];
	else if (specie.EqualTo("pr")) ans = m2SigmasArray[2];
	else if (specie.EqualTo("el")) ans = m2SigmasArray[3];
	else if (specie.EqualTo("mu")) ans = m2SigmasArray[4];
	else if (specie.EqualTo("de")) ans = m2SigmasArray[5];
	else if (specie.EqualTo("tr")) ans = m2SigmasArray[6];
	else if (specie.EqualTo("he3")) ans = m2SigmasArray[7];
	else if (specie.EqualTo("he4")) ans = m2SigmasArray[8];
	return ans;
}

ClassImp(MpdPid);