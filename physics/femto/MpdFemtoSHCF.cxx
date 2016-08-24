/*
 * MpdFemtoSHCF.cxx
 *
 *  Created on: 28 kwi 2016
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdFemtoSHCF.h"
#include <iostream>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_complex.h>

//#define _FINISH_DEBUG_
MpdFemtoSHCF::MpdFemtoSHCF() :
	fMaxJM(0),
	fScale(0),
	fNormMin(0),
	fNormMax(0.5),
	fNumReal(NULL),
	fNumImag(NULL),
	fDenReal(NULL),
	fDenImag(NULL),
	fCovNum(NULL),
	fCovDen(NULL),
	fLabels(NULL),
	fComment(""),
	fCFReal(NULL),
	fCFImag(NULL),
	fFactorialsSize(0),
	covmnum(NULL),
	covmden(NULL),
	covmcfc(NULL),
	fNormPurity(0),
	fNormRadius(0),
	fNormBohr(0),
	fEls(NULL),
	fEms(NULL),
	fElsi(NULL),
	fEmsi(NULL),
	fFactorials(NULL),
	fCfcov(NULL)
{
	gSystem->Load("libgsl.so");
	gSystem->Load("libgslcblas.so");
}

MpdFemtoSHCF::MpdFemtoSHCF(Int_t maxl) :
	fMaxJM((maxl+1)*(maxl+1)),
	fScale(0),
	fNormMin(0),
	fNormMax(0.5),
	fNumReal(NULL),
	fNumImag(NULL),
	fDenReal(NULL),
	fDenImag(NULL),
	fCovNum(NULL),
	fCovDen(NULL),
	fLabels(NULL),
	fComment(""),
	fCFReal(NULL),
	fCFImag(NULL),
	fFactorialsSize((maxl+1)*4),
	covmnum(NULL),
	covmden(NULL),
	covmcfc(NULL),
	fNormPurity(0),
	fNormRadius(0),
	fNormBohr(0),
	fEls(NULL),
	fEms(NULL),
	fElsi(NULL),
	fEmsi(NULL),
	fFactorials(NULL),
	fCfcov(NULL)
{
	int el = 0;
	int em = 0;
	int il = 0;
	fEls = new Double_t[fMaxJM];
	fEms = new Double_t[fMaxJM];
	fElsi = new Double_t[fMaxJM];
	fEmsi = new Double_t[fMaxJM];
	do {
		fEls[il] = el;
		fEms[il] = em;
		fElsi[il] = (int) el;
		fEmsi[il] = (int) em;

#ifdef _FINISH_DEBUG_
		std::cout << "il el em " << il << " " << fElsi[il] << " " << fEmsi[il] << std::endl;
#endif
		em++;
		il++;
		if (em > el) {
			el++;
			em = -el;
		}
	} while (el <= maxl);

	fFactorials = new Double_t[fFactorialsSize];
	Double_t fac = 1;
	fFactorials[0] = 1;
	for (int iter = 1; iter < fFactorialsSize; iter++) {
		fac *= iter;
		fFactorials[iter] = fac;
	}
	gSystem->Load("libgsl.so");
	gSystem->Load("libgslcblas.so");
}

TH1D* MpdFemtoSHCF::GetCFRe(int el, int em) const{
	if (GetIndexForLM(el, em)>=0&&fCFReal!=NULL){
		return fCFReal[GetIndexForLM(el, em)];
	}else{
		return NULL;
	}
}

TH1D* MpdFemtoSHCF::GetCFIm(int el, int em) const{
	if (GetIndexForLM(el, em)>=0&&fCFImag!=NULL){
		return fCFImag[GetIndexForLM(el, em)];
	 }else{
		 return NULL;
	 }
}

TH1D* MpdFemtoSHCF::GetNumRe(int el, int em) const {
	  if (GetIndexForLM(el, em)>=0)
	    return fNumReal[GetIndexForLM(el, em)];
	  else
	    return NULL;
}

TH1D* MpdFemtoSHCF::GetNumIm(int el, int em) const {
	  if (GetIndexForLM(el, em)>=0)
	    return fNumImag[GetIndexForLM(el, em)];
	  else
	    return NULL;
}

TH1D* MpdFemtoSHCF::GetDenRe(int el, int em) const {
	  if (GetIndexForLM(el, em)>=0)
	    return fDenReal[GetIndexForLM(el, em)];
	  else
	    return NULL;
}

TH1D* MpdFemtoSHCF::GetDenIm(int el, int em) const {
	  if (GetIndexForLM(el, em)>=0)
	    return fDenImag[GetIndexForLM(el, em)];
	  else
	    return NULL;
}

Int_t MpdFemtoSHCF::GetIndexForLM(int el, int em) const
{
  for (int iter=0; iter<fMaxJM; iter++)
    if ((el == fElsi[iter]) && (em == fEmsi[iter]))
      return iter;
  return -1;
}

Int_t MpdFemtoSHCF::GetBin(int qbin, int ilmzero, int zeroimag, int ilmprim,
		int primimag) const {
	  return (qbin*fMaxJM*fMaxJM*4 +
		  (ilmprim*2 + primimag) *fMaxJM*2 +
		  ilmzero*2 + zeroimag);
}

void MpdFemtoSHCF::SetNumRe(TH1D** histograms, Bool_t clone) {
	if(clone){
		if(fNumReal==NULL)
			fNumReal = new TH1D*[fMaxJM];
		for(int i=0;i<fMaxJM;i++){
			fNumReal[i] = (TH1D*)histograms[i]->Clone();
		}
	}else{
		fNumReal = histograms;
	}
}

void MpdFemtoSHCF::SetNumIm(TH1D** histograms, Bool_t clone) {
	if(clone){
		if(fNumImag==NULL)
			fNumImag = new TH1D*[fMaxJM];
		for(int i=0;i<fMaxJM;i++){
			fNumImag[i] = (TH1D*)histograms[i]->Clone();
		}
	}else{
		fNumImag = histograms;
	}
}

void MpdFemtoSHCF::SetDenRe(TH1D** histograms, Bool_t clone) {
	if(clone){
		if(fDenReal==NULL)
			fDenReal = new TH1D*[fMaxJM];
		for(int i=0;i<fMaxJM;i++){
			fDenReal[i] = (TH1D*)histograms[i]->Clone();
		}
	}else{
		fDenReal = histograms;
	}
}

void MpdFemtoSHCF::SetDenIm(TH1D** histograms, Bool_t clone) {
	if(clone){
		if(fDenImag==NULL)
			fDenImag = new TH1D*[fMaxJM];
		for(int i=0;i<fMaxJM;i++){
			fDenImag[i] = (TH1D*)histograms[i]->Clone();
		}
	}else{
		fDenImag = histograms;
	}
}

void MpdFemtoSHCF::SetCovMatrix(TH3D* num, TH3D* den, Bool_t clone) {
	if(clone){
		fCovDen = (TH3D*)den->Clone();
		fCovNum = (TH3D*)num->Clone();
	}else{
		fCovDen = den;
		fCovNum = num;
	}
}

void MpdFemtoSHCF::PackCfcCovariance()
{
	char bufname[200];

	if (fCfcov) delete fCfcov;
	sprintf(bufname, "CovCfc%s", fNumReal[0]->GetName()+10);
	fCfcov  = new TH3D(bufname,bufname,
		     fCFReal[0]->GetNbinsX(), fCFReal[0]->GetXaxis()->GetXmin(), fCFReal[0]->GetXaxis()->GetXmax(),
		     GetMaxJM()*2, -0.5, GetMaxJM()*2 - 0.5,
		     GetMaxJM()*2, -0.5, GetMaxJM()*2 - 0.5);

	double tK;
	for (int ibin=1; ibin<=fCfcov->GetNbinsX(); ibin++)
		for (int ilmz=0; ilmz<GetMaxJM()*2; ilmz++)
			for (int ilmp=0; ilmp<GetMaxJM()*2; ilmp++) {
				tK = covmcfc[GetBin(ibin-1, ilmz/2, ilmz%2, ilmp/2, ilmp%2)];
// 	tE = fCFReal[0]->GetEntries();
// 	if (ilmz%2) {
// 	  if (ilmp%2) {
// 	    tB = fCFImag[ilmz/2]->GetBinContent(ibin)*fCFImag[ilmp/2]->GetBinContent(ibin);
// 	  }
// 	  else {
// 	    tB = fCFImag[ilmz/2]->GetBinContent(ibin)*fCFReal[ilmp/2]->GetBinContent(ibin);
// 	  }
// 	}
// 	else {
// 	  if (ilmp%2) {
// 	    tB = fCFReal[ilmz/2]->GetBinContent(ibin)*fCFImag[ilmp/2]->GetBinContent(ibin);
// 	  }
// 	  else {
// 	    tB = fCFReal[ilmz/2]->GetBinContent(ibin)*fCFReal[ilmp/2]->GetBinContent(ibin);
// 	  }
// 	}

				fCfcov->SetBinContent(ibin, ilmz+1, ilmp+1, tK);
			}
	fCfcov->SetBinContent(0,0,0,1.0);
}

void MpdFemtoSHCF::GetElEmForIndex(int aIndex, double & aEl,
	double& aEm) const {
	aEl = fEls[aIndex];
	aEm = fEms[aIndex];
}

void MpdFemtoSHCF::GetElEmForIndex(int aIndex, int &aEl,
	int & aEm) const {
	 aEl = fEls[aIndex];
	aEm = fEms[aIndex];
}

void MpdFemtoSHCF::NormalizeBy(int el, int em,Option_t *opt) {
	TString option = opt;
	TH1D *num = NULL;
	TH1D *den = NULL;
	if(option.EqualTo("re")){
		num = GetNumRe(el,em);
		den = GetDenRe(el,em);
	}else{
		num = GetNumIm(el,em);
		den = GetDenIm(el,em);
		if(num)
		num->SetLineColor(kRed);
	}
	fScale  =1.0;
	return;
	Double_t n_integral, d_integral;
	if(fNormMax==fNormMin){
		n_integral = num->Integral(1,num->GetNbinsX());
		d_integral = den->Integral(1,den->GetNbinsX());
	}else{
		n_integral = num->Integral(num->GetXaxis()->FindBin(fNormMin),num->GetXaxis()->FindBin(fNormMax));
		d_integral = den->Integral(den->GetXaxis()->FindBin(fNormMin),den->GetXaxis()->FindBin(fNormMax));
	}
	fScale = d_integral/n_integral;
}

void MpdFemtoSHCF::AddLabel(TString label) {
	if(fLabels==NULL){
		fLabels = new TList();
		fLabels->SetName("Labels");
	}
	fLabels->AddLast(new TObjString(label));
}

void MpdFemtoSHCF::GetIndependentLM(int ibin, int &el, int &em, int &im) const{
	int cbin = ibin;
	if (cbin == 0) {
		el = 0;
		em = 0;
		im = 0;
		return;
	} else
		cbin--;
	if (cbin == 0) {
		el = 2;
		em = 0;
		im = 0;
		return;
	} else
		cbin--;
	im = cbin % 2;
	el = 2;
	em = cbin / 2 + 1;
	return;
}

void MpdFemtoSHCF::InvertYlmIndependentMatrix(double *inmat, double *outmat)
{
	// Invert the Ylm matrix by inverting only the matrix
	// with independent elements and filling in the rest
	// according to sign rules
	double mU[GetMaxJM() * GetMaxJM() * 4];
	int isize = PackYlmMatrixIndependentOnly(inmat, mU);
	//  std::cout << "Independent count " << isize << std::endl;
	gsl_matrix_view matU = gsl_matrix_view_array(mU, isize, isize);
	// Identity matrix helper for inversion
	double mI[GetMaxJM() * GetMaxJM() * 4];
	for (int iterm = 0; iterm < isize; iterm++)
		for (int iterp = 0; iterp < isize; iterp++)
			if (iterm == iterp)
				mI[iterm * isize + iterp] = 1.0;
			else
				mI[iterm * isize + iterp] = 0.0;
	gsl_matrix_view matI = gsl_matrix_view_array(mI, isize, isize);
	// Invert the matrix
	gsl_blas_dtrsm(CblasLeft, CblasUpper, CblasNoTrans, CblasNonUnit, 1.0,
			&matU.matrix, &matI.matrix);
	UnPackYlmMatrixIndependentOnly(mI, outmat, isize);
}

void MpdFemtoSHCF::UnpackCovariances() {
	std::cout << "Unpacking covariances" << std::endl;
	if (covmnum) {
		delete[] covmnum;
	}
	if (covmden) {
		delete[] covmden;
	}
	if (covmcfc) {
		delete[] covmcfc;
	}
	covmnum = new Double_t[fMaxJM * 4 * fMaxJM * fCovNum->GetNbinsX()];
	covmden = new Double_t[fMaxJM * 4 * fMaxJM * fCovDen->GetNbinsX()];
	covmcfc = new Double_t[fMaxJM * 4 * fMaxJM * fCovDen->GetNbinsX()];
	Bool_t nanfound = kFALSE;
	if (fCovNum) {
		for (int ibin = 1; ibin <= fCovNum->GetNbinsX(); ibin++){
			for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++){
				for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++){
					covmnum[GetBin(ibin - 1, ilmz / 2, ilmz % 2, ilmp / 2,
							ilmp % 2)] = fCovNum->GetBinContent(ibin, ilmz + 1,
							ilmp + 1);
					if(TMath::IsNaN(fCovNum->GetBinContent(ibin, ilmz + 1,
							ilmp + 1))){
						nanfound = kTRUE;
					}
				}
			}
		}
	}

	if(nanfound){
		std::cout<<"NaN found in  numerator covariance matrix !" <<std::endl;
	}
	nanfound = kFALSE;
	if (fCovDen) {
		for (int ibin = 1; ibin <= fCovDen->GetNbinsX(); ibin++){
			for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++){
				for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++){
					covmden[GetBin(ibin - 1, ilmz / 2, ilmz % 2, ilmp / 2,
							ilmp % 2)] = fCovDen->GetBinContent(ibin, ilmz + 1,
							ilmp + 1);
				if(TMath::IsNaN( fCovDen->GetBinContent(ibin, ilmz + 1,ilmp + 1))){
					nanfound = kTRUE;
				}
				}
			}
		}
	}
	if(nanfound){
		std::cout <<"NaN found in  denominator covariance matrix !" <<std::endl;
	}

	/*  if (covcfc) {
	 for (int ibin=1; ibin<=covcfc->GetNbinsX(); ibin++)
	 for (int ilmz=0; ilmz<GetMaxJM()*2; ilmz++)
	 for (int ilmp=0; ilmp<GetMaxJM()*2; ilmp++)
	 covmcfc[GetBin(ibin-1, ilmz/2, ilmz%2, ilmp/2, ilmp%2)] = covcfc->GetBinContent(ibin, ilmz+1, ilmp+1);
	 }*/
}

void MpdFemtoSHCF::GetMtilde(std::complex<double>* aMat, double* aMTilde) {
	// Create the Mtilde for a given q bin
	double lzero, mzero;
	double lprim, mprim;
	double lbis, mbis;

	int lzeroi, mzeroi;
	int lprimi, mprimi;
	int lbisi, mbisi;

	for (int iz = 0; iz < GetMaxJM() * 2; iz++)
		for (int ip = 0; ip < GetMaxJM() * 2; ip++)
			aMTilde[iz * GetMaxJM() * 2 + ip] = 0.0;

	for (int izero = 0; izero < GetMaxJM(); izero++) {
		GetElEmForIndex(izero, lzero, mzero);
		GetElEmForIndex(izero, lzeroi, mzeroi);
		//     if (mzero < 0)
		//       continue;
		for (int ibis = 0; ibis < GetMaxJM(); ibis++) {
			GetElEmForIndex(ibis, lbis, mbis);
			GetElEmForIndex(ibis, lbisi, mbisi);

			//       if (mbis<0) continue;

			std::complex<double> val = std::complex<double>(0.0, 0.0);
			std::complex<double> mcomp[fMaxJM];
			for (int iprim = 0; iprim < GetMaxJM(); iprim++) {

				GetElEmForIndex(iprim, lprim, mprim);
				GetElEmForIndex(iprim, lprimi, mprimi);

				// 	if (mprim < 0 ) continue;

				if (abs(mzeroi) % 2)
					mcomp[iprim] = std::complex<double>(-1.0, 0.0); // (-1)^m
				else
					mcomp[iprim] = std::complex<double>(1.0, 0.0);

				mcomp[iprim] *= sqrt(
						(2 * lzero + 1) * (2 * lprim + 1) * (2 * lbis + 1)); // P1
				mcomp[iprim] *= WignerSymbol(lzero, 0, lprim, 0, lbis, 0); // W1
				mcomp[iprim] *= WignerSymbol(lzero, -mzero, lprim, mprim, lbis,
						mbis); // W2
				mcomp[iprim] *= aMat[iprim];
				//	if (
				val += mcomp[iprim];
			}

			aMTilde[(izero * 2) * (2 * GetMaxJM()) + (ibis * 2)] = real(val);
			aMTilde[(izero * 2 + 1) * (2 * GetMaxJM()) + (ibis * 2)] = imag(
					val);
			if (imag(val) != 0.0)
				aMTilde[(izero * 2) * (2 * GetMaxJM()) + (ibis * 2 + 1)] =
						-imag(val);
			else
				aMTilde[(izero * 2) * (2 * GetMaxJM()) + (ibis * 2 + 1)] = 0.0;
			aMTilde[(izero * 2 + 1) * (2 * GetMaxJM()) + (ibis * 2 + 1)] = real(
					val);
		}
	}
}

void MpdFemtoSHCF::UnPackYlmMatrixIndependentOnly(double *inmat,
		double *outmat, int insize) {
	int lmax = sqrt(insize) - 1;
	//  std::cout << "lmax is  " << lmax << std::endl;
	if (0) {
		lmax *= 2;
	}
	int tmax = (lmax + 1) * (lmax + 1) * 2;
	int indexfrom[tmax];
	int multfrom[tmax];

	int el, em;
	for (int iter = 0; iter < tmax; iter++) {
		int im = iter % 2;
		GetElEmForIndex(iter / 2, el, em);
		if (em == 0) {
			if (im == 1) {
				indexfrom[iter] = 0;
				multfrom[iter] = 0;
			} else {
				indexfrom[iter] = el * el;
				multfrom[iter] = 1;
			}
		} else if (em < 0) {
			indexfrom[iter] = (el * el) + (-em) * 2 - 1;
			if (im)
				indexfrom[iter]++;
			if ((-em) % 2)
				if (im)
					multfrom[iter] = 1;
				else
					multfrom[iter] = -1;
			else if (im)
				multfrom[iter] = -1;
			else
				multfrom[iter] = 1;
		} else if (em > 0) {
			indexfrom[iter] = (el * el) + (em) * 2 - 1;
			if (im)
				indexfrom[iter]++;
			multfrom[iter] = 1;
		}
	}

//   std::cout << "From Mult " << std::endl;
//   for (int iter=0; iter<tmax; iter++)
//     std::cout << indexfrom[iter] << " ";
//   std::cout << std::endl;
//   for (int iter=0; iter<tmax; iter++)
//     std::cout << multfrom[iter] << " ";
//   std::cout << std::endl;

	for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++)
		for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++)
			outmat[ilmz * GetMaxJM() * 2 + ilmp] = inmat[(indexfrom[ilmz]
					* insize) + indexfrom[ilmp]] * multfrom[ilmz]
					* multfrom[ilmp];
}

TH1D* MpdFemtoSHCF::GetHisto(int el,int em,Bool_t norm, Option_t* opt) const{
	TString option = opt;
	TH1D *cf = NULL;
	if(option.EqualTo("re")){
		cf = GetCFRe(el,em);
	}else{
		cf = GetCFIm(el,em);
	}
	cf = (TH1D*)cf->Clone();
	cf->GetYaxis()->SetTitle(Form("C^{%i}_{%i}",el,em));
	cf->SetTitle(Form("CF^{%i}_{%i}",el,em));
	cf->SetMinimum(0);
	cf->SetMaximum(cf->GetBinContent(cf->GetMaximumBin())*1.1);
	if(norm&&fScale!=0)
	cf->Scale(fScale);
	return cf;
}

TH1D* MpdFemtoSHCF::Histo(int lm, int em, Option_t* opt, Double_t scale) const {
	TH1D *cf = GetHisto(lm,em,kFALSE,opt);
	TString option = opt;
	if(option=="im"){
		cf->Scale(-scale);
	}else{
		cf->Scale(scale);
	}
	if(em<0) cf->Scale(2.0);
	return cf;
}

int  MpdFemtoSHCF::PackYlmVectorIndependentOnly(double *invec, double *outvec) const{
  int ioutcount = 0;
  int em, el;
  for (int ilm=0; ilm<GetMaxJM(); ilm++) {
    GetElEmForIndex(ilm, el, em);
    if (em<0) continue;
    outvec[ioutcount++] = invec[ilm*2];
    if (em == 0)
      continue;
    outvec[ioutcount++] = invec[ilm*2 + 1];
  }
  return ioutcount;
}

double MpdFemtoSHCF::ClebschGordan(double aJot1, double aEm1, double aJot2, double aEm2, double aJot, double aEm) const
{
  int mint, maxt;
  double cgc = 0.0;
  int titer;
  double coef;

  maxt = lrint(aJot1 + aJot2 - aJot);
  mint = 0;
  if (lrint(aJot1 - aEm1) < maxt) maxt = lrint(aJot1 - aEm1);
  if (lrint(aJot2 + aEm2) < maxt) maxt = lrint(aJot2 + aEm2);
  if (lrint(-(aJot-aJot2+aEm1)) > mint) mint = lrint(-(aJot-aJot2+aEm1));
  if (lrint(-(aJot-aJot1-aEm2)) > mint) mint = lrint(-(aJot-aJot1-aEm2));

  for (titer = mint; titer<=maxt; titer ++)
    {
      coef = TMath::Power(-1, titer);
      coef *= TMath::Sqrt((2*aJot+1)*
			  fFactorials[lrint(aJot1+aEm1)] *
			  fFactorials[lrint(aJot1-aEm1)] *
			  fFactorials[lrint(aJot2+aEm2)] *
			  fFactorials[lrint(aJot2-aEm2)] *
			  fFactorials[lrint(aJot+aEm)] *
			  fFactorials[lrint(aJot-aEm)]);
      coef /= (fFactorials[titer] *
    		  fFactorials[lrint(aJot1+aJot2-aJot-titer)] *
			  fFactorials[lrint(aJot1-aEm1-titer)] *
			  fFactorials[lrint(aJot2+aEm2-titer)] *
			  fFactorials[lrint(aJot-aJot2+aEm1+titer)] *
			  fFactorials[lrint(aJot-aJot1-aEm2+titer)]);

      cgc += coef;
    }

  cgc *= DeltaJ(aJot1, aJot2, aJot);

  return cgc;
}

double MpdFemtoSHCF::WignerSymbol(double aJot1, double aEm1, double aJot2, double aEm2, double aJot, double aEm) const
{
  if (lrint(aEm1+aEm2+aEm) != 0.0)
    return 0.0;
  double cge = ClebschGordan(aJot1, aEm1, aJot2, aEm2, aJot, -aEm);
  if (lrint(abs(aJot1 - aJot2 - aEm)) % 2)
    cge *= -1.0;
  cge /= sqrt(2*aJot + 1);

  if (cge == -0.0) cge = 0.0;

  return cge;
}

double MpdFemtoSHCF::DeltaJ(double aJot1, double aJot2, double aJot) const
{
  if ((aJot1+aJot2-aJot) < 0) {
    //    std::cout << "J1+J2-J3 < 0 !!!" << " " << aJot1 << " " << aJot2 << " " << aJot << std::endl;
    return 0;
  }
  if ((aJot1-aJot2+aJot) < 0) {
    //    std::cout << "J1-J2+J3 < 0 !!!" << " " << aJot1 << " " << aJot2 << " " << aJot << std::endl;
    return 0;
  }
  if ((-aJot1+aJot2+aJot) < 0) {
    //    std::cout << "-J1+J2+J3 < 0 !!!" << " " << aJot1 << " " << aJot2 << " " << aJot << std::endl;
    return 0;
  }
  if ((aJot1+aJot2+aJot+1) < 0) {
    //    std::cout << "J1+J2+J3+1 < 0 !!!" << " " << aJot1 << " " << aJot2 << " " << aJot << std::endl;
    return 0;
  }
  double res = TMath::Sqrt(1.0 *
			   fFactorials[lrint(aJot1+aJot2-aJot)] *
			   fFactorials[lrint(aJot1-aJot2+aJot)] *
			   fFactorials[lrint(-aJot1+aJot2+aJot)] /
			   fFactorials[lrint(aJot1+aJot2+aJot+1)]);

  return res;
}

int MpdFemtoSHCF::PackYlmMatrixIndependentOnly(double* inmat,
	double* outmat) const {
	int ioutcountz = 0;
	int ioutcountp = 0;
	int emz, elz;
	int emp, elp;
	int finalsize = 0;

	for (int ilm = 0; ilm < GetMaxJM(); ilm++) {
		GetElEmForIndex(ilm, elz, emz);
		if (emz < 0)
			continue;
		finalsize++;
		if (emz == 0)
			continue;
		finalsize++;
	}

	//  std::cout << "Final size " << finalsize << std::endl;

	for (int ilmz = 0; ilmz < GetMaxJM(); ilmz++) {
		GetElEmForIndex(ilmz, elz, emz);
		ioutcountp = 0;

		if (emz < 0)
			continue;
		for (int ilmp = 0; ilmp < GetMaxJM(); ilmp++) {
			GetElEmForIndex(ilmp, elp, emp);
			if (emp < 0)
				continue;
			outmat[ioutcountz * finalsize + ioutcountp] = inmat[GetBin(0, ilmz,
					0, ilmp, 0)];
			ioutcountp++;
			if (emp == 0)
				continue;
			outmat[ioutcountz * finalsize + ioutcountp] = inmat[GetBin(0, ilmz,
					0, ilmp, 1)];
			ioutcountp++;
		}
		ioutcountz++;

		if (emz == 0)
			continue;
		ioutcountp = 0;
		for (int ilmp = 0; ilmp < GetMaxJM(); ilmp++) {
			GetElEmForIndex(ilmp, elp, emp);
			if (emp < 0)
				continue;
			outmat[ioutcountz * finalsize + ioutcountp] = inmat[GetBin(0, ilmz,
					1, ilmp, 0)];
			ioutcountp++;
			if (emp == 0)
				continue;
			outmat[ioutcountz * finalsize + ioutcountp] = inmat[GetBin(0, ilmz,
					1, ilmp, 1)];
			ioutcountp++;
		}
		ioutcountz++;
	}

	return ioutcountz;
}

TH3D* MpdFemtoSHCF::GetCovCF() const {
	return fCfcov;
}

void MpdFemtoSHCF::RecalculateCF() {
	if (fDenImag == NULL) {
		std::cout << "No imaginary denominators!"<< std::endl;
		return;
	}
	if (fDenReal == NULL) {
		std::cout << "No real denominators"<< std::endl;
		return;
	}
	if (fNumReal == NULL) {
		std::cout << "No real numeraotrs"<< std::endl;
		return;
	}
	if (fNumImag == NULL) {
		std::cout << "No imaginary numerators"<< std::endl;
		return;
	}
	if (fCovNum == NULL) {
		std::cout << "No covariance matrix for numerator"<< std::endl;
		return;
	}
	if (fCovDen == NULL) {
		std::cout << "No covariance matrix for denominators"<< std::endl;
		return;
	}

	std::cout << "Computing SF correlation functions"<< std::endl;
	if (fCFReal)
		delete[] fCFReal;
	if (fCFImag)
		delete[] fCFImag;
	fCFReal = new TH1D*[fMaxJM];
	fCFImag = new TH1D*[fMaxJM];
	UnpackCovariances();
	for (int i = 0; i < fMaxJM; i++) {
		TString name = fNumReal[i]->GetName();
		name.ReplaceAll("Num", "CF");
		fCFReal[i] = new TH1D(name, name, fNumReal[i]->GetNbinsX(),
				fNumReal[i]->GetXaxis()->GetBinLowEdge(1),
				fNumReal[i]->GetXaxis()->GetBinUpEdge(
						fNumReal[i]->GetNbinsX()));
		name = fNumImag[i]->GetName();
		name.ReplaceAll("Num", "CF");
		fCFImag[i] = new TH1D(name, name, fNumImag[i]->GetNbinsX(),
				fNumImag[i]->GetXaxis()->GetBinLowEdge(1),
				fNumImag[i]->GetXaxis()->GetBinUpEdge(
						fNumReal[i]->GetNbinsX()));
		fCFImag[i]->SetLineColor(kRed);
		fCFImag[i]->SetLineColor(kRed);
		int el, em;
		GetElEmForIndex(i, el, em);
		fCFReal[i]->GetYaxis()->SetTitle(Form("C^{%i}_{%i}", el, em));
		fCFReal[i]->SetTitle(Form("CF^{%i}_{%i} Re", el, em));
		fCFImag[i]->GetYaxis()->SetTitle(Form("C^{%i}_{%i}", el, em));
		fCFImag[i]->SetTitle(Form("CF^{%i}_{%i} Im", el, em));
	}

	std::complex<double> tMq0[fMaxJM];
	std::complex<double> tTq0[fMaxJM];
	double tMTilde[fMaxJM * fMaxJM * 4];
	//	std::complex<double> tCq0[fMaxJM];
	int recalccov = 1;
	if ((fCovNum) && (fCovNum->GetBinContent(0, 0, 0) > 0.0)) {
		std::cout
				<< "Detected calculated covariance matrix. Do not recalculate !!!"
				<< std::endl;
		//  recalccov = 0;
	}

	double normfactor = 1.0;
	//TODO Fix/improvenormalization
	double normbinmax =0;// fDenReal[0]->FindBin(fNormMax);
	double normbinmin = 0;//fDenReal[0]->FindBin(fNormMin);
	if (normbinmax > 0) {
		double sksum = 0.0;
		double wksum = 0.0;

		double sk, wk, ks;
		if (normbinmin < 1)
			normbinmin = 1;
		if (normbinmax > fDenReal[0]->GetNbinsX())
			normbinmax = fDenReal[0]->GetNbinsX();
		for (int ib = normbinmin; ib <= normbinmax; ib++) {
			ks = fDenReal[0]->GetXaxis()->GetBinCenter(ib);
			sk =
					fNumReal[0]->GetBinContent(ib)
							/ (fDenReal[0]->GetBinContent(ib)
									* (1.0
											- fNormPurity
													/ (fNormRadius * fNormBohr
															* ks * ks)));
			wk = fNumReal[0]->GetBinContent(ib);
			sksum += sk * wk;
			wksum += wk;
		}
		normfactor *= sksum / wksum;
		normfactor /= fNumReal[0]->GetEntries() / fDenReal[0]->GetEntries();
	}

	for (int ibin = 1; ibin <= fNumReal[0]->GetNbinsX(); ibin++) {
		for (int ilm = 0; ilm < fMaxJM; ilm++) {
			//      std::cout << fNumImag[ilm]->GetBinContent(ibin) << std::endl;
			if (recalccov) {
				tMq0[ilm] = std::complex<double>(
						fDenReal[ilm]->GetBinContent(ibin)
								/ (fDenReal[0]->GetEntries() / normfactor),
						fDenImag[ilm]->GetBinContent(ibin)
								/ (fDenReal[0]->GetEntries() / normfactor));
				tTq0[ilm] = std::complex<double>(
						fNumReal[ilm]->GetBinContent(ibin)
								/ fNumReal[0]->GetEntries(),
						fNumImag[ilm]->GetBinContent(ibin)
								/ fNumReal[0]->GetEntries());
			} else {
				tMq0[ilm] = std::complex<double>(
						fDenReal[ilm]->GetBinContent(ibin) / normfactor,
						fDenImag[ilm]->GetBinContent(ibin) / normfactor);
				tTq0[ilm] = std::complex<double>(
						fNumReal[ilm]->GetBinContent(ibin),
						fNumImag[ilm]->GetBinContent(ibin));
			}
			//      std::cout << imag(tTq0[ilm]) << std::endl;
		}

		// Calculate the proper error matrix for T
		// from the temporary covariance matrices
		//    int tabshift = (ibin-1)*GetMaxJM()*GetMaxJM()*4;
		if (recalccov) {
			for (int ilmzero = 0; ilmzero < GetMaxJM(); ilmzero++)
				for (int ilmprim = 0; ilmprim < GetMaxJM(); ilmprim++) {
					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)])) {
						// 	    std::cout << "NaN !!!! RR " << ilmzero << " " << ilmprim << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)])) {
						// 	    std::cout << "NaN !!!! RI " << ilmzero << " " << ilmprim << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)])) {
						// 	    std::cout << "NaN !!!! IR " << ilmzero << " " << ilmprim << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)])) {
						// 	    std::cout << "NaN !!!! II " << ilmzero << " " << ilmprim << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)] /=
							fNumReal[0]->GetEntries();
					covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)] /=
							fNumReal[0]->GetEntries();
					covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)] /=
							fNumReal[0]->GetEntries();
					covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)] /=
							fNumReal[0]->GetEntries();

					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)])) {
						// 	    std::cout << "NaN !!!! RR" << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)])) {
						// 	    std::cout << "NaN !!!! RI" << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)])) {
						// 	    std::cout << "NaN !!!! IR" << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)])) {
						// 	    std::cout << "NaN !!!! II" << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)] -= real(
							tTq0[ilmzero]) * real(tTq0[ilmprim]);
					covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)] -= real(
							tTq0[ilmzero]) * imag(tTq0[ilmprim]);
					covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)] -= imag(
							tTq0[ilmzero]) * real(tTq0[ilmprim]);
					covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)] -= imag(
							tTq0[ilmzero]) * imag(tTq0[ilmprim]);

					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)])) {
						// 	    std::cout << "NaN !!!! RR" << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)])) {
						// 	    std::cout << "NaN !!!! RI" << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)])) {
						// 	    std::cout << "NaN !!!! IR" << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)])) {
						// 	    std::cout << "NaN !!!! II" << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)] /=
							(fNumReal[0]->GetEntries() - 1);
					covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)] /=
							(fNumReal[0]->GetEntries() - 1);
					covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)] /=
							(fNumReal[0]->GetEntries() - 1);
					covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)] /=
							(fNumReal[0]->GetEntries() - 1);

					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)])) {
						// 	    std::cout << "NaN !!!! RR" << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)])) {
						// 	    std::cout << "NaN !!!! RI" << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)])) {
						// 	    std::cout << "NaN !!!! IR" << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
					if (isnan(
							covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)])) {
						// 	    std::cout << "NaN !!!! II" << std::endl;
						// 	    std::cout << fNumReal[0]->GetEntries() << " "
						// 		 << real(tTq0[ilmzero]) << " "
						// 		 << real(tTq0[ilmprim]) << " "
						// 		 << imag(tTq0[ilmzero]) << " "
						// 		 << imag(tTq0[ilmprim]) << " " << std::endl;
					}
				}
		}

		GetMtilde(tMq0, tMTilde);

		// Perform the solution for the correlation function itself and the errors
		//     std::cout << "=============================" << std::endl;
		//     std::cout << "C calculation for bin " << (ibin-1) << std::endl;
		//     std::cout << std::endl;
		//     std::cout << "Input: " << std::endl;
		//     std::cout << "T vector " << std::endl;
		//     for (int ilm=0; ilm<GetMaxJM(); ilm++)
		//       std::cout << real(tTq0[ilm]) << " " << imag(tTq0[ilm]) << "   ";
		//     std::cout << std::endl << "M vector " << std::endl;
		//     for (int ilm=0; ilm<GetMaxJM(); ilm++)
		//       std::cout << real(tMq0[ilm]) << " " << imag(tMq0[ilm]) << "   ";
		//     std::cout << std::endl;

		if (fNumReal[0]->GetBinContent(ibin) > 0) {

			// Rewrite the new way to use the solving wherever there is inversion
			double mDeltaT[fMaxJM * fMaxJM * 4];
			for (int ilmzero = 0; ilmzero < GetMaxJM() * 2; ilmzero++)
				for (int ilmprim = 0; ilmprim < GetMaxJM() * 2; ilmprim++)
					mDeltaT[(ilmzero * fMaxJM * 2) + ilmprim] = (covmnum[GetBin(
							ibin - 1, ilmzero / 2, ilmzero % 2, ilmprim / 2,
							ilmprim % 2)]);

#ifdef _FINISH_DEBUG_
			std::cout << "Delta T matrix " << std::endl;
			for (int ilmz=0; ilmz<GetMaxJM()*2; ilmz++) {
				for (int ilmp=0; ilmp<GetMaxJM()*2; ilmp++) {
					std::cout.precision(3);
					std::cout.width(10);
					std::cout << mDeltaT[ilmz*GetMaxJM()*2 + ilmp];
				}
				std::cout << std::endl;
			}
#endif

			double mDeltaTPacked[fMaxJM * fMaxJM * 4];
			int msize = PackYlmMatrixIndependentOnly(mDeltaT, mDeltaTPacked);

#ifdef _FINISH_DEBUG_
			std::cout << "Delta T matrix packed " << std::endl;
			for (int ilmz=0; ilmz<msize; ilmz++) {
				for (int ilmp=0; ilmp<msize; ilmp++) {
					std::cout.precision(3);
					std::cout.width(10);
					std::cout << mDeltaTPacked[ilmz*msize + ilmp];
				}
				std::cout << std::endl;
			}
#endif

			// (1) Solve (DeltaT)^1 Mtilde = Q

			// Prepare halper matrices

			double mM[fMaxJM * fMaxJM * 4];
			double mMPacked[fMaxJM * fMaxJM * 4];
			for (int iter = 0; iter < fMaxJM * fMaxJM * 4; iter++)
				mM[iter] = tMTilde[iter];
			PackYlmMatrixIndependentOnly(mM, mMPacked);

			gsl_matrix_view matM = gsl_matrix_view_array(mMPacked, msize,
					msize);

#ifdef _FINISH_DEBUG_
			std::cout << "Mtilde matrix " << std::endl;
			for (int ilmz=0; ilmz<GetMaxJM()*2; ilmz++) {
				for (int ilmp=0; ilmp<GetMaxJM()*2; ilmp++) {
					std::cout.precision(3);
					std::cout.width(10);
					std::cout << mM[ilmz*GetMaxJM()*2 + ilmp];
				}
				std::cout << std::endl;
			}

			std::cout << "Mtilde matrix packed " << std::endl;
			for (int ilmz=0; ilmz<msize; ilmz++) {
				for (int ilmp=0; ilmp<msize; ilmp++) {
					std::cout.precision(3);
					std::cout.width(10);
					std::cout << mMPacked[ilmz*msize + ilmp];
				}
				std::cout << std::endl;
			}
#endif

			// Inverting matrix DeltaT.

			double mU[fMaxJM * fMaxJM * 4];
			InvertYlmIndependentMatrix(mDeltaT, mU);

			double mDTInvertedPacked[fMaxJM * fMaxJM * 4];
			PackYlmMatrixIndependentOnly(mU, mDTInvertedPacked);

			gsl_matrix_view matDTI = gsl_matrix_view_array(mDTInvertedPacked,
					msize, msize);

#ifdef _FINISH_DEBUG_
			std::cout << "Delta T matrix inverted packed " << std::endl;
			for (int ilmz=0; ilmz<msize; ilmz++) {
				for (int ilmp=0; ilmp<msize; ilmp++) {
					std::cout.precision(3);
					std::cout.width(10);
					std::cout << mDTInvertedPacked[ilmz*msize + ilmp];
				}
				std::cout << std::endl;
			}
#endif

			// (2) Multiply DeltaT^1 M = Q
			double mQ[fMaxJM * fMaxJM * 4];
			for (int iter = 0; iter < msize * msize; iter++)
				mQ[iter] = 0.0;
			gsl_matrix_view matQ = gsl_matrix_view_array(mQ, msize, msize);

			gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, &matDTI.matrix,
					&matM.matrix, 0.0, &matQ.matrix);

			double mTest[fMaxJM * fMaxJM * 4];
			gsl_matrix_view matTest = gsl_matrix_view_array(mTest, msize,
					msize);

			double mF[fMaxJM * fMaxJM * 4];
			for (int iter = 0; iter < fMaxJM * fMaxJM * 4; iter++)
				mF[iter] = mDeltaTPacked[iter];
			gsl_matrix_view matF = gsl_matrix_view_array(mF, msize, msize);

			gsl_blas_dgemm(CblasTrans, CblasNoTrans, 1.0, &matF.matrix,
					&matQ.matrix, 0.0, &matTest.matrix);

#ifdef _FINISH_DEBUG_
			std::cout << "Test matrix packed - compare to Mtilde" << std::endl;
			for (int ilmz=0; ilmz<msize; ilmz++) {
				for (int ilmp=0; ilmp<msize; ilmp++) {
					std::cout.precision(3);
					std::cout.width(10);
					std::cout << mTest[ilmz*msize + ilmp];
				}
				std::cout << std::endl;
			}
#endif

			// (2) Multiply Mtilde^T Q = P

			double mP[fMaxJM * fMaxJM * 4];
			for (int iter = 0; iter < fMaxJM * fMaxJM * 4; iter++)
				mP[iter] = 0;

			gsl_matrix_view matP = gsl_matrix_view_array(mP, msize, msize);

			gsl_blas_dgemm(CblasTrans, CblasNoTrans, 1.0, &matM.matrix,
					&matQ.matrix, 0.0, &matP.matrix);

#ifdef _FINISH_DEBUG_
			std::cout << "P matrix packed " << std::endl;
			for (int ilmz=0; ilmz<msize; ilmz++) {
				for (int ilmp=0; ilmp<msize; ilmp++) {
					std::cout.precision(3);
					std::cout.width(10);
					std::cout << mP[ilmz*msize + ilmp];
				}
				std::cout << std::endl;
			}
#endif

			// (3) Solve P^-1 Mtilde^T = R
			double mPUnpacked[fMaxJM * fMaxJM * 4];
			UnPackYlmMatrixIndependentOnly(mP, mPUnpacked, msize);

#ifdef _FINISH_DEBUG_
			std::cout << "P matrix unpacked " << std::endl;
			for (int ilmz=0; ilmz<GetMaxJM()*2; ilmz++) {
				for (int ilmp=0; ilmp<GetMaxJM()*2; ilmp++) {
					std::cout.precision(3);
					std::cout.width(10);
					std::cout << mPUnpacked[ilmz*GetMaxJM()*2 + ilmp];
				}
				std::cout << std::endl;
			}
#endif

			// Invert the P matrix

			double mPInverted[fMaxJM * fMaxJM * 4];
			InvertYlmIndependentMatrix(mPUnpacked, mPInverted);

			double mPInvertedPacked[fMaxJM * fMaxJM * 4];
			PackYlmMatrixIndependentOnly(mPInverted, mPInvertedPacked);

			gsl_matrix_view matPI = gsl_matrix_view_array(mPInvertedPacked,
					msize, msize);

#ifdef _FINISH_DEBUG_
			std::cout << "P matrix inverted packed " << std::endl;
			for (int ilmz=0; ilmz<msize; ilmz++) {
				for (int ilmp=0; ilmp<msize; ilmp++) {
					std::cout.precision(3);
					std::cout.width(10);
					std::cout << mPInvertedPacked[ilmz*msize + ilmp];
				}
				std::cout << std::endl;
			}
#endif

			//       //      gsl_matrix_view matR = gsl_matrix_view_array(mR, msize, msize);

			//       double mG[fMaxJM*fMaxJM*4];
			//       for (int iter=0; iter<fMaxJM*fMaxJM*4; iter++)
			// 	mG[iter] = mP[iter];
			//       gsl_matrix_view matG = gsl_matrix_view_array(mG, msize, msize);

			//       // Decomposing the M matrix
			//       gsl_linalg_SV_decomp(&matG.matrix, &matS.matrix, &vecST.vector, &vecWT.vector);

			//       for (int itert=0; itert<msize; itert++) {
			// 	for (int iterm=0; iterm<msize; iterm++)
			// 	  vCT[iterm] = mMPacked[iterm*msize + itert];
			// 	  // Transvere !!!      ^^^^^         ^^^^^

			// 	// Solving the problem
			// 	gsl_linalg_SV_solve(&matG.matrix, &matS.matrix, &vecST.vector, &vecCT.vector, &vecXT.vector);

			// 	for (int iterm=0; iterm<msize; iterm++)
			// 	  mR[itert*msize + iterm] = vXT[iterm];
			//       }

			double mR[fMaxJM * fMaxJM * 4];
			for (int ir = 0; ir < fMaxJM * fMaxJM * 4; ir++)
				mR[ir] = 0.0;
			gsl_matrix_view matR = gsl_matrix_view_array(mR, msize, msize);

			// (2) Multiply P^-1 M (Trans) = R

#ifdef _FINISH_DEBUG_
			std::cout << "Matrix M Packed " << std::endl;
			for (int ilmz=0; ilmz<msize; ilmz++) {
				for (int ilmp=0; ilmp<msize; ilmp++) {
					std::cout.precision(3);
					std::cout.width(10);
					std::cout << mMPacked[ilmz*msize + ilmp];
				}
				std::cout << std::endl;
			}
#endif

			gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, &matPI.matrix,
					&matM.matrix, 1.0, &matR.matrix);

#ifdef _FINISH_DEBUG_
			std::cout << "R matrix packed " << std::endl;

			for (int ilmz=0; ilmz<msize; ilmz++) {
				for (int ilmp=0; ilmp<msize; ilmp++) {
					std::cout.precision(3);
					std::cout.width(10);
					std::cout << mR[ilmz*msize + ilmp];
				}
				std::cout << std::endl;
			}
#endif

			// (4) Solve DeltaT^-1 T = L
			double vL[fMaxJM * 2];
			gsl_vector_view vecL = gsl_vector_view_array(vL, msize);

			//       // Decomposing the M matrix
			//       gsl_linalg_SV_decomp(&matF.matrix, &matS.matrix, &vecST.vector, &vecWT.vector);

			double vB[fMaxJM * 2];
			for (int iter = 0; iter < GetMaxJM(); iter++) {
				vB[iter * 2] = real(tTq0[iter]);
				vB[iter * 2 + 1] = imag(tTq0[iter]);
			}

			double vBPacked[fMaxJM * 2];
			PackYlmVectorIndependentOnly(vB, vBPacked);

			gsl_vector_view vecB = gsl_vector_view_array(vBPacked, msize);

			//       // Solving the problem
			//       gsl_linalg_SV_solve(&matF.matrix, &matS.matrix, &vecST.vector, &vecB.vector, &vecL.vector);

#ifdef _FINISH_DEBUG_
			std::cout << "L vector packed " << std::endl;
			for (int ilmp=0; ilmp<msize; ilmp++) {
				std::cout.precision(3);
				std::cout.width(10);
				std::cout << vL[ilmp];
			}
			std::cout << std::endl;
#endif

			// Multiply DeltaT^-1 T = L

			gsl_blas_dgemv(CblasNoTrans, 1.0, &matDTI.matrix, &vecB.vector, 0.0,
					&vecL.vector);

			// (5) Multiply R L = C

			double vY[fMaxJM * 2];
			for (int iter = 0; iter < GetMaxJM() * 2; iter++) {
				vY[iter] = 0.0;
			}

			// Prepare inputs for solving the problem
			gsl_vector_view vecY = gsl_vector_view_array(vY, msize);

			gsl_blas_dgemv(CblasNoTrans, 1.0, &matR.matrix, &vecL.vector, 0.0,
					&vecY.vector);

#ifdef _FINISH_DEBUG_
			std::cout << "C vector packed " << std::endl;
			for (int ilmp=0; ilmp<msize; ilmp++) {
				std::cout.precision(3);
				std::cout.width(10);
				std::cout << vY[ilmp];
			}
			std::cout << std::endl;
#endif
			int mpack = 0;
			int el, em;
			for (int ilm = 0; ilm < fMaxJM; ilm++) {
				// 	fCFReal[ilm]->SetBinContent(ibin, vC[mpack++]);
				GetElEmForIndex(ilm, el, em);
				if ((el % 2) == 1) {
					fCFReal[ilm]->SetBinContent(ibin, 0.0);
					fCFImag[ilm]->SetBinContent(ibin, 0.0);
				}
				if (em < 0) {
					fCFReal[ilm]->SetBinContent(ibin, 0.0);
					fCFImag[ilm]->SetBinContent(ibin, 0.0);
				} else {
					fCFReal[ilm]->SetBinContent(ibin, vY[mpack++]);
					if (em == 0)
						fCFImag[ilm]->SetBinContent(ibin, 0);
					else
						//	  fCFImag[ilm]->SetBinContent(ibin, vC[mpack++]);
						fCFImag[ilm]->SetBinContent(ibin, vY[mpack++]);
				}
			}

			// invert the P matrix to get C errors
			//      double mS[fMaxJM*fMaxJM*4];

			//       for (int iterz=0; iterz<msize; iterz++)
			// 	for (int iterp=0; iterp<msize; iterp++)
			// 	  if (iterp == iterz)
			// 	    mS[iterz*msize + iterp] = 1.0;
			// 	  else
			// 	    mS[iterz*msize + iterp] = 0.0;

			//      gsl_matrix_view matS = gsl_matrix_view_array(mS, msize, msize);

			// Invert V

			//       gsl_blas_dtrsm(CblasLeft, CblasUpper, CblasNoTrans, CblasNonUnit, 1.0, &matP.matrix, &matS.matrix);

			mpack = 0;
			for (int ilm = 0; ilm < fMaxJM; ilm++) {
				GetElEmForIndex(ilm, el, em);
				if (em < 0) {
					fCFReal[ilm]->SetBinError(ibin, 0);
					fCFImag[ilm]->SetBinError(ibin, 0);
				} else {
					fCFReal[ilm]->SetBinError(ibin,
							sqrt(
									fabs(
											mPInvertedPacked[mpack * msize
													+ mpack])));
					mpack++;
					if (em == 0)
						fCFImag[ilm]->SetBinError(ibin, 0);
					else {
						fCFImag[ilm]->SetBinError(ibin,
								sqrt(
										fabs(
												mPInvertedPacked[mpack * msize
														+ mpack])));
						mpack++;
					}
				}
			}

			for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++) {
				for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++) {
					if (ilmp > ilmz)
						covmcfc[GetBin(ibin - 1, ilmz / 2, ilmz % 2, ilmp / 2,
								ilmp % 2)] = mPInverted[ilmz * GetMaxJM() * 2
								+ ilmp];
					else
						covmcfc[GetBin(ibin - 1, ilmz / 2, ilmz % 2, ilmp / 2,
								ilmp % 2)] = mPInverted[ilmp * GetMaxJM() * 2
								+ ilmz];
				}
			}
		} else {
			for (int ilm = 0; ilm < fMaxJM; ilm++) {
				fCFReal[ilm]->SetBinError(ibin, 0);
				fCFImag[ilm]->SetBinError(ibin, 0);
			}

			for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++) {
				for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++) {
					covmcfc[GetBin(ibin - 1, ilmz / 2, ilmz % 2, ilmp / 2,
							ilmp % 2)] = 0.0;
				}
			}
		}
	}
	PackCfcCovariance();
}

MpdFemtoSHCF::~MpdFemtoSHCF() {
	if(fNumImag) delete []fNumImag;
	if(fNumReal) delete []fNumReal;
	if(fDenReal) delete []fDenReal;
	if(fDenImag) delete []fDenImag;
	if(fCovNum) delete fCovNum;
	if(fCovDen) delete fCovDen;
	if(fCFReal) delete []fCFReal;
	if(fCFImag) delete []fCFImag;
	if(covmnum) delete []covmnum;
	if(covmden) delete []covmden;
	if(covmcfc) delete []covmcfc;
	if(fEls) delete []fEls;
	if(fEms) delete []fEms;
	if(fElsi) delete []fElsi;
	if(fEmsi) delete []fEmsi;
	if(fCfcov) delete fCfcov;
	if(fLabels) delete fLabels;
}
