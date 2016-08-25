/*
 * MpdFemtoSHCF.h
 *
 *  Created on: 22 sie 2016
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_PHYSICS_FEMTO_MPDFEMTOSHCF_H_
#define MPDROOT_PHYSICS_FEMTO_MPDFEMTOSHCF_H_

#include <cstdlib>
#include <cmath>
#include <complex>
#include <TObject.h>
#include <TMath.h>
#include <TList.h>
#include <TObjString.h>
#include <TROOT.h>
#include <TH1D.h>
#include <TH3D.h>
#include <TVector3.h>
#include <TSystem.h>
#include "MpdFemtoYlm.h"
/**
 * class for calculation of correlation functions in spherical harmonics
 */

class MpdFemtoSHCF : public TObject{
public:
	/** default constructor for streamer
	 */
	MpdFemtoSHCF();
	/**
	 *  construtor
	 * @param kinematics frame
	 * @param maxL maximum L
	 */
	MpdFemtoSHCF(Int_t maxL);
	/**
	 * add comment to this object
	 * @param comment
	 */
	void SetComment(TString comment){fComment = comment;};
	/**
	 * set norm purity
	 * @param purity
	 */
	void SetNormPurity(Double_t purity){fNormPurity = purity;}
	/**
	 * set norm radius
	 * @param radius
	 */
	void SetNormRadius(Double_t radius){fNormRadius = radius;};
	/**
	 * set bohr radius for normalization
	 * @param bohr
	 */
	void SettNormBohr(Double_t bohr){ fNormBohr = bohr;};
	 /**
	 * @param label
	 */
	/**
	 * add label to this object
	 * @param label
	 */
	void AddLabel(TString label);
	/**
	 * recalculate correlation function, should be used if numerator/denominator was changed
	 */
	void RecalculateCF();
	/**
	 * add real numerators
	 * @param histograms array of numerators
	 * @param clone if true then copy of histograms is made, otherwise only pointer is copied
	 */
	void SetNumRe(TH1D **histograms, Bool_t clone  = kTRUE);
	/**
	 * add imaginary numerators
	 * @param histograms array of numerators
	 * @param clone if true then copy of histograms is made, otherwise only pointer is copied
	 */
	void SetNumIm(TH1D **histograms, Bool_t clone = kTRUE);
	/**
	 * add real denominators
	 * @param histograms array of numerators
	 * @param clone if true then copy of histograms is made, otherwise only pointer is copied
	 */
	void SetDenRe(TH1D **histograms, Bool_t clone = kTRUE);
	/**
	 * add imaginary denominators
	 * @param histograms array of numerators
	 * @param clone if true then copy of histograms is made, otherwise only pointer is copied
	 */
	void SetDenIm(TH1D **histograms, Bool_t clone = kTRUE);
	/**
	 * add covariance matrix
	 * @param num covariance matrix for numerator
	 * @param den covariance matrix for denominator
	 * @param clone if tlue then matrixes are copied into new histograms, otherwise only pointer is copied
	 */
	void SetCovMatrix(TH3D *num, TH3D *den, Bool_t clone=kTRUE);
	/**
	 * normalize by using normalization algorithm for given histogram
	 * @param el L
	 * @param em M
	 * @param opt specifiy with histogram should be used - real "re" or imaginary "im"
	 */
	void NormalizeBy(int el, int em, Option_t *opt="re");
	/**
	 * set scale
	 * @param scale
	 */
	void SetScale(Double_t scale){fScale = scale;};
	/**
	 * return computed correlation function
	 * @param el
	 * @param em
	 * @return
	 */
	TH1D *GetCFRe(int el, int em) const ;
	/**
	 *
	 * @param el
	 * @param em
	 * @return computed correlation function
	 */
	TH1D *GetCFIm(int el, int em) const;
	/**
	 *
	 * @param el L
	 * @param em M
	 * @return real numerator with given index
	 */
	TH1D *GetNumRe(int el, int em) const;
	/**
	 *
	 * @param el L
	 * @param em M
	 * @return imaginary numerator with given index
	 */
	TH1D *GetNumIm(int el, int em) const;
	/**
	 *
	 * @param el L
	 * @param em M
	 * @return real denominator with given index
	 */
	TH1D *GetDenRe(int el, int em) const;
		/**
		 *
		 * @param el L
		 * @param em M
		 * @return imaginary denominator with given index
		 */
	TH1D *GetDenIm(int el, int em) const;
	TH1D **GetCFRe()const {return fCFReal;};
	TH1D **GetCFIm() const {return fCFImag;};
	/**
	 *
	 * @return covariance metrix for numerator
	 */
	TH3D *GetCovNum()const{ return fCovNum;};
	/**
	 *
	 * @return covariance matrix for denominator
	 */
	TH3D *GetCovDen()const {return fCovDen;};
	/**
	 *
	 * @return covariance matrix for correlation function
	 */
	TH3D *GetCovCF() const;
	/**
	 *
	 * @return L
	 */
	Int_t GetL()const{return TMath::Sqrt(fMaxJM-1);};
	virtual ~MpdFemtoSHCF();
protected:
	Int_t GetMaxJM() const { return fMaxJM;};
private:
	const Int_t fMaxJM;
	Double_t fScale;
	Double_t fNormMin;
	Double_t fNormMax;
	TH1D **fNumReal;    //[fMaxJM] Real parts of Ylm components of the numerator
	TH1D **fNumImag; // [fMaxJM]Imaginary parts of Ylm components of the numerator
	TH1D **fDenReal;  //[fMaxJM] Real parts of Ylm components of the denominator
	TH1D **fDenImag; //[fMaxJM] Imaginary parts of Ylm components of the denominator
	TH3D *fCovNum;               // Numerator covariance matrix packed into TH3D
	TH3D *fCovDen;             // Denominator covariance matrix packed into TH3D
	TList *fLabels;
	TString fComment;

	TH1D **fCFReal; 				//[fMaxJM] real CF's
	TH1D **fCFImag;				//[fMaxJM] img CF's
	Int_t  fFactorialsSize;
	Double_t *covmnum;            //! Covariance matrix for the numerator
	Double_t *covmden;            //! Covariance matrix for the denominator
	Double_t *covmcfc;   			//! Covariance matrix for the  CF
	Double_t fNormPurity;		//
	Double_t fNormRadius;		//
	Double_t fNormBohr;	//
	Double_t *fEls;				//[fMaxJM]
	Double_t *fEms;			//[fMaxJM]
	Double_t *fElsi;				//[fMaxJM]
	Double_t *fEmsi;			//[fMaxJM]
	Double_t *fFactorials;	//[fFactorialsSize]
	TH3D *fCfcov;				//


	Int_t GetIndexForLM(int el, int em) const;
	Int_t GetBin(int qbin, int ilmzero, int zeroimag, int ilmprim, int primimag) const;
	TH1D *Histo(int ilm, int em, Option_t *opt, Double_t scale) const;
	void PackCfcCovariance();
	void UnpackCovariances();
	void GetElEmForIndex(int aIndex, double &aEl, double &aEm) const;
	void GetElEmForIndex(int aIndex, int &aEl, int &aEm) const;
	void GetMtilde(std::complex<double> *aMat, double *aMTilde);
	void InvertYlmIndependentMatrix(double *inmat, double *outmat);
	void UnPackYlmMatrixIndependentOnly(double *inmat, double *outmat, int insize);
	void GetIndependentLM(int ibin, int &el, int &em, int &im) const;
	int PackYlmMatrixIndependentOnly(double *inmat, double *outmat) const;
	int PackYlmVectorIndependentOnly(double *invec, double *outvec) const;
	double WignerSymbol(double aJot1, double aEm1, double aJot2, double aEm2, double aJot, double aEm) const;
	double ClebschGordan(double aJot1, double aEm1, double aJot2, double aEm2, double aJot, double aEm) const;
	double DeltaJ(double aJot1, double aJot2, double aJot) const;
	/**
	 * return histogram
	 * @param el l
	 * @param em m
	 * @param norm if norm = true return normalized
	 * @param opt can be "im" or "re"
	 * @return
	 */
	TH1D *GetHisto(int el,int em,Bool_t norm,Option_t *opt="") const;


	ClassDef(MpdFemtoSHCF,1)
};
#endif /* MPDROOT_PHYSICS_FEMTO_MPDFEMTOSHCF_H_ */
