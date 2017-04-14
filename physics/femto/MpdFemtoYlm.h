/*
 * MpdFemtoYlm.h
 *
 *  Created on: 22 sie 2016
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 *		Derived from AliROOT AliFemtoYlm
 */
#ifndef MPDROOT_PHYSICS_FEMTO_MPDFEMTOYLM_H_
#define MPDROOT_PHYSICS_FEMTO_MPDFEMTOYLM_H_

#include <cstdlib>
#include <cmath>
#include <complex>
#include <TMath.h>
#include <TObject.h>

class MpdFemtoYlm: public TObject{
public:
	MpdFemtoYlm();
	virtual ~MpdFemtoYlm();
	static MpdFemtoYlm *Instance();
	MpdFemtoYlm(const MpdFemtoYlm& aYlm);
	MpdFemtoYlm& operator=(const MpdFemtoYlm& aYlm);
	static double Legendre(int ell, int emm, double ctheta);
	static void   LegendreUpToYlm(int lmax, double ctheta, double *lbuf);
	static std::complex<double> Ylm(int ell,int m,double theta,double phi);
	static std::complex<double> Ylm(int ell, int m, double x, double y, double z);
	static void YlmUpToL(int lmax, double x, double y, double z, std::complex<double> *ylms);
	static void YlmUpToL(int lmax, double ctheta, double phi, std::complex<double> *ylms);
	static double ReYlm(int ell, int m, double theta, double phi);
	static double ReYlm(int ell, int m, double x, double y, double z);
	static double ImYlm(int ell, int m, double theta, double phi);
	static double ImYlm(int ell, int m, double x, double y, double z);
	static void InitializeYlms();
private:
	static std::complex<double> Ceiphi(double phi);
	static MpdFemtoYlm *fgInstance;
	static double *fgPrefactors;
	static int    *fgPrefshift;
	static int    *fgPlmshift;
	ClassDef(MpdFemtoYlm,1)
};

#endif /* MPDROOT_PHYSICS_FEMTO_MPDFEMTOYLM_H_ */
