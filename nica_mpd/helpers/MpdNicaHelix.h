/*
 * NicaHelix.h
 *
 *  Created on: 28 gru 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 *		Derived from AliHelix (see https://github.com/alisw/AliRoot/blob/master/STEER/STEER/AliHelix.h for more information)
 */
#ifndef INTERFACES_MPDROOT_HELPERS_MPDNICAHELIX_H_
#define INTERFACES_MPDROOT_HELPERS_MPDNICAHELIX_H_
#include <TObject.h>
#include <TMath.h>
#include <TVector3.h>

class MpdNicaHelix: public TObject {
private:
	static Double_t fgHelixBz;
	const Double_t kB2C ;
	MpdNicaHelix &operator=(const MpdNicaHelix&helix);
	Double_t fHelix[9];    //helix parameters
	static Double_t GetBz();
public:
	MpdNicaHelix();
	MpdNicaHelix(const MpdNicaHelix &t);
	MpdNicaHelix(TVector3 x, TVector3 mom, Double_t charge, Double_t conversion=0.);
	MpdNicaHelix(Double_t x[3], Double_t p[3], Double_t charge = 1,Double_t conversion = 0.);
	void Evaluate(Double_t t, Double_t r[3]) const;
	/**
	 *
	 * @param t
	 * @param r radius vector
	 * @param g first derivevatives
	 * @param gg second derivatives
	 */
	void Evaluate(Double_t t, Double_t r[3],
			Double_t g[3],
			Double_t gg[3]) const;
	/**
	 *
	 * @param phase
	 * @param p momentum (is returned)
	 * @param conversion
	 * @param xr
	 */
	void GetMomentum(Double_t phase, Double_t p[4], Double_t conversion = 0.,
			Double_t *xr = 0) const;
	void GetAngle(Double_t t1, const MpdNicaHelix &h, Double_t t2,
			Double_t angle[3]) const;
	static void SetMagFiled(Double_t Bz);
	void SetParams(TVector3 &x, TVector3 &mom, Double_t charge, Double_t conversion=0.);
	Double_t GetHelixR(Double_t phase = 0) const;
	Double_t GetHelixZ(Double_t phase = 0) const;
	/**
	 *
	 * @param x0
	 * @param y0
	 * @return phase for nearest point
	 */
	Double_t GetPhase(Double_t x0, Double_t y0) const;
	Double_t GetPhaseAbs(Double_t x0, Double_t y0) const;
	/**
	 *
	 * @param z0
	 * @return phase for given z
	 */
	Double_t GetPhaseZ(Double_t z0) const;
	Double_t GetPointAngle(const MpdNicaHelix &h, Double_t phase[2],const Float_t *vertex) const;
	Double_t GetHelix(Int_t i) const {return fHelix[i];}
	/**
	 *
	 * @param r0
	 * @param t
	 * @return phase for the nearest point
	 */
	Int_t GetPhase(Double_t r0, Double_t t[2]) const;
	Int_t GetRPHIintersections(const MpdNicaHelix &h, Double_t phase[2][2],
			Double_t ri[2], Double_t cut = 3.) const;
	Int_t GetClosestPhases(const MpdNicaHelix &h, Double_t phase[2][2]) const;
	Int_t LinearDCA(const MpdNicaHelix &h, Double_t &t1, Double_t &t2, Double_t &R,
			Double_t &dist) const;
	/**
	 *
	 * @param h helixes
	 * @param t1
	 * @param t2
	 * @param R
	 * @param dist
	 * @param iter
	 * @return
	 */
	Int_t ParabolicDCA(
			const MpdNicaHelix&h,  //helixes
			Double_t &t1, Double_t &t2, Double_t &R, Double_t &dist,
			Int_t iter = 1) const;
	/**
	 *
	 * @param h helixes
	 * @param t1
	 * @param t2
	 * @param R
	 * @param dist
	 * @param err
	 * @param iter
	 * @return
	 */
	Int_t ParabolicDCA2(
			const MpdNicaHelix&h,
			Double_t &t1, Double_t &t2, Double_t &R, Double_t &dist,
			Double_t err[3], Int_t iter = 1) const;
	virtual ~MpdNicaHelix() {};
	ClassDef(MpdNicaHelix,1)    // NicaHelix
};


#endif /* INTERFACES_MPDROOT_HELPERS_MPDNICAHELIX_H_ */
