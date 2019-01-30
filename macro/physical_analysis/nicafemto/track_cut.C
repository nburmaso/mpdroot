/*
 * track_cut.C
 *
 *  Created on: 30 sty 2019
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#if !defined(__CINT__) && !defined(__CLING__)
#include "NicaTrackToFMass2Cut.h"
#include "NicaTrackTpcCut.h"
#include "NicaTrackDCACut.h"
#include "NicaTrackTpcToFCut.h"
#include "NicaFemtoBasicAna.h"
#include "NicaTrackChargeCut.h"
#include "NicaTrackPCut.h"
#include "NicaTrackPtCut.h"
#include "NicaCutMonitorX.h"
#include "NicaCutMonitorXY.h"
#include "NicaTrackEtaCut.h"
#include "NicaTrackPdgCut.h"
#endif


NicaTrackToFMass2Cut *GetToF(){
	NicaTrackTofMass2CutNoZeros *mass = new NicaTrackTofMass2CutNoZeros();
	#ifdef POLY_TOF
		mass->SetPolyLineDown(-0.01,-0.03,-0.06);
		mass->SetPolyLineUp(0.1,0.03,0.06);
		mass->SetUpPoints(0.170, 0.031, 0.785, 0.058, 1.985, 0.085);
		mass->SetDownPoints(0.115,0.004,0.508,-0.007,1.315,-0.057);
		mass->SetMinMax(-0.3,0.30);
	#else
		//mass->SetUpPoints(0.0, 0.05, 1, 0.1, 2, 0.2);
		//mass->SetDownPoints(0.0,0.0,1,-0.1,2,-0.2);
		//mass->SetDownPoints(0.1, 2.5E-3, 0.3,2.5E-3 , 0.4, 0);
		mass->SetDownPoints(0.1, -2, 0.3,-2 , 0.4, -2);
		mass->SetUpPoints(0.1, 2, 0.3,2 , 0.4, 2);
		mass->SetMinMax(-0.3,0.15);
	#endif
	//	mass->SetPolyLineUp(mass->GetMax(0), 0,0);
	//	mass->SetPolyLineDown(mass->GetMin(0), 0,0);
	return mass;
}

NicaTrackTpcCut *GetTpc( Double_t sigma){
	NicaTrackTpcCut *cut = new NicaTrackTpcCut();
	cut->SetSigma(-sigma,sigma,cut->PionSigma());
	cut->SetSigma(-sigma,sigma,cut->KaonSigma());
	cut->SetSigma(-sigma,sigma,cut->ProtonSigma());
	cut->SetSigma(-sigma,sigma,cut->ElectronSigma());
	cut->SetDeDx(-1E+6, 1E+6);

	cut->SetNHits(30, 90);
	cut->SetActiveSigma(cut->PionSigma());

	cut->SetCharge(1);
	return cut;
}

NicaTrackDCACut *GetDCACut(){
	NicaTrackDCACut *dca1 = new NicaTrackDCACut();
	dca1->SetMinMax(-10,1.25,dca1->DCAxy());
	dca1->SetMinMax(-0.75,0.75,dca1->DCAz());
	return dca1;
}

NicaTrackTpcToFCut *GetCompbined(){
	NicaTrackToFMass2Cut *tof  = GetToF();
	NicaTrackTpcCut *tpc = GetTpc(2);
	NicaTrackTpcToFCut *super = new NicaTrackTpcToFCut(tpc, tof);
	super->SetThreshold(0.5);
	return super;
}

void PrepTrackCuts(NicaFemtoBasicAna *femto, Bool_t mc){
	NicaTrackTpcToFCut *id_cut = GetCompbined();
	NicaTrackDCACut *dca = GetDCACut();
	NicaTrackChargeCut *charge_cut = new NicaTrackChargeCut();
	charge_cut->SetMinAndMax(1);
	femto->Add(charge_cut,"fast");
	NicaTrackPCut *p = new NicaTrackPCut();
	femto->SetMass(NicaConst::PionPlusMass());

	femto->Add(id_cut,"re");
	femto->Add(dca,"re");
	femto->Add(p);

	//p vs dedx
	NicaCutMonitorXY *mon = new NicaCutMonitorXY(p->CutName(),0,id_cut->CutName("re"),id_cut->DeDx());
	mon->SetXaxis(100,0, 3);
	mon->SetYaxis(100, 0, 1E+4);
	femto->Add(mon);
	// p vs tof mass2
	NicaCutMonitorXY *monk2 = new NicaCutMonitorXY(p->CutName(),0,id_cut->CutName("re"),id_cut->M2());
	monk2->SetXaxis(100,0, 2);
	monk2->SetYaxis(100, -0.2, 2);
	femto->Add(monk2);

	//dcaxy
	NicaCutMonitorX *dca_x = new NicaCutMonitorX(dca->CutName("re"),dca->DCAxy());
	dca_x->SetXaxis(200, 0,10);
	femto->Add(dca_x);
	//dcaz
	NicaCutMonitorX *dca_z = new NicaCutMonitorX(dca->CutName("re"),dca->DCAz());
	dca_z->SetXaxis(100, -10,10);
	femto->Add(dca_z);


	NicaTrackPtCut *pt = new NicaTrackPtCut();
	pt->SetMinMax(0.1, 1.3);
	NicaTrackEtaCut *eta = new NicaTrackEtaCut();
	eta->SetMinMax(-1.2, 1.2);
	femto->Add(pt);
	femto->Add(eta);

	if(mc){
		Int_t pid = 211;
		NicaTrackPdgCut *pidCut = new NicaTrackPdgCut();
		pidCut->SetMinAndMax(pid);
		femto->Add(pidCut,"fast+im");
	}
}
