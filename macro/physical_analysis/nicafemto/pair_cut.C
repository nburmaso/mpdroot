/*
 * pair_cut.C
 *
 *  Created on: 30 sty 2019
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */

#if !defined(__CINT__) && !defined(__CLING__)
#include "NicaCutMonitorXY.h"
#include "NicaTwoTrackFemtoQinvCut.h"
#include "NicaPdgBinPairCut.h"
#include "NicaCutMonitorXYZ.h"
#include "NicaTwoTrackKtCut.h"
#include "MpdAlicePairQuality.h"
#include "MpdPairSharedHitsCut.h"
#include "MpdPairTpcEntranceCut.h"
#include "NicaPairDeltaQinvCut.h"
#include "MpdPairTpcSeparationSimpleCut.h"
#include "MpdPairDeltaDCA.h"
#include "MpdPairSharedPadsCut.h"
#include "MpdPairTpcEntrySimpleCut.h"
#endif


TString label_re;
// create cut monitor q-inv vs something
NicaCutMonitorXY *GetMonitor(TString name, Int_t par, Int_t nbins, Double_t min, Double_t max){
	NicaCutMonitorXY *mon = new NicaCutMonitorXY(label_re,0,name,par);
	mon->SetXaxis(50, 0, 0.1);
	mon->SetYaxis(nbins, min, max);
	return mon;
}
void pair_cuts(NicaFemtoBasicAna *femto, NicaOptionConverter *opt){
	Double_t mass_particle = NicaConst::PionPlusMass();

	/* Q-inv cuts */
	Int_t dummy = 0;
	NicaTwoTrackFemtoQinvCut *qinv = new NicaTwoTrackFemtoQinvCut(mass_particle);
	NicaTwoTrackFemtoQinvCut *fast_quinv = new NicaTwoTrackFemtoQinvCut();
	fast_quinv->SetMinMax(0.0,0.15);
	femto->Add(fast_quinv,"{4x1}+fast");
	femto->Add(qinv,"{4x1}+re");
	femto->Add(qinv,"{4x1}+im");

	/* qinv reco vs mc */
	label_re = qinv->CutName("re");
	femto->Add(GetMonitor(qinv->CutName("im"),1,50,0,0.1));

	NicaPdgBinPairCut *pid2 = new NicaPdgBinPairCut();
	pid2->SetMinMax(-10, 10,pid2->FirstParticle());
	pid2->SetMinMax(-10, 10,pid2->SecondParticle());
	femto->Add(pid2,"{4x1}+im");
	NicaCutMonitorXYZ *monP = new NicaCutMonitorXYZ(pid2->CutName("im"),1,pid2->CutName("im"),2,qinv->CutName("re"),0);
	monP->SetXaxis(15,-7.5,7.5);
	monP->SetYaxis(15,-7.5,7.5);
	monP->SetZaxis(50,0,0.1);
	femto->Add(monP);

	/* kt cuts */
	NicaTwoTrackKtCut *kt = new NicaTwoTrackKtCut();
	kt->SetMinMax(0, 0.1);
	kt->SetMinMax(0.1, 0.2);
	femto->Add(kt,"{0}+fast");
	kt->SetMinMax(0.2, 0.3);
	femto->Add(kt,"{1}+fast");
	kt->SetMinMax(0.3, 0.4);
	femto->Add(kt,"{2}+fast");
	kt->SetMinMax(0.4, 1);
	femto->Add(kt,"{3}+fast");

	MpdAlicePairQuality *aliceQ = new MpdAlicePairQuality();
	aliceQ->SetMinAndMax(-10,10);
	femto->Add(aliceQ,"{4x1}+re");

	NicaPairDeltaQinvCut * dqinv = new NicaPairDeltaQinvCut();
	dqinv->SetMinMax(-1E+6, 1E+6,dqinv->Absolute());
	dqinv->SetMinMax(-1E+6, 1E+6,dqinv->Relative());
	femto->Add(dqinv,"{4x1}");

	// entry cut in 3D
	MpdPairTpcEntrySimpleCut *entry = new MpdPairTpcEntrySimpleCut();
	entry->SetMinMax(0, 9000,entry->XYZ());

	femto->Add(entry,"{4x1}+re");

	MpdPairTpcSeparationSimpleCut *sep = new MpdPairTpcSeparationSimpleCut();
	if(opt->GetOption(dummy, "sep")){
		Double_t min_sep = dummy;
		min_sep = min_sep/10;
		sep->SetMinMax(min_sep, 1E+6, sep->Entry());
	}else{
		sep->SetMinMax(0, 1E+6, sep->Entry());
	}


	femto->Add(sep,"{4x1}+re");

//	NicaTwoTrackPhiCut *phi_pair = new NicaTwoTrackPhiCut();
//	phi_pair->SetMinMax(-1E+6, 1E+6);
//	femto->Add(phi_pair,"{4x1}+re");

	MpdPairSharedPadsCut *sharedPads = new MpdPairSharedPadsCut();
	femto->Add(sharedPads,"{4x1}+re");
	if(opt->GetOption(dummy, "hits")){
		Double_t dcaz = 0;
		dcaz = dummy;
		sharedPads->SetMinMax(0, dcaz, sharedPads->SharedPads());
	}


// make cut monitors
	femto->Add(GetMonitor(sharedPads->CutName("re"),sharedPads->SharedPads(),40,0,100));
	femto->Add(GetMonitor(sharedPads->CutName("re"),sharedPads->OverlappedPads(),40,0,100));

	femto->Add(GetMonitor(dqinv->CutName(),dqinv->Absolute(),50,-0.05,0.05));
	femto->Add(GetMonitor(dqinv->CutName(),dqinv->Relative(),50,-25,25));
	femto->Add(GetMonitor(aliceQ->CutName("re"),0,50,-0.5,1));

	femto->Add(GetMonitor(entry->CutName("re"),entry->XYZ(),50,0,25));
	femto->Add(GetMonitor(entry->CutName("re"),entry->XY(),50,0,25));
	femto->Add(GetMonitor(entry->CutName("re"),entry->Z(),50,0,25));

	femto->Add(GetMonitor(sep->CutName("re"),sep->Entry(),200,0,20));
	femto->Add(GetMonitor(sep->CutName("re"),sep->Average(),50,0,25));

}


