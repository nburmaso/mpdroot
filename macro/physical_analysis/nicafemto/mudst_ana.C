/*
 * mudst_ana.C
 *
 *  Created on: 30 sty 2019
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */

#if !defined(__CINT__) && !defined(__CLING__)
#include "NicaFemtoKtAna.h"
#include "NicaFemtoBasicAna.h"
#include "NicaFemtoWeightGeneratorLednicky.h"
#include "NicaFemto1DCF.h"
#include "NicaConst.h"
#include "NicaMpdDstMCEventTpcPads.h"
#include "MpdPIDOnTheFly.h"
#endif

#include "pair_cut.C"
#include "track_cut.C"


NicaFemtoKtAna *GetAna(){

	NicaFemtoWeightGeneratorLednicky *c = new NicaFemtoWeightGeneratorLednicky();
	c->SetStrongOff();
	c->SetQuantumOn();
	c->SetCoulOn();

	NicaFemtoSourceModel1D *gaus = new NicaFemtoSourceModelGaus1D();
	gaus->SetRadii(5.0);
	NicaFemtoFreezoutGeneratorLCMS *lcms = new NicaFemtoFreezoutGeneratorLCMS();
	lcms->SetSourceModel(gaus);

	TString background_option = NicaTwoTrackAna::BackgroundOptionMixed();


	NicaFemtoKtAna *femto = new NicaFemtoKtAna();
	NicaFemto1DCF *cf = new NicaFemto1DCF("cf",150,0,0.15,kLCMS);
	femto->SetCorrFctn(cf);
	femto->AddKtBin(0, 2);
	femto->SetOption("ignore_sign");
	femto->Add(lcms);
	femto->Add(c);
	femto->SetOption(background_option);
	femto->SetMixSize(8);
	femto->SetMass(NicaConst::PionPlusMass());
	NicaMpdDstMCEventTpcPads *event = new NicaMpdDstMCEventTpcPads();
	event->OnlyGlobal();
	femto->SetFormat(event);
	return femto;
}
void mudst_ana(TString inFile, TString outFile){
	TFile *file = TFile::Open(inFile);
	file->Close();
	FairRunAna *ana = new FairRunAna();
	FairFileSource *filex = new FairFileSource(inFile);
	ana->SetSource(filex);
	ana->SetOutputFile(outFile);
	/*set magnetic field (neede for helix calculation, there is problem with
	reading this directly from file*/
	NicaEventAna::SetMagField(0.5);
	NicaFemtoKtAna *femto = GetAna();
	PrepTrackCuts(femto,kFALSE);

	Double_t shared_fraction = 0;
	Double_t separation_dist  = 2;
	Double_t max_z_dca[4] = {100,0.4,0.2,0.2};
	/*create options and pass them to pair cut maker
	 *  options must be integers
	 */
	NicaOptionConverter *opt = new NicaOptionConverter();
	opt->RegisterOption("shared", shared_fraction*100);
	opt->RegisterOption("sep", separation_dist*10);
	pair_cuts(femto, opt);
	femto->SetFormatOption(NicaEventAna::kCompress);
	// for old files - need to fix n-sigma
	ana->AddTask(new MpdPIDOnTheFly());
	//ad femto ana on fixed data
	ana->AddTask(femto);
	ana->Init();
	ana->Run(5);
	cout << "Macro finished successfully." << endl;     // marker of successful execution for CDASH
}




