#include "MpdPidQA.h"

MpdPidQA::MpdPidQA() : MpdPid()
{
  /// Default constructor
  Init("CF");
}

MpdPidQA::MpdPidQA(Double_t sigmaTof, Double_t sigmaEloss, Double_t sqrts, 
	Double_t koef, TString Generator, TString Tracking, TString NSigPart)
	: MpdPid(sigmaTof, sigmaEloss, sqrts, koef, Generator, Tracking, NSigPart)
	{
		Init(Tracking);
	}
	
MpdPidQA::~MpdPidQA() 
{
	for (Int_t i = 0; i < nQAHists; i++) {delete Pi[i]; delete Ka[i]; delete Pr[i];}
	delete m2_pi; delete m2_ka; delete m2_pr;
	delete AllPiPos; delete AllKaPos; delete AllPrPos; delete AllPiNeg; delete AllKaNeg; delete AllPrNeg;
	delete IdPiPos; delete IdKaPos; delete IdPrPos; delete IdPiNeg; delete IdKaNeg; delete IdPrNeg;
	delete IdRightPiPos; delete IdRightKaPos; delete IdRightPrPos; delete IdRightPiNeg; delete IdRightKaNeg; delete IdRightPrNeg;
	delete IdWrongPiPos; delete IdWrongKaPos; delete IdWrongPrPos; delete IdWrongPiNeg; delete IdWrongKaNeg; delete IdWrongPrNeg;
	delete AmplPiPlus; delete AmplPiMinus; delete AmplPrPlus; delete AmplPrMinus; delete AmplKaPlus; delete AmplKaMinus;
}
	
void MpdPidQA::Init(TString Tracking)
{
	cout << "MpdPidQA::Init().." << endl;
	
	for (Int_t i = 0; i < (nQAHists - 8); i++) Xlow[i] = 0.05 * (1. + i);
	Xlow[32] = 1.7; Xlow[33] = 1.8; Xlow[34] = 1.9; Xlow[35] = 2.0; Xlow[36] = 2.1; Xlow[37] = 2.2; Xlow[38] = 2.4; Xlow[39] = 2.6;
	for (Int_t i = 0; i < (nQAHists - 9); i++) Xhigh[i] = 0.05 + 0.05 * (1. + i);
	Xhigh[31] = 1.7; Xhigh[32] = 1.8; Xhigh[33] = 1.9; Xhigh[34] = 2.0; Xhigh[35] = 2.1; Xhigh[36] = 2.2; Xhigh[37] = 2.4; Xhigh[38] = 2.6; Xhigh[39] = 3.0;
	for (Int_t i=0; i<40; i++) X[i] = (Xlow[i] + Xhigh[i]) / 2;
	
	const Int_t nbins = 200, nbins2 = 100;
	
	if (Tracking == "HP")
	{
		Pi[0] = new TH1D("Pi0", "", nbins, 1.e-06, 15.e-06); Pr[0] = new TH1D("Pr0", "", nbins2, 75.e-06, 180.e-06); Ka[0] = new TH1D("Ka0", "", nbins2, 10.e-06, 100.e-06);
		Pi[1] = new TH1D("Pi1", "", nbins, 1.e-06, 6.e-06); Pr[1] = new TH1D("Pr1", "", nbins, 40.e-06, 100.e-06); Ka[1] = new TH1D("Ka1", "", nbins, 10.e-06, 50.e-06);
		Pi[2] = new TH1D("Pi2", "", nbins, 1.e-06, 3.5e-06); Pr[2] = new TH1D("Pr2", "", nbins, 20.e-06, 70.e-06); Ka[2] = new TH1D("Ka2", "", nbins, 3.e-06, 25.e-06);
		Pi[3] = new TH1D("Pi3", "", nbins, 1.e-06, 3.e-06); Pr[3] = new TH1D("Pr3", "", nbins, 10.e-06, 35.e-06); Ka[3] = new TH1D("Ka3", "", nbins, 3.e-06, 12.e-06);
		Pi[4] = new TH1D("Pi4", "", nbins, 1.e-06, 2.5e-06); Pr[4] = new TH1D("Pr4", "", nbins, 10.e-06, 25.e-06); Ka[4] = new TH1D("Ka4", "", nbins, 3.e-06, 8.e-06);
		Pi[5] = new TH1D("Pi5", "", nbins, 1.e-06, 2.5e-06); Pr[5] = new TH1D("Pr5", "", nbins, 8.e-06, 17.e-06); Ka[5] = new TH1D("Ka5", "", nbins, 2.e-06, 7.e-06);
		Pi[6] = new TH1D("Pi6", "", nbins, 1.e-06, 2.2e-06); Pr[6] = new TH1D("Pr6", "", nbins, 6.e-06, 13.e-06); Ka[6] = new TH1D("Ka6", "", nbins, 2.e-06, 5.e-06);
		Pi[7] = new TH1D("Pi7", "", nbins, 1.e-06, 2.1e-06); Pr[7] = new TH1D("Pr7", "", nbins, 5.e-06, 11.e-06); Ka[7] = new TH1D("Ka7", "", nbins, 1.5e-06, 4.5e-06);
		Pi[8] = new TH1D("Pi8", "", nbins, 1.e-06, 2.1e-06); Pr[8] = new TH1D("Pr8", "", nbins, 4.e-06, 9.e-06); Ka[8] = new TH1D("Ka8", "", nbins, 1.5e-06, 4.e-06);
		Pi[9] = new TH1D("Pi9", "", nbins, 1.e-06, 2.1e-06); Pr[9] = new TH1D("Pr9", "", nbins, 3.5e-06, 7.5e-06); Ka[9] = new TH1D("Ka9", "", nbins, 1.5e-06, 3.5e-06);
		Pi[10] = new TH1D("Pi10", "", nbins, 1.e-06, 2.1e-06); Pr[10] = new TH1D("Pr10", "", nbins, 3.e-06, 6.5e-06); Ka[10] = new TH1D("Ka10", "", nbins, 1.5e-06, 3.e-06);
		Pi[11] = new TH1D("Pi11", "", nbins, 1.e-06, 2.1e-06); Pr[11] = new TH1D("Pr11", "", nbins, 3.e-06, 6.e-06); Ka[11] = new TH1D("Ka11", "", nbins, 1.e-06, 3.e-06);
		Pi[12] = new TH1D("Pi12", "", nbins, 1.e-06, 2.1e-06); Pr[12] = new TH1D("Pr12", "", nbins, 2.5e-06, 5.e-06); Ka[12] = new TH1D("Ka12", "", nbins, 1.e-06, 3.e-06);
		Pi[13] = new TH1D("Pi13", "", nbins, 1.e-06, 2.1e-06); Pr[13] = new TH1D("Pr13", "", nbins, 2.e-06, 5.e-06); Ka[13] = new TH1D("Ka13", "", nbins, 1.e-06, 3.e-06);
		Pi[14] = new TH1D("Pi14", "", nbins, 1.e-06, 2.1e-06); Pr[14] = new TH1D("Pr14", "", nbins, 2.e-06, 4.5e-06); Ka[14] = new TH1D("Ka14", "", nbins, 1.e-06, 3.e-06);
		Pi[15] = new TH1D("Pi15", "", nbins, 1.e-06, 2.1e-06); Pr[15] = new TH1D("Pr15", "", nbins, 2.e-06, 4.e-06); Ka[15] = new TH1D("Ka15", "", nbins, 1.e-06, 2.5e-06);
		Pi[16] = new TH1D("Pi16", "", nbins, 1.e-06, 2.1e-06); Pr[16] = new TH1D("Pr16", "", nbins, 2.e-06, 4.e-06); Ka[16] = new TH1D("Ka16", "", nbins, 1.e-06, 2.5e-06);
		Pi[17] = new TH1D("Pi17", "", nbins, 1.e-06, 2.1e-06); Pr[17] = new TH1D("Pr17", "", nbins, 1.5e-06, 3.5e-06); Ka[17] = new TH1D("Ka17", "", nbins, 1.e-06, 2.5e-06);
		Pi[18] = new TH1D("Pi18", "", nbins, 1.e-06, 2.1e-06); Pr[18] = new TH1D("Pr18", "", nbins, 1.5e-06, 3.5e-06); Ka[18] = new TH1D("Ka18", "", nbins, 1.e-06, 2.5e-06);
		Pi[19] = new TH1D("Pi19", "", nbins, 1.e-06, 2.1e-06); Pr[19] = new TH1D("Pr19", "", nbins, 1.5e-06, 3.5e-06); Ka[19] = new TH1D("Ka19", "", nbins, 1.e-06, 2.5e-06);
		Pi[20] = new TH1D("Pi20", "", nbins, 1.e-06, 2.1e-06); Pr[20] = new TH1D("Pr20", "", nbins, 1.5e-06, 3.5e-06); Ka[20] = new TH1D("Ka20", "", nbins, 1.e-06, 2.5e-06);
		Pi[21] = new TH1D("Pi21", "", nbins, 1.e-06, 2.1e-06); Pr[21] = new TH1D("Pr21", "", nbins, 1.5e-06, 3.e-06); Ka[21] = new TH1D("Ka21", "", nbins, 1.e-06, 2.5e-06);
		Pi[22] = new TH1D("Pi22", "", nbins, 1.e-06, 2.1e-06); Pr[22] = new TH1D("Pr22", "", nbins, 1.5e-06, 3.e-06); Ka[22] = new TH1D("Ka22", "", nbins, 1.e-06, 2.5e-06);
		Pi[23] = new TH1D("Pi23", "", nbins, 1.e-06, 2.1e-06); Pr[23] = new TH1D("Pr23", "", nbins, 1.e-06, 3.e-06); Ka[23] = new TH1D("Ka23", "", nbins, 1.e-06, 2.5e-06);
		Pi[24] = new TH1D("Pi24", "", nbins, 1.e-06, 2.1e-06); Pr[24] = new TH1D("Pr24", "", nbins, 1.e-06, 3.e-06); Ka[24] = new TH1D("Ka24", "", nbins, 1.e-06, 2.5e-06);
		Pi[25] = new TH1D("Pi25", "", nbins, 1.e-06, 2.1e-06); Pr[25] = new TH1D("Pr25", "", nbins, 1.e-06, 3.e-06); Ka[25] = new TH1D("Ka25", "", nbins, 1.e-06, 2.5e-06);
		Pi[26] = new TH1D("Pi26", "", nbins, 1.e-06, 2.1e-06); Pr[26] = new TH1D("Pr26", "", nbins, 1.e-06, 3.e-06); Ka[26] = new TH1D("Ka26", "", nbins, 1.e-06, 2.5e-06);
		Pi[27] = new TH1D("Pi27", "", nbins, 1.e-06, 2.1e-06); Pr[27] = new TH1D("Pr27", "", nbins, 1.e-06, 3.e-06); Ka[27] = new TH1D("Ka27", "", nbins, 1.e-06, 2.5e-06);
		Pi[28] = new TH1D("Pi28", "", nbins, 1.e-06, 2.1e-06); Pr[28] = new TH1D("Pr28", "", nbins, 1.e-06, 3.e-06); Ka[28] = new TH1D("Ka28", "", nbins, 1.e-06, 2.5e-06);
		Pi[29] = new TH1D("Pi29", "", nbins, 1.e-06, 2.1e-06); Pr[29] = new TH1D("Pr29", "", nbins, 1.e-06, 3.e-06); Ka[29] = new TH1D("Ka29", "", nbins, 1.e-06, 2.5e-06);
		Pi[30] = new TH1D("Pi30", "", nbins, 1.e-06, 2.1e-06); Pr[30] = new TH1D("Pr30", "", nbins, 1.e-06, 3.e-06); Ka[30] = new TH1D("Ka30", "", nbins, 1.e-06, 2.5e-06);
		Pi[31] = new TH1D("Pi31", "", nbins, 1.e-06, 2.1e-06); Pr[31] = new TH1D("Pr31", "", nbins, 1.e-06, 2.5e-06); Ka[31] = new TH1D("Ka31", "", nbins, 1.e-06, 2.5e-06);
		Pi[32] = new TH1D("Pi32", "", nbins, 1.e-06, 2.1e-06); Pr[32] = new TH1D("Pr32", "", nbins, 1.e-06, 2.5e-06); Ka[32] = new TH1D("Ka32", "", nbins, 1.e-06, 2.5e-06);
		Pi[33] = new TH1D("Pi33", "", nbins, 1.e-06, 2.1e-06); Pr[33] = new TH1D("Pr33", "", nbins, 1.e-06, 2.5e-06); Ka[33] = new TH1D("Ka33", "", nbins, 1.e-06, 2.5e-06);
		Pi[34] = new TH1D("Pi34", "", nbins, 1.e-06, 2.1e-06); Pr[34] = new TH1D("Pr34", "", nbins, 1.e-06, 2.5e-06); Ka[34] = new TH1D("Ka34", "", nbins, 1.e-06, 2.5e-06);
		Pi[35] = new TH1D("Pi35", "", nbins, 1.e-06, 2.1e-06); Pr[35] = new TH1D("Pr35", "", nbins, 1.e-06, 2.5e-06); Ka[35] = new TH1D("Ka35", "", nbins, 1.e-06, 2.5e-06);
		Pi[36] = new TH1D("Pi36", "", nbins, 1.e-06, 2.1e-06); Pr[36] = new TH1D("Pr36", "", nbins, 1.e-06, 2.5e-06); Ka[36] = new TH1D("Ka36", "", nbins, 1.e-06, 2.5e-06);
		Pi[37] = new TH1D("Pi37", "", nbins, 1.e-06, 2.1e-06); Pr[37] = new TH1D("Pr37", "", nbins, 1.e-06, 2.5e-06); Ka[37] = new TH1D("Ka37", "", nbins, 1.e-06, 2.5e-06);
		Pi[38] = new TH1D("Pi38", "", nbins, 1.e-06, 2.1e-06); Pr[38] = new TH1D("Pr38", "", nbins, 1.e-06, 2.5e-06); Ka[38] = new TH1D("Ka38", "", nbins, 1.e-06, 2.5e-06);
		Pi[39] = new TH1D("Pi39", "", nbins, 1.e-06, 2.1e-06); Pr[39] = new TH1D("Pr39", "", nbins, 1.e-06, 2.5e-06); Ka[39] = new TH1D("Ka39", "", nbins, 1.e-06, 2.5e-06);
	}
	else
	{
		Pi[0] = new TH1D("Pi0", "", nbins, 1000., 25000.); Pr[0] = new TH1D("Pr0", "", nbins2, 100000., 800000.); Ka[0] = new TH1D("Ka0", "", nbins2, 10000., 10000.);
		Pi[1] = new TH1D("Pi1", "", nbins, 2000., 10000.); Pr[1] = new TH1D("Pr1", "", nbins, 50000., 250000.); Ka[1] = new TH1D("Ka1", "", nbins, 12000., 60000.);
		Pi[2] = new TH1D("Pi2", "", nbins, 2000., 8000.); Pr[2] = new TH1D("Pr2", "", nbins, 20000., 180000.); Ka[2] = new TH1D("Ka2", "", nbins, 8000., 40000.);
		Pi[3] = new TH1D("Pi3", "", nbins, 2000., 6000.); Pr[3] = new TH1D("Pr3", "", nbins, 15000., 90000.); Ka[3] = new TH1D("Ka3", "", nbins, 5000., 22000.);
		Pi[4] = new TH1D("Pi4", "", nbins, 2000., 6000.); Pr[4] = new TH1D("Pr4", "", nbins, 10000., 50000.); Ka[4] = new TH1D("Ka4", "", nbins, 3000., 17000.);
		Pi[5] = new TH1D("Pi5", "", nbins, 2000., 6000.); Pr[5] = new TH1D("Pr5", "", nbins, 10000., 30000.); Ka[5] = new TH1D("Ka5", "", nbins, 2000., 13000.);
		Pi[6] = new TH1D("Pi6", "", nbins, 2000., 6000.); Pr[6] = new TH1D("Pr6", "", nbins, 8000., 25000.); Ka[6] = new TH1D("Ka6", "", nbins, 2000., 13000.);
		Pi[7] = new TH1D("Pi7", "", nbins, 2000., 6000.); Pr[7] = new TH1D("Pr7", "", nbins, 8000., 19000.); Ka[7] = new TH1D("Ka7", "", nbins, 2000., 10000.);
		Pi[8] = new TH1D("Pi8", "", nbins, 2000., 6000.); Pr[8] = new TH1D("Pr8", "", nbins, 8000., 17000.); Ka[8] = new TH1D("Ka8", "", nbins, 2000., 10000.);
		Pi[9] = new TH1D("Pi9", "", nbins, 2000., 6000.); Pr[9] = new TH1D("Pr9", "", nbins, 5000., 15000.); Ka[9] = new TH1D("Ka9", "", nbins, 2000., 9000.);
		Pi[10] = new TH1D("Pi10", "", nbins, 2000., 6000.); Pr[10] = new TH1D("Pr10", "", nbins, 5000., 15000.); Ka[10] = new TH1D("Ka10", "", nbins, 2000., 9000.);
		Pi[11] = new TH1D("Pi11", "", nbins, 2000., 6000.); Pr[11] = new TH1D("Pr11", "", nbins, 5000., 15000.); Ka[11] = new TH1D("Ka11", "", nbins, 2000., 7000.);
		Pi[12] = new TH1D("Pi12", "", nbins, 2000., 6000.); Pr[12] = new TH1D("Pr12", "", nbins, 5000., 12000.); Ka[12] = new TH1D("Ka12", "", nbins, 2000., 7000.);
		Pi[13] = new TH1D("Pi13", "", nbins, 2000., 6000.); Pr[13] = new TH1D("Pr13", "", nbins, 5000., 12000.); Ka[13] = new TH1D("Ka13", "", nbins, 2000., 6000.);
		Pi[14] = new TH1D("Pi14", "", nbins, 2000., 6000.); Pr[14] = new TH1D("Pr14", "", nbins, 2000., 12000.); Ka[14] = new TH1D("Ka14", "", nbins, 1000., 6000.);
		Pi[15] = new TH1D("Pi15", "", nbins, 2000., 6000.); Pr[15] = new TH1D("Pr15", "", nbins, 3000., 9000.); Ka[15] = new TH1D("Ka15", "", nbins, 1000., 5000.);
		Pi[16] = new TH1D("Pi16", "", nbins, 2000., 6000.); Pr[16] = new TH1D("Pr16", "", nbins, 3000., 9000.); Ka[16] = new TH1D("Ka16", "", nbins, 1000., 5000.);
		Pi[17] = new TH1D("Pi17", "", nbins, 2000., 6000.); Pr[17] = new TH1D("Pr17", "", nbins, 3000., 8000.); Ka[17] = new TH1D("Ka17", "", nbins, 1000., 5000.);
		Pi[18] = new TH1D("Pi18", "", nbins, 2000., 6000.); Pr[18] = new TH1D("Pr18", "", nbins, 3000., 8000.); Ka[18] = new TH1D("Ka18", "", nbins, 1000., 5000.);
		Pi[19] = new TH1D("Pi19", "", nbins, 2000., 6000.); Pr[19] = new TH1D("Pr19", "", nbins, 3000., 8000.); Ka[19] = new TH1D("Ka19", "", nbins, 1000., 5000.);
		Pi[20] = new TH1D("Pi20", "", nbins, 2000., 6000.); Pr[20] = new TH1D("Pr20", "", nbins, 3000., 7000.); Ka[20] = new TH1D("Ka20", "", nbins, 1000., 5000.);
		Pi[21] = new TH1D("Pi21", "", nbins, 2000., 6000.); Pr[21] = new TH1D("Pr21", "", nbins, 3000., 7000.); Ka[21] = new TH1D("Ka21", "", nbins, 1000., 5000.);
		Pi[22] = new TH1D("Pi22", "", nbins, 2000., 6000.); Pr[22] = new TH1D("Pr22", "", nbins, 3000., 7000.); Ka[22] = new TH1D("Ka22", "", nbins, 1000., 5000.);
		Pi[23] = new TH1D("Pi23", "", nbins, 2000., 6000.); Pr[23] = new TH1D("Pr23", "", nbins, 2000., 6000.); Ka[23] = new TH1D("Ka23", "", nbins, 1000., 5000.);
		Pi[24] = new TH1D("Pi24", "", nbins, 2000., 6000.); Pr[24] = new TH1D("Pr24", "", nbins, 2000., 6000.); Ka[24] = new TH1D("Ka24", "", nbins, 1000., 5000.);
		Pi[25] = new TH1D("Pi25", "", nbins, 2000., 6000.); Pr[25] = new TH1D("Pr25", "", nbins, 2000., 6000.); Ka[25] = new TH1D("Ka25", "", nbins, 1000., 5000.);
		Pi[26] = new TH1D("Pi26", "", nbins, 2000., 6000.); Pr[26] = new TH1D("Pr26", "", nbins, 2000., 6000.); Ka[26] = new TH1D("Ka26", "", nbins, 1000., 5000.);
		Pi[27] = new TH1D("Pi27", "", nbins, 2000., 6000.); Pr[27] = new TH1D("Pr27", "", nbins, 2000., 6000.); Ka[27] = new TH1D("Ka27", "", nbins, 1000., 5000.);
		Pi[28] = new TH1D("Pi28", "", nbins, 2000., 6000.); Pr[28] = new TH1D("Pr28", "", nbins, 2000., 6000.); Ka[28] = new TH1D("Ka28", "", nbins, 1000., 5000.);
		Pi[29] = new TH1D("Pi29", "", nbins, 2000., 6000.); Pr[29] = new TH1D("Pr29", "", nbins, 2000., 6000.); Ka[29] = new TH1D("Ka29", "", nbins, 1000., 5000.);
		Pi[30] = new TH1D("Pi30", "", nbins, 2000., 6000.); Pr[30] = new TH1D("Pr30", "", nbins, 2000., 6000.); Ka[30] = new TH1D("Ka30", "", nbins, 1000., 5000.);
		Pi[31] = new TH1D("Pi31", "", nbins, 2000., 6000.); Pr[31] = new TH1D("Pr31", "", nbins, 2000., 6000.); Ka[31] = new TH1D("Ka31", "", nbins, 1000., 5000.);
		Pi[32] = new TH1D("Pi32", "", nbins, 2000., 6000.); Pr[32] = new TH1D("Pr32", "", nbins, 2000., 6000.); Ka[32] = new TH1D("Ka32", "", nbins, 1000., 5000.);
		Pi[33] = new TH1D("Pi33", "", nbins, 2000., 6000.); Pr[33] = new TH1D("Pr33", "", nbins, 2000., 6000.); Ka[33] = new TH1D("Ka33", "", nbins, 1000., 5000.);
		Pi[34] = new TH1D("Pi34", "", nbins, 2000., 6000.); Pr[34] = new TH1D("Pr34", "", nbins, 2000., 6000.); Ka[34] = new TH1D("Ka34", "", nbins, 1000., 5000.);
		Pi[35] = new TH1D("Pi35", "", nbins, 2000., 6000.); Pr[35] = new TH1D("Pr35", "", nbins, 2000., 6000.); Ka[35] = new TH1D("Ka35", "", nbins, 1000., 5000.);
		Pi[36] = new TH1D("Pi36", "", nbins, 2000., 6000.); Pr[36] = new TH1D("Pr36", "", nbins, 2000., 6000.); Ka[36] = new TH1D("Ka36", "", nbins, 1000., 5000.);
		Pi[37] = new TH1D("Pi37", "", nbins, 2000., 6000.); Pr[37] = new TH1D("Pr37", "", nbins, 2000., 6000.); Ka[37] = new TH1D("Ka37", "", nbins, 1000., 5000.);
		Pi[38] = new TH1D("Pi38", "", nbins, 2000., 6000.); Pr[38] = new TH1D("Pr38", "", nbins, 2000., 6000.); Ka[38] = new TH1D("Ka38", "", nbins, 1000., 5000.);
		Pi[39] = new TH1D("Pi39", "", nbins, 2000., 6000.); Pr[39] = new TH1D("Pr39", "", nbins, 2000., 6000.); Ka[39] = new TH1D("Ka39", "", nbins, 1000., 5000.);
	}
	
	m2_pi = new TH2D ("m2_pi", "", nbins, 0., 4., nbins, -0.5, 1.5);
	m2_pr = new TH2D ("m2_pr", "", nbins, 0., 4., nbins, -0.5, 1.5);
	m2_ka = new TH2D ("m2_ka", "", nbins, 0., 4., nbins, -0.5, 1.5);
	
	AllPiPos = new TH1D("AllPiPos","",60,0.,3.); AllPiNeg = new TH1D("AllPiNeg","",60,0.,3.);
	AllPrPos = new TH1D("AllPrPos","",60,0.,3.); AllPrNeg = new TH1D("AllPrNeg","",60,0.,3.);
	AllKaPos = new TH1D("AllKaPos","",60,0.,3.); AllKaNeg = new TH1D("AllKaNeg","",60,0.,3.);
	IdPiPos = new TH1D("IdPiPos","",60,0.,3.); IdPiNeg = new TH1D("IdPiNeg","",60,0.,3.);
	IdPrPos = new TH1D("IdPrPos","",60,0.,3.); IdPrNeg = new TH1D("IdPrNeg","",60,0.,3.);
	IdKaPos = new TH1D("IdKaPos","",60,0.,3.); IdKaNeg = new TH1D("IdKaNeg","",60,0.,3.);
	IdRightPiPos = new TH1D("IdRightPiPos","",60,0.,3.); IdRightPiNeg = new TH1D("IdRightPiNeg","",60,0.,3.);
	IdRightPrPos = new TH1D("IdRightPrPos","",60,0.,3.); IdRightPrNeg = new TH1D("IdRightPrNeg","",60,0.,3.);
	IdRightKaPos = new TH1D("IdRightKaPos","",60,0.,3.); IdRightKaNeg = new TH1D("IdRightKaNeg","",60,0.,3.);
	IdWrongPiPos = new TH1D("IdWrongPiPos","",60,0.,3.); IdWrongPiNeg = new TH1D("IdWrongPiNeg","",60,0.,3.);
	IdWrongPrPos = new TH1D("IdWrongPrPos","",60,0.,3.); IdWrongPrNeg = new TH1D("IdWrongPrNeg","",60,0.,3.);
	IdWrongKaPos = new TH1D("IdWrongKaPos","",60,0.,3.); IdWrongKaNeg = new TH1D("IdWrongKaNeg","",60,0.,3.);
	
	AmplPiPlus = new TH1D("AmplPiPlus","",600,0.,3.); AmplPiMinus = new TH1D("AmplPiMinus","",600,0.,3.);
	AmplPrPlus = new TH1D("AmplPrPlus","",600,0.,3.); AmplPrMinus = new TH1D("AmplPrMinus","",600,0.,3.);
	AmplKaPlus = new TH1D("AmplKaPlus","",600,0.,3.); AmplKaMinus = new TH1D("AmplKaMinus","",600,0.,3.);
}

void MpdPidQA::FillDedxHists(Double_t p, Double_t dedx, Int_t pdg)
{
	Int_t pdgc = TMath::Abs(pdg);
	for (Int_t i = 0; i < nQAHists; i++)
	{
		if ( (p > Xlow[i]) && (p <= Xhigh[i]) )
		switch(pdgc)
		{
			case 211: Pi[i]->Fill(dedx); break;
			case 2212: Pr[i]->Fill(dedx); break;
			case 321: Ka[i]->Fill(dedx); break;
			default: break;
		}
	}
}

void MpdPidQA::Fillm2Hists(Double_t p, Double_t m2, Int_t pdg)
{
	Int_t pdgc = TMath::Abs(pdg);
	switch(pdgc)
	{
		case 211: m2_pi->Fill(p, m2); break;
		case 2212: m2_pr->Fill(p, m2); break;
		case 321: m2_ka->Fill(p, m2); break;
		default: break;
	}
}

void MpdPidQA::FillAmplHists(Double_t p, Int_t pdg)
{
	if (p <= 3.0)
	{
		switch (pdg)
		{
			case 211: AmplPiPlus->Fill(p); break;
			case -211: AmplPiMinus->Fill(p); break;
			case 2212: AmplPrPlus->Fill(p); break;
			case -2212: AmplPrMinus->Fill(p); break;
			case 321: AmplKaPlus->Fill(p); break;
			case -321: AmplKaMinus->Fill(p); break;
			default: break;
		}
	}
}

Bool_t MpdPidQA::FillEffContHists(MpdTrack* track, Int_t pdg, Double_t fProbCut)
{
	Double_t px=track->GetPx(),py=track->GetPy(),pz=track->GetPz();   
	Double_t p=TMath::Sqrt(px*px+py*py+pz*pz);
	FillEffDenominator(p, pdg);
	if (!FillProbs(track)) return kFALSE;
	Double_t pion = GetProbPi();
	Double_t electron = GetProbEl();
	Double_t proton = GetProbPr();
	Double_t kaon = GetProbKa();
	Double_t Probs[] = {0, electron, pion, kaon, proton};
	Double_t maxprob =  TMath::MaxElement(5, Probs);
	if (maxprob < fProbCut) return kFALSE;
	Int_t pidpdg = GetMaxProb();
	FillEffContHists(p, pidpdg, pdg);
	return kTRUE;
}

Bool_t MpdPidQA::FillEffContHists(Double_t p, Double_t dedx, Int_t charge, Int_t pdg, Double_t fProbCut)
{
	FillEffDenominator(p, pdg);
	if (!FillProbs(p, dedx, charge)) return kFALSE;
	Double_t pion = GetProbPi();
	Double_t electron = GetProbEl();
	Double_t proton = GetProbPr();
	Double_t kaon = GetProbKa();
	Double_t Probs[] = {0, electron, pion, kaon, proton};
	Double_t maxprob =  TMath::MaxElement(5, Probs);
	if (maxprob < fProbCut) return kFALSE;
	Int_t pidpdg = GetMaxProb();
	FillEffContHists(p, pidpdg, pdg);
	return kTRUE;
}

Bool_t MpdPidQA::FillEffContHists(Double_t p, Double_t dedx, Double_t m2, Int_t charge, Int_t pdg, Double_t fProbCut)
{
	FillEffDenominator(p, pdg);
	if (!FillProbs(p, dedx, m2, charge)) return kFALSE;
	Double_t pion = GetProbPi();
	Double_t electron = GetProbEl();
	Double_t proton = GetProbPr();
	Double_t kaon = GetProbKa();
	Double_t Probs[] = {0, electron, pion, kaon, proton};
	Double_t maxprob =  TMath::MaxElement(5, Probs);
	if (maxprob < fProbCut) return kFALSE;
	Int_t pidpdg = GetMaxProb();
	FillEffContHists(p, pidpdg, pdg);
	return kTRUE;
}

Bool_t MpdPidQA::FillEffContHists(Double_t p, Int_t pidpdg, Int_t pdg)
{
	switch(pidpdg)
	{
		case 211: IdPiPos->Fill(p); break;
		case -211: IdPiNeg->Fill(p); break;
		case 2212: IdPrPos->Fill(p); break;
		case -2212: IdPrNeg->Fill(p); break;
		case 321: IdKaPos->Fill(p); break;
		case -321: IdKaNeg->Fill(p); break;
		default: break;
	}
	
	if(pidpdg == pdg)
	{
		switch(pidpdg) 
		{
			case 211: IdRightPiPos->Fill(p); break;
			case -211: IdRightPiNeg->Fill(p); break;
			case 2212: IdRightPrPos->Fill(p); break;
			case -2212: IdRightPrNeg->Fill(p); break;
			case 321: IdRightKaPos->Fill(p); break;
			case -321: IdRightKaNeg->Fill(p); break;
			default: break;
		}
	}
	else //pdgc != pdg fill falsly identified
	{
		switch(pidpdg)
		{
			case 211: IdWrongPiPos->Fill(p); break;
			case -211: IdWrongPiNeg->Fill(p); break;
			case 2212: IdWrongPrPos->Fill(p); break;
			case -2212: IdWrongPrNeg->Fill(p); break;
			case 321: IdWrongKaPos->Fill(p); break;
			case -321: IdWrongKaNeg->Fill(p); break;
			default: break;
		}
	}
	return kTRUE;
}

void MpdPidQA::FillEffDenominator(Double_t p, Int_t pdg)
{
	switch (pdg)
	{
		case 211: AllPiPos->Fill(p); break;
		case -211: AllPiNeg->Fill(p); break;
		case 2212: AllPrPos->Fill(p); break;
		case -2212: AllPrNeg->Fill(p); break;
		case 321: AllKaPos->Fill(p); break;
		case -321: AllKaNeg->Fill(p); break;
		default: break;
	}
}

void MpdPidQA::GetDedxQA(TString dir)
{
	Double_t Y_Pi[nQAHists], Y_Ka[nQAHists], Y_Pr[nQAHists];
	Double_t sigmaPi[nQAHists], sigmaPr[nQAHists], sigmaKa[nQAHists], sigma1, sigma2, mom;
	TString pistr = "x*" + parPiBB->GetExpFormula();
	TString kastr = "x*" + parKaBB->GetExpFormula();
	TString prstr = "x*" + parPrBB->GetExpFormula();
	TF1 *parPiBBMultX = new TF1("parPiBBMultX", pistr, 0., 3.); for (Int_t k=0; k<5; k++) {parPiBBMultX->SetParameter(k, parPiBB->GetParameter(k));}
	TF1 *parKaBBMultX = new TF1("parKaBBMultX", kastr, 0., 3.); for (Int_t k=0; k<5; k++) {parKaBBMultX->SetParameter(k, parKaBB->GetParameter(k));}
	TF1 *parPrBBMultX = new TF1("parPrBBMultX", prstr, 0., 3.); for (Int_t k=0; k<5; k++) {parPrBBMultX->SetParameter(k, parPrBB->GetParameter(k));}
	
	for (Int_t i = 0; i < nQAHists; i++)
	{
		TF1* GausPi = new TF1("GausPi", "gaus", Pi[i]->GetXaxis()->GetXmin(), Pi[i]->GetXaxis()->GetXmax());
		Pi[i]->Fit(GausPi, "Q0R");
		TF1* AGausPi = new TF1("AGausPi", this, &MpdPidQA::AsymGaus, Pi[i]->GetXaxis()->GetXmin(), Pi[i]->GetXaxis()->GetXmax(), 4, "MpdPid", "AsymGaus");
		AGausPi->SetParameters(GausPi->GetParameter(0), GausPi->GetParameter(1), GausPi->GetParameter(2), 0.01);
		Pi[i]->Fit("AGausPi","Q0RW");
		mom = parPiBBMultX->Integral(Xlow[i],Xhigh[i])/(parPiBB->Integral(Xlow[i],Xhigh[i]));
		Y_Pi[i] = AGausPi->GetParameter(1); Y_Pi[i] /= GetDedxPiParam(mom);
		sigma1 = AGausPi->GetParameter(2); sigma2 = (1. + AGausPi->GetParameter(3)) * AGausPi->GetParameter(2);
		sigmaPi[i] = (sigma1 < sigma2) ? sigma1 : sigma2; sigmaPi[i] /= GetDedxPiParam(mom);
		delete GausPi; delete AGausPi;
		
		TF1* GausKa = new TF1("GausKa", "gaus", Ka[i]->GetXaxis()->GetXmin(), Ka[i]->GetXaxis()->GetXmax());
		Ka[i]->Fit(GausKa, "Q0R");
		TF1* AGausKa = new TF1("AGausKa", this, &MpdPidQA::AsymGaus, Ka[i]->GetXaxis()->GetXmin(), Ka[i]->GetXaxis()->GetXmax(), 4, "MpdPid", "AsymGaus");
		AGausKa->SetParameters(GausKa->GetParameter(0), GausKa->GetParameter(1), GausKa->GetParameter(2), 0.01);
		Ka[i]->Fit("AGausKa","Q0RW");
		mom = parKaBBMultX->Integral(Xlow[i],Xhigh[i])/(parKaBB->Integral(Xlow[i],Xhigh[i]));
		Y_Ka[i] = AGausKa->GetParameter(1); Y_Ka[i] /= GetDedxKaParam(mom);
		sigma1 = AGausKa->GetParameter(2); sigma2 = (1. + AGausKa->GetParameter(3)) * AGausKa->GetParameter(2);
		sigmaKa[i] = (sigma1 < sigma2) ? sigma1 : sigma2; sigmaKa[i] /= GetDedxKaParam(mom);
		delete GausKa; delete AGausKa;
		
		TF1* GausPr = new TF1("GausPr", "gaus", Pr[i]->GetXaxis()->GetXmin(), Pr[i]->GetXaxis()->GetXmax());
		Pr[i]->Fit(GausPr, "Q0R");
		TF1* AGausPr = new TF1("AGausPr", this, &MpdPidQA::AsymGaus, Pr[i]->GetXaxis()->GetXmin(), Pr[i]->GetXaxis()->GetXmax(), 4, "MpdPid", "AsymGaus");
		AGausPr->SetParameters(GausPr->GetParameter(0), GausPr->GetParameter(1), GausPr->GetParameter(2), 0.01);
		Pr[i]->Fit("AGausPr","Q0RW");
		mom = parPrBBMultX->Integral(Xlow[i],Xhigh[i])/(parPrBB->Integral(Xlow[i],Xhigh[i]));
		Y_Pr[i] = AGausPr->GetParameter(1); Y_Pr[i] /= GetDedxPrParam(mom);
		sigma1 = AGausPr->GetParameter(2); sigma2 = (1. + AGausPr->GetParameter(3)) * AGausPr->GetParameter(2);
		sigmaPr[i] = (sigma1 < sigma2) ? sigma1 : sigma2; sigmaPr[i] /= GetDedxPrParam(mom);
		delete GausPr; delete AGausPr;
	}
	Double_t X_err[40]; for (Int_t i=0; i<nQAHists; i++) X_err[i] = 0.;
	
	TGraphErrors *grPions = new TGraphErrors(nQAHists,X,Y_Pi,X_err,sigmaPi);
	TGraphErrors *grKaons = new TGraphErrors(nQAHists,X,Y_Ka,X_err,sigmaKa);
	TGraphErrors *grProtons = new TGraphErrors(nQAHists,X,Y_Pr,X_err,sigmaPr);
	
	SaveDedxGraphs(grPions, grKaons, grProtons, dir);
	
	delete parPiBBMultX; delete parKaBBMultX; delete parPrBBMultX; delete grPions; delete grKaons; delete grProtons;
}

void MpdPidQA::Getm2QA(TString dir)
{
	TF1 *fitgausPi = new TF1 ("fitgausPi", "gaus", -0.4, 0.4);
	TF1 *fitgausPr = new TF1 ("fitgausPr", "gaus", 0.1, 1.5);
	TF1 *fitgausKa = new TF1 ("fitgausKa", "gaus", -0.2, 0.8);
	
	Int_t nbinsx = 200, minbin, maxbin;
	
	for (Int_t i=1; i<=nbinsx; i++)
	{
		if (m2_pi->GetXaxis()->GetBinCenter(i) < 0.2) continue;
		minbin = i; break;
	}
	maxbin = nbinsx;
	for (Int_t i=minbin; i<=nbinsx; i++)
	{
		if (m2_pi->GetXaxis()->GetBinCenter(i) < 3.0) continue;
		maxbin = i-1; break;
	}
	m2_pi->FitSlicesY(fitgausPi, minbin, maxbin, 0, "QNR G4", 0);
	TH1D* m2_pi_2 = (TH1D*)gDirectory->Get("m2_pi_2");
	m2_pi_2->SetAxisRange(m2_pi_2->GetBinLowEdge(minbin), m2_pi_2->GetBinLowEdge(maxbin+1));
	
	for (Int_t i=1; i<=nbinsx; i++)
	{
		if (m2_pr->GetXaxis()->GetBinCenter(i) < 0.35) continue;
		minbin = i; break;
	}
	maxbin = nbinsx;
	for (Int_t i=minbin; i<=nbinsx; i++)
	{
		if (m2_pr->GetXaxis()->GetBinCenter(i) < 2.2) continue;
		maxbin = i-1; break;
	}
	m2_pr->FitSlicesY(fitgausPr, minbin, maxbin, 0, "QNR G5", 0);
	TH1D* m2_pr_2 = (TH1D*)gDirectory->Get("m2_pr_2");
	m2_pr_2->SetAxisRange(m2_pr_2->GetBinLowEdge(minbin), m2_pr_2->GetBinLowEdge(maxbin+1));
	
	for (Int_t i=1; i<=nbinsx; i++)
	{
		if (m2_ka->GetXaxis()->GetBinCenter(i) < 0.2) continue;
		minbin = i; break;
	}
	maxbin = nbinsx;
	for (Int_t i=minbin; i<=nbinsx; i++)
	{
		if (m2_ka->GetXaxis()->GetBinCenter(i) < 2.8) continue;
		maxbin = i-1; break;
	}
	m2_ka->FitSlicesY(fitgausKa, minbin, maxbin, 0, "QNR G4", 0);
	TH1D* m2_ka_2 = (TH1D*)gDirectory->Get("m2_ka_2");
	m2_ka_2->SetAxisRange(m2_ka_2->GetBinLowEdge(minbin), m2_ka_2->GetBinLowEdge(maxbin+1));
	
	Savem2Hists(m2_pi_2, m2_ka_2, m2_pr_2, dir);
	
	delete fitgausPi; delete fitgausPr; delete fitgausKa; delete m2_pi_2; delete m2_pr_2; delete m2_ka_2;
}

void MpdPidQA::GetAmplQA(TString dir)
{
	Double_t koefPiPlus = parPiPosMom->GetParameter(0), koefPiMinus = parPiNegMom->GetParameter(0);
	Double_t koefPrPlus = parPrPosMom->GetParameter(0), koefPrMinus = parPrNegMom->GetParameter(0);
	Double_t koefKaPlus = parKaPosMom->GetParameter(0), koefKaMinus = parKaNegMom->GetParameter(0);
	Double_t PrRat = AmplPrPlus->Integral() / AmplPrMinus->Integral();
	if (Multiplicities[4] != 0) parPiPosMom->SetParameter(0, parPiPosMom->GetParameter(0) * AmplPiPlus->Integral() / Multiplicities[4]);
	else cout << "Positive charged pion's multiplicity normalization is incorrect!" << endl;
	if (Multiplicities[5] != 0) parPiNegMom->SetParameter(0, parPiNegMom->GetParameter(0) * AmplPiMinus->Integral() / Multiplicities[5]);
	else cout << "Negative charged pion's multiplicity normalization is incorrect!" << endl;
	if (Multiplicities[6] != 0) parKaPosMom->SetParameter(0, parKaPosMom->GetParameter(0) * AmplKaPlus->Integral() / Multiplicities[6]);
	else cout << "Positive charged kaon's multiplicity normalization is incorrect!" << endl;
	if (Multiplicities[7] != 0) parKaNegMom->SetParameter(0, parKaNegMom->GetParameter(0) * AmplKaMinus->Integral() / Multiplicities[7]);
	else cout << "Negative charged kaon's multiplicity normalization is incorrect!" << endl;
	if (Multiplicities[8] != 0) parPrPosMom->SetParameter(0, parPrPosMom->GetParameter(0) * AmplPrPlus->Integral() / Multiplicities[8]);
	else cout << "Proton's multiplicity normalization is incorrect!" << endl;
	if (Multiplicities[9] != 0) parPrNegMom->SetParameter(0, parPrNegMom->GetParameter(0) * AmplPrMinus->Integral() / Multiplicities[9]);
	else cout << "Anti-proton's multiplicity normalization is incorrect!" << endl;
	SaveAmplHists(dir);
	parPiPosMom->SetParameter(0, koefPiPlus); parPiNegMom->SetParameter(0, koefPiMinus);
	parPrPosMom->SetParameter(0, koefPrPlus); parPrNegMom->SetParameter(0, koefPrMinus);
	parKaPosMom->SetParameter(0, koefKaPlus); parKaNegMom->SetParameter(0, koefKaMinus);
	cout << "Default value N_protons / N_antiprotons = " << prrat << endl;
	cout << "Value from your data = " << PrRat << endl;
	cout << "Use MpdPid::SetPrRat(Double_t) in case of difference" << endl;
}

void MpdPidQA::GetEffContQA(TString dir)
{
	Int_t rebin = 2;
	AllPiPos->Rebin(rebin); AllPiNeg->Rebin(rebin); AllPrPos->Rebin(rebin); AllPrNeg->Rebin(rebin); AllKaPos->Rebin(rebin); AllKaNeg->Rebin(rebin);
	IdPiPos->Rebin(rebin); IdPiNeg->Rebin(rebin); IdPrPos->Rebin(rebin); IdPrNeg->Rebin(rebin); IdKaPos->Rebin(rebin); IdKaNeg->Rebin(rebin);
	IdRightPiPos->Rebin(rebin); IdRightPiNeg->Rebin(rebin); IdRightPrPos->Rebin(rebin); IdRightPrNeg->Rebin(rebin); IdRightKaPos->Rebin(rebin); IdRightKaNeg->Rebin(rebin);
	IdWrongPiPos->Rebin(rebin); IdWrongPiNeg->Rebin(rebin); IdWrongPrPos->Rebin(rebin); IdWrongPrNeg->Rebin(rebin); IdWrongKaPos->Rebin(rebin); IdWrongKaNeg->Rebin(rebin);
	
	IdRightPiPos->Divide(AllPiPos); IdRightPiNeg->Divide(AllPiNeg); IdRightPrPos->Divide(AllPrPos); IdRightPrNeg->Divide(AllPrNeg); IdRightKaPos->Divide(AllKaPos); IdRightKaNeg->Divide(AllKaNeg);
	IdWrongPiPos->Divide(IdPiPos); IdWrongPiNeg->Divide(IdPiNeg); IdWrongPrPos->Divide(IdPrPos); IdWrongPrNeg->Divide(IdPrNeg); IdWrongKaPos->Divide(IdKaPos); IdWrongKaNeg->Divide(IdKaNeg);
	
	SaveEffContHists(dir);
}

void MpdPidQA::SaveDedxGraphs(TGraphErrors *grPions, TGraphErrors *grKaons, TGraphErrors *grProtons, TString dir)
{
	TString name_pions = "dEdXQA_pions.C", name_kaons = "dEdXQA_kaons.C", name_protons = "dEdXQA_protons.C", path;
	
	TCanvas *can1 = new TCanvas ("can1", "<dE/dx> / <Pid Bethe-Bloch> vs momentum (pions)", 1200, 800);
	gPad->SetGrid();
	grPions->SetTitle("");
	grPions->GetYaxis()->SetTitle("<dE/dx> / <Pid Bethe-Bloch>");
	grPions->GetXaxis()->SetTitle("Momentum, GeV/c");
	grPions->SetMarkerStyle(21); grPions->SetMarkerSize(2);
	grPions->GetXaxis()->SetLabelSize(0.06); grPions->GetYaxis()->SetLabelSize(0.06);
	grPions->GetXaxis()->SetTitleSize(0.06); grPions->GetYaxis()->SetTitleSize(0.06);
	grPions->GetXaxis()->CenterTitle(); grPions->GetYaxis()->CenterTitle();
	grPions->GetYaxis()->SetRangeUser(0.8,1.2);
	grPions->Draw("AP");
	path = dir + name_pions;
	can1->SaveAs(path);
	
	TCanvas *can2 = new TCanvas ("can2", "<dE/dx> / <Pid Bethe-Bloch> vs momentum (kaons)", 1200, 800);
	gPad->SetGrid();
	grKaons->SetTitle("");
	grKaons->GetYaxis()->SetTitle("<dE/dx> / <Pid Bethe-Bloch>");
	grKaons->GetXaxis()->SetTitle("Momentum, GeV/c");
	grKaons->SetMarkerStyle(21); grKaons->SetMarkerSize(2);
	grKaons->GetXaxis()->SetLabelSize(0.06); grKaons->GetYaxis()->SetLabelSize(0.06);
	grKaons->GetXaxis()->SetTitleSize(0.06); grKaons->GetYaxis()->SetTitleSize(0.06);
	grKaons->GetXaxis()->CenterTitle(); grKaons->GetYaxis()->CenterTitle();
	grKaons->GetYaxis()->SetRangeUser(0.8,1.2);
	grKaons->Draw("AP");
	path = dir + name_kaons;
	can2->SaveAs(path);
	
	TCanvas *can3 = new TCanvas ("can3", "<dE/dx> / <Pid Bethe-Bloch> vs momentum (protons)", 1200, 800);
	gPad->SetGrid();
	grProtons->SetTitle("");
	grProtons->GetYaxis()->SetTitle("<dE/dx> / <Pid Bethe-Bloch>");
	grProtons->GetXaxis()->SetTitle("Momentum, GeV/c");
	grProtons->SetMarkerStyle(21); grProtons->SetMarkerSize(2);
	grProtons->GetXaxis()->SetLabelSize(0.06); grProtons->GetYaxis()->SetLabelSize(0.06);
	grProtons->GetXaxis()->SetTitleSize(0.06); grProtons->GetYaxis()->SetTitleSize(0.06);
	grProtons->GetXaxis()->CenterTitle(); grProtons->GetYaxis()->CenterTitle();
	grProtons->GetYaxis()->SetRangeUser(0.8,1.2);
	grProtons->Draw("AP");
	path = dir + name_protons;
	can3->SaveAs(path);
	
	delete can1; delete can2; delete can3;
}

void MpdPidQA::Savem2Hists(TH1D *m2_pi_2, TH1D *m2_ka_2, TH1D *m2_pr_2, TString dir)
{
	TString name_pions = "m2QA_pions.C", name_kaons = "m2QA_kaons.C", name_protons = "m2QA_protons.C", path;
	
	TCanvas *can4 = new TCanvas ("can4", "Sigma vs momentum (pions)", 1200, 800);
	gPad->SetGrid();
	m2_pi_2->SetMarkerStyle(22);
	m2_pi_2->SetMarkerSize(2);
	m2_pi_2->SetStats(kFALSE);
	m2_pi_2->SetTitle("");
	m2_pi_2->GetXaxis()->SetLabelSize(0.06); m2_pi_2->GetYaxis()->SetLabelSize(0.06);
	m2_pi_2->GetXaxis()->SetTitleSize(0.08); m2_pi_2->GetYaxis()->SetTitleSize(0.08);
	m2_pi_2->GetXaxis()->CenterTitle(); m2_pi_2->GetYaxis()->CenterTitle();
	m2_pi_2->GetXaxis()->SetTitle("Momentum, GeV/c");
	m2_pi_2->GetYaxis()->SetTitle("#sigma, (GeV/c^{2})^{2}");
	m2_pi_2->Draw("P"); 
	parPiLowPM2->SetRange(0., 1.4); parPiLowPM2->Draw("same"); parPiHighPM2->SetRange(1.4, 3.); parPiHighPM2->Draw("same");
	path = dir + name_pions;
	can4->SaveAs(path);
	
	TCanvas *can5 = new TCanvas ("can5", "Sigma vs momentum (kaons)", 1200, 800);
	gPad->SetGrid();
	m2_ka_2->SetMarkerStyle(22);
	m2_ka_2->SetMarkerSize(2);
	m2_ka_2->SetStats(kFALSE);
	m2_ka_2->SetTitle("");
	m2_ka_2->GetXaxis()->SetLabelSize(0.06); m2_ka_2->GetYaxis()->SetLabelSize(0.06);
	m2_ka_2->GetXaxis()->SetTitleSize(0.08); m2_ka_2->GetYaxis()->SetTitleSize(0.08);
	m2_ka_2->GetXaxis()->CenterTitle(); m2_ka_2->GetYaxis()->CenterTitle();
	m2_ka_2->GetXaxis()->SetTitle("Momentum, GeV/c");
	m2_ka_2->GetYaxis()->SetTitle("#sigma, (GeV/c^{2})^{2}");
	m2_ka_2->Draw("P"); parKaM2->Draw("same");
	path = dir + name_kaons;
	can5->SaveAs(path);
	
	TCanvas *can6 = new TCanvas ("can6", "Sigma vs momentum (protons)", 1200, 800);
	gPad->SetGrid();
	m2_pr_2->SetMarkerStyle(22);
	m2_pr_2->SetMarkerSize(2);
	m2_pr_2->SetStats(kFALSE);
	m2_pr_2->SetTitle("");
	m2_pr_2->GetXaxis()->SetLabelSize(0.06); m2_pr_2->GetYaxis()->SetLabelSize(0.06);
	m2_pr_2->GetXaxis()->SetTitleSize(0.08); m2_pr_2->GetYaxis()->SetTitleSize(0.08);
	m2_pr_2->GetXaxis()->CenterTitle(); m2_pr_2->GetYaxis()->CenterTitle();
	m2_pr_2->GetXaxis()->SetTitle("Momentum, GeV/c");
	m2_pr_2->GetYaxis()->SetTitle("#sigma, (GeV/c^{2})^{2}");
	m2_pr_2->Draw("P");
	parPrLowPM2->SetRange(0., 1.4); parPrLowPM2->Draw("same"); parPrHighPM2->SetRange(1.4, 3.); parPrHighPM2->Draw("same");
	path = dir + name_protons;
	can6->SaveAs(path);
	
	delete can4; delete can5; delete can6;
}

void MpdPidQA::SaveAmplHists(TString dir)
{
	TString name_amplPiPlus = "amplPiPos.C", name_amplKaPlus = "amplKaPos.C", name_amplPrPlus = "amplPrPos.C";
	TString name_amplPiMinus = "amplPiNeg.C", name_amplKaMinus = "amplKaNeg.C", name_amplPrMinus = "amplPrNeg.C", path;
	
	TCanvas *can7 = new TCanvas("can7", "positive charged pions", 1200, 800);
	AmplPiPlus->SetStats(kFALSE); 
	AmplPiPlus->GetXaxis()->SetTitle("Momentum, GeV/c");
	AmplPiPlus->GetYaxis()->SetTitle("Count");
	AmplPiPlus->Draw(); parPiPosMom->Draw("same");
	path = dir + name_amplPiPlus;
	can7->SaveAs(path);
	
	TCanvas *can8 = new TCanvas("can8", "negative charged pions", 1200, 800);
	AmplPiMinus->SetStats(kFALSE); 
	AmplPiMinus->GetXaxis()->SetTitle("Momentum, GeV/c");
	AmplPiMinus->GetYaxis()->SetTitle("Count");
	AmplPiMinus->Draw(); parPiNegMom->Draw("same");
	path = dir + name_amplPiMinus;
	can8->SaveAs(path);
	
	TCanvas *can9 = new TCanvas("can9", "positive charged kaons", 1200, 800);
	AmplKaPlus->SetStats(kFALSE); 
	AmplKaPlus->GetXaxis()->SetTitle("Momentum, GeV/c");
	AmplKaPlus->GetYaxis()->SetTitle("Count");
	AmplKaPlus->Draw(); parKaPosMom->Draw("same");
	path = dir + name_amplKaPlus;
	can9->SaveAs(path);
	
	TCanvas *can10 = new TCanvas("can10", "negative charged kaons", 1200, 800);
	AmplKaMinus->SetStats(kFALSE); 
	AmplKaMinus->GetXaxis()->SetTitle("Momentum, GeV/c");
	AmplKaMinus->GetYaxis()->SetTitle("Count");
	AmplKaMinus->Draw(); parKaNegMom->Draw("same");
	path = dir + name_amplKaMinus;
	can10->SaveAs(path);
	
	TCanvas *can11 = new TCanvas("can11", "protons", 1200, 800);
	AmplPrPlus->SetStats(kFALSE); 
	AmplPrPlus->GetXaxis()->SetTitle("Momentum, GeV/c");
	AmplPrPlus->GetYaxis()->SetTitle("Count");
	AmplPrPlus->Draw(); parPrPosMom->Draw("same");
	path = dir + name_amplPrPlus;
	can11->SaveAs(path);
	
	TCanvas *can12 = new TCanvas("can12", "anti-protons", 1200, 800);
	AmplPrMinus->SetStats(kFALSE); 
	AmplPrMinus->GetXaxis()->SetTitle("Momentum, GeV/c");
	AmplPrMinus->GetYaxis()->SetTitle("Count");
	AmplPrMinus->Draw(); parPrNegMom->Draw("same");
	path = dir + name_amplPrMinus;
	can12->SaveAs(path);
	
	delete can7; delete can8; delete can9; delete can10; delete can11; delete can12;
}

void MpdPidQA::SaveEffContHists(TString dir)
{
	TString name_effpos = "efficiency_pos.C", name_effneg = "efficiency_neg.C", name_contpos = "contamination_pos.C", name_contneg = "contamination_neg.C", path;
	IdRightPiPos->SetMarkerColor(kBlue+1); IdRightPiNeg->SetMarkerColor(kBlue+1); IdWrongPiPos->SetMarkerColor(kBlue+1); IdWrongPiNeg->SetMarkerColor(kBlue+1);
	IdRightPrPos->SetMarkerColor(kRed+1); IdRightPrNeg->SetMarkerColor(kRed+1); IdWrongPrPos->SetMarkerColor(kRed+1); IdWrongPrNeg->SetMarkerColor(kRed+1);
	IdRightKaPos->SetMarkerColor(kGreen+1); IdRightKaNeg->SetMarkerColor(kGreen+1); IdWrongKaPos->SetMarkerColor(kGreen+1); IdWrongKaNeg->SetMarkerColor(kGreen+1);
	IdRightPiPos->SetLineColor(kBlue+1); IdRightPiNeg->SetLineColor(kBlue+1); IdWrongPiPos->SetLineColor(kBlue+1); IdWrongPiNeg->SetLineColor(kBlue+1);
	IdRightPrPos->SetLineColor(kRed+1); IdRightPrNeg->SetLineColor(kRed+1); IdWrongPrPos->SetLineColor(kRed+1); IdWrongPrNeg->SetLineColor(kRed+1);
	IdRightKaPos->SetLineColor(kGreen+1); IdRightKaNeg->SetLineColor(kGreen+1); IdWrongKaPos->SetLineColor(kGreen+1); IdWrongKaNeg->SetLineColor(kGreen+1);
	IdRightPiPos->SetLineWidth(6); IdRightPiNeg->SetLineWidth(6); IdWrongPiPos->SetLineWidth(3); IdWrongPiNeg->SetLineWidth(3);
	IdRightPrPos->SetLineWidth(6); IdRightPrNeg->SetLineWidth(6); IdWrongPrPos->SetLineWidth(3); IdWrongPrNeg->SetLineWidth(3);
	IdRightKaPos->SetLineWidth(6); IdRightKaNeg->SetLineWidth(6); IdWrongKaPos->SetLineWidth(3); IdWrongKaNeg->SetLineWidth(3);
	IdRightPiPos->GetYaxis()->SetTitle("Efficiency"); IdRightPiNeg->GetYaxis()->SetTitle("Efficiency"); IdWrongPiPos->GetYaxis()->SetTitle("Contamination"); IdWrongPiNeg->GetYaxis()->SetTitle("Contamination");
	IdRightPiPos->GetXaxis()->SetTitle("p, GeV/c"); IdRightPiNeg->GetXaxis()->SetTitle("p, GeV/c"); IdWrongPiPos->GetXaxis()->SetTitle("p, GeV/c"); IdWrongPiNeg->GetXaxis()->SetTitle("p, GeV/c");
	IdRightPiPos->GetXaxis()->CenterTitle(kTRUE); IdRightPiNeg->GetXaxis()->CenterTitle(kTRUE); IdWrongPiPos->GetXaxis()->CenterTitle(kTRUE); IdWrongPiNeg->GetXaxis()->CenterTitle(kTRUE);
	IdRightPiPos->GetYaxis()->CenterTitle(kTRUE); IdRightPiNeg->GetYaxis()->CenterTitle(kTRUE); IdWrongPiPos->GetYaxis()->CenterTitle(kTRUE); IdWrongPiNeg->GetYaxis()->CenterTitle(kTRUE);
	IdRightPiPos->GetXaxis()->SetDecimals(); IdRightPiNeg->GetXaxis()->SetDecimals(); IdWrongPiPos->GetXaxis()->SetDecimals(); IdWrongPiNeg->GetXaxis()->SetDecimals();
	IdRightPiPos->GetYaxis()->SetDecimals(); IdRightPiNeg->GetYaxis()->SetDecimals(); IdWrongPiPos->GetYaxis()->SetDecimals(); IdWrongPiNeg->GetYaxis()->SetDecimals();
	IdRightPiPos->SetStats(kFALSE); IdRightPiNeg->SetStats(kFALSE); IdWrongPiPos->SetStats(kFALSE); IdWrongPiNeg->SetStats(kFALSE);
	
	TCanvas *c1 = new TCanvas("c1", "Efficiency of positive charged particles", 1200, 800);
	gPad->SetGridx(); gPad->SetGridy();
	gPad->SetTickx(); gPad->SetTicky();
	gPad->SetBorderMode(0); gPad->SetBorderSize(0);
	gPad->SetRightMargin(0.02); gPad->SetLeftMargin(0.1);
	gPad->SetTopMargin(0.02); gPad->SetBottomMargin(0.1);
	IdRightPiPos->GetYaxis()->SetRangeUser(0,1.1);
	IdRightPiPos->Draw(); IdRightPrPos->Draw("same"); IdRightKaPos->Draw("same");
	TLegend *leg1 = new TLegend(0.1035397,0.6631426,0.4910908,0.90477,"","br");
	leg1->SetFillColor(18);
	leg1->AddEntry(IdRightPrPos, "p^{#plus}", "l");
	leg1->AddEntry(IdRightPiPos, "#pi^{#plus}", "l");
	leg1->AddEntry(IdRightKaPos, "K^{#plus}", "l");
	leg1->Draw();
	path = dir + name_effpos;
	c1->SaveAs(path);
	
	TCanvas *c2 = new TCanvas("c2", "Efficiency of negative charged particles", 1200, 800);
	gPad->SetGridx(); gPad->SetGridy();
	gPad->SetTickx(); gPad->SetTicky();
	gPad->SetBorderMode(0); gPad->SetBorderSize(0);
	gPad->SetRightMargin(0.02); gPad->SetLeftMargin(0.1);
	gPad->SetTopMargin(0.02); gPad->SetBottomMargin(0.1);
	IdRightPiNeg->GetYaxis()->SetRangeUser(0,1.1);
	IdRightPiNeg->Draw(); IdRightPrNeg->Draw("same"); IdRightKaNeg->Draw("same");
	TLegend *leg2 = new TLegend(0.1035397,0.6631426,0.4910908,0.90477,"","br");
	leg2->SetFillColor(18);
	leg2->AddEntry(IdRightPrNeg, "p^{#minus}", "l");
	leg2->AddEntry(IdRightPiNeg, "#pi^{#minus}", "l");
	leg2->AddEntry(IdRightKaNeg, "K^{#minus}", "l");
	leg2->Draw();
	path = dir + name_effneg;
	c2->SaveAs(path);
	
	TCanvas *c3 = new TCanvas("c3", "Contamination of positive charged particles", 1200, 800);
	gPad->SetGridx(); gPad->SetGridy();
	gPad->SetTickx(); gPad->SetTicky();
	gPad->SetBorderMode(0); gPad->SetBorderSize(0);
	gPad->SetRightMargin(0.02); gPad->SetLeftMargin(0.1);
	gPad->SetTopMargin(0.02); gPad->SetBottomMargin(0.1);
	IdWrongPiPos->GetYaxis()->SetRangeUser(0,1.1);
	IdWrongPiPos->Draw(); IdWrongPrPos->Draw("same"); IdWrongKaPos->Draw("same");
	TLegend *leg3 = new TLegend(0.1035397,0.6631426,0.4910908,0.90477,"","br");
	leg3->SetFillColor(18);
	leg3->AddEntry(IdWrongPrPos, "p^{#plus}", "l");
	leg3->AddEntry(IdWrongPiPos, "#pi^{#plus}", "l");
	leg3->AddEntry(IdWrongKaPos, "K^{#plus}", "l");
	leg3->Draw();
	path = dir + name_contpos;
	c3->SaveAs(path);
	
	TCanvas *c4 = new TCanvas("c4", "Contamination of negative charged particles", 1200, 800);
	gPad->SetGridx(); gPad->SetGridy();
	gPad->SetTickx(); gPad->SetTicky();
	gPad->SetBorderMode(0); gPad->SetBorderSize(0);
	gPad->SetRightMargin(0.02); gPad->SetLeftMargin(0.1);
	gPad->SetTopMargin(0.02); gPad->SetBottomMargin(0.1);
	IdWrongPiNeg->GetYaxis()->SetRangeUser(0,1.1);
	IdWrongPiNeg->Draw(); IdWrongPrNeg->Draw("same"); IdWrongKaNeg->Draw("same");
	TLegend *leg4 = new TLegend(0.1035397,0.6631426,0.4910908,0.90477,"","br");
	leg4->SetFillColor(18);
	leg4->AddEntry(IdWrongPrNeg, "p^{#minus}", "l");
	leg4->AddEntry(IdWrongPiNeg, "#pi^{#minus}", "l");
	leg4->AddEntry(IdWrongKaNeg, "K^{#minus}", "l");
	leg4->Draw();
	path = dir + name_contneg;
	c4->SaveAs(path);
	
	delete c1; delete c2; delete c3; delete c4; delete leg1; delete leg2; delete leg3; delete leg4;
}

ClassImp(MpdPidQA);
