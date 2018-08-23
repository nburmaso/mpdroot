#include "MpdPidQA.h"

MpdPidQA::MpdPidQA() : MpdPid()
{
  /// Default constructor
  Init("CF","pikapr");
}

MpdPidQA::MpdPidQA(Double_t sigmaTof, Double_t sigmaEloss, Double_t sqrts, 
	Double_t koef, TString Generator, TString Tracking, TString NSigPart)
	: MpdPid(sigmaTof, sigmaEloss, sqrts, koef, Generator, Tracking, NSigPart)
	{
		Init(Tracking, NSigPart);
	}
	
Double_t MpdPidQA::mergedPrBBMultX(Double_t *x, Double_t *par)
{
	Double_t ans = -999.;
	if (parPrBBMerged) { ans = x[0] * parPrBBMerged->EvalPar(x,par); }
	return ans;
}
	
void MpdPidQA::Init(TString Tracking, TString Particles)
{
	cout << "MpdPidQA::Init().." << endl;
	Double_t XMIN[nQAHists][nPIDparticles], XMAX[nQAHists][nPIDparticles]; TString histName;
	const Int_t nbins = 200, nbins2 = 100;
	for (Int_t i = 0; i < nQAHists; i++) { for (Int_t j = 0; j < nPIDparticles; j++) { XMIN[i][j] = -1.; XMAX[i][j] = -1.; } }
	TString ECHistNames[4] = { "All", "Id", "IdRight", "IdWrong" };
	nSigPart = Particles;
	
	if (Particles.Contains("pi"))
	{
		if (Tracking == "HP") { XMAX[0][0] = 15.e-06; XMAX[1][0] = 6.e-06; XMAX[2][0] = 3.5e-06; XMAX[3][0] = 3.e-06; XMAX[4][0] = 2.5e-06; XMAX[5][0] = 2.5e-06; XMAX[6][0] = 2.2e-06; for (Int_t i = 0; i < nQAHists; i++) XMIN[i][0] = 1.e-06; for (Int_t i = 7; i < nQAHists; i++) XMAX[i][0] = 2.1e-06; }
		else { XMIN[0][0] = 1000.; XMAX[0][0] = 25000.; XMAX[1][0] = 10000.; XMAX[2][0] = 8000.; for (Int_t i = 1; i < nQAHists; i++) XMIN[i][0] = 2000.; for (Int_t i = 3; i < nQAHists; i++) XMAX[i][0] = 6000.; }
		dEdXStruct SPi;
		for (Int_t i = 0; i < nQAHists; i++)
		{
			histName = "Pi" + TString::Itoa(i, 10);
			SPi.dEdXPart[i] = new TH1D(histName, "", nbins, XMIN[i][0], XMAX[i][0]);
		}
		SPi.ibeg = 0; SPi.iend = 39;
		dEdX.insert( pair<Int_t,dEdXStruct>(211,SPi) );
		TH2D *mass2Pi = new TH2D("mass2Pi", "", nbins, 0., 4., nbins, -0.5, 1.5);
		m2Hists.insert( pair<Int_t,TH2D*>(211,mass2Pi) );
		TH1D *amplpiplus = new TH1D("amplpiplus","",600,0.,3.); ampls.insert( pair<Int_t,TH1D*>(211,amplpiplus) );
		TH1D *amplpiminus = new TH1D("amplpiminus","",600,0.,3.); ampls.insert( pair<Int_t,TH1D*>(-211,amplpiminus) );
		EffContStruct EC1, EC2;
		for (Int_t i = 0; i < 4; i++) { EC1.EffContPart[i] = new TH1D(ECHistNames[i]+"PiPlus","",60,0.,3.); EC1.EffContPart[i]->Sumw2(); } effcont.insert( pair<Int_t,EffContStruct>(211,EC1) );
		for (Int_t i = 0; i < 4; i++) { EC2.EffContPart[i] = new TH1D(ECHistNames[i]+"PiMinus","",60,0.,3.); EC2.EffContPart[i]->Sumw2(); } effcont.insert( pair<Int_t,EffContStruct>(-211,EC2) );
	}
	if (Particles.Contains("ka"))
	{
		if (Tracking == "HP") 
		{
			XMIN[0][1] = 10.e-06; XMIN[1][1] = 10.e-06; XMIN[2][1] = 3.e-06; XMIN[3][1] = 3.e-06; XMIN[4][1] = 3.e-06; XMIN[5][1] = 2.e-06; XMIN[6][1] = 2.e-06; XMIN[7][1] = 1.5e-06; XMIN[8][1] = 1.5e-06; XMIN[9][1] = 1.5e-06; XMIN[10][1] = 1.5e-06; for (Int_t i = 11; i < nQAHists; i++) XMIN[i][1] = 1.e-06;
			XMAX[0][1] = 100.e-06; XMAX[1][1] = 50.e-06; XMAX[2][1] = 25.e-06; XMAX[3][1] = 12.e-06; XMAX[4][1] = 8.e-06; XMAX[5][1] = 7.e-06; XMAX[6][1] = 5.e-06; XMAX[7][1] = 4.5e-06; XMAX[8][1] = 4.e-06; XMAX[9][1] = 3.5e-06; XMAX[10][1] = 3.e-06; XMAX[11][1] = 3.e-06; XMAX[12][1] = 3.e-06; XMAX[13][1] = 3.e-06; XMAX[14][1] = 3.e-06; for (Int_t i = 15; i < nQAHists; i++) XMAX[i][1] = 2.5e-06;
		}
		else 
		{
			XMIN[0][1] = 10000.; XMIN[1][1] = 12000.; XMIN[2][1] = 8000.; XMIN[3][1] = 5000.; XMIN[4][1] = 3000.; for (Int_t i = 5; i < 14; i++) XMIN[i][1] = 2000.; for (Int_t i = 14; i < nQAHists; i++) XMIN[i][1] = 1000.;
			XMAX[0][1] = 100000.; XMAX[1][1] = 60000.; XMAX[2][1] = 40000.; XMAX[3][1] = 22000.; XMAX[4][1] = 17000.; XMAX[5][1] = 13000.; XMAX[6][1] = 13000.; XMAX[7][1] = 10000.; XMAX[8][1] = 10000.; XMAX[9][1] = 9000.; XMAX[10][1] = 9000.; XMAX[11][1] = 7000.; XMAX[12][1] = 7000.; XMAX[13][1] = 6000.; XMAX[14][1] = 6000.; for (Int_t i = 15; i < nQAHists; i++) XMAX[i][1] = 5000.;
		}
		dEdXStruct SKa;
		for (Int_t i = 0; i < nQAHists; i++)
		{
			histName = "Ka" + TString::Itoa(i, 10);
			SKa.dEdXPart[i] = new TH1D(histName, "", nbins, XMIN[i][1], XMAX[i][1]);
		}
		SKa.ibeg = 0; SKa.iend = 39;
		dEdX.insert( pair<Int_t,dEdXStruct>(321,SKa) );
		TH2D *mass2Ka = new TH2D("mass2Ka", "", nbins, 0., 4., nbins, -0.5, 1.5);
		m2Hists.insert( pair<Int_t,TH2D*>(321,mass2Ka) );
		TH1D *amplkaplus = new TH1D("amplkaplus","",600,0.,3.); ampls.insert( pair<Int_t,TH1D*>(321,amplkaplus) );
		TH1D *amplkaminus = new TH1D("amplkaminus","",600,0.,3.); ampls.insert( pair<Int_t,TH1D*>(-321,amplkaminus) );
		EffContStruct EC1, EC2;
		for (Int_t i = 0; i < 4; i++) { EC1.EffContPart[i] = new TH1D(ECHistNames[i]+"KaPlus","",60,0.,3.); EC1.EffContPart[i]->Sumw2(); } effcont.insert( pair<Int_t,EffContStruct>(321,EC1) );
		for (Int_t i = 0; i < 4; i++) { EC2.EffContPart[i] = new TH1D(ECHistNames[i]+"KaMinus","",60,0.,3.); EC2.EffContPart[i]->Sumw2(); } effcont.insert( pair<Int_t,EffContStruct>(-321,EC2) );
	}
	if (Particles.Contains("pr"))
	{
		if (Tracking == "HP") 
		{
			XMIN[0][2] = 75.e-06; XMIN[1][2] = 40.e-06; XMIN[2][2] = 20.e-06; XMIN[3][2] = 10.e-06; XMIN[4][2] = 10.e-06; XMIN[5][2] = 8.e-06; XMIN[6][2] = 6.e-06; XMIN[7][2] = 5.e-06; XMIN[8][2] = 4.e-06; XMIN[9][2] = 3.5e-06; XMIN[10][2] = 3.e-06; XMIN[11][2] = 3.e-06; XMIN[12][2] = 2.5e-06; for (Int_t i = 13; i < 17; i++) XMIN[i][2] = 2.e-06; for (Int_t i = 17; i < 23; i++) XMIN[i][2] = 1.5e-06; for (Int_t i = 23; i < nQAHists; i++) XMIN[i][2] = 1.e-06;
			XMAX[0][2] = 180.e-06; XMAX[1][2] = 100.e-06; XMAX[2][2] = 70.e-06; XMAX[3][2] = 35.e-06; XMAX[4][2] = 25.e-06; XMAX[5][2] = 17.e-06; XMAX[6][2] = 13.e-06; XMAX[7][2] = 11.e-06; XMAX[8][2] = 9.e-06; XMAX[9][2] = 7.5e-06; XMAX[10][2] = 6.5e-06; XMAX[11][2] = 6.e-06; XMAX[12][2] = 5.e-06; XMAX[13][2] = 5.e-06; XMAX[14][2] = 4.5e-06; XMAX[15][2] = 4.e-06; XMAX[16][2] = 4.e-06; for (Int_t i = 17; i < 21; i++) XMAX[i][2] = 3.5e-06; for (Int_t i = 21; i < 31; i++) XMAX[i][2] = 3.e-06; for (Int_t i = 31; i < nQAHists; i++) XMAX[i][2] = 2.5e-06;
		}
		else 
		{
			XMIN[0][2] = 100000.; XMIN[1][2] = 50000.; XMIN[2][2] = 20000.; XMIN[3][2] = 15000.; XMIN[4][2] = 10000.; XMIN[5][2] = 10000.; XMIN[6][2] = 8000.; XMIN[7][2] = 8000.; XMIN[8][2] = 8000.; for (Int_t i = 9; i < 14; i++) XMIN[i][2] = 5000.; XMIN[14][2] = 2000.; for (Int_t i = 15; i < 23; i++) XMIN[i][2] = 3000.; for (Int_t i = 23; i < nQAHists; i++) XMIN[i][2] = 2000.;
			XMAX[0][2] = 800000.; XMAX[1][2] = 250000.; XMAX[2][2] = 180000.; XMAX[3][2] = 90000.; XMAX[4][2] = 50000.; XMAX[5][2] = 30000.; XMAX[6][2] = 25000.; XMAX[7][2] = 19000.; XMAX[8][2] = 17000.; XMAX[9][2] = 15000.; XMAX[10][2] = 15000.; XMAX[11][2] = 15000.; XMAX[12][2] = 12000.; XMAX[13][2] = 12000.; XMAX[14][2] = 12000.; XMAX[15][2] = 9000.; XMAX[16][2] = 9000.; XMAX[17][2] = 8000.; XMAX[18][2] = 8000.; XMAX[19][2] = 8000.; XMAX[20][2] = 7000.; XMAX[21][2] = 7000.; XMAX[22][2] = 7000.; for (Int_t i = 23; i < nQAHists; i++) XMAX[i][2] = 6000.;
		}
		dEdXStruct SPr;
		for (Int_t i = 0; i < nQAHists; i++)
		{
			histName = "Pr" + TString::Itoa(i, 10);
			SPr.dEdXPart[i] = new TH1D(histName, "", nbins, XMIN[i][2], XMAX[i][2]);
		}
		SPr.ibeg = 0; SPr.iend = 39;
		dEdX.insert( pair<Int_t,dEdXStruct>(2212,SPr) );
		TH2D *mass2Pr = new TH2D("mass2Pr", "", nbins, 0., 4., nbins, -0.5, 1.5);
		m2Hists.insert( pair<Int_t,TH2D*>(2212,mass2Pr) );
		TH1D *amplprplus = new TH1D("amplprplus","",600,0.,3.); ampls.insert( pair<Int_t,TH1D*>(2212,amplprplus) );
		TH1D *amplprminus = new TH1D("amplprminus","",600,0.,3.); ampls.insert( pair<Int_t,TH1D*>(-2212,amplprminus) );
		EffContStruct EC1, EC2;
		for (Int_t i = 0; i < 4; i++) { EC1.EffContPart[i] = new TH1D(ECHistNames[i]+"PrPlus","",60,0.,3.); EC1.EffContPart[i]->Sumw2(); } effcont.insert( pair<Int_t,EffContStruct>(2212,EC1) );
		for (Int_t i = 0; i < 4; i++) { EC2.EffContPart[i] = new TH1D(ECHistNames[i]+"PrMinus","",60,0.,3.); EC2.EffContPart[i]->Sumw2(); } effcont.insert( pair<Int_t,EffContStruct>(-2212,EC2) );
	}
	if (Particles.Contains("el"))
	{
		if (Tracking == "HP") { for (Int_t i = 0; i < nQAHists; i++) { XMIN[i][3] = 1.e-06; XMAX[i][3] = 3.e-06; } }
		else { for (Int_t i = 0; i < 5; i++) { XMIN[i][3] = 2000.; XMAX[i][3] = 5000.; } for (Int_t i = 5; i < nQAHists; i++) { XMIN[i][3] = 2500.; XMAX[i][3] = 5500.; } }
		dEdXStruct SEl;
		for (Int_t i = 0; i < nQAHists; i++)
		{
			histName = "El" + TString::Itoa(i, 10);
			SEl.dEdXPart[i] = new TH1D(histName, "", nbins, XMIN[i][3], XMAX[i][3]);
		}
		SEl.ibeg = 0; SEl.iend = 39;
		dEdX.insert( pair<Int_t,dEdXStruct>(11,SEl) );
		TH2D *mass2El = new TH2D("mass2El", "", nbins, 0., 4., nbins, -0.7, 0.7);
		m2Hists.insert( pair<Int_t,TH2D*>(11,mass2El) );
		TH1D *amplelplus = new TH1D("amplelplus","",600,0.,3.); ampls.insert( pair<Int_t,TH1D*>(-11,amplelplus) );
		TH1D *amplelminus = new TH1D("amplelminus","",600,0.,3.); ampls.insert( pair<Int_t,TH1D*>(11,amplelminus) );
		EffContStruct EC1, EC2;
		for (Int_t i = 0; i < 4; i++) { EC1.EffContPart[i] = new TH1D(ECHistNames[i]+"ElMinus","",60,0.,3.); EC1.EffContPart[i]->Sumw2(); } effcont.insert( pair<Int_t,EffContStruct>(11,EC1) );
		for (Int_t i = 0; i < 4; i++) { EC2.EffContPart[i] = new TH1D(ECHistNames[i]+"ElPlus","",60,0.,3.); EC2.EffContPart[i]->Sumw2(); } effcont.insert( pair<Int_t,EffContStruct>(-11,EC2) );
	}
	if (Particles.Contains("mu"))
	{
		if (Tracking == "HP") { XMAX[0][4] = 15.e-06; XMAX[1][4] = 5.e-06; for (Int_t i = 0; i < nQAHists; i++) XMIN[i][4] = 1.e-06; for (Int_t i = 2; i < nQAHists; i++) XMAX[i][4] = 3.e-06; }
		else { XMIN[0][4] = 2000.; XMIN[1][4] = 1000.; XMAX[0][4] = 15000.; XMAX[1][4] = 8000.; for (Int_t i = 2; i < nQAHists; i++) { XMIN[i][4] = 1000.; XMAX[i][4] = 5000.; } }
		dEdXStruct SMu;
		for (Int_t i = 0; i < nQAHists; i++)
		{
			histName = "Mu" + TString::Itoa(i, 10);
			SMu.dEdXPart[i] = new TH1D(histName, "", nbins, XMIN[i][4], XMAX[i][4]);
		}
		SMu.ibeg = 0; SMu.iend = 39;
		dEdX.insert( pair<Int_t,dEdXStruct>(13,SMu) );
		TH2D *mass2Mu = new TH2D("mass2Mu", "", nbins, 0., 4., nbins, -0.8, 0.8);
		m2Hists.insert( pair<Int_t,TH2D*>(13,mass2Mu) );
		TH1D *amplmuplus = new TH1D("amplmuplus","",600,0.,3.); ampls.insert( pair<Int_t,TH1D*>(-13,amplmuplus) );
		TH1D *amplmuminus = new TH1D("amplmuminus","",600,0.,3.); ampls.insert( pair<Int_t,TH1D*>(13,amplmuminus) );
		EffContStruct EC1, EC2;
		for (Int_t i = 0; i < 4; i++) { EC1.EffContPart[i] = new TH1D(ECHistNames[i]+"MuMinus","",60,0.,3.); EC1.EffContPart[i]->Sumw2(); } effcont.insert( pair<Int_t,EffContStruct>(13,EC1) );
		for (Int_t i = 0; i < 4; i++) { EC2.EffContPart[i] = new TH1D(ECHistNames[i]+"MuPlus","",60,0.,3.); EC2.EffContPart[i]->Sumw2(); } effcont.insert( pair<Int_t,EffContStruct>(-13,EC2) );
	}
	if (Particles.Contains("de"))
	{
		if (Tracking == "HP") { for (Int_t i = 0; i < (nQAHists - 2); i++) XMIN[i][5] = 1.e-06; XMAX[0][5] = 200.e-06; XMAX[1][5] = 200.e-06; XMAX[2][5] = 150.e-06; XMAX[3][5] = 140.e-06; XMAX[4][5] = 100.e-06; XMAX[5][5] = 80.e-06; XMAX[6][5] = 50.e-06; XMAX[7][5] = 40.e-06; XMAX[8][5] = 30.e-06; XMAX[9][5] = 20.e-06; XMAX[10][5] = 10.e-06; XMAX[11][5] = 8.e-06; for (Int_t i = 12; i < (nQAHists - 2); i++) XMAX[i][5] = 5.e-06; }
		else
		{
			XMIN[0][5] = 50000.; XMIN[1][5] = 20000.; XMIN[2][5] = 10000.; XMIN[3][5] = 8000.; XMIN[4][5] = 8000.; XMIN[5][5] = 5000.; for (Int_t i = 6; i < (nQAHists - 2); i++) XMIN[i][5] = 2000.;
			XMAX[0][5] = 500000.; XMAX[1][5] = 400000.; XMAX[2][5] = 250000.; XMAX[3][5] = 200000.; XMAX[4][5] = 120000.; XMAX[5][5] = 80000.; XMAX[6][5] = 70000.; XMAX[7][5] = 60000.; XMAX[8][5] = 50000.; XMAX[9][5] = 40000.; XMAX[10][5] = 35000.; XMAX[11][5] = 30000.; XMAX[12][5] = 25000.; XMAX[13][5] = 20000.; XMAX[14][5] = 20000.; XMAX[15][5] = 18000.; XMAX[16][5] = 15000.; XMAX[17][5] = 15000.; XMAX[18][5] = 15000.; XMAX[19][5] = 13000.; XMAX[20][5] = 13000.; XMAX[21][5] = 13000.; XMAX[22][5] = 13000.; XMAX[23][5] = 11000.; XMAX[24][5] = 11000.; XMAX[25][5] = 10000.; XMAX[26][5] = 10000.; XMAX[27][5] = 10000.; XMAX[28][5] = 9000.; XMAX[29][5] = 8000.; XMAX[30][5] = 8000.; XMAX[31][5] = 7000.; XMAX[32][5] = 7000.; XMAX[33][5] = 7000.; XMAX[34][5] = 7000.; XMAX[35][5] = 6000.; XMAX[36][5] = 6000.; XMAX[37][5] = 6000.;
		}
		dEdXStruct SDe;
		for (Int_t i = 0; i < nQAHists; i++)
		{
			histName = "De" + TString::Itoa(i, 10);
			SDe.dEdXPart[i] = new TH1D(histName, "", nbins, XMIN[i][5], XMAX[i][5]);
		}
		SDe.ibeg = 2; SDe.iend = 39;
		dEdX.insert( pair<Int_t,dEdXStruct>(PDG_DEUTERON,SDe) );
		TH2D *mass2De = new TH2D("mass2De", "", nbins, 0., 4., nbins, -0.5, 10.);
		m2Hists.insert( pair<Int_t,TH2D*>(PDG_DEUTERON,mass2De) );
		TH1D *ampldeplus = new TH1D("ampldeplus","",300,0.,3.); ampls.insert( pair<Int_t,TH1D*>(PDG_DEUTERON,ampldeplus) );
		EffContStruct EC; for (Int_t i = 0; i < 4; i++) { EC.EffContPart[i] = new TH1D(ECHistNames[i]+"De","",60,0.,3.); EC.EffContPart[i]->Sumw2(); }
		effcont.insert( pair<Int_t,EffContStruct>(PDG_DEUTERON,EC) );
	}
	if (Particles.Contains("tr"))
	{
		if (Tracking == "HP")
		{
			XMIN[0][6] = 3.e-06; XMIN[1][6] = 3.e-06; XMIN[2][6] = 2.e-06; for (Int_t i = 0; i < (nQAHists - 3); i++) XMIN[i][6] = 1.e-06;
			XMAX[0][6] = 200.e-06; XMAX[1][6] = 160.e-06; XMAX[2][6] = 140.e-06; XMAX[3][6] = 110.e-06; XMAX[4][6] = 80.e-06; XMAX[5][6] = 60.e-06; XMAX[6][6] = 40.e-06; for (Int_t i = 7; i < (nQAHists - 3); i++) XMAX[i][6] = 20.e-06;
		}
		else
		{
			XMIN[0][6] = 100000.; XMIN[1][6] = 50000.; XMIN[2][6] = 40000.; XMIN[3][6] = 30000.; XMIN[4][6] = 20000.; XMIN[5][6] = 10000.; XMIN[6][6] = 8000.; XMIN[7][6] = 5000.; for (Int_t i = 8; i < 14; i++) XMIN[i][6] = 2000.; for (Int_t i = 14; i < 20; i++) XMIN[i][6] = 5000.; for (Int_t i = 20; i < 26; i++) XMIN[i][6] = 6000.; for (Int_t i = 26; i < 29; i++) XMIN[i][6] = 5000.; for (Int_t i = 29; i < 35; i++) XMIN[i][6] = 4000.; XMIN[35][6] = 3500.; XMIN[36][6] = 3500.;
			XMAX[0][6] = 500000.; XMAX[1][6] = 500000.; XMAX[2][6] = 350000.; XMAX[3][6] = 250000.; XMAX[4][6] = 200000.; XMAX[5][6] = 160000.; XMAX[6][6] = 140000.; XMAX[7][6] = 100000.; XMAX[8][6] = 90000.; XMAX[9][6] = 80000.; XMAX[10][6] = 60000.; XMAX[11][6] = 50000.; XMAX[12][6] = 45000.; XMAX[13][6] = 40000.; XMAX[14][6] = 40000.; XMAX[15][6] = 30000.; XMAX[16][6] = 30000.; XMAX[17][6] = 30000.; XMAX[18][6] = 25000.; XMAX[19][6] = 25000.; XMAX[20][6] = 22000.; XMAX[21][6] = 22000.; XMAX[22][6] = 20000.; XMAX[23][6] = 18000.; XMAX[24][6] = 18000.; XMAX[25][6] = 16000.; XMAX[26][6] = 16000.; XMAX[27][6] = 15000.; XMAX[28][6] = 14000.; XMAX[29][6] = 14000.; XMAX[30][6] = 13000.; XMAX[31][6] = 11000.; XMAX[32][6] = 11000.; XMAX[33][6] = 10000.; XMAX[34][6] = 10000.; XMAX[35][6] = 9000.; XMAX[36][6] = 8000.;
		}
		dEdXStruct STr;
		for (Int_t i = 0; i < nQAHists; i++)
		{
			histName = "Tr" + TString::Itoa(i, 10);
			STr.dEdXPart[i] = new TH1D(histName, "", nbins, XMIN[i][6], XMAX[i][6]);
		}
		STr.ibeg = 3; STr.iend = 39;
		dEdX.insert( pair<Int_t,dEdXStruct>(PDG_TRITON,STr) );
		TH2D *mass2Tr = new TH2D("mass2Tr", "", nbins, 0., 4., nbins, -0.5, 15.);
		m2Hists.insert( pair<Int_t,TH2D*>(PDG_TRITON,mass2Tr) );
		TH1D *ampltrplus = new TH1D("ampltrplus","",300,0.,3.); ampls.insert( pair<Int_t,TH1D*>(PDG_TRITON,ampltrplus) );
		EffContStruct EC; for (Int_t i = 0; i < 4; i++) { EC.EffContPart[i] = new TH1D(ECHistNames[i]+"Tr","",60,0.,3.); EC.EffContPart[i]->Sumw2(); }
		effcont.insert( pair<Int_t,EffContStruct>(PDG_TRITON,EC) );
	}
	if (Particles.Contains("he3"))
	{
		if (Tracking == "HP")
		{
			XMIN[0][7] = 3.e-06; XMIN[1][7] = 3.e-06; XMIN[2][7] = 3.e-06; XMIN[3][7] = 2.e-06; XMIN[4][7] = 2.e-06; for (Int_t i = 5; i < 9; i++) XMIN[i][7] = 1.5e-06; for (Int_t i = 9; i < (nQAHists - 9); i++) XMIN[i][7] = 1.e-06;
			XMAX[0][7] = 450.e-06; XMAX[1][7] = 350.e-06; XMAX[2][7] = 300.e-06; XMAX[3][7] = 250.e-06; XMAX[4][7] = 200.e-06; XMAX[5][7] = 150.e-06; XMAX[6][7] = 100.e-06; XMAX[7][7] = 80.e-06; XMAX[8][7] = 70.e-06; XMAX[9][7] = 60.e-06; XMAX[10][7] = 50.e-06; XMAX[11][7] = 40.e-06; XMAX[12][7] = 35.e-06; for (Int_t i = 13; i < (nQAHists - 9); i++) XMAX[i][7] = XMAX[11][7] = 32.e-06;
		}
		else
		{
			XMIN[0][7] = 100000.; XMIN[1][7] = 100000.; XMIN[2][7] = 50000.; XMIN[3][7] = 30000.; for (Int_t i = 4; i < (nQAHists - 9); i++) XMIN[i][7] = 10000.;
			XMAX[0][7] = 1800000.; XMAX[1][7] = 1000000.; XMAX[2][7] = 800000.; XMAX[3][7] = 500000.; XMAX[4][7] = 350000.; XMAX[5][7] = 250000.; XMAX[6][7] = 200000.; XMAX[7][7] = 180000.; XMAX[8][7] = 150000.; XMAX[9][7] = 130000.; XMAX[10][7] = 110000.; XMAX[11][7] = 90000.; XMAX[12][7] = 80000.; XMAX[13][7] = 70000.; XMAX[14][7] = 60000.; XMAX[15][7] = 50000.; XMAX[16][7] = 45000.; XMAX[17][7] = 45000.; XMAX[18][7] = 40000.; XMAX[19][7] = 40000.; XMAX[20][7] = 35000.; XMAX[21][7] = 35000.; XMAX[22][7] = 30000.; XMAX[23][7] = 30000.; XMAX[24][7] = 28000.; XMAX[25][7] = 28000.; XMAX[26][7] = 26000.; XMAX[27][7] = 26000.; XMAX[28][7] = 30000.; XMAX[29][7] = 30000.; XMAX[30][7] = 30000.;
		}
		dEdXStruct SHe3;
		for (Int_t i = 0; i < nQAHists; i++)
		{
			histName = "He3" + TString::Itoa(i, 10);
			SHe3.dEdXPart[i] = new TH1D(histName, "", nbins, XMIN[i][7], XMAX[i][7]);
		}
		SHe3.ibeg = 2; SHe3.iend = 32;
		dEdX.insert( pair<Int_t,dEdXStruct>(PDG_HE3,SHe3) );
		TH2D *mass2He3 = new TH2D("mass2He3", "", nbins, 0., 4., nbins, -0.5, 6.);
		m2Hists.insert( pair<Int_t,TH2D*>(PDG_HE3,mass2He3) );
		TH1D *amplhe3plus = new TH1D("amplhe3plus","",150,0.,1.5); ampls.insert( pair<Int_t,TH1D*>(PDG_HE3,amplhe3plus) );
		EffContStruct EC; for (Int_t i = 0; i < 4; i++) { EC.EffContPart[i] = new TH1D(ECHistNames[i]+"He3","",30,0.,1.5); EC.EffContPart[i]->Sumw2(); }
		effcont.insert( pair<Int_t,EffContStruct>(PDG_HE3,EC) );
	}
	if (Particles.Contains("he4"))
	{
		if (Tracking == "HP")
		{
			XMIN[0][8] = 3.e-06; XMIN[1][8] = 2.e-06; for (Int_t i = 2; i < (nQAHists - 12); i++) XMIN[i][8] = 1.e-06;
			XMAX[0][8] = 350.e-06; XMAX[1][8] = 300.e-06; XMAX[2][8] = 250.e-06; XMAX[3][8] = 200.e-06; XMAX[4][8] = 150.e-06; XMAX[5][8] = 100.e-06; XMAX[6][8] = 80.e-06; XMAX[7][8] = 70.e-06; XMAX[8][8] = 60.e-06; XMAX[9][8] = 50.e-06; XMAX[10][8] = 40.e-06; for (Int_t i = 11; i < (nQAHists - 12); i++) XMAX[i][8] = 32.e-06;
		}
		else
		{
			XMIN[0][8] = 100000.; XMIN[1][8] = 80000.; XMIN[2][8] = 50000.; XMIN[3][8] = 30000.; XMIN[4][8] = 30000.; XMIN[5][8] = 20000.; for (Int_t i = 6; i < (nQAHists - 12); i++) XMIN[i][8] = 10000.;
			XMAX[0][8] = 1800000.; XMAX[1][8] = 1200000.; XMAX[2][8] = 900000.; XMAX[3][8] = 700000.; XMAX[4][8] = 500000.; XMAX[5][8] = 350000.; XMAX[6][8] = 300000.; XMAX[7][8] = 250000.; XMAX[8][8] = 200000.; XMAX[9][8] = 180000.; XMAX[10][8] = 160000.; XMAX[11][8] = 140000.; XMAX[12][8] = 130000.; XMAX[13][8] = 120000.; XMAX[14][8] = 100000.; XMAX[15][8] = 90000.; XMAX[16][8] = 80000.; XMAX[17][8] = 70000.; XMAX[18][8] = 70000.; XMAX[19][8] = 60000.; XMAX[20][8] = 55000.; XMAX[21][8] = 50000.; XMAX[22][8] = 45000.; XMAX[23][8] = 45000.; XMAX[24][8] = 40000.; XMAX[25][8] = 40000.; XMAX[26][8] = 40000.; XMAX[27][8] = 40000.;
		}
		dEdXStruct SHe4;
		for (Int_t i = 0; i < nQAHists; i++)
		{
			histName = "He4" + TString::Itoa(i, 10);
			SHe4.dEdXPart[i] = new TH1D(histName, "", nbins, XMIN[i][8], XMAX[i][8]);
		}
		SHe4.ibeg = 3; SHe4.iend = 30;
		dEdX.insert( pair<Int_t,dEdXStruct>(PDG_HE4,SHe4) );
		TH2D *mass2He4 = new TH2D("mass2He4", "", nbins, 0., 4., nbins, -0.5, 8.);
		m2Hists.insert( pair<Int_t,TH2D*>(PDG_HE4,mass2He4) );
		TH1D *amplhe4plus = new TH1D("amplhe4plus","",150,0.,1.5); ampls.insert( pair<Int_t,TH1D*>(PDG_HE4,amplhe4plus) );
		EffContStruct EC; for (Int_t i = 0; i < 4; i++) { EC.EffContPart[i] = new TH1D(ECHistNames[i]+"He4","",30,0.,1.5); EC.EffContPart[i]->Sumw2(); }
		effcont.insert( pair<Int_t,EffContStruct>(PDG_HE4,EC) );
	}
	
	for (Int_t i = 0; i < (nQAHists - 8); i++) Xlow[i] = 0.05 * (1. + i);
	Xlow[32] = 1.7; Xlow[33] = 1.8; Xlow[34] = 1.9; Xlow[35] = 2.0; Xlow[36] = 2.1; Xlow[37] = 2.2; Xlow[38] = 2.4; Xlow[39] = 2.6;
	for (Int_t i = 0; i < (nQAHists - 9); i++) Xhigh[i] = 0.05 + 0.05 * (1. + i);
	Xhigh[31] = 1.7; Xhigh[32] = 1.8; Xhigh[33] = 1.9; Xhigh[34] = 2.0; Xhigh[35] = 2.1; Xhigh[36] = 2.2; Xhigh[37] = 2.4; Xhigh[38] = 2.6; Xhigh[39] = 3.0;
	for (Int_t i=0; i<40; i++) X[i] = (Xlow[i] + Xhigh[i]) / 2;
}

void MpdPidQA::FillDedxHists(Double_t p, Double_t dedx, Int_t pdg)
{
	Int_t pdgc = TMath::Abs(pdg);
	auto ret = dEdX.find(pdgc);
	if (ret != dEdX.end())
	{
		for (Int_t i = 0; i < nQAHists; i++)
		{
			if ( (p > Xlow[i]) && (p <= Xhigh[i]) ) 
			{
				if ( (i < ret->second.ibeg) || (i > ret->second.iend) ) continue;
				ret->second.dEdXPart[i - ret->second.ibeg]->Fill(dedx);
			}
		}
	}
}

void MpdPidQA::Fillm2Hists(Double_t p, Double_t m2, Int_t pdg)
{
	Int_t pdgc = TMath::Abs(pdg);
	auto ret = m2Hists.find(pdgc);
	if (ret != m2Hists.end()) ret->second->Fill(p, m2);
}

void MpdPidQA::FillAmplHists(Double_t p, Int_t pdg)
{
	if (p <= 3.0)
	{
		auto ret = ampls.find(pdg);
		if (ret != ampls.end()) ret->second->Fill(p);
	}
}

Bool_t MpdPidQA::FillEffContHists(MpdTrack* track, Int_t pdg, Double_t fProbCut)
{
	Double_t px=track->GetPx(),py=track->GetPy(),pz=track->GetPz();   
	Double_t p=TMath::Sqrt(px*px+py*py+pz*pz), prob = 0., maxprob = -1.; Int_t pidpdg = 0, mappdg, mappdgc;
	FillEffDenominator(p, pdg); Int_t charge = 1; if (track->GetPt() > 0) charge = -1;
	if (!FillProbs(track)) return kFALSE;
	for (auto it = effcont.begin(); it != effcont.end(); ++it)
	{
		mappdg = it->first; mappdgc = TMath::Abs(mappdg); if ( (mappdgc == 11) || (mappdgc == 13) ) mappdg *= -1.;
		if (mappdg * charge > 0)
		{
			switch (TMath::Abs(it->first))
			{
				case 11: prob = GetProbEl(); break;
				case 13: prob = GetProbMu(); break;
				case 211: prob = GetProbPi(); break;
				case 321: prob = GetProbKa(); break;
				case 2212: prob = GetProbPr(); break;
				case PDG_DEUTERON: prob = GetProbDe(); break;
				case PDG_TRITON: prob = GetProbTr(); break;
				case PDG_HE3: prob = GetProbHe3(); break;
				case PDG_HE4: prob = GetProbHe4(); break;
				default: break;
			}
			if (prob > maxprob) { maxprob = prob; pidpdg = it->first; }
		}
	}
	if (maxprob < fProbCut) return kFALSE;
	FillEffContHists(p, pidpdg, pdg);
	return kTRUE;
}

Bool_t MpdPidQA::FillEffContHists(Double_t p, Double_t dedx, Int_t charge, Int_t pdg, Double_t fProbCut)
{
	FillEffDenominator(p, pdg);
	if (!FillProbs(p, dedx, charge)) return kFALSE;
	Double_t prob = 0., maxprob = -1.; Int_t pidpdg = 0, mappdg, mappdgc;
	for (auto it = effcont.begin(); it != effcont.end(); ++it)
	{
		mappdg = it->first; mappdgc = TMath::Abs(mappdg); if ( (mappdgc == 11) || (mappdgc == 13) ) mappdg *= -1.;
		if (mappdg * charge > 0)
		{
			switch (TMath::Abs(it->first))
			{
				case 11: prob = GetProbEl(); break;
				case 13: prob = GetProbMu(); break;
				case 211: prob = GetProbPi(); break;
				case 321: prob = GetProbKa(); break;
				case 2212: prob = GetProbPr(); break;
				case PDG_DEUTERON: prob = GetProbDe(); break;
				case PDG_TRITON: prob = GetProbTr(); break;
				case PDG_HE3: prob = GetProbHe3(); break;
				case PDG_HE4: prob = GetProbHe4(); break;
				default: break;
			}
			if (prob > maxprob) { maxprob = prob; pidpdg = it->first; }
		}
	}
	if (maxprob < fProbCut) return kFALSE;
	FillEffContHists(p, pidpdg, pdg);
	return kTRUE;
}

Bool_t MpdPidQA::FillEffContHists(Double_t p, Double_t dedx, Double_t m2, Int_t charge, Int_t pdg, Double_t fProbCut)
{
	FillEffDenominator(p, pdg);
	if (!FillProbs(p, dedx, m2, charge)) return kFALSE;
	Double_t prob = 0., maxprob = -1.; Int_t pidpdg = 0, mappdg, mappdgc;
	for (auto it = effcont.begin(); it != effcont.end(); ++it)
	{
		mappdg = it->first; mappdgc = TMath::Abs(mappdg); if ( (mappdgc == 11) || (mappdgc == 13) ) mappdg *= -1.;
		if (mappdg * charge > 0)
		{
			switch (TMath::Abs(it->first))
			{
				case 11: prob = GetProbEl(); break;
				case 13: prob = GetProbMu(); break;
				case 211: prob = GetProbPi(); break;
				case 321: prob = GetProbKa(); break;
				case 2212: prob = GetProbPr(); break;
				case PDG_DEUTERON: prob = GetProbDe(); break;
				case PDG_TRITON: prob = GetProbTr(); break;
				case PDG_HE3: prob = GetProbHe3(); break;
				case PDG_HE4: prob = GetProbHe4(); break;
				default: break;
			}
			if (prob > maxprob) { maxprob = prob; pidpdg = it->first; }
		}
	}
	if (maxprob < fProbCut) return kFALSE;
	FillEffContHists(p, pidpdg, pdg);
	return kTRUE;
}

Bool_t MpdPidQA::FillEffContHists(Double_t p, Int_t pidpdg, Int_t pdg)
{
	auto ret = effcont.find(pidpdg);
	if (ret != effcont.end()) 
	{
		ret->second.EffContPart[1]->Fill(p);
		if (pidpdg == pdg) ret->second.EffContPart[2]->Fill(p);
		else ret->second.EffContPart[3]->Fill(p);
	}
	return kTRUE;
}

void MpdPidQA::FillEffDenominator(Double_t p, Int_t pdg)
{
	auto ret = effcont.find(pdg);
	if (ret != effcont.end()) ret->second.EffContPart[0]->Fill(p);
}

void MpdPidQA::GetDedxQA(TString dir)
{
	Double_t XFUNCMIN, XFUNCMAX, mom;
	TString FuncFormula;
	Int_t ibegloc, iendloc, nQAHistsloc;
	map <Int_t,TGraphAsymmErrors*> graphs;
	
	for (auto it = dEdX.begin(); it != dEdX.end(); ++it)
	{
		ibegloc = it->second.ibeg; iendloc = it->second.iend; nQAHistsloc = iendloc - ibegloc + 1;
		TF1 *parBB; TF1 *parBBMultX;
		switch(it->first)
		{
			case 211: parBB = parPiBB; GetDedxParam = &MpdPid::GetDedxPiParam; break;
			case 321: parBB = parKaBB; GetDedxParam = &MpdPid::GetDedxKaParam; break;
			case 2212: parBB = parPrBBMerged; GetDedxParam = &MpdPid::GetDedxPrParam; break;
			case 11: parBB = parElBB; GetDedxParam = &MpdPid::GetDedxElParam; break;
			case 13: parBB = parMuBB; GetDedxParam = &MpdPid::GetDedxMuParam; break;
			case PDG_DEUTERON: parBB = parDeBB; GetDedxParam = &MpdPid::GetDedxDeParam; break;
			case PDG_TRITON: parBB = parTrBB; GetDedxParam = &MpdPid::GetDedxTrParam; break;
			case PDG_HE3: parBB = parHe3BB; GetDedxParam = &MpdPid::GetDedxHe3Param; break;
			case PDG_HE4: parBB = parHe4BB; GetDedxParam = &MpdPid::GetDedxHe4Param; break;
			default: break;
		}
		parBB->SetName("parBB");
		FuncFormula = "x*(" + parBB->GetExpFormula() + ")";
		if (it->first == 2212)
		{
			parBBMultX = new TF1("parBBMultX",this,&MpdPidQA::mergedPrBBMultX,0.0,3.0,10,"MpdPidQA","mergedPrBBMultX");
			for (Int_t k=0; k<parPrBBMerged->GetNpar(); k++) {parBBMultX->SetParameter(k, parPrBBMerged->GetParameter(k)); cout << "parPrBBMerged->GetParameter(" << k << ") = " << parPrBBMerged->GetParameter(k) << endl;}
		}
		else
		{
			parBBMultX = new TF1("parPiBBMultX", FuncFormula, 0., 3.);
			for (Int_t k=0; k<parBB->GetNpar(); k++) {parBBMultX->SetParameter(k, parBB->GetParameter(k));}
		}
		
		Double_t *Xgraph = new Double_t[nQAHistsloc]; Double_t *Xerrgraph = new Double_t[nQAHistsloc];
		Double_t *Ygraph = new Double_t[nQAHistsloc]; Double_t *sigma = new Double_t[nQAHistsloc];
		Double_t *sigma1 = new Double_t[nQAHistsloc]; Double_t *sigma2 = new Double_t[nQAHistsloc];
		for (Int_t k = 0; k < nQAHistsloc; k++) { Xgraph[k] = X[k + ibegloc]; Xerrgraph[k] = 0.; }
		
		for (Int_t i = 0; i < nQAHistsloc; i++)
		{
			XFUNCMIN = it->second.dEdXPart[i]->GetXaxis()->GetXmin(), XFUNCMAX = it->second.dEdXPart[i]->GetXaxis()->GetXmax();
			TF1* Gaus = new TF1("Gaus", "gaus", XFUNCMIN, XFUNCMAX);
			it->second.dEdXPart[i]->Fit(Gaus, "Q0R");
			TF1* AGaus = new TF1("AGaus", this, &MpdPidQA::AsymGaus, XFUNCMIN, XFUNCMAX, 4, "MpdPid", "AsymGaus");
			AGaus->SetParameters(Gaus->GetParameter(0), Gaus->GetParameter(1), Gaus->GetParameter(2), 0.01);
			it->second.dEdXPart[i]->Fit("AGaus","Q0RW");
			TF1 *Novosib = new TF1("Novosib", this, &MpdPidQA::Novosibirsk, XFUNCMIN, XFUNCMAX, 4, "MpdPidQA", "Novosibirsk");
			Novosib->SetParameters(Gaus->GetParameter(0), 0.01, Gaus->GetParameter(2), Gaus->GetParameter(1));
			it->second.dEdXPart[i]->Fit("Novosib","Q0RW");
			cout << "pdg = " << it->first << ", parBBMultX->Eval(mom) = " << parBBMultX->Eval(mom) << endl;
			mom = parBBMultX->Integral(Xlow[i+ibegloc],Xhigh[i+ibegloc])/(parBB->Integral(Xlow[i+ibegloc],Xhigh[i+ibegloc]));
			Ygraph[i] = Novosib->GetParameter(3); 
			Ygraph[i] /= (this->*GetDedxParam)(mom);
			sigma1[i] = AGaus->GetParameter(2); sigma2[i] = (1. + AGaus->GetParameter(3)) * AGaus->GetParameter(2);
			sigma1[i] /= (this->*GetDedxParam)(mom); sigma2[i] /= (this->*GetDedxParam)(mom);
			delete Gaus; delete AGaus;
		}
		TGraphAsymmErrors *gr = new TGraphAsymmErrors(nQAHistsloc,Xgraph,Ygraph,Xerrgraph,Xerrgraph,sigma1,sigma2);
		graphs.insert( pair<Int_t,TGraphAsymmErrors*>(it->first,gr) );
		delete parBB; delete parBBMultX; delete Xgraph; delete Ygraph; delete Xerrgraph; delete sigma;
	}
	
	SaveDedxGraphs(graphs, dir);
}

void MpdPidQA::Getm2QA(TString dir)
{
	Int_t nbinsx = 200, minbin, maxbin;
	Double_t minbinborder, maxbinborder, MINGAUSX, MAXGAUSX;
	TString mergestr, hname;
	map <Int_t,TH1D*> m2Sigmas;
	
	for (auto it = m2Hists.begin(); it != m2Hists.end(); ++it)
	{
		switch (it->first)
		{
			case 211: minbinborder = 0.2; maxbinborder = 3.0; mergestr = "QNR G4"; MINGAUSX = -0.4; MAXGAUSX = 0.4; hname = "Pi"; break;
			case 321: minbinborder = 0.2; maxbinborder = 2.8; mergestr = "QNR G4"; MINGAUSX = -0.2; MAXGAUSX = 0.8; hname = "Ka"; break;
			case 2212: minbinborder = 0.35; maxbinborder = 2.2; mergestr = "QNR G5"; MINGAUSX = 0.1; MAXGAUSX = 1.5; hname = "Pr"; break;
			case 11: minbinborder = 0.15; maxbinborder = 3.0; mergestr = "QNR G5"; MINGAUSX = -0.07; MAXGAUSX = 0.07; hname = "El"; break;
			case 13: minbinborder = 0.15; maxbinborder = 3.0; mergestr = "QNR G5"; MINGAUSX = -0.08; MAXGAUSX = 0.08; hname = "Mu"; break;
			case PDG_DEUTERON: minbinborder = 0.5; maxbinborder = 3.0; mergestr = "QNR G5"; MINGAUSX = 0.0; MAXGAUSX = 7.0; hname = "De"; break;
			case PDG_TRITON: minbinborder = 0.8; maxbinborder = 3.0; mergestr = "QNR G5"; MINGAUSX = 4.5; MAXGAUSX = 11.0; hname = "Tr"; break;
			case PDG_HE3: minbinborder = 0.5; maxbinborder = 1.5; mergestr = "QNR G5"; MINGAUSX = 0.0; MAXGAUSX = 4.0; hname = "He3"; break;
			case PDG_HE4: minbinborder = 0.7; maxbinborder = 1.5; mergestr = "QNR G5"; MINGAUSX = 1.0; MAXGAUSX = 6.0; hname = "He4"; break;
			default: break;
		}
		TF1 *fitgaus = new TF1("fitgaus","gaus",MINGAUSX,MAXGAUSX);
		for (Int_t i=1; i<=nbinsx; i++) { if (it->second->GetXaxis()->GetBinCenter(i) < minbinborder) continue; minbin = i; break; }
		maxbin = nbinsx;
		for (Int_t i=minbin; i<=nbinsx; i++) { if (it->second->GetXaxis()->GetBinCenter(i) < maxbinborder) continue; maxbin = i-1; break; }
		it->second->FitSlicesY(fitgaus, minbin, maxbin, 0, mergestr, 0);
		hname = "mass2" + hname + "_2";
		TH1D *Sigmas = (TH1D*)gDirectory->Get(hname);
		Sigmas->SetAxisRange(Sigmas->GetBinLowEdge(minbin), Sigmas->GetBinLowEdge(maxbin+1));
		m2Sigmas.insert( pair<Int_t,TH1D*>(it->first,Sigmas) );
		delete fitgaus;
	}
	Savem2Hists(m2Sigmas, dir);
}

void MpdPidQA::GetAmplQA(TString dir)
{
	map <Int_t,TF1*> fitFuncs;
	auto ret1 = ampls.find(-2212); auto ret2 = ampls.find(2212);
	Double_t PrRat = 0.;
	if ( (ret1 != ampls.end()) && (ret2 != ampls.end()) )
	{
		cout << "Default value N_protons / N_antiprotons = " << prrat << endl;
		PrRat = ret2->second->Integral() / ret1->second->Integral();
		cout << "Value from your data = " << PrRat << endl;
		cout << "Use MpdPid::SetPrRat(Double_t) in case of difference" << endl;
	}
	for (auto it = ampls.begin(); it != ampls.end(); ++it)
	{
		switch (it->first)
		{
			case -11:			if (Multiplicities[0] != 0) 
								{
									parElPosMom->SetParameter(0, parElPosMom->GetParameter(0) * it->second->Integral() / Multiplicities[0]);
									fitFuncs.insert( pair<Int_t,TF1*>(-11,parElPosMom) );
								}
								else cout << "Positron's multiplicity normalization is incorrect!" << endl;
								break;
			case 11:			if (Multiplicities[1] != 0)
								{
									parElNegMom->SetParameter(0, parElNegMom->GetParameter(0) * it->second->Integral() / Multiplicities[1]);
									fitFuncs.insert( pair<Int_t,TF1*>(11,parElNegMom) );
								}
								else cout << "Electron's multiplicity normalization is incorrect!" << endl;
								break;
			case -13:			if (Multiplicities[2] != 0)
								{
									parMuPosMom->SetParameter(0, parMuPosMom->GetParameter(0) * it->second->Integral() / Multiplicities[2]);
									fitFuncs.insert( pair<Int_t,TF1*>(-13,parMuPosMom) );
								}
								else cout << "Positive charged muon's multiplicity normalization is incorrect!" << endl;
								break;
			case 13:			if (Multiplicities[3] != 0)
								{
									parMuNegMom->SetParameter(0, parMuNegMom->GetParameter(0) * it->second->Integral() / Multiplicities[3]);
									fitFuncs.insert( pair<Int_t,TF1*>(13,parMuNegMom) );
								}
								else cout << "Negative charged muon's multiplicity normalization is incorrect!" << endl;
								break;
			case 211:			if (Multiplicities[4] != 0)
								{
									parPiPosMom->SetParameter(0, parPiPosMom->GetParameter(0) * it->second->Integral() / Multiplicities[4]);
									fitFuncs.insert( pair<Int_t,TF1*>(211,parPiPosMom) );
								}
								else cout << "Positive charged pion's multiplicity normalization is incorrect!" << endl;
								break;
			case -211:			if (Multiplicities[5] != 0)
								{
									parPiNegMom->SetParameter(0, parPiNegMom->GetParameter(0) * it->second->Integral() / Multiplicities[5]);
									fitFuncs.insert( pair<Int_t,TF1*>(-211,parPiNegMom) );
								}
								else cout << "Negative charged pion's multiplicity normalization is incorrect!" << endl;
								break;
			case 321:			if (Multiplicities[6] != 0)
								{
									parKaPosMom->SetParameter(0, parKaPosMom->GetParameter(0) * it->second->Integral() / Multiplicities[6]);
									fitFuncs.insert( pair<Int_t,TF1*>(321,parKaPosMom) );
								}
								else cout << "Positive charged kaon's multiplicity normalization is incorrect!" << endl;
								break;
			case -321:			if (Multiplicities[7] != 0)
								{
									parKaNegMom->SetParameter(0, parKaNegMom->GetParameter(0) * it->second->Integral() / Multiplicities[7]);
									fitFuncs.insert( pair<Int_t,TF1*>(-321,parKaNegMom) );
								}
								else cout << "Negative charged kaon's multiplicity normalization is incorrect!" << endl;
								break;
			case 2212:			if (Multiplicities[8] != 0)
								{
									parPrPosMom->SetParameter(0, parPrPosMom->GetParameter(0) * it->second->Integral() / Multiplicities[8]);
									fitFuncs.insert( pair<Int_t,TF1*>(2212,parPrPosMom) );
								}
								else cout << "Proton's multiplicity normalization is incorrect!" << endl;
								break;
			case -2212:			if (Multiplicities[9] != 0)
								{
									parPrNegMom->SetParameter(0, parPrNegMom->GetParameter(0) * it->second->Integral() / Multiplicities[9]);
									fitFuncs.insert( pair<Int_t,TF1*>(-2212,parPrNegMom) );
								}
								else cout << "Antiproton's multiplicity normalization is incorrect!" << endl;
								break;
			case PDG_DEUTERON:	if (Multiplicities[10] != 0)
								{
									parDeMom->SetParameter(0, parDeMom->GetParameter(0) * it->second->Integral() / Multiplicities[10]);
									fitFuncs.insert( pair<Int_t,TF1*>(PDG_DEUTERON,parDeMom) );
								}
								else cout << "Deuteron's multiplicity normalization is incorrect!" << endl;
								break;
			case PDG_TRITON:	if (Multiplicities[11] != 0)
								{
									parTrMom->SetParameter(0, parTrMom->GetParameter(0) * it->second->Integral() / Multiplicities[11]);
									fitFuncs.insert( pair<Int_t,TF1*>(PDG_TRITON,parTrMom) );
								}
								else cout << "Triton's multiplicity normalization is incorrect!" << endl;
								break;
			case PDG_HE3:		if (Multiplicities[12] != 0)
								{
									parHe3Mom->SetParameter(0, parHe3Mom->GetParameter(0) * it->second->Integral() / Multiplicities[12]);
									fitFuncs.insert( pair<Int_t,TF1*>(PDG_HE3,parHe3Mom) );
								}
								else cout << "He3's multiplicity normalization is incorrect!" << endl;
								break;
			case PDG_HE4:		if (Multiplicities[13] != 0)
								{
									parHe4Mom->SetParameter(0, parHe4Mom->GetParameter(0) * it->second->Integral() / Multiplicities[13]);
									fitFuncs.insert( pair<Int_t,TF1*>(PDG_HE4,parHe4Mom) );
								}
								else cout << "He4's multiplicity normalization is incorrect!" << endl;
								break;
			default:			break;
		}
	}
	SaveAmplHists(fitFuncs, dir);
}

void MpdPidQA::GetEffContQA(TString dir, TString s1, TString s2, TString s3)
{
	Int_t rebin = 2;
	for (auto it = effcont.begin(); it != effcont.end(); ++it)
	{
		for (Int_t i = 0; i < 4; i++) it->second.EffContPart[i]->Rebin(rebin);
		it->second.EffContPart[2]->Divide(it->second.EffContPart[0]);
		it->second.EffContPart[3]->Divide(it->second.EffContPart[1]);
		it->second.EffContPart[2]->SetStats(kFALSE); it->second.EffContPart[3]->SetStats(kFALSE);
		it->second.EffContPart[2]->GetYaxis()->SetRangeUser(0,1.1); it->second.EffContPart[3]->GetYaxis()->SetRangeUser(0,1.1);
		it->second.EffContPart[2]->GetXaxis()->SetTitle("p, GeV/c"); it->second.EffContPart[3]->GetXaxis()->SetTitle("p, GeV/c");
		it->second.EffContPart[2]->GetYaxis()->SetTitle("Efficiency"); it->second.EffContPart[3]->GetYaxis()->SetTitle("Contamination");
		it->second.EffContPart[2]->GetXaxis()->SetDecimals(); it->second.EffContPart[3]->GetXaxis()->SetDecimals();
		it->second.EffContPart[2]->GetYaxis()->SetDecimals(); it->second.EffContPart[3]->GetYaxis()->SetDecimals();
		it->second.EffContPart[2]->GetXaxis()->CenterTitle(); it->second.EffContPart[3]->GetXaxis()->CenterTitle();
		it->second.EffContPart[2]->GetYaxis()->CenterTitle(); it->second.EffContPart[3]->GetYaxis()->CenterTitle();
	}
	SaveEffContHists(dir, s1, s2, s3);
}

void MpdPidQA::SaveDedxGraphs(map <Int_t,TGraphAsymmErrors*> graphs, TString dir)
{	
	TString PARTNAME, CANNAME, SAVENAME;
	for (auto it = graphs.begin(); it != graphs.end(); ++it)
	{
		switch (it->first)
		{
			case 211: PARTNAME = "pions"; break;
			case 321: PARTNAME = "kaons"; break;
			case 2212: PARTNAME = "protons"; break;
			case 11: PARTNAME = "electrons"; break;
			case 13: PARTNAME = "muons"; break;
			case PDG_DEUTERON: PARTNAME = "deuterons"; break;
			case PDG_TRITON: PARTNAME = "tritons"; break;
			case PDG_HE3: PARTNAME = "he3"; break;
			case PDG_HE4: PARTNAME = "he4"; break;
			default: break;
		}
		
		CANNAME = "<dE/dx> / <Pid Bethe-Bloch> vs momentum (" + PARTNAME + ")";
		TCanvas *can = new TCanvas ("can", CANNAME, 1200, 800);
		gPad->SetGrid();
		it->second->SetTitle("");
		it->second->GetYaxis()->SetTitle("<dE/dx> / <Pid Bethe-Bloch>");
		it->second->GetXaxis()->SetTitle("Momentum, GeV/c");
		it->second->SetMarkerStyle(21); it->second->SetMarkerSize(2);
		it->second->GetXaxis()->SetLabelSize(0.06); it->second->GetYaxis()->SetLabelSize(0.06);
		it->second->GetXaxis()->SetTitleSize(0.06); it->second->GetYaxis()->SetTitleSize(0.06);
		it->second->GetXaxis()->CenterTitle(); it->second->GetYaxis()->CenterTitle();
		it->second->GetYaxis()->SetRangeUser(0.8,1.2);
		it->second->Draw("AP");
		SAVENAME = dir + "dEdXQA_" + PARTNAME + ".C";
		can->SaveAs(SAVENAME);
		delete can;
	}
}

void MpdPidQA::Savem2Hists(map <Int_t,TH1D*> m2Sigmas, TString dir)
{
	TString PARTNAME, CANNAME, SAVENAME;
	vector<TF1*> m2Funcs;
	for (auto it = m2Sigmas.begin(); it != m2Sigmas.end(); ++it)
	{
		switch (it->first)
		{
			case 211: PARTNAME = "pions"; parPiLowPM2->SetRange(0., 1.4); parPiHighPM2->SetRange(1.4, 3.); m2Funcs.push_back(parPiLowPM2); m2Funcs.push_back(parPiHighPM2); break;
			case 321: PARTNAME = "kaons"; m2Funcs.push_back(parKaM2); break;
			case 2212: PARTNAME = "protons"; parPrLowPM2->SetRange(0., 1.4); parPrHighPM2->SetRange(1.4, 3.); m2Funcs.push_back(parPrLowPM2); m2Funcs.push_back(parPrHighPM2); break;
			case 11: PARTNAME = "electrons"; m2Funcs.push_back(parElM2); break;
			case 13: PARTNAME = "muons"; m2Funcs.push_back(parMuM2); break;
			case PDG_DEUTERON: PARTNAME = "deuterons"; m2Funcs.push_back(parDeM2); break;
			case PDG_TRITON: PARTNAME = "tritons"; m2Funcs.push_back(parTrM2); break;
			case PDG_HE3: PARTNAME = "he3"; m2Funcs.push_back(parHe3M2); break;
			case PDG_HE4: PARTNAME = "he4"; m2Funcs.push_back(parHe4M2); break;
			default: break;
		}
		
		CANNAME = "Sigma vs momentum (" + PARTNAME + ")";
		TCanvas *can = new TCanvas("can", CANNAME, 1200, 800);
		gPad->SetGrid();
		it->second->SetMarkerStyle(22);
		it->second->SetMarkerSize(2);
		it->second->SetStats(kFALSE);
		it->second->SetTitle("");
		it->second->GetXaxis()->SetLabelSize(0.06); it->second->GetYaxis()->SetLabelSize(0.06);
		it->second->GetXaxis()->SetTitleSize(0.08); it->second->GetYaxis()->SetTitleSize(0.08);
		it->second->GetXaxis()->CenterTitle(); it->second->GetYaxis()->CenterTitle();
		it->second->GetXaxis()->SetTitle("Momentum, GeV/c");
		it->second->GetYaxis()->SetTitle("#sigma, (GeV/c^{2})^{2}");
		it->second->Draw("P"); 
		for(int i = 0; i < m2Funcs.size(); i++) { m2Funcs[i]->Draw("same"); } m2Funcs.clear();
		SAVENAME = dir + "m2QA_" + PARTNAME + ".C";
		can->SaveAs(SAVENAME);
		delete can;
	}
}

void MpdPidQA::SaveAmplHists(map <Int_t,TF1*> fitFuncs, TString dir)
{
	TString PARTNAME, CANNAME, SAVENAME;
	for (auto it = ampls.begin(); it != ampls.end(); ++it)
	{
		switch (it->first)
		{
			case 11: PARTNAME = "ElNeg"; CANNAME = "Electrons"; break;
			case -11: PARTNAME = "ElPos"; CANNAME = "Positrons"; break;
			case 13: PARTNAME = "MuNeg"; CANNAME = "Negative charged muons"; break;
			case -13: PARTNAME = "MuPos"; CANNAME = "Positive charged muons"; break;
			case 211: PARTNAME = "PiPos"; CANNAME = "Positive charged pions"; break;
			case -211: PARTNAME = "PiNeg"; CANNAME = "Negative charged pions"; break;
			case 321: PARTNAME = "KaPos"; CANNAME = "Positive charged kaons"; break;
			case -321: PARTNAME = "KaNeg"; CANNAME = "Negative charged kaons"; break;
			case 2212: PARTNAME = "PrPos"; CANNAME = "Protons"; break;
			case -2212: PARTNAME = "PrNeg"; CANNAME = "Antiprotons"; break;
			case PDG_DEUTERON: PARTNAME = "De"; CANNAME = "Deuterons"; break;
			case PDG_TRITON: PARTNAME = "Tr"; CANNAME = "Tritons"; break;
			case PDG_HE3: PARTNAME = "He3"; CANNAME = "Helium3"; break;
			case PDG_HE4: PARTNAME = "He4"; CANNAME = "Helium4"; break;
			default: break;
		}
		
		TCanvas *can = new TCanvas("can", CANNAME, 1200, 800);
		gPad->SetGrid();
		it->second->SetStats(kFALSE);
		it->second->SetTitle("");
		it->second->GetXaxis()->SetTitle("Momentum, GeV/c");
		it->second->GetYaxis()->SetTitle("Count");
		it->second->Draw(); auto ret = fitFuncs.find(it->first); if (ret != fitFuncs.end()) ret->second->Draw("same");
		SAVENAME = dir + "ampl" + PARTNAME + ".C";
		can->SaveAs(SAVENAME);
		delete can;
	}
}

void MpdPidQA::SaveEffContHists(TString dir, TString s1, TString s2, TString s3)
{
	if ( (s1 == "") && (s2 = "") && (s3 = "") ) s1 = nSigPart;
	Int_t colors[9] = { 4, 3, 2, 7, 6, 5, 8, 9, 1 }; TString particles[9] = {"pi", "ka", "pr", "el", "mu", "de", "tr", "he3", "he4"};
	TString partSymbol[9] = { "#pi", "K", "p", "e", "#mu", "d", "t", "{}^{3}He", "{}^{4}He" }; TString sign[2] = { "^{#plus}", "^{#minus}" };
	Int_t codes[9] = { 211, 321, 2212, 11, 13, PDG_DEUTERON, PDG_TRITON, PDG_HE3, PDG_HE4 };
	Bool_t match; TString name; TString s[3] = { s1, s2, s3 }; Int_t histNum[4] = { 2, 2, 3, 3 };
	TString ef = "Efficiency ", co = "Contamination ", po = "positive ", ne = "negative ", chp = "charged particles";
	TString CanNames[4] = { ef + "of " + po + chp, ef + "of " + ne + chp, co + "of " + po + chp, co + "of " + ne + chp };
	TString postfix[4] = { "_poscharged_eff.C", "_negcharged_eff.C", "_poscharged_cont.C", "_negcharged_cont.C" };
	for (Int_t sc = 0; sc < 3; sc++)
	{
		if (s[sc])
		{
			for (Int_t k = 0; k < 4; k++)
			{
				match = kFALSE; name = "";
				TCanvas *c = new TCanvas("c", CanNames[k], 1200, 800);
				gPad->SetGridx(); gPad->SetGridy();
				gPad->SetTickx(); gPad->SetTicky();
				gPad->SetBorderMode(0); gPad->SetBorderSize(0);
				gPad->SetRightMargin(0.02); gPad->SetLeftMargin(0.1);
				gPad->SetTopMargin(0.02); gPad->SetBottomMargin(0.1);
				for (Int_t i = 0; i < 9; i++)
				{
					if (s[sc].Contains(particles[i]))
					{
						auto ret = k % 2 ? effcont.find(-codes[i]) : effcont.find(codes[i]);
						if ( ret != effcont.end() )
						{
							name += particles[i];
							ret->second.EffContPart[histNum[k]]->SetMarkerStyle(20);
							ret->second.EffContPart[histNum[k]]->SetMarkerSize(2);
							ret->second.EffContPart[histNum[k]]->SetMarkerColor(colors[i]);
							ret->second.EffContPart[histNum[k]]->SetLineColor(colors[i]);
							ret->second.EffContPart[histNum[k]]->SetLineWidth(6);
							if (match) ret->second.EffContPart[histNum[k]]->Draw("P X0 E1 SAME");
							else { match = kTRUE; ret->second.EffContPart[histNum[k]]->Draw("P X0 E1"); }
						}
					}
				}
				if (match)
				{
					TLegend *leg = new TLegend(0.1,0.7,0.5,0.9);
					leg->SetFillColor(18);
					for (Int_t i = 0; i < 9; i++) 
					{ 
						if (s[sc].Contains(particles[i]))
						{
							auto ret = k % 2 ? effcont.find(-codes[i]) : effcont.find(codes[i]);
							if ( ret != effcont.end() )
							{ 
								if (i < 5) leg->AddEntry(ret->second.EffContPart[2], partSymbol[i] + sign[k%2], "p");
								else leg->AddEntry(ret->second.EffContPart[2], partSymbol[i], "p");
							}
						}
					}
					leg->Draw();
					gPad->Update();
					c->SaveAs(dir + name + postfix[k]);
					delete leg;
				}
				delete c;
			}
		}
	}
}

Double_t MpdPidQA::Novosibirsk(Double_t *x, Double_t *par)
{
	Double_t tail = par[1];
	Double_t width = par[2];
	Double_t peak = par[3];
	if (TMath::Abs(tail) < 1.e-7) 
	return par[0]*TMath::Exp( -0.5 * TMath::Power( ( (x[0] - peak) / width ), 2 ));
	Double_t arg = 1.0 - ( x[0] - peak ) * tail / width;
	if (arg < 1.e-6) return 0.0;  ///Argument of logaritem negative. Real continuation -> function equals zero
	
	Double_t log = TMath::Log(arg);   
	static const Double_t xi = 2.3548200450309494; /// 2 Sqrt( Ln(4) )
	Double_t width_zero = ( 2.0 / xi ) * TMath::ASinH( tail * xi * 0.5 );
	Double_t width_zero2 = width_zero * width_zero;
	Double_t exponent = ( -0.5 / (width_zero2) * log * log ) - ( width_zero2 * 0.5 );
	
	return par[0]*TMath::Exp(exponent);
}

ClassImp(MpdPidQA);
