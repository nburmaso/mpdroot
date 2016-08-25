#include <TStyle.h>
#include <TH1.h>
#include <TH3F.h>
#include <TGraph.h>
#include <TH3.h>
#include <TH3D.h>
#include "MpdFemtoHistos.h"

//--------------------------------------------------------------------------

MpdFemtoHistos::MpdFemtoHistos(Float_t qInv, Int_t nKtBins,Int_t l, const Char_t* out) {
    fQinv = qInv;
    fKtBins = nKtBins;

    fOut = new TFile(out, "RECREATE");

    // Histos for 1D-analysis
    const Int_t nBins1D = 100;
    _hCFQinvNomBase = new TH1F("_hCFQinvNomBase", "_hCFQinvNomBase", nBins1D, 0., qInv);
    _hCFQinvNom = new TH1F("_hCFQinvNom", "_hCFQinvNom", nBins1D, 0., qInv);
    _hCFQinvDenom = new TH1F("_hCFQinvDenom", "_hCFQinvDenom", nBins1D, 0., qInv);
    _hCF = new TH1F("_hCF", "_hCF", nBins1D, 0., qInv);
    _hCFBase = new TH1F("_hCFBase", "_hCFBase", nBins1D, 0., qInv);

    // Histos for 3D-analysis
    const Int_t nBins3D = 60;
    _hCFNom3D = new TH3F*[fKtBins];
    _hCFDenom3D = new TH3F*[fKtBins];
    _hCF3D = new TH3F*[fKtBins];

    for (Int_t iKt = 0; iKt < fKtBins; iKt++) {
        _hCFNom3D[iKt] = new TH3F(Form("_kt%d_Nom_3D", iKt), Form("_kt%d_Nom_3D", iKt), nBins3D, 0., qInv, nBins3D, 0., qInv, nBins3D, 0., qInv);
        _hCFDenom3D[iKt] = new TH3F(Form("_kt%d_Denom_3D", iKt), Form("_kt%d_Denom_3D", iKt), nBins3D, 0., qInv, nBins3D, 0., qInv, nBins3D, 0., qInv);
        _hCF3D[iKt] = new TH3F(Form("_kt%d_CF_3D", iKt), Form("_kt%d_CF_3D", iKt), nBins3D, 0., qInv, nBins3D, 0., qInv, nBins3D, 0., qInv);
        _hCFNom3D[iKt]->Sumw2();
        _hCFDenom3D[iKt]->Sumw2();
    }

    _R_out_kT_3D = new TGraph();
    _R_side_kT_3D = new TGraph();
    _R_long_kT_3D = new TGraph();

    const Int_t nBins1 = 30;
    const Float_t xLow = -0.5;
    const Float_t xUp = -xLow;
    _hDeltaPhiDeltaEta = new TH2F("_hDeltaPhiDeltaEta", "_hDeltaPhiDeltaEta", nBins1, xLow, xUp, nBins1, xLow, xUp);
    _hDeltaPhiDeltaEtaNomin = new TH2F("_hDeltaPhiDeltaEtaNomin", "_hDeltaPhiDeltaEtaNomin", nBins1, xLow, xUp, nBins1, xLow, xUp);
    _hDeltaPhiDeltaEtaDenom = new TH2F("_hDeltaPhiDeltaEtaDenom", "_hDeltaPhiDeltaEtaDenom", nBins1, xLow, xUp, nBins1, xLow, xUp);
    //    _hDeltaPhiDeltaEtaNominProjX = new TH1D("_hDeltaPhiDeltaEtaNominProjX", "_hDeltaPhiDeltaEtaNominProjX", 30, -1., 1.);
    //    _hDeltaPhiDeltaEtaDenomProjX = new TH1D("_hDeltaPhiDeltaEtaDenomProjX", "_hDeltaPhiDeltaEtaDenomProjX", 30, -1., 1.);
    _hDeltaPhiDeltaEtaProjX = new TH1D("_hDeltaPhiDeltaEtaProjX", "_hDeltaPhiDeltaEtaProjX", nBins1, xLow, xUp);
    _hDeltaPhiDeltaEtaProjY = new TH1D("_hDeltaPhiDeltaEtaProjY", "_hDeltaPhiDeltaEtaProjY", nBins1, xLow, xUp);
    _hEtaPhiStar = new TH2F("_hEtaPhiStar", "_hEtaPhiStar", nBins1D, 0., 0., 100, 0., 0.);

    _hQuality = new TH1F("_hQuality", "_hQuality", 100, -0.6, 1.);
    _hSharing = new TH1F("_hSharing", "_hSharing", 100, 0.0, 0.3);
    _hQualityVsSharing = new TH2F("_hQualityVsSharing", "_hQualityVsSharing", 100, -0.6, 1., 100, 0.0, 0.3);

    _hPtNoSplit = new TH1F("_hPtNoSplit", "_hPtNoSplit", 200, 0.2, 1.0);
    _hPtSplit = new TH1F("_hPtSplit", "_hPtSplit", 200, 0.2, 1.0);

    _hEff = new TH1F("_hEff", "_hEff", 200, 0.2, 1.0);

    _hNsplits = new TH1I("_hNsplits", "_hNsplits", 19, 1, 20);

    _hQualityVsNhits = new TH2F("_hQualityVsNhits", "_hQualityVsNhits", 100, -0.6, 1., 50, 4, 54);
    fMaxL = l;
	 fMaxJM = (fMaxL+1)*(fMaxL+1);
	 fFactorialsSize = 4*(fMaxL+1);
		 fFactorials =  new Double_t[fFactorialsSize];
		 int fac = 1;
		 fFactorials[0] = 1;
		 for (int iter=1; iter<4*(fMaxL+1); iter++){
		     fac *= iter;
		     fFactorials[iter] = fac;
		  }

		  // Fill in els and ems table
		  Int_t el = 0;
		  Int_t em = 0;
		  Int_t il = 0;
		  fEls = new Double_t[fMaxJM];
		  fEms = new Double_t[fMaxJM];
		  fElsi = new Int_t[fMaxJM];
		  fEmsi = new Int_t[fMaxJM];
		  do {
		    fEls[il] = el;
		    fEms[il] = em;
		    fElsi[il] = (Int_t) el;
		    fEmsi[il] = (Int_t) em;

		    em++;
		    il++;
		    if (em > el) {
		      el++;
		      em = -el;
		    }
		  }
		  while (el <= fMaxL);
		  _hNumReal = new TH1D**[fKtBins];
		  _hNumImag = new TH1D**[fKtBins];
		  _hDenReal = new TH1D**[fKtBins];
		  _hDenImag = new TH1D**[fKtBins];
		  _hBinCtd = new TH1D*[fKtBins];
		  _hBinCtn = new TH1D*[fKtBins];
		  for(int ikt=0;ikt<fKtBins;ikt++){

			  _hNumReal[ikt] = new TH1D*[fMaxJM];
			  _hNumImag[ikt] = new TH1D*[fMaxJM];
			  _hDenReal[ikt] = new TH1D*[fMaxJM];
			  _hDenImag[ikt] = new TH1D*[fMaxJM];
			  _hBinCtn[ikt] = new TH1D(Form("BinCountNum[%i]",ikt),Form("BinCountNum[%i]",ikt),nBins1D,0,qInv);
			  _hBinCtd[ikt] = new TH1D(Form("BinCountDen[%i]",ikt),Form("BinCountDen[%i]",ikt),nBins1D,0,qInv);
			  for(int ihist = 0;ihist<fMaxJM;ihist++){
				  TString num_name_re = Form("NumReYlm%i%i[%i]",fElsi[ihist],fEmsi[ihist],ikt);
				  TString num_name_im = Form("NumImYlm%i%i[%i]",fElsi[ihist],fEmsi[ihist],ikt);
				  TString den_name_re = Form("DenReYlm%i%i[%i]",fElsi[ihist],fEmsi[ihist],ikt);
				  TString den_name_im = Form("DenImYlm%i%i[%i]",fElsi[ihist],fEmsi[ihist],ikt);
				  _hNumReal[ikt][ihist] = new TH1D(num_name_re,num_name_re,nBins1D,0,qInv);
				  _hNumImag[ikt][ihist] = new TH1D(num_name_im,num_name_re,nBins1D,0,qInv);
				  _hDenReal[ikt][ihist] = new TH1D(den_name_re,num_name_re,nBins1D,0,qInv);
				  _hDenImag[ikt][ihist] = new TH1D(den_name_im,num_name_re,nBins1D,0,qInv);
				  _hNumReal[ikt][ihist]->Sumw2();
				  _hNumImag[ikt][ihist]->Sumw2();
				  _hDenReal[ikt][ihist]->Sumw2();
				  _hDenImag[ikt][ihist]->Sumw2();
			  }
		  }
		  fYlmBuffer = new std::complex<double>[fMaxJM];
		  fCovSize = fMaxJM*fMaxJM*4*nBins1D;
		  fCovmnum = new Double_t*[fKtBins];
		  fCovmden = new Double_t*[fKtBins];
		  for(int ikt=0;ikt<fKtBins;ikt++){
			  fCovmnum[ikt] = new Double_t[fCovSize];
			  fCovmden[ikt] = new Double_t[fCovSize];
			  for(int j = 0;j<fCovSize;j++){
				  fCovmnum[ikt][j] =0;
				  fCovmden[ikt][j]=0;
			  }
		  }
		  _hCovNum = NULL;
		  _hCovDen = NULL;
		MpdFemtoYlm *ylm = MpdFemtoYlm::Instance();
		ylm->InitializeYlms();
}

//--------------------------------------------------------------------------

MpdFemtoHistos::~MpdFemtoHistos() {
    _hCFQinvNomBase->Write();
    _hCFQinvNom->Write();
    _hCFQinvDenom->Write();
    _hCF->Write();
    _hCFBase->Write();

    for (Int_t iKt = 0; iKt < fKtBins; iKt++) {
        _hCFNom3D[iKt]->Write();
        _hCFDenom3D[iKt]->Write();
        _hCF3D[iKt]->Write();
    }

    _hDeltaPhiDeltaEta->Write();
    _hQuality->Write();
    _hSharing->Write();
    _hQualityVsSharing->Write();
    _hPtNoSplit->Write();
    _hPtSplit->Write();
    _hEff->Write();
    _hNsplits->Write();
    _hQualityVsNhits->Write();
    _hDeltaPhiDeltaEtaNomin->Write();
    _hDeltaPhiDeltaEtaDenom->Write();
    //    _hDeltaPhiDeltaEtaNominProjX->Write();
    //    _hDeltaPhiDeltaEtaDenomProjX->Write();
    _hEtaPhiStar->Write();
    _hDeltaPhiDeltaEtaProjX->Write();
    _hDeltaPhiDeltaEtaProjY->Write();
    _R_out_kT_3D->Write();
    _R_side_kT_3D->Write();
    _R_long_kT_3D->Write();

    delete _hCFQinvNomBase;
    delete _hCFQinvNom;
    delete _hCFQinvDenom;
    delete _hCF;
    delete _hCFBase;

    for (Int_t iKt = 0; iKt < fKtBins; iKt++) {
        delete _hCFNom3D[iKt];
        delete _hCFDenom3D[iKt];
        delete _hCF3D[iKt];
    }

    delete _hDeltaPhiDeltaEta;
    delete _hDeltaPhiDeltaEtaNomin;
    delete _hDeltaPhiDeltaEtaDenom;
    delete _hQuality;
    delete _hSharing;
    delete _hQualityVsSharing;
    delete _hPtNoSplit;
    delete _hPtSplit;
    delete _hEff;
    delete _hNsplits;
    delete _hQualityVsNhits;
    //    delete _hDeltaPhiDeltaEtaNominProjX;
    //    delete _hDeltaPhiDeltaEtaDenomProjX;
    delete _hEtaPhiStar;
    delete _hDeltaPhiDeltaEtaProjX;
    delete _hDeltaPhiDeltaEtaProjY;



    std::cout<<"address "<<_hNumReal<<std::endl;
    if(_hNumReal){
	gDirectory->mkdir("SH");
	gDirectory->Cd("SH");
	MpdFemtoSHCF *cf = new MpdFemtoSHCF(fMaxL);
	PackCovariances();
	for(int i=0;i<fKtBins;i++){
		gDirectory->mkdir(Form("kt_%i",i));
		gDirectory->Cd(Form("kt_%i",i));
		cf->SetNumRe(_hNumReal[i],kFALSE);
		cf->SetNumIm(_hNumImag[i],kFALSE);
		cf->SetDenRe(_hDenReal[i],kFALSE);
		cf->SetDenIm(_hDenImag[i],kFALSE);
		cf->SetCovMatrix(_hCovNum[i],_hCovDen[i],kFALSE);
		cf->RecalculateCF();//calculate CF's in proper way
		TH1D **cf_real = cf->GetCFRe();
		TH1D **cf_imag = cf->GetCFIm();
		_hCovNum[i]->Write();
		_hCovDen[i]->Write();
		for(int j=0;j<fMaxJM;j++){
			_hNumReal[i][j]->Write();
			_hNumImag[i][j]->Write();
			_hDenReal[i][j]->Write();
			_hDenImag[i][j]->Write();
			TH1D *cf_re = (TH1D*)cf_real[j]->Clone();
			TH1D *cf_im = (TH1D*)cf_imag[j]->Clone();
			cf_re->Write();
			cf_im->Write();
		}
	}
	//set pointers to null to avoid crash during calling delete MpdFemtoSHCF
	cf->SetNumIm(NULL,kFALSE);
	cf->SetNumRe(NULL,kFALSE);
	cf->SetDenIm(NULL,kFALSE);
	cf->SetDenRe(NULL,kFALSE);
	cf->SetCovMatrix(NULL,NULL,kFALSE);
	delete cf;

    for(int ikt = 0;ikt<fKtBins;ikt++){
    	delete []_hNumReal[ikt];
    	delete []_hNumImag[ikt];
    	delete []_hDenReal[ikt];
    	delete []_hDenImag[ikt];
    	delete []fCovmnum[ikt];
    	delete []fCovmden[ikt];
    }
    delete []_hNumReal;
    delete []_hNumImag;
    delete []_hDenReal;
    delete []_hDenImag;
    delete []_hBinCtn;
    delete []_hBinCtd;
    delete []fCovmnum;
    delete []fCovmden;
    delete fYlmBuffer;
    delete []fEls;
    delete []fEms;
    delete []fElsi;
    delete []fEmsi;
    }

    delete fOut;
}

//--------------------------------------------------------------------------

Double_t* MpdFemtoHistos::GetFitParams1D() {
    _hDeltaPhiDeltaEtaNomin->Sumw2();
    _hDeltaPhiDeltaEtaDenom->Sumw2();
    _hDeltaPhiDeltaEta->Divide(_hDeltaPhiDeltaEtaNomin, _hDeltaPhiDeltaEtaDenom, 1., 1., "B");

    TH1D* nomProjX = _hDeltaPhiDeltaEtaNomin->ProjectionX("pxN", _hDeltaPhiDeltaEtaNomin->GetXaxis()->GetFirst(), _hDeltaPhiDeltaEtaNomin->GetXaxis()->GetLast());
    TH1D* nomProjY = _hDeltaPhiDeltaEtaNomin->ProjectionY("pyN", _hDeltaPhiDeltaEtaNomin->GetYaxis()->GetFirst(), _hDeltaPhiDeltaEtaNomin->GetYaxis()->GetLast());
    TH1D* denomProjX = _hDeltaPhiDeltaEtaDenom->ProjectionX("pxD", _hDeltaPhiDeltaEtaDenom->GetXaxis()->GetFirst(), _hDeltaPhiDeltaEtaDenom->GetXaxis()->GetLast());
    TH1D* denomProjY = _hDeltaPhiDeltaEtaDenom->ProjectionY("pyD", _hDeltaPhiDeltaEtaDenom->GetYaxis()->GetFirst(), _hDeltaPhiDeltaEtaDenom->GetYaxis()->GetLast());

    _hDeltaPhiDeltaEtaProjX->Divide(nomProjX, denomProjX, 1., 1., "B");
    _hDeltaPhiDeltaEtaProjY->Divide(nomProjY, denomProjY, 1., 1., "B");

    _hPtSplit->Sumw2();
    _hPtNoSplit->Sumw2();
    //_hPtSplit->Scale(1. / _hPtSplit->GetEntries());
    //_hPtNoSplit->Scale(1. / _hPtNoSplit->GetEntries());
    _hEff->Divide(_hPtSplit, _hPtNoSplit, 1., 1., "B");
    _hEff->Scale(100.);

    _hQuality->Scale(1. / _hQuality->GetEntries());
    _hSharing->Scale(1. / _hSharing->GetEntries());
    _hQualityVsSharing->Scale(1. / _hQualityVsSharing->GetEntries());
    _hQualityVsNhits->Scale(1. / _hQualityVsSharing->GetEntries());


    // Normalization:
    _hCFQinvNom->Sumw2();
    _hCFQinvNomBase->Sumw2();
    _hCFQinvDenom->Sumw2();
    // _hCF->Sumw2();
    _hCF->Divide(_hCFQinvNom, _hCFQinvDenom, 1., 1., "B");
    // _hCFBase->Sumw2();
    _hCFBase->Divide(_hCFQinvNomBase, _hCFQinvDenom, 1., 1., "B");

    Float_t normDenomFactor = _hCFQinvDenom->Integral(0.25 * _hCFQinvDenom->GetNbinsX(), 0.75 * _hCFQinvDenom->GetNbinsX());
    Float_t normNominFactor = _hCFQinvNom->Integral(0.25 * _hCFQinvNom->GetNbinsX(), 0.75 * _hCFQinvNom->GetNbinsX());
    Float_t normNominBaseFactor = _hCFQinvNomBase->Integral(0.25 * _hCFQinvNomBase->GetNbinsX(), 0.75 * _hCFQinvNomBase->GetNbinsX());

    if (normNominFactor > 0. && normNominBaseFactor > 0.) {
        _hCF->Scale(normDenomFactor / normNominFactor);
        _hCFBase->Scale(normDenomFactor / normNominBaseFactor);
    }

    //Fitting: fit QS only: C2 = N * [1 + lambda * exp(-q^2r0^2/h/h)], h=0.1973 GeV/fm
    TF1* fqs = new TF1("fqs", "[2] * (1 + [1] * exp(-x * x * [0] * [0] / 0.1973 / 0.1973))", 0, fQinv);
    fqs->SetParName(0, "r_{0}");
    fqs->SetParName(1, "#lambda");
    fqs->SetParName(2, "Norm");
    fqs->SetParameters(10., 1., 1.);

    _hCF->Fit(fqs, "SRQ", " ", 0., fQinv);

    return fqs->GetParameters();
}

//--------------------------------------------------------------------------

void MpdFemtoHistos::GetFitParams3D() {
    vector <Float_t> Rout;
    vector <Float_t> Rside;
    vector <Float_t> Rlong;
    vector <Float_t> kt_x;

    for (Int_t iKt = 0; iKt < fKtBins; iKt++) {
        kt_x.push_back(0.5 * (GetfKtRange(iKt) + GetfKtRange(iKt + 1)));
        //        _hCFNom3D[iKt]->Sumw2();
        //        _hCFDenom3D[iKt]->Sumw2();
        //        _hCF3D[iKt]->Sumw2();
        _hCF3D[iKt]->Divide(_hCFNom3D[iKt], _hCFDenom3D[iKt], 1., 1., "B");
    }

    for (Int_t iKt = 0; iKt < fKtBins; iKt++) {
        TF3* fitc = new TF3("fitc", "1 + [3] * exp(-25.76578 * (x * x * [0] + y * y * [1] + z * z * [2]))");
        fitc->SetParameters(3.0, 3.0, 3.0, 1.0);
        fitc->SetParLimits(0, 0., 100.);
        fitc->SetParLimits(1, 0., 100.);
        fitc->SetParLimits(2, 0., 100.);
        fitc->SetParLimits(3, 0., 1.);
        _hCF3D[iKt]->Fit(fitc, "SRQ", " ", 0., fQinv);
        Double_t* params = fitc->GetParameters();
        Rside.push_back(Sqrt(params[0]));
        Rout.push_back(Sqrt(params[1]));
        Rlong.push_back(Sqrt(params[2]));
        delete fitc;
    }

    for (Int_t iKt = 0; iKt < kt_x.size(); iKt++) {
        cout << "kT = [" << GetfKtRange(iKt) << "; " << GetfKtRange(iKt + 1) << "] (in GeV/c)" << endl;
        _R_out_kT_3D->SetPoint(iKt, kt_x[iKt], Rout[iKt]);
        cout << "R_out: " << Rout[iKt] << endl;
        _R_side_kT_3D->SetPoint(iKt, kt_x[iKt], Rside[iKt]);
        cout << "R_side: " << Rside[iKt] << endl;
        _R_long_kT_3D->SetPoint(iKt, kt_x[iKt], Rlong[iKt]);
        cout << "R_long: " << Rlong[iKt] << endl;
        cout << endl;
    }

    const Int_t markerStyle = 24;
    _R_out_kT_3D->SetName("R_{out}, fm");
    _R_out_kT_3D->SetMarkerStyle(markerStyle);
    _R_side_kT_3D->SetName("R_{side}, fm");
    _R_side_kT_3D->SetMarkerStyle(markerStyle);
    _R_long_kT_3D->SetName("R_{long}, fm");
    _R_long_kT_3D->SetMarkerStyle(markerStyle);
}

Int_t MpdFemtoHistos::GetBin(int qbin, int ilmzero, int zeroimag, int ilmprim,
		int primimag) {
	  return (qbin*GetMaxJM()*GetMaxJM()*4 +
		  (ilmprim*2 + primimag) * GetMaxJM()*2 +
		  ilmzero*2 + zeroimag);
}

void MpdFemtoHistos::PackCovariances() {
	if(_hCovNum)
		delete []_hCovNum;
	if(_hCovDen)
		delete []_hCovDen;
	_hCovNum = new TH3D*[fKtBins];
	_hCovDen = new TH3D*[fKtBins];
	for(int i=0;i<fKtBins;i++){
		TString name_num = Form("CovNum%i",i);
		TString name_den = Form("CovDen%i",i);
		TAxis *axis = _hNumReal[0][0]->GetXaxis();
		_hCovNum[i] = new TH3D(name_num,name_num,
				axis->GetNbins(),axis->GetBinLowEdge(1),axis->GetBinUpEdge(axis->GetNbins()),
				GetMaxJM()*2, -0.5, GetMaxJM()*2-0.5,
				GetMaxJM()*2, -0.5, GetMaxJM()*2-0.5);
		_hCovDen[i] = new TH3D(name_den,name_den,
				axis->GetNbins(),axis->GetBinLowEdge(1),axis->GetBinUpEdge(axis->GetNbins()),
				GetMaxJM()*2, -0.5, GetMaxJM()*2-0.5,
				GetMaxJM()*2, -0.5, GetMaxJM()*2-0.5);
		  for (int ibin=1; ibin<=_hCovNum[i]->GetNbinsX(); ibin++){
			 for (int ilmz=0; ilmz<GetMaxJM()*2; ilmz++){
			      for (int ilmp=0; ilmp<GetMaxJM()*2; ilmp++){
			    	  _hCovNum[i]->SetBinContent(ibin,ilmz+1,ilmp+1,fCovmnum[i][GetBin(ibin-1, ilmz/2, ilmz%2, ilmp/2, ilmp%2)]);
			    	  _hCovDen[i]->SetBinContent(ibin,ilmz+1,ilmp+1,fCovmden[i][GetBin(ibin-1, ilmz/2, ilmz%2, ilmp/2, ilmp%2)]);
			      }
			  }
		  }
	}
}

void MpdFemtoHistos::FillSHNumerator(Int_t kt, Double_t qout, Double_t qside,
		Double_t qlong, Double_t weight) {
	Double_t kv = TMath::Sqrt(qout * qout + qside * qside + qlong * qlong);
	MpdFemtoYlm::YlmUpToL(fMaxL, qout, qside, qlong, fYlmBuffer);
	for (int ilm = 0; ilm < GetMaxJM(); ilm++) {
		_hNumReal[kt][ilm]->Fill(kv, real(fYlmBuffer[ilm]) * weight);
		_hNumImag[kt][ilm]->Fill(kv, -imag(fYlmBuffer[ilm]) * weight);
		_hBinCtn[kt]->Fill(kv, 1.0);
	}
	Int_t nqbin = _hBinCtn[kt]->GetXaxis()->FindFixBin(kv) - 1;
	if (nqbin < _hBinCtn[kt]->GetNbinsX()) {
		Double_t weight2 = weight * weight;
		for (int ilmzero = 0; ilmzero < GetMaxJM(); ilmzero++) {
			for (int ilmprim = 0; ilmprim < GetMaxJM(); ilmprim++) {
				Int_t gbin = GetBin(nqbin, ilmzero, 0, ilmprim, 0);
				fCovmnum[kt][gbin] = fCovmnum[kt][gbin]
						+ real(fYlmBuffer[ilmzero]) * real(fYlmBuffer[ilmprim])
								* weight2;
				gbin = GetBin(nqbin, ilmzero, 0, ilmprim, 1);
				fCovmnum[kt][gbin] = fCovmnum[kt][gbin]
						+ real(fYlmBuffer[ilmzero]) * -imag(fYlmBuffer[ilmprim])
								* weight2;
				gbin = GetBin(nqbin, ilmzero, 1, ilmprim, 0);
				fCovmnum[kt][gbin] = fCovmnum[kt][gbin]
						- imag(fYlmBuffer[ilmzero]) * real(fYlmBuffer[ilmprim])
								* weight2;
				gbin = GetBin(nqbin, ilmzero, 1, ilmprim, 1);
				fCovmnum[kt][gbin] = fCovmnum[kt][gbin]
						- imag(fYlmBuffer[ilmzero]) * -imag(fYlmBuffer[ilmprim])
								* weight2;
			}
		}
	}
}

void MpdFemtoHistos::FillSHDenominator(Int_t kt, Double_t qout, Double_t qside,
		Double_t qlong, Double_t weight) {
	Double_t kv = TMath::Sqrt(qout * qout + qside * qside + qlong * qlong);
	MpdFemtoYlm::YlmUpToL(fMaxL, qout, qside, qlong, fYlmBuffer);
	for (int ilm = 0; ilm < GetMaxJM(); ilm++) {
		_hDenReal[kt][ilm]->Fill(kv, real(fYlmBuffer[ilm]) * weight);
		_hDenImag[kt][ilm]->Fill(kv, -imag(fYlmBuffer[ilm]) * weight);
		_hBinCtd[kt]->Fill(kv, 1.0);
	}
	Int_t nqbin = _hBinCtd[kt]->GetXaxis()->FindFixBin(kv) - 1;
	if (nqbin < _hBinCtd[kt]->GetNbinsX()) {
		Double_t weight2 = weight * weight;
		for (int ilmzero = 0; ilmzero < GetMaxJM(); ilmzero++) {
			for (int ilmprim = 0; ilmprim < GetMaxJM(); ilmprim++) {
				Int_t gbin = GetBin(nqbin, ilmzero, 0, ilmprim, 0);
				fCovmden[kt][gbin] = fCovmden[kt][gbin]
						+ real(fYlmBuffer[ilmzero]) * real(fYlmBuffer[ilmprim])
								* weight2;
				gbin = GetBin(nqbin, ilmzero, 0, ilmprim, 1);
				fCovmden[kt][gbin] = fCovmden[kt][gbin]
						+ real(fYlmBuffer[ilmzero]) * -imag(fYlmBuffer[ilmprim])
								* weight2;
				gbin = GetBin(nqbin, ilmzero, 1, ilmprim, 0);
				fCovmden[kt][gbin] = fCovmden[kt][gbin]
						- imag(fYlmBuffer[ilmzero]) * real(fYlmBuffer[ilmprim])
								* weight2;
				gbin = GetBin(nqbin, ilmzero, 1, ilmprim, 1);
				fCovmden[kt][gbin] = fCovmden[kt][gbin]
						- imag(fYlmBuffer[ilmzero]) * -imag(fYlmBuffer[ilmprim])
								* weight2;
			}
		}
	}
}

