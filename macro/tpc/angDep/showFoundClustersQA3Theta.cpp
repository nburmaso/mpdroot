#include <vector>
#include <TH2F.h>
#include <TH1F.h>
#include <TF1.h>
#include <TROOT.h>
#include <TFile.h>
#include <TCanvas.h>
#include <TGraph.h>
#include <TGraphErrors.h>
#include <TStyle.h>
#include <TNamed.h>
#include <cstdlib>
#include <cassert>

TH2F* getH2(const char* name, TFile* file);
TH1F* getH1(const char* name, TFile* file);

struct FitData {
 double _mean;
 double _meanErr;
 double _sigma;
 double _sigmaErr;
 double _angle;
 FitData(): _mean(-1000), _meanErr(-1000), _sigma(-1000), _sigmaErr(-1000), _angle(-1000) {}
};

struct  HistogramData {
  std::string _histName;
  std::string _shotName;
  std::vector<FitData> _fitData;
  HistogramData(const char* hn, const char *sn): _histName(hn), _shotName(sn) {}
};


void collectData(const char *fname, std::vector<HistogramData> &nList, Double_t angle);
void fill(HistogramData &d, const TF1 *f, Double_t angle);
void buildGraphs(const std::vector<HistogramData> &d, std::vector<TGraphErrors*> &grSigma, std::vector<TGraphErrors*> &grMean, const char* xtitle);
void show(std::vector<HistogramData> &nList, const char* xtitle);


void showFoundClustersQA3()
{
 gROOT->SetStyle("Plain");
 gStyle->SetOptStat(10);
 
 std::vector<HistogramData> nList;
 nList.push_back( HistogramData("Local Delta X for TpcHit and track ( z > 0 )", "Delta X (z>0)") );
 nList.push_back( HistogramData("Local Delta Y for TpcHit and track ( z > 0 )", "Delta Y (z>0)") );
 nList.push_back( HistogramData("Local Delta Z for TpcHit and track ( z > 0 )", "Delta Z (z>0)") );
 nList.push_back( HistogramData("Local Delta X for TpcHit and track ( z < 0 )", "Delta X (z<0)") );
 nList.push_back( HistogramData("Local Delta Y for TpcHit and track ( z < 0 )", "Delta Y (z<0)") );
 nList.push_back( HistogramData("Local Delta Z for TpcHit and track ( z < 0 )", "Delta Z (z<0)") );

 for(int i = 0; i < 12; i++)
 {
   Double_t angle = i*5;
//   if (i == 6)
//     continue; //skip 15deg
   char fname[200];
   sprintf(fname, "dataTheta%02iHistogram.root", i);
   collectData(fname, nList, angle);
 }

 std::cout << "Data collected " << std::endl;
 show(nList, "#theta Angle        (deg)");

}

void show(std::vector<HistogramData> &nList, const char* xtitle)
{
 std::vector<TGraphErrors*> gSigmas;
 std::vector<TGraphErrors*> gMeans;

 buildGraphs(nList, gSigmas, gMeans, xtitle);
 std::cout << "Graphs are build " << std::endl;

  TCanvas *c1 = new TCanvas("c1", "c1");
  c1->Divide(3, 2);
  for(int i = 0; i < 3; i++)
  {
   c1->cd(i+1);
   gSigmas[i]->Draw("AP");
   c1->cd(i+4);
   gMeans[i]->Draw("AP");
  }
  c1->Update(); 

}

void buildGraphs(const std::vector<HistogramData> &d, 
			    std::vector<TGraphErrors*> &grSigma, 
                               std::vector<TGraphErrors*> &grMean,
                               const char* xtitle)
{
 for(int iHist = 0; iHist < d.size(); iHist++)
 {
  Int_t size = d[iHist]._fitData.size();
  Double_t mean[size], meanErr[size], sigma[size], sigmaErr[size], angle[size], angleErr[size];

   for(int i = 0; i < size; i++)
   {
    angle[i] = d[iHist]._fitData[i]._angle;
    angleErr[i] = 0;
    mean[i] = 10*d[iHist]._fitData[i]._mean;
    meanErr[i] = 10*d[iHist]._fitData[i]._meanErr;
    sigma[i] = 10*d[iHist]._fitData[i]._sigma;
    sigmaErr[i] = 10*d[iHist]._fitData[i]._sigmaErr;
   }
  std::string name = std::string(d[iHist]._histName) + " Sigma";
  TGraphErrors *gs = new TGraphErrors(size, angle, sigma, angleErr, sigmaErr);
  gs->SetTitle(name.c_str());
  gs->SetMarkerStyle(20);
  gs->SetFillColor(2);
  gs->GetXaxis()->SetTitle(xtitle);
  gs->GetYaxis()->SetTitle("#sigma        (mm)");

  name = std::string(d[iHist]._histName) + " Mean";
  TGraphErrors *gm = new TGraphErrors(size, angle, mean, angleErr, meanErr);
  gm->SetTitle(name.c_str());
  gm->SetMarkerStyle(20);
  gm->SetFillColor(2);
  gm->GetXaxis()->SetTitle(xtitle);
  gm->GetYaxis()->SetTitle("mean        (mm)");

  grSigma.push_back( gs );
  grMean.push_back( gm );
 }
}

void collectData(const char *fname, std::vector<HistogramData> &nList, Double_t angle)
{ 
 TFile *f = new TFile(fname, "READ");
 std::vector<TH1F*>  hists;
 for(int i = 0; i < nList.size(); i++)
 {
   std::string name = std::string("/QA/TPC/") + nList[i]._histName;
   hists.push_back( getH1(name.c_str(), f) );
 }
 std::vector<TF1*> fits;
 for(int i = 0; i < hists.size(); i++)
 {
  if (hists[i]->GetEntries() ) 
   {
      hists[i]->Fit("gaus", "");
      fits.push_back( hists[i]->GetFunction("gaus") );
   }
   else
     fits.push_back( 0 );
 }
 assert( fits.size() == nList.size() );
 for(int i = 0; i < nList.size(); i++)
 {
  fill(nList[i], fits[i], angle);
 }

  gStyle->SetOptFit(1111);
 TCanvas *c = new TCanvas(fname, fname);
  c->Divide(3, 2); //not correct at all
  assert(nList.size() == 6);
  for(int i = 0; i < nList.size(); i++)
  {
   c->cd(i+ 1);
   hists[i]->Draw();
  }
 std::string name = std::string(fname) + ".ps";
 c->Print(name.c_str());
 delete c;
 gStyle->SetOptFit(0);
}

void fill(HistogramData &hd, const TF1 *f, Double_t angle)
{
 FitData d;
  if (f)
  {
    d._mean = f->GetParameter(1);
    d._meanErr = f->GetParError(1);
    d._sigma = f->GetParameter(2);
    d._sigmaErr = f->GetParError(2);
    d._angle = angle;
  }
 hd._fitData.push_back ( d );
}

TH2F* getH2(const char* name, TFile* file)
{
 TH2F *h = (TH2F*) file->Get(name);
 if (!h)
   {
     std::cout << "Cannot find histogram " << name << std::endl;
     exit(1);
   }
 return h;
}

TH1F* getH1(const char* name, TFile* file)
{
 TH1F *h = (TH1F*) file->Get(name);
 if (!h)
   {
     std::cout << "Cannot find histogram " << name << std::endl;
     exit(1);
   }
 return h;
}

