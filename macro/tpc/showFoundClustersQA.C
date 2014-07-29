TH2F* getH2(const char* name, TFile* file);
TH1F* getH1(const char* name, TFile* file);

showFoundClustersQA()
{
 gROOT->SetStyle("Plain");
 gStyle->SetOptStat(10);
 TFile *f = new TFile("tpcTest.root", "READ");
 TH2F *hADC1 = getH2("/QA/TPC/XY distribution of ADC Signal ( z > 0 )", f);
 TH2F *hADC2 = getH2("/QA/TPC/XY distribution of ADC Signal ( z < 0 )", f);
 TH2F *hFC1 = getH2("/QA/TPC/XY distribution of Found Clusters ( z > 0 )", f);
 TH2F *hFC2 = getH2("/QA/TPC/XY distribution of Found Clusters ( z < 0 )", f);

  TCanvas *c1 = new TCanvas("c1", "c1");
  c1->Divide(2, 2);
  c1->cd(1);
  hADC1->Draw();
  c1->cd(2);
  hADC2->Draw();
  c1->cd(3);
  hFC1->Draw("colz");
  c1->cd(4);
  hFC2->Draw("colz");

  c1->Update(); 
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

