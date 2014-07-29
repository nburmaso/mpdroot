TH2F* getH2(const char* name, TFile* file);
TH1F* getH1(const char* name, TFile* file);

showClusterizerQA()
{
 gROOT->SetStyle("Plain");
 gStyle->SetOptStat(0);
 TFile *f = new TFile("tpcTest.root", "READ");
 TH1F *hGasCSD = getH1("/QA/TPC/Gas CSD", f);
 TH1F *hMCCSD = getH1("/QA/TPC/Cluster size distribution", f);
 
 hGasCSD->SetFillColor(16);
 hGasCSD->SetLineColor(16);
 hGasCSD->Draw();
 hMCCSD->SetLineWidth(2);
 hMCCSD->Draw("same");
 
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

