TH2F* getH2(const char* name, TFile* file);
TH1F* getH1(const char* name, TFile* file);

showFoundClustersQA4()
{
 gROOT->SetStyle("Plain");
 gStyle->SetOptStat(10);
 gStyle->SetOptFit(1111);
 TFile *f = new TFile("tpcTest.root", "READ");
 TH2F *hD[2];
 hD[0] = getH2("/QA/TPC/Local Delta X for TpcHit and track vs X Interval ( z > 0 )", f);
 hD[1] = getH2("/QA/TPC/Local Delta X for TpcHit and track vs X Interval ( z < 0 )", f);

 TH1F *hD2[2];
 hD2[0] = getH1("/QA/TPC/Local Delta X for TpcHit and track ( z > 0 )", f);
 hD2[1] = getH1("/QA/TPC/Local Delta X for TpcHit and track ( z < 0 )", f);

 
  TCanvas *c1 = new TCanvas("c1", "c1");
  c1->Divide(2, 2);
  for(int i = 0; i < 2; i++)
 {
  c1->cd(i+1);
  hD[i]->Draw("box");
 }
  for(int i = 2; i < 4; i++)
 {
  c1->cd(i+1);
  hD2[i - 2]->Draw();
 }

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

