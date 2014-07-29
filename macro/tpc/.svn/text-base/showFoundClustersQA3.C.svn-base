TH2F* getH2(const char* name, TFile* file);
TH1F* getH1(const char* name, TFile* file);

showFoundClustersQA3()
{
 gROOT->SetStyle("Plain");
 gStyle->SetOptStat(10);
 gStyle->SetOptFit(1111);
 TFile *f = new TFile("tpcTest.root", "READ");
 TH1F *hD[6];
 hD[0] = getH1("/QA/TPC/Local Delta X for TpcHit and track ( z > 0 )", f);
 hD[3] = getH1("/QA/TPC/Local Delta X for TpcHit and track ( z < 0 )", f);
 hD[1] = getH1("/QA/TPC/Local Delta Y for TpcHit and track ( z > 0 )", f);
 hD[4] = getH1("/QA/TPC/Local Delta Y for TpcHit and track ( z < 0 )", f);
 hD[2] = getH1("/QA/TPC/Local Delta Z for TpcHit and track ( z > 0 )", f);
 hD[5] = getH1("/QA/TPC/Local Delta Z for TpcHit and track ( z < 0 )", f);

 for(int i = 0; i < 6; i++)
 {
  if ( hD[i]->GetEntries() )
  {
   hD[i]->Fit("gaus", "Q");
   TF1* fun = hD[i]->GetFunction("gaus");
   std::cout << hD[i]->GetTitle() << " Mean = " << fun->GetParameter(1) << " \pm " << fun->GetParError(1) << std::endl;
  }
 }
 
  TCanvas *c1 = new TCanvas("c1", "c1");
  c1->Divide(3, 2);
  for(int i = 0; i < 6; i++)
 {
  c1->cd(i+1);
  hD[i]->Draw();
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

