/*************************************************************************************
*  Example by: Elena Litvinenko
*  e-mail:     litvin@nf.jinr.ru
*  Version:    11-Dec-2012
************************************************************************************/
  
void extract_zdc_histo_from_dst (const char *filename="dst_auau.root")
{

  Double_t x,y,ee;  

  TFile *f=new TFile (filename);

  TH2F*  h_zdc_1_orig=(TH2F*)f->Get("HistZdc1En");
  TH2F*  h_zdc_2_orig=(TH2F*)f->Get("HistZdc2En");

  TChain chain ("cbmsim");
  chain.Add(filename);
  TChain *t=&chain;  
  Int_t n_events = t->GetEntries();

  TClonesArray *ELossZdc1Histo_arr=0;
  t->SetBranchAddress("ELossZdc1Histo",&ELossZdc1Histo_arr);  
  TClonesArray *ELossZdc2Histo_arr=0;
  t->SetBranchAddress("ELossZdc2Histo",&ELossZdc2Histo_arr);

  TString main_title_zdc1=h_zdc_1_orig->GetTitle();
  TString main_title_zdc2=h_zdc_2_orig->GetTitle();

  for (int i=1;i<4;i++) {

    t->GetEntry(i); 

    TH2F *h_zdc_1 = h_zdc_1_orig->Clone("h_zdc_1");
    TH2F *h_zdc_2 = h_zdc_2_orig->Clone("h_zdc_2");

    TVectorT<float> &v1=*((TVectorT<float>*)ELossZdc1Histo_arr->At(0));
    h_zdc_1->Set(v1.GetNoElements(),v1.GetMatrixArray());
    h_zdc_1->SetEntries(v1.GetNoElements());
    h_zdc_1->SetTitle(Form("%s (event %d)",main_title_zdc1.Data(),i));
    cout << h_zdc_1->GetTitle() << " " << h_zdc_1->GetSum() << endl;
     
    if (i<8)
       new TCanvas(); 
    h_zdc_1->Draw("colz");

    TVectorT<float> &v2=*((TVectorT<float>*)ELossZdc2Histo_arr->First());
    h_zdc_2->Set(v2.GetNoElements(),v2.GetMatrixArray());
    h_zdc_2->SetEntries(v2.GetNoElements());
    h_zdc_2->SetTitle(Form("%s (event %d)",main_title_zdc2.Data(),i));
    cout << h_zdc_2->GetTitle() << " " << h_zdc_2->GetSum() << endl;
      
     if (i<8)
       new TCanvas(); 
    h_zdc_2->Draw("colz");
  }
}

