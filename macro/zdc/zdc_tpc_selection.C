/*************************************************************************************
 * Basic macro to obtain MPD centrality maps from MC transported data 
*  Created by: litvin@nf.jinr.ru
*  Version:    30-Jan-2012
************************************************************************************/ 

void zdc_tpc_selection (const char *filename1="evetest.root", 
			const char *output_filename="tmp.root")
{
  char request[100];
  char *ename;
  Int_t n_events=100;
  Int_t nn, ne;
  TH1F *htmp=0;
  Double_t ee;
  Double_t bb[]={0,3.4,4.74,6.7,8.21,9.4,10.6,11.6,12.55,20};
  Int_t pp[]={0,5,10,20,30,40,50,60,70,100};
  Int_t cc[]={kCyan-2,kRed, kBlue,kGreen-5,kBlack,kAzure+2,kMagenta+2,kBlue+2,kRed+2,kBlue };
 
  if (output_filename) {
    TFile *fo=new TFile (output_filename,"UPDATE"); 

    TChain chain ("mpdsim");
    chain.Add(filename1);
    TChain *t=&chain;  
    n_events = t->GetEntries();  

    for (int b=0;b<9;b++) {
      TCut ccurrent=Form("MCEventHeader.fB>%g&&MCEventHeader.fB<=%g",bb[b],bb[b+1]);
      sprintf (request,">>ep%d",pp[b+1]);
      ename = (&(request[2]));
      nn=t->Draw(request,ccurrent,"entrylist");
      TEntryList *evN=(TEntryList*)gDirectory->Get(ename);
      ne=evN->GetN();

      if (ne) {
	t->SetEntryList(evN);
	ename = (&(request[3]));
	TH2F *h2=new TH2F(ename,ename,500,0,1000,250,0,50);

	for (int i=0;i<ne;i++) {
	  nn=t->Draw("TpcPoint.fTrackID","(MCTrack.fNPoints&2)&&MCTrack.fMotherId==-1","goff",1,i);
	  t->Draw("ZdcPoint.fTrackID>>htmp","ZdcPoint.fELoss*(ZdcPoint.fELoss>0)","goff",1,i);
	  htmp=(TH1F*)gROOT->FindObject("htmp");
	  ee=htmp->Integral(); 
	  h2->Fill(nn,ee);
	  htmp->Delete();
	}
	h2->SetMarkerColor(cc[b]);
	h2->SetMarkerStyle(20);
	h2->SetLineColor(cc[b]);
	h2->SetTitle(Form("%d-%d%%",pp[b],pp[b+1]));
	h2->GetXaxis()->SetTitle("TPC primary tracks multiplicity");
	h2->GetYaxis()->SetTitle("Eloss at ZDC"); 
	h2->SetDirectory(fo);
 
        cout << h2->GetTitle() << "  " << ne << endl;

	t->SetEntryList(0);
      } 

      evN->Delete();
    }
    fo->Write();
  }
}
