#include <TChain.h>
#include <TParticle.h>
#include <TParticlePDG.h>
#include <TDatabasePDG.h>

void femto_test() {

  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  mpdloadlibs(1, 1); // load main libraries

  MpdFemto* femto = new MpdFemto("$VMCWORKDIR/macro/mpd/mpddst_100events.root");
  
  femto->SetSourceSize(3.);      // Source size in fm;
  femto->SetPdgCode(211);        // pi+ are considered only;
  femto->SetEtaCuts(-1.0, 1.0);  // Define a range over pseudorapidity: val1 < eta < val2;
  femto->SetPtCuts(0.15, 1.5);   // Define a range over Pt: val1 < Pt < val2;
  femto->SetNumMixedEvents(10);  // Number of events to be mixed
  femto->SetKtCuts(0.15, 1.5);   // Define a range over Kt: val1 < Kt < val2;
  femto->SetQinv(0.20);
    
  femto->MakeCFs_1D();
    
  // Histos Draw & Save 
  MpdFemtoHistos * histos[4] = {femto->GetHistos()->GetNominator(),
        femto->GetHistos()->GetDenominator(),
        femto->GetHistos()->GetCF(),
        femto->GetHistos()->GetCFBase()
  };
  
  TCanvas* c = new TCanvas("1", "1", 600, 600);
  c->Divide(2, 2);
  
  for (Int_t iPad = 1; iPad < 5; iPad++) {
      c->cd(iPad);
      histos[iPad - 1]->Draw();
  }

  c->SaveAs("testCF.png");  
  
  
  
  
  
  
  
  
  
  delete femto;

}
