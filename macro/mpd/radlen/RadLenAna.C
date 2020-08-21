/* Macro draws histograms of radiation length scan of the MPD
 Output result values are "effective radiation lengths" which are calculated as
 radiation length (cm) divided by distance (cm). 14/07/2012 A. Basalaev
 Updated:  august 2020 by RO rogachevsky@jinr.ru
*/

#include "TMath.h"
#include "TROOT.h"
#include <TChain.h>
#include <TClonesArray.h>
#include <TH2F.h>
#include <TVector3.h>

R__ADD_INCLUDE_PATH($VMCWORKDIR)
#include "macro/mpd/mpdloadlibs.C"


using namespace TMath;

//void DrawHisto(TH2F *h, TString xName, TString yName, TCanvas *c);

//___________________________________________________________
void RadLenAna(TString infile = "radlen.root") {

  TH2F *hXY = new TH2F("hXY", "Material budget in XY plane", 3000, -370., 370.,
                       3000, -370., 370.);
  TH2F *hZR = new TH2F("hZR", "Material budget in ZR plane", 3000, 0., 500.,
                       1500, 0., 370.);
  TH2F *hZX_narrow =
      new TH2F("hZX_narrow", "Material budget in ZX plane, |y| < 10.0 cm", 3000,
               -500., 500., 3000, -370., 370.);
  TH2F *hZX = new TH2F("hZX", "Material budget in ZX plane", 3000, -500., 500.,
                       3000, -370., 370.);
  TH2F *hEtaDist = new TH2F("hEtaDist", "Material budget in Eta", 3000,
                            -5.5, 5.5, 1500, 0., 600.);

  TFile file(infile);
  TTreeReader reader("mpdsim", &file);

  TTreeReaderValue <TClonesArray> fRadLenPs(reader, "RadLen"); //

  TVector3 PosIn, PosOut, DistVec;

  while (reader.Next()) {
    Int_t fNpoints = fRadLenPs->GetEntriesFast();

    for (Int_t pointIndex = 0; pointIndex < fNpoints; pointIndex++) {
      FairRadLenPoint *RadLenP = (FairRadLenPoint *)fRadLenPs->At(pointIndex);
      Float_t Len = RadLenP->GetRadLength();
      PosIn = RadLenP->GetPosition();
      PosOut = RadLenP->GetPositionOut();
      DistVec = PosIn - PosOut;
      Float_t Dist = DistVec.Mag();
      Float_t LenEff = Dist / Len;

      Float_t X = PosIn.X();
      Float_t Y = PosIn.Y();
      Float_t Z = PosIn.Z();
      Float_t r = Sqrt(X * X + Y * Y);
      Float_t Theta = PosIn.Theta();
      Float_t Eta = -Log(Tan(Theta / 2));

      if (Abs(Y) < 10.0)
        hZX_narrow->Fill(Z, X, LenEff);
      hZX->Fill(Z, X, LenEff);
      hXY->Fill(X, Y, LenEff);
      hZR->Fill(Z, r, LenEff);
      hEtaDist->Fill(Eta, PosIn.Mag(), LenEff);


    }

  }

 gStyle->SetOptStat(0);

 TCanvas* canv= new TCanvas("c","Radiation length, X/X0 %",1000,600);
   canv->Divide(3,2);

   gStyle->SetPaintTextFormat(".2g");
   gStyle->SetOptStat(0);
   const char* opt = "COLZ";

   canv->cd(1); hXY->Draw(opt);
   canv->cd(2); hZR->Draw(opt);
   canv->cd(3); hZX->Draw(opt);
   canv->cd(4); hZX_narrow->Draw(opt);
   canv->cd(5); hEtaDist->Draw(opt);

}

/*void DrawHisto(TH2F *h, TString xName, TString yName, TCanvas *c) {
  h->GetXaxis()->SetTitle(xName);
  h->GetXaxis()->CenterTitle();
  h->GetYaxis()->SetTitle(yName);
  h->GetYaxis()->CenterTitle();
  h->GetZaxis()->SetTitle("Radiation length, X/X0");
  h->SetMaximum(10.);
  h->Draw("colz");
}
*/
