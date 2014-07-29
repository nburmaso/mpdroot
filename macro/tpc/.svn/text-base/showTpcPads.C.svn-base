// Macro for running reconstruction

#if !defined(__CINT__) || defined(__MAKECINT__)
// ROOT includes
#include "TString.h"
#include "TStopwatch.h"
#include "TSystem.h"

// Fair includes
#include "FairRunAna.h"
#include "FairRuntimeDb.h"
#include "FairParRootFileIo.h"
#include "FairTask.h"
#include "FairField.h"
#include "FairTrackParP.h"

// MPD includes
#include "TpcClusterizerTask.h"
#include "TpcDriftTask.h"
#include "TpcHitFinderTask.h"
#include "MpdKalmanFilter.h"
#include "TpcLheHitsMaker.h"
#include "TpcLheKalmanFilter.h"
#include "MpdTofHitProducer.h"
#include "MpdTofMatching.h"
#include "MpdFillDstTask.h"
#include "MpdTpcPadPlane.h"
//#include "MpdTpcSector.h"
//#include "TaskHelpers.h"

#include <iostream>
using namespace std;
#endif

class MpdTpcPadPlane;
void FillArea(const MpdTpcPadPlane*, const TVector2& p, Double_t r, TH2F *h);

void showTpcPads()
{
  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  mpdloadlibs(kTRUE, kTRUE);      // load full set of main libraries

  MpdTpcPadPlane *plane = new MpdTpcPadPlane();
  plane->Initialize();
  
  TCanvas *c = new TCanvas("C1", "C1", 1200, 800);
  c->Divide(2,1);
  c->cd(1);
  TH2F *h = plane->buildQAHistogram(200);
   h->Draw("colz");
  
   TH2F *h2 = new TH2F("Pad Areas", "Pad Areas", 201, -100, 100, 201, -100, 100);
   h2->SetXTitle("X (cm)");
   h2->SetYTitle("Y (cm)"); 
   
   FillArea(plane, TVector2(60,60), 10, h2);
   FillArea(plane, TVector2(-40, -40), 10, h2);
   FillArea(plane, TVector2(0, 60), 10, h2);
   FillArea(plane, TVector2(25, -25), 10, h2);
   FillArea(plane, TVector2(60, -20), 10, h2);
   
   c->cd(2);
   h2->Draw("colz");
   c->Update();
   
 delete plane;
}

void FillArea(const MpdTpcPadPlane *plane, const TVector2& p, Double_t r, TH2F *h)
{
   std::vector<int> ps1 = plane->PadArea(p, r);
   std::cout << "Pad area (" << p.X() << ", " << p.Y() << ", " << r << ") has " << ps1.size() << " entries" << std::endl;
   for(int i = 0; i < ps1.size(); i++)
    {
      TVector2 point = plane->PadPositionByID(ps1[i]);
      h->Fill( point.X(), point.Y());
    }
}