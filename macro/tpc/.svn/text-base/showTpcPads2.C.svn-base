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
#include "TpcPadPlane.h"
//#include "MpdTpcSector.h"
//#include "TaskHelpers.h"

#include <iostream>
using namespace std;
#endif

class TpcPadPlane;

void showTpcPads2()
{
  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  mpdloadlibs(kTRUE, kTRUE);      // load full set of main libraries

  const TpcPadPlane *plane = TpcPadPlane::Instance();
  
  TCanvas *c = new TCanvas("C1", "C1", 1200, 800);
  c->cd();
  TH2F *h = plane->buildQAHistogram2(200);
   h->Draw("colz");
  
    c->Update();
   
 delete plane;
}
