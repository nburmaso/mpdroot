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
//#include "TpcSector.h"
#include "TaskHelpers.h"
#include <TVector2.h>

#include <iostream>
using namespace std;
#endif

class TpcPadPlane;

const Double_t spread = 0.196;

void testPadPlane2()
{
  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  mpdloadlibs(kTRUE, kTRUE);      // load full set of main libraries

  const TpcPadPlane *plane = TpcPadPlane::Instance();
  
  TH1F *hDeltaX = new TH1F("Delta X", "Delta X", 200, -2, 2);
  hDeltaX->SetTitle("Distance (cm)");
  TH1F *hDeltaY = new TH1F("Delta Y", "Delta Y", 200, -2, 2);
  hDeltaY->SetTitle("Distance (cm)");
  TH1F *hDeltaXLocal = new TH1F("Delta X Local", "Delta X Local", 200, -2, 2);
  hDeltaXLocal->SetTitle("Distance (cm)");
  TH1F *hDeltaYLocal = new TH1F("Delta Y Local", "Delta Y Local", 200, -2, 2);
  hDeltaYLocal->SetTitle("Distance (cm)");
  
  for(int i = 0; i < 10000; i++)
  {
   if (i % 100 == 0)
     std::cout << "i = " << i << std::endl; 
    Double_t x = gRandom->Uniform(-100, 100);
    Double_t y = gRandom->Uniform(-100, 100);
    TVector2 p(x, y);
    TVector3 pLocal3D = plane->toSectorReferenceFrame( TVector3(x, y, 0) );
    TVector2 pLocal( pLocal3D.X(), pLocal3D.Y() );

    std::vector<int> lightedPads = plane->buildPadArea( p, spread*3);
    for(int j = 0; j < lightedPads.size(); j++)
    {
       int pad = lightedPads[j];
       assert( pad != -1);
       TVector2 ppLocal = plane->LocalPadPositionByID ( pad );
       TVector2 pp = plane->PadPositionByID ( pad );
       TVector2 d = p - pp;
       TVector2 dLocal = pLocal - ppLocal;
       hDeltaX -> Fill( d.X() );
       hDeltaY -> Fill( d.Y() );
       hDeltaXLocal -> Fill( dLocal.X() );
       hDeltaYLocal -> Fill( dLocal.Y() );
    }
  }
  TCanvas *c = new TCanvas("C1", "C1", 1200, 800);
  c->Divide(2, 2);
  c->cd(1);
  hDeltaX->Draw();
  c->cd(2);
  hDeltaY->Draw();
  c->cd(3);
  hDeltaXLocal->Draw();
  c->cd(4);
  hDeltaYLocal->Draw();
  c->Update();
   
}
