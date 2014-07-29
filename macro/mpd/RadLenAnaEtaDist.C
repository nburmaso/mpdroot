/* Macro draws histograms of radiation length scan of the MPD TPC.
  Output result values are "effective radiation lengths" which are calculated as radiation length (cm) divided by distance (cm).
 14/07/2012 A. Basalaev*/

#include <fstream>
#include <TH2D.h>
#include "TROOT.h"
#include "TSystem.h"


void RadLenAnaEtaDist(TString infile1="RadLenSimZR.root", TString infile2="RadLenSimEtaDist.root") {
  
  
  
    TProfile2D *h2 = new TProfile2D("h2", "Material budget in the MPD", 1500, 0., 3., 1900, 0., 190.);
    TH2D *h1 = new TH2D("h1", "Integrated radiation length", 3000, 0., 3., 1800, 0., 1.8);
    


    TStopwatch timer;
    timer.Start();

    /* Load basic libraries */
    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(kTRUE, kTRUE); // all libs
    
    
    
    TFile *inputEtaDist = new TFile(infile2);
    
    TChain *cbmTreeEtaDist = new TChain("cbmsim");
    cbmTreeEtaDist->Add(infile2);
    inputEtaDist->Close();
    
    
    if(inputEtaDist->IsZombie())
    {
      cout<<"No simulation files found!"<<endl<<"Aborting!"<<endl;
      exit(0);
    }
    
    if(!inputEtaDist->IsZombie())
{
     // Activate branches
    MpdEvent *eventEtaDist;
    cbmTreeEtaDist->SetBranchAddress("MCEventHeader.", &eventEtaDist);
    TClonesArray *fRadLenPs;
    cbmTreeEtaDist->SetBranchAddress("RadLen", &fRadLenPs);
    TClonesArray *fMCTracks;
    cbmTreeEtaDist->SetBranchAddress("MCTrack", &fMCTracks);

    Int_t events = cbmTreeEtaDist->GetEntries();
    std::cout << " Processing RadLenSimEtaDist..."<<std::endl;
    std::cout << " Number of events in file = " << events << std::endl;
    
    Int_t _points=0;
    Int_t _fNpoints;
    
    Double_t _X,_Y,_Z,_Len,_Dist,_LenEff, _Zin, _Zout, _step;
    TVector3 _PosIn, _PosOut, _DistVec;
    
    for (Int_t i = 0; i < events; i++) {
        cbmTreeEtaDist->GetEntry(i);
        _fNpoints = fRadLenPs->GetEntriesFast();
	  _points=_points+_fNpoints;
    }
    cout<<"Points total = "<<_points<<endl;
    Double_t *EtaArr = new Double_t[_points];
    Double_t *RadLenArr = new Double_t[_points];
    
    
    Double_t Eta, LenEffSum=0., EtaSum=0., Theta, PosInMag;
    Int_t Etacounter=1;
    Int_t _counter = 0;
    

    for (Int_t i = 0; i < events; i++) {
        cbmTreeEtaDist->GetEntry(i);
	
	  
        _fNpoints = fRadLenPs->GetEntriesFast();
	
	RadLenP = (FairRadLenPoint*) fRadLenPs->UncheckedAt(0);
	Int_t tracknum0=RadLenP->GetTrackID()-1;
	

        for (Int_t pointIndex = 0; pointIndex < _fNpoints; pointIndex++) {
            RadLenP = (FairRadLenPoint*) fRadLenPs->UncheckedAt(pointIndex);
            _Len = RadLenP->GetRadLength();
	    Int_t tracknum=RadLenP->GetTrackID();
	    _PosIn = RadLenP->GetPosition();
	    _PosOut = RadLenP->GetPositionOut();	    
	    _DistVec=_PosIn-_PosOut;
	    
	    _Dist=_DistVec.Mag();
	    _LenEff=_Dist/_Len;
	    Theta=_PosOut.Theta();
	    Eta=-log(tan(Theta/2));
	    
	   _X=_PosIn.X();
	   _Y=_PosIn.Y();
	   _Z=_PosIn.Z();
	   
	    if(tracknum==tracknum0)
	    {
	      if(_X<115. && _Z<170.)
	      LenEffSum=LenEffSum+_LenEff;
	      EtaSum=EtaSum+Eta;
	      Etacounter++;
	    }
	    else
	    {
	      
	      h1->Fill(EtaSum/Etacounter,LenEffSum);

	      LenEffSum=_LenEff;
	      EtaSum=0;
	      Etacounter=1;
	    }
	    tracknum0=tracknum;
	    
	    
	    //if(_Dist<1 || _LenEff<.1)
	    h2->Fill(Eta,_PosIn.Mag(), _LenEff);

	  /* else
	   {
	     PosInMag=_PosIn.Mag();
	     _step=.2;
	     while(PosInMag<_PosOut.Mag())
	     {
	       h2->Fill(Eta,PosInMag, _LenEff);
	       PosInMag+=_step;
	       
	      }
	   }*/
	   
	   Double_t perc=_counter;
  
           if(_counter%10000==0)
              {cout<<Int_t(perc/_points*100.)<<"% ("<<_counter<<" out of "<<_points<<" points) stored"<<endl;}
              _counter++;

        } // track loop
	
    } // event loop
    
    
    

TAxis *xax = h2->GetXaxis();
TAxis *yax = h2->GetYaxis();
TAxis *zax = h2->GetZaxis();
xax->SetTitle("Pseudorapidity");
yax->SetTitle("Distance from IP, cm");
zax->SetTitle("Radiation length, X/X0");
cout<<"Drawing histo..."<<endl;
TCanvas *c2 = new TCanvas("c2", "c2");
gStyle->SetOptStat(0);
h2->SetMaximum(.7);
h2->Draw("colz");


TCanvas *c3 = new TCanvas("c3", "c3");
TAxis *xax = h1->GetXaxis();
TAxis *yax = h1->GetYaxis();
xax->SetTitle("Pseudorapidity");
yax->SetTitle("Integrated Radiation length, X/X0");
h1->Draw();
}



}
     
