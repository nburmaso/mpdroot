/* Macro draws histograms of radiation length scan of the MPD TPC
 Output result values are "effective radiation lengths" which are calculated as radiation length (cm) divided by distance (cm).
 14/07/2012 A. Basalaev*/

#include <fstream>
#include <TH2D.h>
#include "TROOT.h"
#include "TSystem.h"


void RadLenAnaZR(TString infile1="RadLenSimZR.root") {
  
  
  
    TProfile2D *h3 = new TProfile2D("h3", "Material budget in the MPD", 1250, 0., 250., 600, 0., 120.);
    


    TStopwatch timer;
    timer.Start();

    /* Load basic libraries */
    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(kTRUE, kTRUE); // all libs
    
    
    TFile *inputZR = new TFile(infile1);

    TChain *cbmTreeZR = new TChain("cbmsim");
    cbmTreeZR->Add(infile1);
    inputZR->Close();
    
    
    if(inputZR->IsZombie())
    {
      cout<<"No simulation files found!"<<endl<<"Aborting!"<<endl;
      exit(0);
    }
    
    if(!inputZR->IsZombie()){
    // Activate branches
    MpdEvent *eventZR;
    cbmTreeZR->SetBranchAddress("MCEventHeader.", &eventZR);
    TClonesArray *fRadLenPs;
    cbmTreeZR->SetBranchAddress("RadLen", &fRadLenPs);
    TClonesArray *fMCTracks;
    cbmTreeZR->SetBranchAddress("MCTrack", &fMCTracks);

    Int_t events = cbmTreeZR->GetEntries();
    std::cout << " Processing RadLenSimZR..."<<std::endl;
    std::cout << " Number of events in file = " << events << std::endl;

    FairRadLenPoint *RadLenP;
    
    Int_t points=0;
    
    for (Int_t i = 0; i < events; i++) {
        cbmTreeZR->GetEntry(i);
        Int_t fNpoints = fRadLenPs->GetEntriesFast();
	  points=points+fNpoints;
    }
    cout<<"Points total = "<<points<<endl;
    
    
    Double_t X,Y,Z,Len,Dist,LenEff, Zin, Zout, step;
    Int_t i=0, counter=0;
    TVector3 PosIn, PosOut, DistVec;
    

    for (Int_t i = 0; i < events; i++) {
        cbmTreeZR->GetEntry(i);
	  
        Int_t fNpoints = fRadLenPs->GetEntriesFast();

        for (Int_t pointIndex = 0; pointIndex < fNpoints; pointIndex++) {
            RadLenP = (FairRadLenPoint*) fRadLenPs->UncheckedAt(pointIndex);
            Len = RadLenP->GetRadLength();
	    PosIn = RadLenP->GetPosition();
	    PosOut = RadLenP->GetPositionOut();	    
	    DistVec=PosIn-PosOut;
	    Dist=DistVec.Mag();
	    LenEff=Dist/Len;
	    
	    
	   X=PosIn.X();
	   Y=PosIn.Y();
	   Z=PosIn.Z();

	   //Adding more points to histo to make the picture more realistic
	   //Does NOT affect radiation length values
	    if(Dist<1. || LenEff<.1)
	   h3->Fill(Z,X,LenEff);
	   else
	  { 
		i=0;
		Zin=Z;
		Zout=PosOut.Z();
		step=.2; //binx size
		while(Zin<Zout)
	            {
		      Zin+=step;
		      h3->Fill(Zin,X,LenEff);
		      i++;
		    }
	     }
	   
	      
	      Double_t perc=counter;
  
              if(counter%10000==0)
              {cout<<Int_t(perc/points*100.)<<"% ("<<counter<<" out of "<<points<<" points) stored"<<endl;}
              counter++;

        } // track loop
	
    } // event loop
        
    
TAxis *xax = h3->GetXaxis();
TAxis *yax = h3->GetYaxis();
TAxis *zax = h3->GetZaxis();
xax->SetTitle("Z, cm");
yax->SetTitle("R, cm");
zax->SetTitle("Radiation length, X/X0");
gStyle->SetOptStat(0);
h3->SetMaximum(.7);
cout<<"Drawing histo..."<<endl;
TCanvas *c1 = new TCanvas("c1", "c1");
h3->Draw("colz");
      }
      
      
      
}
     
