#include <TChain.h>
#include <TString.h>
#include <TClonesArray.h>
#include <fstream>
#include <TVector3.h>
#include <TVector2.h>

// @(#)bmnroot/macro/howTo:$Id$
// Author: Pavel Batyuk <pavel.batyuk@jinr.ru> 2017-07-18

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// readEve.C                                                                  //
//                                                                            //
// An example how to read data (MC) from evetest.root                         //
// It demonstrates how to select GEM hits which belong to the GEM track       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
R__ADD_INCLUDE_PATH($VMCWORKDIR)
void CpcAnalysis(TString fileName = "/home/zarina/Alex/GEOMETRIA_MEXNICA/evetestMEXNICA.root") 
{
	cout<<"HELLO!"<<endl;
	if (fileName == "") 
	{
		cout << "File not specified!" << endl;
		return;
	}
	gROOT->LoadMacro("/home/zarina/mpdroot/macro/mpd/mpdloadlibs.C");
	cout<<"GOT HERE"<<endl;
	//mpdloadlibs(); // load libraries
	//gROOT->LoadMacro("/home/computer/demezhan/mpdroot/macro/cpc/geometry_cpc_demezhan.C");
	//gROOT->LoadMacro("/home/computer/demezhan/mpdroot/macro/mpd/geometry/geometry_cpc_demezhan.C");
	TChain* out = new TChain("cbmsim");
	out->Add(fileName.Data());
	cout << "#recorded entries = " << out->GetEntries() << endl;
			

		
					
	TCanvas* c1 = new TCanvas("c1","CPC distribution1", 1500, 1500);
	TCanvas* c2 = new TCanvas("c2","CPC distribution2", 1500, 1500);
	TCanvas* c3 = new TCanvas("c3","CPC distribution3", 1500, 1500);
	TCanvas* c4cpc = new TCanvas("cpc","CPC distribution4", 1500, 1500);
	TCanvas* c4tpc = new TCanvas("tpc","TPC distribution4", 1500, 1500);
	TCanvas* c5 = new TCanvas("c5","CPC distribution5", 1500, 1500);
	TCanvas* c6 = new TCanvas("c6","CPC distribution6", 1500, 1500);
	TCanvas* c7 = new TCanvas("c7","CPC distribution7", 1500, 1500);
	TCanvas* c8 = new TCanvas("c8","CPC distribution8", 1500, 1500);
	//TCanvas* c9 = new TCanvas("c9","CPC distribution9", 1500, 1500);
	TCanvas* c10 = new TCanvas("c10","CPC distribution10", 1500,1500);
	TCanvas* c11 = new TCanvas("c11","CPC distribution11", 1500, 3000);

	c1->Divide(1,1);
	c2->Divide(1,1);
	c3->Divide(1,1);
	c4cpc->Divide(1,1);
	c4tpc->Divide(1,1);
	c5->Divide(1,1);
	c6->Divide(1,1);
	c7->Divide(1,1);
	c8->Divide(1,1);
	//c9->Divide(1,1);
	c10->Divide(1,2);
	c11->Divide(1,2);
	TH1F* fR = new TH1F("r","r", 500, 0., 55.);
	TH2F* fXY = new TH2F("xy"," ", 100, -50., 50., 100, -50., 50.);
	TH1F* fZ = new TH1F("z"," ", 10000, -250., 250.);
	TH2F* fRphi = new TH2F("r #phi"," ", 100, 0, 2*TMath::Pi(), 100, 0., 50.);
	TH1F* cpceta = new TH1F("cpc #eta","", 1000., -5., 5.);
	TH1F* tpceta = new TH1F("tpc #eta","", 1000., -5., 5.);
	TH1F* fnum = new TH1F("num"," ", 500., -TMath::TwoPi(), TMath::TwoPi());
	TH1F* hist = new TH1F("PDGcode"," ", 10000., -3000, 3000);
	TH2F* fpxpy = new TH2F("evplane"," ", 100, -2, 2, 100, -2, 2);
	TProfile* fvtwo1 = new TProfile("v_{2}"," ", 100, 0., 1.5, 0., 0.3);
	TH1F* fE1 = new TH1F("Energy of All tracks", " ", 100., 0., 0.015);
	TH1F* fE2 = new TH1F("Energy of track #1", " ", 300.,0., 0.015);
	TH1F* fE3 = new TH1F("Energy of track #2", " ", 300.,0., 0.015);
	TH1F* fE4 = new TH1F("Energy of track #3", " ", 300.,0., 0.015);
	TH1F* fE5 = new TH1F("Energy of track #4", " ", 300.,0., 0.015);
	TH1F* fE6 = new TH1F("Energy of track #5", " ", 300.,0., 0.015);
	TH1F* fE7 = new TH1F("Energy of track #6", " ", 300.,0., 0.015);
	TH1F* fE8 = new TH1F("Energy of track #7", " ", 300.,0., 0.015);
	TH1F* fE9 = new TH1F("Energy of track #8", " ", 300.,0., 0.015);
	TH2F* fvtwo = new TH2F("p_{t}(v_{2})"," ", 100, 0., 3., 100., 0., 0.3);



	TStopwatch timer;
	timer.Start();
	gDebug=0;
	ofstream fout;
	
	{	
		FairMCEventHeader* mcEventHeader = NULL;
		TClonesArray* cpcPoints = NULL;
		TClonesArray* cpcTracks = NULL;
		out->SetBranchAddress("CPCPoint", &cpcPoints);
		out->SetBranchAddress("MCTrack", &cpcTracks);
		out->SetBranchAddress("MCEventHeader.", &mcEventHeader);
		

		
		double sumpx, sumpy;
		double *vtwo = new double [cpcPoints->GetEntriesFast()];
		double *sumvtwo = new double[out->GetEntries()];
		double *sumpt = new double[out->GetEntries()];
		double temp, temp1, trash;
		//fout.open("/home/demezhan/Desktop/Work/Work_05.02.2019/PDG_secprotons_10000ev_11GEV");
		//fout.open("/home/demezhan/Desktop/Work/Work_13.02.2019/vtwo1.txt");
		for (Int_t iEv = 0; iEv < out->GetEntries(); iEv++) 
		//for (Int_t iEv = 0; iEv < 100; iEv++) 
		{
			Int_t n = cpcPoints->GetEntriesFast();
			n=n+1;
			out->GetEntry(iEv);
			//double b = mcEventHeader -> GetB();
			//fb->Fill(b);
			for (Int_t iPoint = 0; iPoint < cpcPoints->GetEntriesFast(); iPoint++) 
			{
				MpdCpcPoint* cpcPoint = (MpdCpcPoint*) cpcPoints->UncheckedAt(iPoint);
				Int_t TrackID = cpcPoint->GetTrackID();
				//Int_t EventID = cpcPoint->GetEventID();
				FairMCTrack* mcTrack = (FairMCTrack*) cpcTracks->UncheckedAt(TrackID);
				//FairMCEventHeader* mcevent = (FairMCEventHeader*) mcEventHeader ->UncheckedAt(iEv);
				

			
				
				int code = mcTrack -> GetPdgCode();
				int mother = mcTrack -> GetMotherId();
				//int detId = cpcPoint -> GetDetectorID();
				//int det = cpcPoint->GetDetectorID();
				//int number = mcTrack -> GetNPoints(det);
				//double rapidity = mcTrack -> GetRapidity();
				//if (code==211 || code ==-211)
				//{
					Double_t x = cpcPoint->GetX();
					Double_t y = cpcPoint->GetY();
					Double_t z = cpcPoint->GetZ();
					Int_t N = mcTrack ->GetNPoints(kCPC); 
					Double_t px = mcTrack->GetPx();
					Double_t py = mcTrack->GetPy();
					Double_t pt = mcTrack->GetPt();
					Double_t ELOSS = cpcPoint->GetEnergyLoss();
					Double_t E = mcTrack->GetEnergy();
					Double_t b = mcEventHeader -> GetB();
					Int_t numb = mcEventHeader -> GetNPrim();
					
					
					vtwo[iPoint] = ((px*px-py*py)/(pt*pt));
					temp+=vtwo[iPoint]/n;
					sumvtwo[iEv] = temp;
					temp1+=pt/n;
					sumpt[iEv]=temp1;
					sumpx+=fabs(px)/n;
					sumpy+=fabs(py)/n;
					double r = sqrt(x*x+y*y);
					//double eps = N/numb;
					
					
					fXY->Fill(cpcPoint->GetX(), cpcPoint->GetY());
					TVector2 v(x, y);
					fR->Fill(v.Mod());
					fRphi->Fill(v.Phi(), v.Mod());
					TVector3 rad = TVector3(cpcPoint->GetX(), cpcPoint->GetY(), cpcPoint->GetZ());
					cpceta->Fill(rad.PseudoRapidity());
					fnum->Fill(rad.Phi());
					fZ->Fill(cpcPoint->GetZ());
					hist->Fill(code);
					fpxpy->Fill(mcTrack->GetPx(), mcTrack->GetPy());
									
					//fvdva->Fill(pt, vtwo);
					fE1->Fill(ELOSS);	
					if (r>=10 && r<=15) fE2->Fill(ELOSS);
					if (r>=15 && r<=20) fE3->Fill(ELOSS);
					if (r>=20 && r<=25) fE4->Fill(ELOSS);
					if (r>=25 && r<=30) fE5->Fill(ELOSS);
					if (r>=30 && r<=35) fE6->Fill(ELOSS);
					if (r>=35 && r<=40) fE7->Fill(ELOSS);
					if (r>=40 && r<=45) fE8->Fill(ELOSS);
					if (r>=45 && r<=50) fE9->Fill(ELOSS);
					//fout<<iEv<<"	"<<iPoint<<"	"<<code<<"	"<<mother<<endl;
				//}
			}
			//fout<<iEv<<"	"<<n<<"	"<<sumpt[iEv]<<"	"<<fabs(sumvtwo[iEv])<<endl;	
			fvtwo->Fill(sumpt[iEv], fabs(sumvtwo[iEv]));
			fvtwo1->Fill(sumpt[iEv], fabs(sumvtwo[iEv]));
			sumpx=0;
			sumpy=0;
			temp=0; temp1 = 0;
			//sumpt[0]=0;
			//sumvtwo[0]=0;
		}

		for (int iEv = 0; iEv < out->GetEntries(); iEv++) 
		{
			for (int iEvent = 0; iEvent < out->GetEntries() - iEv; iEvent++)
			{
				if (sumpt[iEvent] > sumpt[iEvent + 1]) 
				{
					// меняем элементы местами
					trash = sumpt[iEvent];
					sumpt[iEvent] = sumpt[iEvent + 1];
					sumpt[iEvent + 1] = trash;
				}
			}
		}
			// Вывод отсортированного массива на экран
		//for (int iEv = 0; iEv < out->GetEntries(); iEv++) 
		//{
		//    fout<<sumpt[iEv] << "	" << sumvtwo[iEv]<<endl;
		//}
	}
/*	
	{
		TClonesArray* tpcPoints = NULL;
		TClonesArray* tpcTracks = NULL;
		out->SetBranchAddress("TpcPoint", &tpcPoints);
		out->SetBranchAddress("MCTrack", &tpcTracks);
		//for (Int_t iEv = 0; iEv < out->GetEntries(); iEv++) 
		for (Int_t iEv = 0; iEv < 100; iEv++) 
		{
			out->GetEntry(iEv);
			for (Int_t iPoint = 0; iPoint < tpcPoints->GetEntriesFast(); iPoint++) 
			{
				TpcPoint* tpcPoint = (TpcPoint*) tpcPoints->UncheckedAt(iPoint);
				Int_t TrackID = tpcPoint->GetTrackID();
				FairMCTrack* mcTrack = (FairMCTrack*) tpcTracks->UncheckedAt(TrackID);
				TVector3 rad = TVector3(tpcPoint->GetX(), tpcPoint->GetY(), tpcPoint->GetZ());
				tpceta->Fill(rad.PseudoRapidity());
			
			}
		
		}
	}
	
*/	
    
	c1->cd(1);
	fXY->SetTitle("Plane Distribution of particles;r, cm;r, cm");
	fXY->Draw();
	c2->cd(1);
	fR->SetTitle("Distribution of particles by radius; r, cm; counts");
	fR->Draw();
	c3->cd(1);
	fRphi->SetTitle("#phi Distribution of particles by angle; #phi in radians; r, cm");
	fRphi->Draw();	
	c4cpc->cd(1);
	//c4cpc->SetLogy(); doesn't work
	cpceta->SetLineColor(kAzure);
	cpceta->SetTitle("Distribution of particles by pseudorapidity #eta; #eta ; counts");
	cpceta->Draw("SAME");	
	//c4cpc->cd(1);
	//c4tpc->SetLogy(); doesn't work
	//tpceta->SetLineColor(kRed);
	//tpceta->Draw("SAME");
	c5->cd(1);
	fZ->SetTitle("Oz Distribution of particles; cm; counts");
	fZ->Draw();
	c6->cd(1);
	fnum->SetTitle("#phi Distribution of particles; #phi in radians; counts");
	fnum->Draw();
	c7->cd(1);
	hist->SetTitle("Distribution of particles by PDG codes; PDG codes; counts");
	hist->Draw();
	c8->cd(1);
	fpxpy->SetTitle("Distribution of particles; p_{x}; p_{y}");
	fpxpy->Draw();
	//c9->cd(1);
	//fvtwo->SetTitle("Distribution of particles; $p_t; $v_2");
	//fvtwo->Draw();
	c10->cd(2);
	fE1->SetLineColor(kRed);
	fE1->SetTitle("Energy loss of particles in total; E, GeV; counts");
	fE1->Draw();
	c10->cd(1);
	fE2->SetLineColor(kBlue);
	fE2->SetTitle("Energy loss of particles in layers 1; E, GeV; counts");
	fE2->Draw("SAME");
	c10->cd(1);
	fE3->SetLineColor(kYellow);
	fE3->SetTitle("Energy loss of particles in layers 2; E, GeV; counts");
	fE3->Draw("SAME");
	c10->cd(1);
	fE4->SetLineColor(kGreen);
	fE4->SetTitle("Energy loss of particles in layers 3; E, GeV; counts");
	fE4->Draw("SAME");
	c10->cd(1);
	fE5->SetLineColor(kPink);
	fE5->SetTitle("Energy loss of particles in layers 4; E, GeV; counts");
	fE5->Draw("SAME");
	c10->cd(1);
	fE6->SetLineColor(kViolet);
	fE6->SetTitle("Energy loss of particles in layers 5; E, GeV; counts");
	fE6->Draw("SAME");
	c10->cd(1);
	fE7->SetLineColor(kOrange);
	fE7->SetTitle("Energy loss of particles in layers 6; E, GeV; counts");
	fE7->Draw("SAME");
	c10->cd(1);
	fE8->SetLineColor(kAzure);
	fE8->SetTitle("Energy loss of particles in layers 7; E, GeV; counts");
	fE8->Draw("SAME");
	c10->cd(1);
	fE9->SetLineColor(kMagenta);
	fE9->SetTitle("Energy loss of particles in layers 8; E, GeV; counts");
	fE9->Draw("SAME");
	c11->cd(1);
	fvtwo->SetTitle("Eliptic flow; p_{t}; v_{2}");
	fvtwo->Draw();
	c11->cd(2);
	fvtwo1->SetTitle("Eliptic flow; p_{t}; v_{2}");
	fvtwo1->Draw();
	fout.close();  
	timer.Stop();
	Double_t rtime = timer.RealTime();
	Double_t ctime = timer.CpuTime();
	printf("RealTime=%f seconds, CpuTime=%f seconds\n",rtime,ctime);
	cout << " Test passed" << endl;
	cout << " All ok " << endl;


    //delete [] sumpt; // освобождение памяти;
    //delete [] sumvtwo; // освобождение памяти;
    
    return 0;
}


	  
                        

