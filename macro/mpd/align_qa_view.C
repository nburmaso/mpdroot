#include "../../kalman/MpdKalmanHit.h"
#include "../../kalman/MpdKalmanTrack.h"
#include "../../kalman/MpdTpcKalmanTrack.h"
#include "../../tpc/MpdTpcHit.h"
#include "../../mpdbase/MpdEvent.h"
#include "../../mpdbase/MpdHelix.h"
#include "../../mpdbase/MpdTrack.h"

double SqDev(std::vector<double> & dca)
{
	double sqDev = 0.;
	for (double d : dca)
		sqDev += pow(d, 2);
	
	//cout << "sqDev = " << sqDev << "   " << dca.size() << endl;
	return sqrt(sqDev / (double)(dca.size() - 1));
}

void align_qa_view(std::string inFileEve = "evetest.root",std::string inFileDst = "mpddst_align.root", long long event = 0, Double_t sqDevMax = 0.2)
{
	auto C = new TCanvas();
	C->Divide(2,2);
	
	gStyle->SetOptTitle(kFALSE);
	gStyle->SetOptStat(100010);
	
	TGraph * grx = new TGraph();
	grx->SetMarkerStyle(kFullCircle);
	grx->SetMarkerColor(kRed);
	grx->SetMarkerSize(0.25);
	TGraph * grxMC = new TGraph();
	grxMC->SetMarkerStyle(kFullCircle);
	grxMC->SetMarkerColor(kBlue);
	grxMC->SetMarkerSize(0.25);
	
	TGraph * gry = new TGraph();
	gry->SetMarkerStyle(kFullCircle);
	gry->SetMarkerColor(kRed);
	gry->SetMarkerSize(0.25);
	TGraph * gryMC = new TGraph();
	gryMC->SetMarkerStyle(kFullCircle);
	gryMC->SetMarkerColor(kBlue);
	gryMC->SetMarkerSize(0.25);
	
	TGraph * grz = new TGraph();
	grz->SetMarkerStyle(kFullCircle);
	grz->SetMarkerColor(kRed);
	grz->SetMarkerSize(0.25);
	TGraph * grzMC = new TGraph();
	grzMC->SetMarkerStyle(kFullCircle);
	grzMC->SetMarkerColor(kBlue);
	grzMC->SetMarkerSize(0.25);
	
	TGraph * grR = new TGraph();
	grR->SetMarkerStyle(kFullCircle);
	grR->SetMarkerColor(kRed);
	grR->SetMarkerSize(0.25);
	TGraph * grRMC = new TGraph();
	grRMC->SetMarkerStyle(kFullCircle);
	grRMC->SetMarkerColor(kBlue);
	grRMC->SetMarkerSize(0.25);
	
	TGraph2D * gr = new TGraph2D();
	gr->GetXaxis()->SetTitle("X");
	gr->GetYaxis()->SetTitle("Y");
	gr->GetZaxis()->SetTitle("Z");
	gr->SetMarkerColor(kRed);
	
	TGraph2D * grMC = new TGraph2D();
	grMC->SetMarkerColor(kBlue);
	
	auto CDev = new TCanvas();
	CDev->SetWindowSize(1600, 950);
	
	//TH1D * hDev = new TH1D("SqDev", "Square Deviation, RecoTrackHelix(MCPoints)", 100, 0., 0.5);
	TH1D * hDev = new TH1D("SqDev", "#splitline{Square Deviation MC points (blue) and TPC hits (red)}{from track reconstructed helix with misalignment and alignment.}", 100, 0., 0.5);
	hDev->GetXaxis()->SetTitle("SqDev, cm");
	hDev->GetYaxis()->SetTitle("Tracks, number");
	//TH1D * hDevHit = new TH1D("SqDevHit", "Square Deviation, RecoTrackHelix(TpcHits)", 100, 0., 0.5);
	TH1D * hDevHit = new TH1D("SqDevHit", "", 100, 0., 0.5);
	hDevHit->GetXaxis()->SetTitle("SqDev, cm");
	hDevHit->GetYaxis()->SetTitle("Tracks, number");
	hDevHit->SetLineColor(kRed);
	
	TFile * fileEve = TFile::Open(inFileEve.c_str());
	TTree * treeEve = (TTree *)fileEve->Get("mpdsim");
	
	TFile * fileDst = TFile::Open(inFileDst.c_str());
	TTree * treeDst = (TTree *)fileDst->Get("mpdsim");
	TClonesArray * fKalmanTracks = 0;
	treeDst->SetBranchAddress("TpcKalmanTrack", &fKalmanTracks);
	TClonesArray * fTpcRecPts = 0;
	treeDst->SetBranchAddress("TpcRecPoint", &fTpcRecPts);
	MpdEvent *fEvent = 0;
	treeDst->SetBranchAddress("MPDEvent.", &fEvent);
	treeDst->GetEntry(event);
	int nEntries = treeDst->GetEntriesFast();
	TClonesArray * fGlobTracks = fEvent->GetGlobalTracks();
	
	treeEve->GetEntry(event);
	TLeaf * fX = treeEve->FindLeaf("TpcPoint.fX");
	TLeaf * fY = treeEve->FindLeaf("TpcPoint.fY");
	TLeaf * fZ = treeEve->FindLeaf("TpcPoint.fZ");
	TLeaf * fTr = treeEve->FindLeaf("TpcPoint.fTrackID");
	int nPointsCurEve = fX->GetLen();

	int nPointsEve = 0;
	int nPointsDst = 0;
	int nTracksCurDst = fKalmanTracks->GetEntriesFast();
	
	std::vector<double> sqDevs;
	std::vector<double> sdDevsHits;
	for (int tr = 0; tr < nTracksCurDst; ++tr)
	{
		std::vector<double> dev;
		std::vector<double> devHits;
		MpdTrack * fGlobTrack = (MpdTrack *)fGlobTracks->UncheckedAt(tr);
		MpdHelix fTrackHelix = fGlobTrack->GetHelix();
		
		MpdTpcKalmanTrack * fTpcTrack = (MpdTpcKalmanTrack *)fKalmanTracks->UncheckedAt(tr);
		Int_t fMCTrId = fTpcTrack->GetTrackID();
		
		Int_t fNTrHits = fTpcTrack->GetNofTrHits();
		if (fNTrHits > 25)
		{
			TClonesArray * fTrHits = fTpcTrack->GetTrHits();
			for (Int_t h = 0; h < fNTrHits; ++h)
			{
				MpdKalmanHit * khit = (MpdKalmanHit*)fTrHits->UncheckedAt(h);
				Int_t fRecPtId = khit->GetIndex();
				MpdTpcHit * fTpcHit = (MpdTpcHit *)fTpcRecPts->UncheckedAt(fRecPtId);
				TVector3 hitPos;
				fTpcHit->Position(hitPos);
				double dh = fTrackHelix.distance(hitPos);
				devHits.push_back(dh);
			}

			for (int j = 0; j < nPointsCurEve; ++j)
			{
				Int_t tr = fTr->GetValue(j);
				if(tr == fMCTrId)
				{
					double x = fX->GetValue(j);
					double y = fY->GetValue(j);
					double z = fZ->GetValue(j);

					double d = fTrackHelix.distance({x, y, z});
					//cout << "distance " << d << endl;
					dev.push_back(d);
				}
			}

			double curDev = SqDev(dev);
			double curDevHits = SqDev(devHits);
			//cout << "TrNo = " << tr << setprecision(numeric_limits<double>::max_digits10) << "  SqDevMC = " << curDev << "  SqDevHits = " << curDevHits << endl;
			hDev->Fill(curDev);
			hDevHit->Fill(curDevHits);
			sqDevs.push_back(curDev);
			sdDevsHits.push_back(curDevHits);
		}
		
		//if (fNTrHits > 25 && curDev > sqDevMax)
		{
			Int_t fNTrHits = fTpcTrack->GetNofTrHits();
			//cout << "TrNo = " << tr << "  HitsNo = " << fNTrHits << endl;
			//if (fNTrHits > 25)
			{
				TClonesArray * fTrHits = fTpcTrack->GetTrHits();
				for (Int_t h = 0; h < fNTrHits; ++h)
				{
					MpdKalmanHit * khit = (MpdKalmanHit*)fTrHits->UncheckedAt(h);
					Int_t fRecPtId = khit->GetIndex();
					MpdTpcHit * fTpcHit = (MpdTpcHit *)fTpcRecPts->UncheckedAt(fRecPtId);
					TVector3 hitPos;
					fTpcHit->Position(hitPos);
					gr->SetPoint(nPointsDst + h, hitPos.X(), hitPos.Y(), hitPos.Z());
					grx->SetPoint(nPointsDst + h, hitPos.Z(), hitPos.Y());
					gry->SetPoint(nPointsDst + h, hitPos.Z(), hitPos.X());
					grz->SetPoint(nPointsDst + h, hitPos.X(), hitPos.Y());
					grR->SetPoint(nPointsDst + h, hitPos.Z(), sqrt(pow(hitPos.X(), 2) + pow(hitPos.Y(), 2)));
				}
				nPointsDst += fNTrHits;

				int nPointsCurEveGr = 0;
				for (int j = 0; j < nPointsCurEve; ++j)
				{
					Int_t tr = fTr->GetValue(j);
					if(tr == fMCTrId)
					{
						double x = fX->GetValue(j);
						double y = fY->GetValue(j);
						double z = fZ->GetValue(j);
						grMC->SetPoint(nPointsEve + nPointsCurEveGr, x, y, z);
						grxMC->SetPoint(nPointsEve + nPointsCurEveGr, z, y);
						gryMC->SetPoint(nPointsEve + nPointsCurEveGr, z, x);
						grzMC->SetPoint(nPointsEve + nPointsCurEveGr, x, y);
						grRMC->SetPoint(nPointsEve + nPointsCurEveGr, z, sqrt(pow(x, 2) + pow(y, 2)));
						nPointsCurEveGr++;
					}
				}
				nPointsEve += nPointsCurEveGr;
			}
		}
	}
	
	int noZeroDevs = 0;
	for (double d : sqDevs)
		if (d > sqDevMax) ++noZeroDevs;
	
	cout << noZeroDevs / (double)sqDevs.size() * 100 << "%" << " reconstructed tracks with SqDeviation more than " << sqDevMax << " cm" << endl;
	
	C->cd(1);
	TMultiGraph * mgx = new TMultiGraph();
	mgx->Add(grx, "P");
	mgx->Add(grxMC, "P");
	mgx->GetXaxis()->SetTitle("Z");
	mgx->GetYaxis()->SetTitle("Y");
	mgx->Draw("A");
	
	C->cd(2);
	TMultiGraph * mgy = new TMultiGraph();
	mgy->Add(gry, "P");
	mgy->Add(gryMC, "P");
	mgy->GetXaxis()->SetTitle("Z");
	mgy->GetYaxis()->SetTitle("X");
	mgy->Draw("A");
	
	C->cd(3);
	TMultiGraph * mgz = new TMultiGraph();
	mgz->Add(grz, "P");
	mgz->Add(grzMC, "P");
	mgz->GetXaxis()->SetTitle("X");
	mgz->GetYaxis()->SetTitle("Y");
	mgz->Draw("A");
	
	C->cd(4);
	//gr->Draw("P");
	//grMC->Draw("SAME P");
	TMultiGraph * mgR = new TMultiGraph();
	mgR->Add(grRMC, "P");
	mgR->Add(grR, "P");
	mgR->GetXaxis()->SetTitle("Z");
	mgR->GetYaxis()->SetTitle("R");
	mgR->Draw("A");
	
	gStyle->SetOptTitle(kTRUE);
	
	CDev->cd();
	hDev->Draw();
	hDevHit->Draw("SAME");
}
