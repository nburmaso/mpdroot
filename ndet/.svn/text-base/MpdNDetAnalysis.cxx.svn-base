/*
    First simple version of Nuetron Detector (NDet) analysis
    May 2009

*/

#include "MpdNDetAnalysis.h"

#include "MpdNDetPoint.h"
#include "MpdNDetPointLite.h"
//... #include "MpdNDetHitFastMC.h"
//... #include "MpdNDetRecParticle.h"

#include "FairRootManager.h"
#include "FairMCTrack.h"

#include "TParticle.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TVector3.h"
#include "TLorentzVector.h"

#include <iostream>
#include <stdlib.h>

using namespace std;

//------------------------------------------------------------------------------
MpdNDetAnalysis::MpdNDetAnalysis() :FairTask()
{
}

//------------------------------------------------------------------------------
MpdNDetAnalysis::MpdNDetAnalysis(const char *name, const char *title)
  :FairTask(name)
{
  SetTitle(title);
  fEvent = 0;
  fDebug = "";
  Double_t pi = TMath::Pi();


  cout<<"Start ANALYSIS-------------------->"<<endl;

  fhNevents = new TH1F("hNevents" ,"Number of events",1, 0.5,1.5);
  // Neutron tracks histograms
  //Primary
  fhNeut0Pabs = new TH1F("hNeut0Pabs" ,"Momentum of prim. neutrons",100, 0.,10.);
  fhNeut0Pt   = new TH1F("hNeut0Pt"   ,"p_{T} of prim. neutrons"   ,100, 0.,2.);
  fhNeut0Eta  = new TH1F("hNeut0Eta"  ,"#eta of prim. neutrons"    , 50,-2., 2.);
  fhNeut0Theta= new TH1F("hNeut0Theta","#theta of prim. neutrons"  ,100, 0., pi);
  fhNeut0Phi  = new TH1F("hNeut0Phi"  ,"#phi of prim. neutrons"    ,100,-pi, pi);
  fhNeut0PtEta= new TH2F("hNeut0PtEta","p_{T},#eta of prim. neutrons",100, 0.,2., 50,-2., 2.);
  fhNeut0Rad  = new TH1F("hNeut0Rad","r=#sqrt{x^{2}+y^{2}}",200,0.,200.);
  fhNeut0RadZ  = new TH2F("hNeut0RadZ","n_{prim} r=#sqrt{x^{2}+y^{2}} vs Z",200,-500.,500.,200,0.,200.);
  //Secondary
  fhNeut1Pabs = new TH1F("hNeut1Pabs" ,"Momentum of sec. neutrons",100, 0.,10.);
  fhNeut1Pt   = new TH1F("hNeut1Pt"   ,"p_{T} of sec. neutrons"   ,100, 0.,2.);
  fhNeut1Eta  = new TH1F("hNeut1Eta"  ,"#eta of sec. neutrons"    , 50,-2., 2.);
  fhNeut1Theta= new TH1F("hNeut1Theta","#theta of sec. neutrons"  ,100, 0., pi);
  fhNeut1Phi  = new TH1F("hNeut1Phi"  ,"#phi of sec. neutrons"    ,100,-pi, pi);
  fhNeut1PtEta= new TH2F("hNeut1PtEta","p_{T},#eta of sec. neutrons",100, 0.,2., 50,-2., 2.);
  fhNeut1Rad  = new TH1F("hNeut1Rad","r=#sqrt{x^{2}+y^{2}}",200,0.,200.);
  fhNeut1RadZ  = new TH2F("hNeut1RadZ","n_{seco} r=#sqrt{x^{2}+y^{2}} vs Z",200,-500.,500.,200,0.,200.);

  fhNeut1RadE  = new TH2F("hNeut1RadE","n_{seco} r=#sqrt{x^{2}+y^{2}} vs Ekin",200,0.,2.,200,0.,200.);
  // Not Neutoron tracks histograms
  //Protons prim
  fhProt0Pabs = new TH1F("hProt0Pabs" ,"Momentum of prim. protons",100, 0.,10.);
  fhProt0Pt   = new TH1F("hProt0Pt"   ,"p_{T} of prim. protons"   ,100, 0.,2.);
  fhProt0Eta  = new TH1F("hProt0Eta"  ,"#eta of prim. protons"    , 50,-2., 2.);
  fhProt0Theta= new TH1F("hProt0Theta","#theta of prim. protons"  ,100, 0., pi);
  fhProt0Phi  = new TH1F("hProt0Phi"  ,"#phi of prim. protons"    ,100,-pi, pi);
  fhProt0PtEta= new TH2F("hProt0PtEta","p_{T},#eta of prim. protons",100, 0.,2., 50,-2., 2.);
  fhProt0Rad  = new TH1F("hProt0Rad","r=#sqrt{x^{2}+y^{2}}",200,0.,200.);

  //Protons sec
  fhProt1Pabs = new TH1F("hProt1Pabs" ,"Momentum of sec. protons",100, 0.,10.);
  fhProt1Pt   = new TH1F("hProt1Pt"   ,"p_{T} of sec. protons"   ,100, 0.,2.);
  fhProt1Eta  = new TH1F("hProt1Eta"  ,"#eta of sec. protons"    , 50,-2., 2.);
  fhProt1Theta= new TH1F("hProt1Theta","#theta of sec. protons"  ,100, 0., pi);
  fhProt1Phi  = new TH1F("hProt1Phi"  ,"#phi of sec. protons"    ,100,-pi, pi);
  fhProt1PtEta= new TH2F("hProt1PtEta","p_{T},#eta of sec. protons",100, 0.,2., 50,-2., 2.);
  fhProt1Rad  = new TH1F("hProt1Rad","r=#sqrt{x^{2}+y^{2}}",200,0.,200.);


  // MC points histograms

  fhMcpPabs = new TH1F("hMcpPabs" ,"Momentum of detected particles",120, 0.,30.);
  fhMcpPt   = new TH1F("hMcpPt"   ,"p_{T} of detected particles"   ,100, 0.,10.);
  fhMcpEta  = new TH1F("hMcpEta"  ,"#eta of detected particles"    , 80,-2., 6.);
  fhMcpTheta= new TH1F("hMcpTheta","#theta of detected particles"  ,100, 0., pi);
  fhMcpPhi  = new TH1F("hMcpPhi"  ,"#phi of detected particles"    ,100,-pi, pi);
  fhMcpPtEta= new TH2F("hMcpPtEta","p_{T},#eta of detected particles",
		       100, 0.,10., 80,-2., 6.);
  fhMcpXY   = new TH2F("hMcpXY","(X,Y) coordinates of MC points",
		       300,-600,600,300,-600,600);

  //MC pionts PDG code 
  fhMcpPdgNdet = new TH1D("hMcpPdgNdet" ,"PDD code on the enter of Ndet",10001, -5000.,5000.);

  // Hits histograms

  fhHitXY = new TH2F("hHitXY","NDET hit (x,y)",100,-600.,600.,100,-500.,500.);
  fhHitE  = new TH1F("hHitE" ,"NDET hit energy",120,0.,30.);

  // Reconstructed particles histograms

  fhRecPabs = new TH1F("hRecPabs","Reconstructed |p|"  ,120, 0.,30.);
  fhRecPt   = new TH1F("hRecPt"  ,"Reconstructed p_{T}",100, 0.,10.);
  fhRecYrap = new TH1F("hRecYrap","Reconstructed y"    , 80,-2., 6.);
  fhRecPhi  = new TH1F("hRecPhi" ,"Reconstructed #phi" ,100,-pi, pi);

  // Acceptance

  fhPtYrap = new TH2F("PtYrap","pt vs y of primary particles", 100,0.,6.,100,0.,5.);
  fhPtYrapSignal = new TH2F("PtYrapSignal","pt vs y of signal particles", 100,0.,6.,100,0.,5.);
  fhPtYrapAccept = new TH2F("PtYrapAccept","NDET p_{T}-y acceptance",100,0.,6.,100,0.,5.);

}

//------------------------------------------------------------------------------
void MpdNDetAnalysis::WriteOutput()
{
  cout<<"Write histograms to MY FILE neutron.root"<<endl;
  ftest = new TFile("neutron.root","recreate");

  if(ftest) {
  fhNevents->Write();

  fhMcpPdgNdet->Write();

  fhNeut0Pabs ->Write();
  fhNeut0Pt   ->Write();
  fhNeut0Eta  ->Write();
  fhNeut0Theta->Write();
  fhNeut0Phi  ->Write();
  fhNeut0PtEta->Write();
  fhNeut0Rad->Write();
  fhNeut0RadZ->Write();

  fhNeut1Pabs ->Write();
  fhNeut1Pt   ->Write();
  fhNeut1Eta  ->Write();
  fhNeut1Theta->Write();
  fhNeut1Phi  ->Write();
  fhNeut1PtEta->Write();
  fhNeut1Rad->Write();
  fhNeut1RadZ->Write();
fhNeut1RadE->Write();

  fhProt0Pabs ->Write();
  fhProt0Pt   ->Write();
  fhProt0Eta  ->Write();
  fhProt0Theta->Write();
  fhProt0Phi  ->Write();
  fhProt0PtEta->Write();
  fhProt0Rad->Write();

  fhProt1Pabs ->Write();
  fhProt1Pt   ->Write();
  fhProt1Eta  ->Write();
  fhProt1Theta->Write();
  fhProt1Phi  ->Write();
  fhProt1PtEta->Write();
  fhProt1Rad->Write();
  
  ftest->Close();
  //cout<<"finish writting..."<<endl;
  }
}
//------------------------------------------------------------------------------
MpdNDetAnalysis::~MpdNDetAnalysis()
{

  cout<<"Destruct the run<----------------"<<endl;


 }

//------------------------------------------------------------------------------
InitStatus MpdNDetAnalysis::Init()
{
  // Activate branches with NDET objects

  FairRootManager *manager= FairRootManager::Instance();
  
  // all tracks
  //  fMCtrack        = (TClonesArray*)manager->ActivateBranch("MCTrack");
  fMCtrack        = (TClonesArray*)manager->GetObject("MCTrack");    // EL

  //NDET MC points inside NDET
  //  fNDetPointLite  = (TClonesArray*)manager->ActivateBranch("NDetPointLite");
  fNDetPointLite  = (TClonesArray*)manager->GetObject("NDetPointLite");   // EL

  //NDET MC points on entrance to NDET
  //  fNDetPoint  = (TClonesArray*)manager->ActivateBranch("NDetPoint");
  fNDetPoint  = (TClonesArray*)manager->GetObject("NDetPoint");   // EL

  //NDET hits
  /* 
     fListNDEThits     = (TClonesArray*)manager->ActivateBranch("EcalHitFastMC");
  */

  //NDET reconstructed points
  /*
    fListNDETrp       = (TClonesArray*)manager->ActivateBranch("EcalRecParticle");
  */

  return kSUCCESS;
}

//------------------------------------------------------------------------------
void MpdNDetAnalysis::Exec(Option_t* option)
{
  // make NDET analysis

  printf("NDET analysis: event %d\n",fEvent);
  
  OneEventAnalysis();
  
  //cout<<"option = "<<*option<<endl;

  //if(*option==-1)
  Finish();
}

//------------------------------------------------------------------------------
void MpdNDetAnalysis::OneEventAnalysis()
{


  //ReadMCTrack();
  Acceptance();

  /*  AnalyzePrimary(); */

  /* AnalyzeMCPointsEdep(); */
  /* AnalyzeMCPointsWall(); */
  /* AnalyzeHits(); */
  /* AnalyzeRecParticles(); */

  fEvent++;fhNevents->Fill(1.);

}

//------------------------------------------------------------------------------
void MpdNDetAnalysis::ReadMCTrack()
{
  //Test of MC tracks and points ...
  FairMCTrack *tr;
  FairMCTrack *mtr;
  MpdNDetPoint *wp;
  MpdNDetPointLite *lp;
  TVector3 mom3;
  Int_t n=fMCtrack->GetEntries();
  cout<<"Number of MCTracks:  "<<n<<endl;
  Int_t lite = fNDetPointLite->GetEntries();
  cout<<"Number of NDETPointLite (inside volume):  "<<lite<<endl;
  Int_t point = fNDetPoint->GetEntries();
  cout<<"Number of NDETPoint (on front wall):  "<<point<<endl;
  Double_t Mn=0.939565560;
  //loop over front hits(points)
  for(Int_t i=0;i<point;i++) {
    wp = (MpdNDetPoint*)fNDetPoint->At(i);
    if(wp->TrackID()>-1&&wp->TrackID()<n+1)
      tr = (FairMCTrack*)fMCtrack->At(wp->TrackID());
    else {cout<<"Wrong TrackID at MpdNDetPoint"<<endl; exit(-1);}
    Int_t MoPdg = -111111111;
    //    if(tr->GetMotherID()>-1) {
    if(tr->GetMotherId()>-1) {
      //     mtr=(FairMCTrack*)fMCtrack->At(tr->GetMotherID());
       mtr=(FairMCTrack*)fMCtrack->At(tr->GetMotherId());
     MoPdg=mtr->GetPdgCode();
    }
    if(tr->GetPdgCode() == 2212)//neutron
    cout<<"TID "<<wp->TrackID()
	<<"\t C: "<<wp->X()<<" "<<wp->Y()<<" "<<wp->Z()
	<<"\t P: "<<wp->Px()<<" "<<wp->Py()<<" "<<wp->Pz()
	<<"\t Ekin: "<<wp->ELoss()
      //<<" E "<<TMath::Sqrt(wp->Px()*wp->Px()+wp->Py()*wp->Py()+wp->Pz()*wp->Pz()+Mn*Mn)-Mn
	<<"\t T: "<<wp->Time()
	<<"\t L: "<<wp->Length()
	<<"\t Di: "<<wp->DetectorID()
	<<"\t Ekin: "<<wp->ELoss()
	<<"\t PDG: "<<tr->GetPdgCode()
	<<" MoPGD "<<MoPdg
	<<endl;
  }
  cout<<"===================================="<<endl;
  cout<<"===================================="<<endl;
  cout<<"===================================="<<endl;
  cout<<"===================================="<<endl;
  //loop over hits(points) inside ndet
   for(Int_t i=0;i<lite;i++) {
     lp = (MpdNDetPointLite*)fNDetPointLite->At(i);
    if(lp->GetTrackID()>-1&&lp->GetTrackID()<n+1)
      tr = (FairMCTrack*)fMCtrack->At(lp->GetTrackID());
    else {cout<<"Wrong TrackID at MpdNDetPointLite"<<endl; exit(-1);}
    Int_t MoPdg = -111111111;
    //    if(tr->GetMotherID()>-1) {
    if(tr->GetMotherId()>-1) {
      //      mtr=(FairMCTrack*)fMCtrack->At(tr->GetMotherID());
      mtr=(FairMCTrack*)fMCtrack->At(tr->GetMotherId());
      MoPdg=mtr->GetPdgCode();
    }
    if(tr->GetPdgCode() == 12212)//neutron
    cout<<"TID "<<lp->GetTrackID()
	<<"\t T "<<lp->GetTime()
	<<"\t Eloss "<<lp->GetEnergyLoss()
	<<"\t PDG: "<<tr->GetPdgCode()
	<<"\t MoPGD "<<MoPdg
	<<"\t Det "<<lp->GetDetectorID()
	<<endl;
   }

}
//------------------------------------------------------------------------------
void MpdNDetAnalysis::Acceptance() 
{
  FairMCTrack *tr;//track
  FairMCTrack *mtr;//mother track
  MpdNDetPoint *wp;//MC pionts on the enter of ndet
  MpdNDetPointLite *lp;
  TLorentzVector momentum;
  TLorentzVector coordinate;
  TVector3 mvtx; 
  TVector3 vtx; 
  TVector3 tmom;
  Int_t n=fMCtrack->GetEntries();
  cout<<"Number of MCTracks:  "<<n<<endl;
  Int_t lite = fNDetPointLite->GetEntries();
  cout<<"Number of NDETPointLite (inside volume):  "<<lite<<endl;
  Int_t point = fNDetPoint->GetEntries();
  cout<<"Number of NDETPoint (on front wall):  "<<point<<endl;
  Double_t Mn=0.939565560;
  //loop over front hits(points)
  for(Int_t i=0;i<point;i++) {
    wp = (MpdNDetPoint*)fNDetPoint->At(i);
    if(wp->TrackID()>-1&&wp->TrackID()<n+1)
      tr = (FairMCTrack*)fMCtrack->At(wp->TrackID());
    else {cout<<"Wrong TrackID at MpdNDetPoint"<<endl; exit(-1);}
    //    vtx=tr->GetStartVertex();
    tr->GetStartVertex(vtx);
    //    tmom=tr->GetMomentum();
    tr->GetMomentum(tmom);
    Int_t MoPdg = -111111111;
    //    if(tr->GetMotherID()>-1) {
    if(tr->GetMotherId()>-1) {
      //      mtr=(FairMCTrack*)fMCtrack->At(tr->GetMotherID());
      mtr=(FairMCTrack*)fMCtrack->At(tr->GetMotherId());
      MoPdg=mtr->GetPdgCode();
      //      mvtx=mtr->GetStartVertex();
      mtr->GetStartVertex(mvtx);
      //if(tr->GetPdgCode() == 2112) cout<<TMath::Sqrt(mvtx.X()*mvtx.X()+mvtx.Y()*mvtx.Y())<<endl;
    }
    momentum.SetPxPyPzE(wp->Px(),wp->Py(),wp->Pz(),wp->ELoss());
    coordinate.SetXYZT(wp->X(),wp->Y(),wp->Z(),wp->Time());
    fhMcpPdgNdet->Fill(tr->GetPdgCode());
    Double_t rad=TMath::Sqrt(wp->X()*wp->X()+wp->Y()*wp->Y());
    Double_t mrad=TMath::Sqrt(vtx.X()*vtx.X()+vtx.Y()*vtx.Y());
    //TMath::Sqrt(mvtx.X()*mvtx.X()+mvtx.Y()*mvtx.Y());
    //Coming in particles
    if( 
       momentum.E() > Mn+0.001 //1 MeV 
       //&& momentum.Px()*coordinate.X()>0  
       //&& momentum.Py()*coordinate.Y()>0
       && tmom.Px()*vtx.X()>=0  
       && tmom.Py()*vtx.Y()>=0
       && rad<155.01         
       && mrad<155.              //cut secondary neutrons which creates in ndet  
	) {
    //neutrons
      if(tr->GetPdgCode() == 2112) {
	//	if(tr->GetMotherID()==-1) {//primary
	if(tr->GetMotherId()==-1) {//primary
	  fhNeut0Pabs ->Fill(momentum.P());
	  fhNeut0Pt   ->Fill(momentum.Pt());
	  fhNeut0Eta  ->Fill(momentum.Eta());
	  fhNeut0Theta->Fill(momentum.Theta());
	  fhNeut0Phi  ->Fill(momentum.Phi());
	  fhNeut0PtEta->Fill(momentum.Pt(),momentum.Eta());
	  fhNeut0Rad->Fill(rad);
	  fhNeut0RadZ->Fill(vtx.Z(),mrad);
	} else {
	  //cout<<momentum.Px()<<" "<<tmom.Px()<<endl;
	  fhNeut1Pabs ->Fill(momentum.P());
	  fhNeut1Pt   ->Fill(momentum.Pt());
	  fhNeut1Eta  ->Fill(momentum.Eta());
	  fhNeut1Theta->Fill(momentum.Theta());
	  fhNeut1Phi  ->Fill(momentum.Phi());
	  fhNeut1PtEta->Fill(momentum.Pt(),momentum.Eta());
	  fhNeut1Rad->Fill(mrad);
	  fhNeut1RadZ->Fill(vtx.Z(),mrad);
	  //fhNeut1RadE->Fill(momentum.E()-Mn,mrad);
	  fhNeut1RadE->Fill(momentum.P(),mrad);
	}
      }
      if(tr->GetPdgCode() == 2212) {//protons
	//	if(tr->GetMotherID()==-1) {//primary
	if(tr->GetMotherId()==-1) {//primary
	  fhProt0Pabs ->Fill(momentum.P());
	  fhProt0Pt   ->Fill(momentum.Pt());
	  fhProt0Eta  ->Fill(momentum.Eta());
	  fhProt0Theta->Fill(momentum.Theta());
	  fhProt0Phi  ->Fill(momentum.Phi());
	  fhProt0PtEta->Fill(momentum.Pt(),momentum.Eta());
	  fhProt0Rad->Fill(rad);
	} else {
	  fhProt1Pabs ->Fill(momentum.P());
	  fhProt1Pt   ->Fill(momentum.Pt());
	  fhProt1Eta  ->Fill(momentum.Eta());
	  fhProt1Theta->Fill(momentum.Theta());
	  fhProt1Phi  ->Fill(momentum.Phi());
	  fhProt1PtEta->Fill(momentum.Pt(),momentum.Eta());
	  fhProt1Rad->Fill(mrad);
	}
      }
    }//coming in ...
    if(tr->GetPdgCode() == 12212)//neutron
    cout<<"TID "<<wp->TrackID()
	<<"\t C: "<<wp->X()<<" "<<wp->Y()<<" "<<wp->Z()
	<<"\t P: "<<wp->Px()<<" "<<wp->Py()<<" "<<wp->Pz()
	<<"\t Ekin: "<<wp->ELoss()
      //<<" E "<<TMath::Sqrt(wp->Px()*wp->Px()+wp->Py()*wp->Py()+wp->Pz()*wp->Pz()+Mn*Mn)-Mn
	<<"\t T: "<<wp->Time()
	<<"\t L: "<<wp->Length()
	<<"\t Di: "<<wp->DetectorID()
	<<"\t Etot: "<<wp->ELoss()
	<<"\t PDG: "<<tr->GetPdgCode()
	<<" MoPGD "<<MoPdg
	<<endl;
  }
  
}
//------------------------------------------------------------------------------
void MpdNDetAnalysis::AnalyzePrimary()
{
  // Neutary track analysis

  FairMCTrack *primary;
  fNprimary = 0;
  TVector3 mom3;
  TLorentzVector momentum;
  Float_t energyTotal = 0;
  cout<<"Number of MCTracks:  "<<fMCtrack->GetEntries()<<endl;

  for (Int_t iNeutary=0; iNeutary<fMCtrack->GetEntries(); iNeutary++) {
    primary = (FairMCTrack*)fMCtrack->At(iNeutary);
    //if (primary->GetMotherID() != -1) continue;
    fNprimary++;
    //    mom3=primary->GetMomentum();
    primary->GetMomentum(mom3);
    /*    
    cout<<"# "<<iNeutary
	<<" ID: "<<primary->GetPdgCode()
	<<"MID: "<<primary->GetMotherID()
	<<" Px: "<<mom3.Px()
	<<endl;
    */

    /*
    if (strstr(fDebug,"prim"))
      printf("track %d: id=%d, p=(%f,%f,%f,%f) GeV/c, pt,y,theta=%f,%f,%f\n",
	     iNeutary,
	     primary->GetPdgCode(),
	     momentum.Px(),
	     momentum.Py(),
	     momentum.Pz(),
	     momentum.E(),
	     momentum.Pt(),
	     momentum.Eta(),
	     momentum.Theta());
    fhNeutPabs ->Fill(momentum.P());
    fhNeutPt   ->Fill(momentum.Pt());
    fhNeutEta  ->Fill(momentum.Eta());
    fhNeutTheta->Fill(momentum.Theta());
    fhNeutPhi  ->Fill(momentum.Phi());
    fhNeutPtEta->Fill(momentum.Pt(),momentum.Eta());
    energyTotal+=momentum.P();
    */
  }
  printf("Total energy of primaries: %F GeV\n",energyTotal);

  if (strstr(fDebug,"prim"))
    printf("\tNumber of primaries: %d\n",fNprimary);

  
}

//------------------------------------------------------------------------------
void MpdNDetAnalysis::AnalyzeMCPointsEdep()
{
  // Monte-Carlo points inside NDET

  if (fNDetPointLite == 0) return;
  if (strstr(fDebug,"mcp"))
      printf("\tNumber of NDET MC points in NDET: %d\n",fNDetPointLite->GetEntries());

  MpdNDetPointLite* mcPoint=NULL;
  TVector3 xyz;
  TLorentzVector momentum;

  Float_t energyTotal = 0;
  for (Int_t iPoint=0; iPoint<fNDetPointLite->GetEntries(); iPoint++) {
    mcPoint = (MpdNDetPointLite*)fNDetPointLite->At(iPoint); // MC point
    energyTotal+=mcPoint->GetEnergyLoss();
  }
  printf("Total energy in MC points: %F GeV\n",energyTotal);
}

//------------------------------------------------------------------------------
void MpdNDetAnalysis::AnalyzeMCPointsWall()
{
  // Monte-Carlo points on entrance to NDET

  if (fNDetPoint == 0) return;
  if (strstr(fDebug,"mcp"))
      printf("\tNumber of NDET MC points on NDET: %d\n",fNDetPoint->GetEntries());

  MpdNDetPoint* mcPoint=NULL;
  TVector3 xyz;
  TVector3 mom3;
  TLorentzVector momentum;

  for (Int_t iPoint=0; iPoint<fNDetPoint->GetEntries(); iPoint++) {
    mcPoint = (MpdNDetPoint*)fNDetPoint->At(iPoint); // MC point
    FairMCTrack* track = (FairMCTrack*)fMCtrack->At(mcPoint->GetTrackID());
    if (track->GetPdgCode() != 22 ) continue; // not a photon!
    mcPoint->Position(xyz);
    fhMcpXY->Fill(xyz.X(),xyz.Y());

    //    mom3=track->GetMomentum();
    track->GetMomentum(mom3);
    if (strstr(fDebug,"mcp"))
      printf("MC point %d: id=%d, p=(%f,%f,%f,%f) GeV/c, pt,y,theta=%f,%f,%f\n",
	     iPoint,
	     track->GetPdgCode(),
	     momentum.Px(),
	     momentum.Py(),
	     momentum.Pz(),
	     momentum.Energy(),
	     momentum.Pt(),
	     momentum.Eta(),
	     momentum.Theta());
    fhMcpPabs ->Fill(momentum.P());
    fhMcpPt   ->Fill(momentum.Pt());
    fhMcpEta  ->Fill(momentum.Eta());
    fhMcpTheta->Fill(momentum.Theta());
    fhMcpPhi  ->Fill(momentum.Phi());
    fhMcpPtEta->Fill(momentum.Pt(),momentum.Eta());
  }
}

//------------------------------------------------------------------------------
//... void MpdNDetAnalysis::AnalyzeHits()
//... {
//...   // NDET hit analysis for Fast MC
//... 
//...   if (fListNDEThits == 0) return;
//...   if (strstr(fDebug,"hit"))
//...       printf("\tNumber of NDET hits: %d\n",fListNDEThits->GetEntries());
//... 
//...   MpdNDetHitFastMC* hit=NULL;
//... 
//...   for (Int_t iHit=0; iHit<fListNDEThits->GetEntries(); iHit++) {
//...     hit = (MpdNDetHitFastMC*)fListNDEThits->At(iHit);
//...     fhHitXY->Fill(hit->GetX(),hit->GetY());
//...     fhHitE ->Fill(hit->GetAmplitude());
//...   }
//... }

//------------------------------------------------------------------------------
//... void MpdNDetAnalysis::AnalyzeRecParticles()
//... {
//...   // NDET reconstructed particle analysis
//... 
//...   if (fListNDETrp   == 0) return;
//...   if (strstr(fDebug,"rp"))
//...       printf("\tNumber of NDET reconstructed particles: %d\n",fListNDETrp->GetEntries());
//... 
//...   MpdNDetRecParticle* recpart=NULL;
//... 
//...   for (Int_t iRP=0; iRP<fListNDETrp->GetEntries(); iRP++) {
//...     recpart = (MpdNDetRecParticle*)fListNDETrp->At(iRP);
//...     TLorentzVector momentum = recpart->GetMomentum();
//...     fhRecPabs->Fill(momentum.P());
//...     fhRecPt  ->Fill(momentum.Pt());
//...     fhRecYrap->Fill(momentum.Rapidity());
//...     fhRecPhi ->Fill(momentum.Phi());
//...   }
//... }

//------------------------------------------------------------------------------
void MpdNDetAnalysis::Finish()
{
  WriteOutput();
}


//------------------------------------------------------------------------------
ClassImp(MpdNDetAnalysis)
