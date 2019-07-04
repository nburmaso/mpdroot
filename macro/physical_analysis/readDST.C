/* Macro reads DST file produced by macro reco.C */

#include <TChain.h>
#include <TClonesArray.h>
#include <TFile.h>

R__ADD_INCLUDE_PATH($VMCWORKDIR)

using std::cout;
using std::endl;

using TMath::Abs;
using TMath::Pi;

/* define MPD cuts */
const float CutPtMin = 0.2; // Pt>CutPtMin
const float CutPtMax = 3.2; // Pt<CutPtMax
const float CutEta   = 1.2; // pseudo rapidity
const float CutHits  = 25;  // minimum number of hits per track
const float CutProb  = 0.8; // pid probability
/* beam */
const float CutVr    = TMath::Sqrt((3.*0.1)*(3.*0.1)+(3.*0.1)*(3.*0.1)); // sqrt{(3.\sigma_x)^2+(3.\sigma_x)^2}
const float CutVz    = 3.*24.; // 3.\sigma_z

class FairMCTrack;
MpdHelix MakeHelix(const MpdKalmanTrack *tr);

void readDST(TString inFileName="mpddst.root", float inEnergy=9.0)
{

  TChain *dst = new TChain("cbmsim");
  dst->Add(inFileName.Data());
  /* it is possible to add list of files from one file */
  //TFileCollection fc("dummy","","collection.txt");
  //dst->AddFileInfoList(fc.GetList());

  /* define output */
  TFile *outFile = new TFile("output.root", "RECREATE");

  auto hNofHits  = new TH1I("NofHits","Number of hits for all reconstructed tracks;Number of Hits;1/N_{ev}  N",60,0.5,60.5);
  auto hChi2NofH = new TH1D("Chi2NofH","#Chi^{2}/NofHits;#Chi^{2}/NofHits;1/N_{ev}  N",40,0.,4.);
  auto prEtaPtNofHits = new TProfile2D("EtaPtNofHist","distribytion of hits;#eta;p_{T}",150,-2.,2.,70.,0.,2.);


  dst->SetCacheSize(10000000);
  // disable all branches
  dst->SetBranchStatus("*", 0);
  // re-enable branches
  dst->SetBranchStatus("MPDEvent.*", 1);
  dst->SetBranchStatus("TpcKalmanTrack*", 1);
  dst->SetBranchStatus("MCEventHeader.*", 1);
  dst->SetBranchStatus("MCTrack*", 1);

  MpdEvent     *Event   =0;
  TClonesArray *KFTracks=0;

  dst->SetBranchAddress("MPDEvent.", &Event);
  dst->SetBranchAddress("TpcKalmanTrack",&KFTracks);

  MpdPid *pid = new MpdPid(4.0, 4.0, inEnergy, 1.0, "DEFAULT", "CF", "pikapr");

  /* MC information */
  // header
  MpdMCEventHeader *MCHeader=0;
  dst->SetBranchAddress("MCEventHeader.", &MCHeader);
  // track
  TClonesArray *MCTracks=0;
  dst->SetBranchAddress("MCTrack", &MCTracks);


  int nEvents = dst->GetEntries();
  cout << "Number of Events in DST file = " << nEvents << endl;

  int CountEvents=0;

  /* Loop over events */
  for (int iEv=0; iEv<nEvents; ++iEv)
  {
    if (iEv%10==0) cout << "Read Event: " << iEv << endl;

    dst->GetEntry(iEv);

    /* MC info */
    float  MCb=MCHeader->GetB();       // impact parameter
    double MCPsiRP=MCHeader->GetPhi(); // reaction plane
    /* ******* */

    // Vertex
    float Vxx=Event->GetPrimaryVerticesX();
    float Vyy=Event->GetPrimaryVerticesY();
    float Vzz=Event->GetPrimaryVerticesZ();
    // vertex in accordance with beam info
    if (Vxx*Vxx+Vyy*Vyy>CutVr*CutVr) continue; // [CutVertex]
    if (TMath::Abs(Vzz)>CutVz)       continue; // [CutVertex]
    /* primary vertex */
    TVector3 primaryVertex(Vxx,Vyy,Vzz);

    CountEvents++;  // real number of events

    TClonesArray *MpdTracks = Event->GetGlobalTracks();
    /* Loop over reco tracks in the current event */
    for (int i=0; i<MpdTracks->GetEntriesFast(); ++i)
    {
      MpdTrack   *mpdtrack = (MpdTrack*)   MpdTracks->UncheckedAt(i);  // track

      int NofHits=mpdtrack->GetNofHits();

      hNofHits->Fill(NofHits);
      hChi2NofH->Fill(mpdtrack->GetChi2()/(double) NofHits);

      FairMCTrack *mctrack = (FairMCTrack*) MCTracks->At(mpdtrack->GetID());  // MC info
      if (!mctrack) {cout << "empty mctrack" << endl;}                        // no info, ghost

      /* ***** only primary tracks ***** */
      MpdTpcKalmanTrack  *kftrack = (MpdTpcKalmanTrack*) KFTracks->UncheckedAt(i);  // KF образ
      //if (mctrack->GetMotherId()>-1) continue; // select primary tracks, naively
      MpdHelix helix = MakeHelix(kftrack);
      double pathLength = helix.pathLength(primaryVertex);
      TVector3 pca;
      pca = helix.at(pathLength);
      pca -= primaryVertex;
      double dca = pca.Mag();

      /* See mpddata/MpdTrack.h for more methods */
      double QPt=mpdtrack->GetPt();      // signe of charge
      double Qq =mpdtrack->GetCharge();  // signe of charge
      double Pt =TMath::Abs(QPt);
      double Pz =mpdtrack->GetPz();
      double PP =TMath::Sqrt(Pt*Pt+Pz*Pz);
      double Eta=mpdtrack->GetEta();
      double dEdX=mpdtrack->GetdEdXTPC();  // dummy

      prEtaPtNofHits->Fill(Eta,Pt,NofHits);

      /* CUTs */
      if (NofHits<CutHits)         continue;  // [CutHits]
      if (TMath::Abs(Eta)>CutEta)  continue;  // [CutEta]
      if (Pt<CutPtMin)             continue;  // [CutPt]
      if (dca>2.)                  continue;  // [CutDCA] 2cm

      if (mctrack) {
        TLorentzVector mom;
        mctrack->Get4Momentum(mom);
        double  PtMC=mom.Perp();             // true values
        double EtaMC=mom.PseudoRapidity();   // true values
        double   yMC=mom.Rapidity();         // true values
        int    pdgMC=mctrack->GetPdgCode();  // true values
      }

      /* PID: see mpdpid/README.md */
      double PIDproton =0.;
      if(!pid->FillProbs(mpdtrack)) {
        cout << "no pid: " << endl;
      } else {
        PIDproton =pid->GetProbPr();
      }
      if(PIDproton>CutProb) {
        // proton
      }
      // Long_t MpdPid::GetMaxProb() returns the most probable PDG-code (including charge information)

    }  // end of loop over RECO Tracks


    /* Loop over MC tracks in the current event */
    for (int i=0; i<MCTracks->GetEntries(); ++i)
    {
      FairMCTrack *mctrack = (FairMCTrack*) MCTracks->UncheckedAt(i);
      /* only primary tracks */
      if (mctrack->GetMotherId()>-1) continue; // select primary tracks
      //if (mctrack->GetNPoints(kTPC)==0) continue; // take only tracks in TPC

      TLorentzVector momMC;
      mctrack->Get4Momentum(momMC);
      double  PtMC=momMC.Perp();             // true values
      double EtaMC=momMC.PseudoRapidity();   // true values
      double   yMC=momMC.Rapidity();         // true values
      int    pdgMC=mctrack->GetPdgCode();    // true values

      /* CUT Eta */
      if (TMath::Abs(EtaMC)>CutEta)  continue;  // [CutEta]
      if (PtMC<CutPtMin)             continue;  // [CutPt]

      if (pdgMC==+2212) {
        // proton
      }
    } // end of loop over MC Tracks

  }  // end of loop over Events

  hNofHits->Scale(1./(double) CountEvents);
  hChi2NofH->Scale(1./(double) CountEvents);

  outFile->Write();  
  outFile->Close();
  exit(0);
};


MpdHelix MakeHelix(const MpdKalmanTrack *tr) 
{
  const Double_t F_CUR0 = 0.3 * 0.01 * 5 / 10; // 5kG
    Double_t r = tr->GetPosNew();
    Double_t phi = tr->GetParam(0) / r;
    Double_t x = r * TMath::Cos(phi);
    Double_t y = r * TMath::Sin(phi);
    Double_t dip = tr->GetParam(3);
    Double_t cur = F_CUR0 * TMath::Abs (tr->GetParam(4));
    TVector3 o(x, y, tr->GetParam(1));
    Int_t h = (Int_t) TMath::Sign(1.1,tr->GetParam(4));
    MpdHelix helix(cur, dip, tr->GetParam(2)-TMath::PiOver2()*h, o, h);
    return helix;
};

