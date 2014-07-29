/* Macro reads DST file produced by macro reco.C */

void readDST()
{
 TStopwatch timer;
 timer.Start();
 
  /* Load basic libraries */
  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
  //  mpdloadlibs(kTRUE,kFALSE); // only reco libs
  mpdloadlibs(kTRUE,kTRUE); // all libs
            
  TFile fileDST("mpddst.root");
    
  TChain *dstTree = new TChain("cbmsim");
  dstTree->Add("mpddst.root");

  // Activate branches
  MpdEvent *event;
  dstTree->SetBranchAddress("MPDEvent.", &event);
  TClonesArray *fMCTracks;
  dstTree->SetBranchAddress("MCTrack", &fMCTracks);

  Int_t events = dstTree->GetEntries();
  cout << " Number of events in DST file = " << events << endl;

  MpdTrack *pDSTtrack;    TClonesArray *mpdTracks;
  for (Int_t i = 0; i < events; i++){
      dstTree->GetEntry(i);
      //event->Get....

      mpdTracks = event->GetGlobalTracks();
      Int_t fNtracks = mpdTracks->GetEntriesFast();

      cout << " Number of tracks = " << fNtracks << endl;
      for (Int_t DSTtrackIndex = 0; DSTtrackIndex < fNtracks; DSTtrackIndex++){
          pDSTtrack = (MpdTrack*) mpdTracks->UncheckedAt(DSTtrackIndex);
          //pDSTtrack->Get...
      
          //track->Dump();
          /*cout << "Track id = " << pDSTtrack->GetID()
           << "  Pt = "    << pDSTtrack->GetPt()
           << "  Theta = " << pDSTtrack->GetTheta()
           << "  Phi = "   << pDSTtrack->GetPhi()
           << "  Px = "  << pDSTtrack->GetPx()
           << "  Py = "  << pDSTtrack->GetPy()
           << "  Pz = "  << pDSTtrack->GetPz()
           << "  Eta = "  << pDSTtrack->GetEta()
           << endl;
          */
          /* See mpddata/MpdTrack.h for more methods */
      
      } // track loop
  } // event loop
     
  timer.Print();

  cout << " Test passed" << endl;
  cout << " All ok " << endl;
  exit(0);
}
