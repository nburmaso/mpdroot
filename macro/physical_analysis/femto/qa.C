/*
 * qa.C
 *
 *  Created on: 27 lis 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 *
 *
 *		example of QA task from NicaFemto, this macro use mudst files
 */


NicaEvent* e;  // needed to use namespaces

NicaQAPlot GetTrackPlots() {
  NicaQAPlot qa_track(ENicaCutUpdate::kTrackUpdate);
  const Int_t monitor_sim_data  = NicaDataFormatFieldID::NicaFieldIDImStep;
  const Int_t monitor_reco_data = NicaDataFormatFieldID::NicaFieldIDReStep;
  Int_t histo_id;
  histo_id = qa_track.AddTH2("tpc",
                             NicaDataFormatFieldID::ETrackFieldID::kP + monitor_reco_data,
                             NicaDataFormatFieldID::EExpTrackFieldID::kTpcDedx + monitor_reco_data);
  qa_track.SetAxis2D(histo_id, 200, 0, 4, 200, 0, 1E+5);

  histo_id = qa_track.AddTH2("spectra",
                             NicaDataFormatFieldID::ETrackFieldID::kEta + monitor_reco_data,
                             NicaDataFormatFieldID::ETrackFieldID::kPt + monitor_reco_data);
  qa_track.SetAxis2D(histo_id, 200, -2, 2, 200, 0, 4);
  histo_id = qa_track.AddTH2("tof_beta",
                             monitor_reco_data + NicaDataFormatFieldID::ETrackFieldID::kP,
                             monitor_reco_data + NicaDataFormatFieldID::EExpTrackFieldID::kToFBeta);
  qa_track.SetAxis2D(histo_id, 200, 0, 4, 150, 0, 1.5);
  histo_id = qa_track.AddTH2("tof_m",
                             monitor_reco_data + NicaDataFormatFieldID::ETrackFieldID::kP,
                             monitor_reco_data + NicaDataFormatFieldID::EExpTrackFieldID::kTofM2);
  qa_track.SetAxis2D(histo_id, 200, 0, 4, 200, -0.1, 1.4);
  return qa_track;
}

NicaQAPlot GetEventPlots() {
  Int_t histo_id;
  const Int_t monitor_sim_data  = NicaDataFormatFieldID::NicaFieldIDImStep;
  const Int_t monitor_reco_data = NicaDataFormatFieldID::NicaFieldIDReStep;
  NicaQAPlot qa_event(ENicaCutUpdate::kEventUpdate);
  histo_id = qa_event.AddTH2("impact vs multiplicity",
                             NicaDataFormatFieldID::EMcEventFieldID::kB + monitor_sim_data,
                             NicaDataFormatFieldID::EEventFieldID::kTracksNo + monitor_sim_data);
  qa_event.SetAxis2D(histo_id, 200, 0, 20, 200, 0, 2000);
  histo_id = qa_event.AddTH2("reco vs sim track No",
                             NicaDataFormatFieldID::EEventFieldID::kTracksNo + monitor_reco_data,
                             NicaDataFormatFieldID::EEventFieldID::kTracksNo + monitor_sim_data);
  qa_event.SetAxis2D(histo_id, 200, 0, 2000, 200, 0, 2000);
  return qa_event;
}
void qa2() {

  TString dataList = "/eos/nica/mpd/users/wielanek/data/tpc_sigma/list.txt";

  FairRunAna* run = new FairRunAna();
  // set path to your files
  NicaMiniDstSource* source =
    new NicaMiniDstSource("/media/daniel/WD/mpdminidst/urqmd-BiBi-09.0GeV-0-14fm-eos0-250-0-0.reco.MiniDst.root");
  source->AddFile("/media/daniel/WD/mpdminidst/urqmd-BiBi-09.0GeV-0-14fm-eos0-250-0-1.reco.MiniDst.root");

  run->SetSource(source);
  run->SetOutputFile("qa.root");

  NicaQATrackTask* ana = new NicaQATrackTask();
  ana->SetFormat(new NicaMpdMiniDstFullEvent());

  /**
   * adding 2 event cuts with different collection iD - therefore we have two group of events
   * this works like in any analysis task from NicaFemto
   */
  NicaEventImpactParameterCut b;
  b.SetMinMax(0, 4.5);
  ana->AddCut(b, "im{0}");
  b.SetMinMax(4.5, 9);
  ana->AddCut(b, "im{1}");

  /**
   *  adding 6 groups of track cuts , therefore we have event groups * track groups = 12 groups of data
   */
  NicaTrackBasicMCCut mc;
  mc.SetEtaCut(-1, 1);
  mc.SetPdgCut(211);
  mc.SetPtCut(0.1, 1);
  ana->AddCut(mc, "im{0}");
  mc.SetPdgCut(321);
  ana->AddCut(mc, "im{1}");
  mc.SetPdgCut(2212);
  ana->AddCut(mc, "im{2}");


  mc.SetPdgCut(211);
  ana->AddCut(mc, "im{3}");
  mc.SetPdgCut(321);
  ana->AddCut(mc, "im{4}");
  mc.SetPdgCut(2212);
  ana->AddCut(mc, "im{5}");

  /**
   * primary cuts are added only to track groups 3,4,5 to select primary particles
   */
  NicaTrackOnlyPrimariesCut prim;
  ana->AddCut(prim, "im{3}");
  ana->AddCut(prim, "im{4}");
  ana->AddCut(prim, "im{5}");

  ana->SetQAPlot(GetTrackPlots());
  ana->SetQAPlot(GetEventPlots());

  /**
   * set names for event and cut groups
   */
  ana->SetEventCollectionNames({"central", "semicentral"});
  ana->SetTrackCollectionNames({"pion", "kaon", "proton", "prim_pion", "prim_kaon", "prim_proton"});


  run->AddTask(ana);
  run->Init();
  run->Run(50000);
}
