#include <Rtypes.h>
#include <TChain.h>
#include <TString.h>
#include <TFile.h>

R__ADD_INCLUDE_PATH($VMCWORKDIR)
#include "macro/mpd/mpdloadlibs.C"
        
/* 
 * Main macro to do femtoscopy studies with MpdFemtoPackage ($VMCWORKDIR/physics/femto) 
 * Input file (filename) should be given in MpdMcDst-format ($VMCWORKDIR/mpddst/mcDst)
 * nevents is a number of events to be proccesed
 */        

void femtoAna(TString filename = "", Int_t nevents = -1) {
    if (filename.IsNull() || nevents == -1) {
        cout << "Please, specify a correct input!" << endl;
        return;
    }
    //
    // Monte Carlo input setup
    //
    MpdMcDstReader *mcDstReader = new MpdMcDstReader(filename.Data());
    mcDstReader->Init();
    mcDstReader->setStatus("*", 0);
    mcDstReader->setStatus("Event", 1);
    mcDstReader->setStatus("Particle", 1);

    if (!mcDstReader->chain()) {
        std::cout << "No chain has been found." << std::endl;
    }
    Long64_t events2read = mcDstReader->chain()->GetEntries();
    std::cout << "Number of events in chain: " << events2read << std::endl;

    // Correct the amount event to read
    if (events2read >= nevents)
        events2read = nevents;

    //
    // StHbtMaker setup
    //

    // StHbtMaker
    MpdFemtoMaker* hbtMaker = new MpdFemtoMaker("HBT", "title");
    std::cout << "StHbtMaker instantiated" << std::endl;

    // Create manager
    MpdFemtoManager* hbtManager = hbtMaker->hbtManager();

    // Setup StHbtReader
    MpdFemtoMcDstReader *mcHbtReader = new MpdFemtoMcDstReader(mcDstReader);
    mcHbtReader->setRotateEventPlane(false);
    mcHbtReader->setMagneticField(-5.); // in kilogaus = -0.5 Tesla

    // Add reader to the manager
    hbtManager->setEventReader(mcHbtReader);

    // Create and set analysis
    MpdFemtoAnalysis *hbtAnalysis = new MpdFemtoAnalysis();
    hbtAnalysis->setVerboseMode(false);

    // Create and set event cut
    MpdFemtoBasicEventCut *eventCut = new MpdFemtoBasicEventCut();
    hbtAnalysis->setEventCut(eventCut);

    // Create and set track cut
    MpdFemtoBasicTrackCut *trackCut = new MpdFemtoBasicTrackCut();
    trackCut->setIsTheory(true);
    trackCut->setPdgId(321);
    trackCut->setEta(-1., 1.);
    trackCut->setPt(0.15, 1.55);
    trackCut->setMass(M_KAON_PLUS); // from femto/phys_constants.h

    // Create and set cut monitor
    MpdFemtoTrackCutMonitor *trackMoniPass = new MpdFemtoTrackCutMonitor("_moniPass_", M_KAON_PLUS);
    MpdFemtoTrackCutMonitor *trackMoniFail = new MpdFemtoTrackCutMonitor("_moniFail_", M_KAON_PLUS);
    trackCut->addCutMonitor(trackMoniPass, trackMoniFail);

    // Set particle cuts
    hbtAnalysis->setFirstParticleCut(trackCut);
    hbtAnalysis->setSecondParticleCut(trackCut);

    // Create and set pair cut
    MpdFemtoBasicPairCut *pairCut = new MpdFemtoBasicPairCut();

    // Create pair cut monitors
    MpdFemtoPairCutMonitor *pairMoniPass = new MpdFemtoPairCutMonitor("_moniPass_");
    MpdFemtoPairCutMonitor *pairMoniFail = new MpdFemtoPairCutMonitor("_moniFail_");
    pairCut->addCutMonitor(pairMoniPass, pairMoniFail);

    // Add pair cut to the analysis
    hbtAnalysis->setPairCut(pairCut);

    // Set how many events to mix
    hbtAnalysis->setNumEventsToMix(5);

    // Lednicky weight generator
    MpdFemtoModelWeightGeneratorLednicky *hbtWeight = new MpdFemtoModelWeightGeneratorLednicky();
    hbtWeight->setPairType(MpdFemtoModelWeightGeneratorLednicky::KaonPlusKaonPlus());
    hbtWeight->setCoulOff();
    hbtWeight->setQuantumOn();
    hbtWeight->setStrongOff();
    hbtWeight->set3BodyOff();

    // Theoretical model manager
    MpdFemtoModelManager *thModelManager = new MpdFemtoModelManager();
    thModelManager->setWeightGenerator(hbtWeight);
    thModelManager->createCopyHiddenInfo(true);

    // Create 1D correlation function integrated over kT
    MpdFemtoModelQinvCorrFctn *thCorrFctn = new MpdFemtoModelQinvCorrFctn("hTheorQinv", 40, 0., 0.4);
    thCorrFctn->connectToManager(thModelManager);

    // Create 3D correlation function integrated with kT binning
    MpdFemtoModelBPLCMS3DCorrFctnKt *thCorrFctn3D = new MpdFemtoModelBPLCMS3DCorrFctnKt("hTheorBPLCMS", 80, -0.4, 0.4, 9, 0.05, 0.95);
    thCorrFctn3D->connectToManager(thModelManager);

    // Add correlation function to the analysis
    hbtAnalysis->addCorrFctn(thCorrFctn);
    hbtAnalysis->addCorrFctn(thCorrFctn3D);

    // Add analysis to the manager
    hbtManager->addAnalysis(hbtAnalysis);

    // Initialize StHbtMaker
    int iret = hbtMaker->init();


    // Loop over events
    for (int iEvent = 0; iEvent < events2read; iEvent++) {

        hbtMaker->clear();
        std::cout << "hbtMcDst -- Working on event # " << iEvent << std::endl;
        iret = hbtMaker->make();

        if (iret != 0) {
            std::cout << "[WARNING] StHbtMaker returned status: " << iret
                    << ". Processing will be finished" << std::endl;
            break;
        }
    } // for (int iEvent=0; iEvent<events2read; iEvent++)

    // Maker finish flag
    iret = hbtMaker->Finish();

    // Write histograms to the output file
    TFile *oFile = new TFile("oHbtTest.root", "recreate");
    trackMoniPass->writeOutHistos();
    trackMoniFail->writeOutHistos();
    pairMoniPass->writeOutHistos();
    pairMoniFail->writeOutHistos();
    thCorrFctn->writeOutHistos();
    thCorrFctn3D->writeOutHistos();
    oFile->Close();
}
