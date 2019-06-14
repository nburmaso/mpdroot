#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

// Main MpdFemtoMaker classes
#pragma link C++ class MpdFemtoMaker++;
#pragma link C++ class MpdFemtoManager++;

// Analyses
#pragma link C++ class MpdFemtoAnalysis++;
#pragma link C++ class MpdFemtoLikeSignAnalysis++;
#pragma link C++ class MpdFemtoReactionPlaneAnalysis++;
#pragma link C++ class MpdFemtoVertexAnalysis++;
#pragma link C++ class MpdFemtoVertexMultAnalysis++;

// Base classes
#pragma link C++ class MpdFemtoBaseAnalysis++;
#pragma link C++ class MpdFemtoBaseEventReader++;
#pragma link C++ class MpdFemtoBaseEventCut++;
#pragma link C++ class MpdFemtoBaseTrackCut++;
#pragma link C++ class MpdFemtoBaseParticleCut++;
#pragma link C++ class MpdFemtoBasePairCut++;
#pragma link C++ class MpdFemtoBaseCutMonitor++;
#pragma link C++ class MpdFemtoBaseCorrFctn++;
#pragma link C++ class MpdFemtoBaseModelFreezeOutGenerator++;
#pragma link C++ class MpdFemtoBaseModelWeightGenerator++;

// Cuts and
#pragma link C++ class MpdFemtoMultiTrackCut++;
#pragma link C++ class MpdFemtoCutMonitorHandler++;

// Main classes
#pragma link C++ class MpdFemtoEvent++;
#pragma link C++ class MpdFemtoTrack++;
#pragma link C++ class MpdFemtoV0++;
#pragma link C++ class MpdFemtoKink++;
#pragma link C++ class MpdFemtoXi++;
#pragma link C++ class MpdFemtoTriplet++;
#pragma link C++ class MpdFemtoParticle++;
#pragma link C++ class MpdFemtoPair++;

// Internal PicoEvent and PicoCollection
#pragma link C++ class MpdFemtoPicoEvent++;
#pragma link C++ class MpdFemtoPicoEventCollectionVectorHideAway++;

// Monte Carlo weights
#pragma link C++ class MpdFemtoModelHiddenInfo++;
#pragma link C++ class MpdFemtoModelGausLCMSFreezeOutGenerator++;
#pragma link C++ class MpdFemtoModelManager++;
#pragma link C++ class MpdFemtoModelWeightGeneratorLednicky++;

// StarClassLibrary adopted classes
#pragma link C++ class MpdFemtoHelix++;
#pragma link C++ class MpdFemtoPhysicalHelix++;

// Monitors
#pragma link C++ class MpdFemtoEventCutMonitor++;
#pragma link C++ class MpdFemtoTrackCutMonitor++;
#pragma link C++ class MpdFemtoPairCutMonitor++;

// Cuts
#pragma link C++ class MpdFemtoBasicEventCut++;
#pragma link C++ class MpdFemtoBasicTrackCut++;
#pragma link C++ class MpdFemtoBasicPairCut++;
#pragma link C++ class MpdFemtoDummyTrackCut++;

// Readers
#pragma link C++ class MpdFemtoMcDstReader++;

// Corrrelation functions
#pragma link C++ class MpdFemtoCorrFctn3DLCMSSym++;
#pragma link C++ class MpdFemtoModelQinvCorrFctn++;
#pragma link C++ class MpdFemtoQinvCorrFctnKt++;
#pragma link C++ class MpdFemtoBPLCMS3DCorrFctnKt++;
#pragma link C++ class MpdFemtoModelBPLCMS3DCorrFctnKt++;

// Other classes
#pragma link C++ class MpdFemtoCoulomb++;
#pragma link C++ class MpdFemtoSmearPair++;


#endif

