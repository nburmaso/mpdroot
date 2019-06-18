#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

// Monitors
#pragma link C++ class MpdFemtoEventCutMonitor+;
#pragma link C++ class MpdFemtoTrackCutMonitor+;
#pragma link C++ class MpdFemtoPairCutMonitor+;

// Cuts
#pragma link C++ class MpdFemtoBasicEventCut+;
#pragma link C++ class MpdFemtoBasicTrackCut+;
#pragma link C++ class MpdFemtoBasicPairCut+;
#pragma link C++ class MpdFemtoDummyTrackCut+;

// Readers
#pragma link C++ class MpdFemtoMcDstReader+;

// Corrrelation functions
#pragma link C++ class MpdFemtoCorrFctn3DLCMSSym+;
#pragma link C++ class MpdFemtoModelQinvCorrFctn+;
#pragma link C++ class MpdFemtoQinvCorrFctnKt+;
#pragma link C++ class MpdFemtoBPLCMS3DCorrFctnKt+;
#pragma link C++ class MpdFemtoModelBPLCMS3DCorrFctnKt+;

// Other classes
#pragma link C++ class MpdFemtoCoulomb+;
#pragma link C++ class MpdFemtoSmearPair+;

#endif

