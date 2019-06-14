# Create a library called "MpdFemto" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
 ${BASE_INCLUDE_DIRECTORIES}
 ${CMAKE_SOURCE_DIR}/mcstack
 ${CMAKE_SOURCE_DIR}/passive
 ${CMAKE_SOURCE_DIR}/mpdbase
 ${CMAKE_SOURCE_DIR}/mpddst
 ${CMAKE_SOURCE_DIR}/mpdfield
 ${CMAKE_SOURCE_DIR}/tpc
 ${CMAKE_SOURCE_DIR}/lhetrack
 ${CMAKE_SOURCE_DIR}/kalman
 ${CMAKE_SOURCE_DIR}/physics/femto 
 ${CMAKE_SOURCE_DIR}/mpddst/mcDst 
)

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR}
 ${GSL_INCLUDE_DIRS}
 ${CLHEP_INCLUDE_DIR}
 ${Boost_INCLUDE_DIRS}
)

include_directories(${INCLUDE_DIRECTORIES})
Include_Directories(SYSTEM ${SYSTEM_INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
 ${ROOT_LIBRARY_DIR}
 ${FAIRROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

# List of source files
set(SRCS
TpcLocalTransform.cxx
MpdFemtoXi.cxx
MpdFemtoVertexMultAnalysis.cxx
MpdFemtoVertexAnalysis.cxx
MpdFemtoV0.cxx
MpdFemtoTriplet.cxx
MpdFemtoTrack.cxx
MpdFemtoReactionPlaneAnalysis.cxx
MpdFemtoPicoEvent.cxx
MpdFemtoPicoEventCollectionVectorHideAway.cxx
MpdFemtoPhysicalHelix.cxx
MpdFemtoParticle.cxx
MpdFemtoPair.cxx
MpdFemtoMultiTrackCut.cxx
MpdFemtoModelWeightGeneratorLednicky.cxx
MpdFemtoModelManager.cxx
MpdFemtoModelHiddenInfo.cxx
MpdFemtoModelGausLCMSFreezeOutGenerator.cxx
MpdFemtoManager.cxx
MpdFemtoMaker.cxx
MpdFemtoLikeSignAnalysis.cxx
MpdFemtoKink.cxx
MpdFemtoHelix.cxx
MpdFemtoEvent.cxx
MpdFemtoCutMonitorHandler.cxx
MpdFemtoBaseModelWeightGenerator.cxx
MpdFemtoBaseModelFreezeOutGenerator.cxx
MpdFemtoBaseEventReader.cxx
MpdFemtoBaseCutMonitor.cxx
MpdFemtoBaseCorrFctn.cxx
MpdFemtoAnalysis.cxx

MpdFemtoSmearPair.cxx
MpdFemtoQinvCorrFctnKt.cxx
MpdFemtoModelQinvCorrFctn.cxx
MpdFemtoModelBPLCMS3DCorrFctnKt.cxx
MpdFemtoMcDstReader.cxx
MpdFemtoDummyTrackCut.cxx
MpdFemtoCoulomb.cxx
MpdFemtoCorrFctn3DLCMSSym.cxx
MpdFemtoBPLCMS3DCorrFctnKt.cxx
MpdFemtoBasicTrackCut.cxx
MpdFemtoBasicPairCut.cxx
MpdFemtoPairCutMonitor.cxx
MpdFemtoEventCutMonitor.cxx
MpdFemtoBasicEventCut.cxx
MpdFemtoTrackCutMonitor.cxx
)

Set(HEADERS)
Set(LINKDEF LinkDef.h)
Set(LIBRARY_NAME MpdFemto)
Set(DEPENDENCIES Core Base Physics MpdBase MpdFsiTools MpdMcDst)

GENERATE_LIBRARY()
