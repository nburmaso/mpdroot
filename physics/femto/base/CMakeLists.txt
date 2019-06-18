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
 ${CMAKE_SOURCE_DIR}/physics/femto/base 
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
MpdFemtoAnalysis.cxx
MpdFemtoBaseCorrFctn.cxx
MpdFemtoBaseCutMonitor.cxx
MpdFemtoBaseEventReader.cxx
MpdFemtoBaseModelFreezeOutGenerator.cxx
MpdFemtoBaseModelWeightGenerator.cxx
MpdFemtoCutMonitorHandler.cxx
MpdFemtoEvent.cxx
MpdFemtoHelix.cxx
MpdFemtoLikeSignAnalysis.cxx
MpdFemtoMaker.cxx
MpdFemtoManager.cxx
MpdFemtoModelGausLCMSFreezeOutGenerator.cxx
MpdFemtoModelHiddenInfo.cxx
MpdFemtoModelManager.cxx
MpdFemtoModelWeightGeneratorLednicky.cxx
MpdFemtoMultiTrackCut.cxx
MpdFemtoPair.cxx
MpdFemtoParticle.cxx
MpdFemtoPhysicalHelix.cxx
MpdFemtoPicoEvent.cxx
MpdFemtoPicoEventCollectionVectorHideAway.cxx
MpdFemtoReactionPlaneAnalysis.cxx
MpdFemtoTriplet.cxx
MpdFemtoVertexAnalysis.cxx
MpdFemtoVertexMultAnalysis.cxx
MpdFemtoXi.cxx
TpcLocalTransform.cxx
MpdFemtoKink.cxx
MpdFemtoTrack.cxx
MpdFemtoV0.cxx
)

Set(HEADERS)
Set(LINKDEF LinkDef.h)
Set(LIBRARY_NAME MpdFemtoBase)
Set(DEPENDENCIES Core Base Physics MpdBase MpdFsiTools)

GENERATE_LIBRARY()
