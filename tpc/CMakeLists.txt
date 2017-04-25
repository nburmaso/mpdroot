# Create a library called "" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CLHEP_INCLUDE_DIR}
${Boost_INCLUDE_DIRS}
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base/sim
${CMAKE_SOURCE_DIR}/base/source
${CMAKE_SOURCE_DIR}/base/steer
${CMAKE_SOURCE_DIR}/base/event
${CMAKE_SOURCE_DIR}/base/field
${CMAKE_SOURCE_DIR}/mpdfield
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/passive
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/tpc
${CMAKE_SOURCE_DIR}/tof
${CMAKE_SOURCE_DIR}/lhetrack
${CMAKE_SOURCE_DIR}/kalman
)

include_directories(${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

set(TPC_SRCS
TaskHelpers.cxx
TpcContFact.cxx
TpcDetector.cxx
TpcGas.cxx
TpcGeo.cxx
TpcGeoPar.cxx
TpcPoint.cxx
MpdTpcSector.cxx
TpcTimeBin.cxx
MpdTpcClusterFinderTask.cxx
MpdTpcDigit.cxx
MpdTpcPeak.cxx
MpdTpcFoundHit.cxx
MpdTpcClusterFinderQAHistograms.cxx
MpdParticleIdentification.cxx
MpdTpcDigitizerTask.cxx
MpdTpcDigitizerQAHistograms.cxx
MpdTpc2dCluster.cxx
MpdTpcHit.cxx
MpdTpcHitProducer.cxx
MpdTpcSectorGeo.cxx
MpdTpcDigitizerAZ.cxx
MpdTpcClusterFinderAZ.cxx
MpdTPCpid.cxx
MpdTpcClusterFinderMlem.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h TPC_HEADERS "${TPC_SRCS}")

set(TPC_LINKDEF  tpcLinkDef.h)
set(TPC_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/tpcDict.cxx) 

ROOT_GENERATE_DICTIONARY("${TPC_HEADERS}" "${TPC_LINKDEF}" "${TPC_DICTIONARY}" "${INCLUDE_DIRECTORIES}")

#SET(TPC_SRCS ${TPC_SRCS} ${TPC_DICTIONARY} dbgstream.cxx)
SET(TPC_SRCS ${TPC_SRCS} ${TPC_DICTIONARY} )

add_library(tpc SHARED ${TPC_SRCS})
target_link_libraries(tpc ${ROOT_LIBRARIES})
set_target_properties(tpc PROPERTIES VERSION 0.0.1 SOVERSION 0 )

################ install ###################
install(TARGETS tpc DESTINATION ${CMAKE_BINARY_DIR}/lib)
