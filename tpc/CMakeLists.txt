# Create a library called "libtpc" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
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

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR} 
 ${CLHEP_INCLUDE_DIR}
 ${Boost_INCLUDE_DIRS}
)

include_directories(${INCLUDE_DIRECTORIES})
Include_Directories(SYSTEM ${SYSTEM_INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
 ${ROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

# List of source files
set(SRCS
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

Set(HEADERS)
Set(LINKDEF tpcLinkDef.h)
Set(LIBRARY_NAME tpc)
Set(DEPENDENCIES Tof Core Base)

GENERATE_LIBRARY()
