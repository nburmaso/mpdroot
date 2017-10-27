# Create a library called "libEtof" which includes the source files given in
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
${CMAKE_SOURCE_DIR}/mcstack 
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/tof
${CMAKE_SOURCE_DIR}/kalman
${CMAKE_SOURCE_DIR}/lhetrack
${CMAKE_SOURCE_DIR}/etof
)

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR}
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
MpdEtofGeo.cxx
MpdEtofGeoPar.cxx
MpdEtof.cxx
MpdEtofContFact.cxx
MpdEtofHitProducer.cxx
MpdEtofHitProducerIdeal.cxx
MpdEtofGeoUtils.cxx
MpdEtofMatching.cxx
)

Set(HEADERS)
Set(LINKDEF MpdEtofLinkDef.h)
Set(LIBRARY_NAME Etof)
Set(DEPENDENCIES Core Base)

GENERATE_LIBRARY()
