# Create a library called "libBase" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR}
${Boost_INCLUDE_DIRS}
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

include_directories(${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

set(TOF_SRCS
MpdTofGeo.cxx 
MpdTofGeoPar.cxx 
MpdTof.cxx 
MpdTofContFact.cxx 
MpdTofHit.cxx 
MpdTofHitProducer.cxx 
MpdTofHitProducerIdeal.cxx 
MpdTofPoint.cxx
MpdTofUtils.cxx
MpdTofGeoUtils.cxx
MpdTofMatching.cxx
MpdTofMatchingData.cxx
MpdTofMatchingQA.cxx
MpdTofHitProducerQA.cxx
MpdTOFpid.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h TOF_HEADERS "${TOF_SRCS}")

set(TOF_LINKDEF TofLinkDef.h)
set(TOF_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdTofDict.cxx) 

ROOT_GENERATE_DICTIONARY("${TOF_HEADERS}" "${TOF_LINKDEF}" "${TOF_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(TOF_SRCS ${TOF_SRCS} ${TOF_DICTIONARY})


add_library(Tof SHARED ${TOF_SRCS})
target_link_libraries(Tof ${ROOT_LIBRARIES} Kalman)
set_target_properties(Tof PROPERTIES ${FAIRROOT_LIBRARY_PROPERTIES})

################ install ###################
install(TARGETS Tof DESTINATION ${CMAKE_BINARY_DIR}/lib)
