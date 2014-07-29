# Create a library called "libMpdData" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR}
${CMAKE_SOURCE_DIR}/geobase
${CMAKE_SOURCE_DIR}/parbase
${CMAKE_SOURCE_DIR}/base
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/mpddata
${CMAKE_SOURCE_DIR}/kalman
${CMAKE_SOURCE_DIR}/lhetrack
${CMAKE_SOURCE_DIR}/tof
${CMAKE_SOURCE_DIR}/tpc
${CMAKE_SOURCE_DIR}/etof
${CMAKE_SOURCE_DIR}/mpdfield
${CMAKE_SOURCE_DIR}/fairtools
)

include_directories( ${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories( ${LINK_DIRECTORIES})

set(MPDDATA_SRCS
MpdFillDstTask.cxx
MpdEvent.cxx
MpdTrack.cxx
MpdVertex.cxx 
MpdRoInvMassTask.cxx
MpdParticle.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h MPDDATA_HEADERS "${MPDDATA_SRCS}")

set(MPDDATA_LINKDEF MpdDataLinkDef.h)
set(MPDDATA_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdDataDict.cxx) 

ROOT_GENERATE_DICTIONARY("${MPDDATA_HEADERS}" "${MPDDATA_LINKDEF}" "${MPDDATA_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(MPDDATA_SRCS ${MPDDATA_SRCS} ${MPDDATA_DICTIONARY})


add_library(MpdData SHARED ${MPDDATA_SRCS})
target_link_libraries(MpdData ${ROOT_LIBRARIES})
set_target_properties(MpdData PROPERTIES VERSION 0.0.1 SOVERSION 0)

################ install ###################
install(TARGETS MpdData DESTINATION ${CMAKE_BINARY_DIR}/lib)
