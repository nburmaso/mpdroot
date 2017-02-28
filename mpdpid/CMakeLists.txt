# Create a library called "libMpdPid" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR}
${CMAKE_SOURCE_DIR}/geobase
${CMAKE_SOURCE_DIR}/parbase
${CMAKE_SOURCE_DIR}/base/event
${CMAKE_SOURCE_DIR}/base/steer
${CMAKE_SOURCE_DIR}/base/field
${CMAKE_SOURCE_DIR}/base/sim
${CMAKE_SOURCE_DIR}/base/source
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/mpddata
${CMAKE_SOURCE_DIR}/kalman
${CMAKE_SOURCE_DIR}/lhetrack
${CMAKE_SOURCE_DIR}/tof
${CMAKE_SOURCE_DIR}/tpc
${CMAKE_SOURCE_DIR}/etof
${CMAKE_SOURCE_DIR}/mpdfield
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/mpdpid
)

include_directories( ${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories( ${LINK_DIRECTORIES} )

set(MPDPID_SRCS
MpdPid.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h MPDPID_HEADERS "${MPDPID_SRCS}")

set(MPDPID_LINKDEF MpdPidLinkDef.h)
set(MPDPID_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdPidDict.cxx) 

ROOT_GENERATE_DICTIONARY("${MPDPID_HEADERS}" "${MPDPID_LINKDEF}" "${MPDPID_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(MPDPID_SRCS ${MPDPID_SRCS} ${MPDPID_DICTIONARY})


add_library(MpdPid SHARED ${MPDPID_SRCS})
target_link_libraries(MpdPid ${ROOT_LIBRARIES} )
set_target_properties(MpdPid PROPERTIES VERSION 0.0.1 SOVERSION 0)

################ install ###################
install(TARGETS MpdPid DESTINATION ${CMAKE_BINARY_DIR}/lib)
