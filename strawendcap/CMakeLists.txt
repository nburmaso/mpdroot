# Create a library called "libBase" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base 
${CMAKE_SOURCE_DIR}/field 
${CMAKE_SOURCE_DIR}/cbmbase 
${CMAKE_SOURCE_DIR}/mcstack 
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/strawendcap

)

include_directories( ${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories( ${LINK_DIRECTORIES})

set(STRAWENDCAP_SRCS
MpdStrawendcapGeo.cxx 
MpdStrawendcapGeoPar.cxx 
MpdStrawendcap.cxx 
MpdStrawendcapContFact.cxx 
MpdStrawendcapHit.cxx 
MpdStrawendcapHitProducer.cxx 
MpdStrawendcapPoint.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h STRAWENDCAP_HEADERS "${STRAWENDCAP_SRCS}")

set(STRAWENDCAP_LINKDEF MpdStrawendcapLinkDef.h)
set(STRAWENDCAP_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdStrawendcapDict.cxx) 

ROOT_GENERATE_DICTIONARY("${STRAWENDCAP_HEADERS}" "${STRAWENDCAP_LINKDEF}" "${STRAWENDCAP_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(STRAWENDCAP_SRCS ${STRAWENDCAP_SRCS} ${STRAWENDCAP_DICTIONARY})


add_library(Strawendcap SHARED ${STRAWENDCAP_SRCS})
target_link_libraries(Strawendcap ${ROOT_LIBRARIES})
set_target_properties(Strawendcap PROPERTIES VERSION 0.0.1 SOVERSION 0)

################ install ###################
install(TARGETS Strawendcap DESTINATION ${CMAKE_BINARY_DIR}/lib)
