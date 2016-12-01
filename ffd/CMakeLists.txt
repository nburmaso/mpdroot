# Create a library called "libFsa" which includes the source files given in
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
${CMAKE_SOURCE_DIR}/mcstack 
${CMAKE_SOURCE_DIR}/ffd
${CMAKE_SOURCE_DIR}/fairtools
)

include_directories(${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

set(FFD_SRCS
MpdFfdGeo.cxx 
MpdFfdGeoPar.cxx 
MpdFfd.cxx 
MpdFfdContFact.cxx 
MpdFfdHit.cxx 
MpdFfdHitProducer.cxx 
MpdFfdPoint.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h FFD_HEADERS "${FFD_SRCS}")

set(FFD_LINKDEF MpdFfdLinkDef.h)
set(FFD_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdFfdDict.cxx) 

ROOT_GENERATE_DICTIONARY("${FFD_HEADERS}" "${FFD_LINKDEF}" "${FFD_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(FFD_SRCS ${FFD_SRCS} ${FFD_DICTIONARY})


add_library(Ffd SHARED ${FFD_SRCS})
target_link_libraries(Ffd ${ROOT_LIBRARIES})
set_target_properties(Ffd PROPERTIES ${FAIRROOT_LIBRARY_PROPERTIES})

################ install ###################
install(TARGETS Ffd DESTINATION ${CMAKE_BINARY_DIR}/lib)
