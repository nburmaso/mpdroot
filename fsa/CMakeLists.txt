# Create a library called "libFsa" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base 
${CMAKE_SOURCE_DIR}/field 
${CMAKE_SOURCE_DIR}/cbmbase 
${CMAKE_SOURCE_DIR}/fsa
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/fairtools
)

include_directories( ${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories( ${LINK_DIRECTORIES})

set(FSA_SRCS
MpdFsaGeo.cxx 
MpdFsaGeoPar.cxx 
MpdFsa.cxx 
MpdFsaContFact.cxx 
MpdFsaHit.cxx 
MpdFsaHitProducer.cxx 
MpdFsaPoint.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h FSA_HEADERS "${FSA_SRCS}")

set(FSA_LINKDEF MpdFsaLinkDef.h)
set(FSA_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdFsaDict.cxx) 

ROOT_GENERATE_DICTIONARY("${FSA_HEADERS}" "${FSA_LINKDEF}" "${FSA_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(FSA_SRCS ${FSA_SRCS} ${FSA_DICTIONARY})


add_library(Fsa SHARED ${FSA_SRCS})
target_link_libraries(Fsa ${ROOT_LIBRARIES})
set_target_properties(Fsa PROPERTIES ${FAIRROOT_LIBRARY_PROPERTIES})

################ install ###################
install(TARGETS Fsa DESTINATION ${CMAKE_BINARY_DIR}/lib)
