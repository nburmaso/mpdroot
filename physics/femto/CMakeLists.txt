# Create a library called "" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CLHEP_INCLUDE_DIR} 
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base 
${CMAKE_SOURCE_DIR}/field
${CMAKE_SOURCE_DIR}/mpdfield
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/passive
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/tpc
${CMAKE_SOURCE_DIR}/lhetrack
${CMAKE_SOURCE_DIR}/kalman
${CMAKE_SOURCE_DIR}/physics/femto
${CMAKE_SOURCE_DIR}/mpddata
)

include_directories( ${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories( ${LINK_DIRECTORIES})

set(PHYS_FEMTO_SRCS
MpdFemto.cxx
MpdFemtoContainer.cxx
MpdFemtoHistos.cxx
MpdFemtoShareQualityPairCut.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h PHYS_FEMTO_HEADERS "${PHYS_FEMTO_SRCS}")

set(PHYS_FEMTO_LINKDEF LinkDef.h)
set(PHYS_FEMTO_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/physDict.cxx) 

ROOT_GENERATE_DICTIONARY("${PHYS_FEMTO_HEADERS}" "${PHYS_FEMTO_LINKDEF}" "${PHYS_FEMTO_DICTIONARY}" "${INCLUDE_DIRECTORIES}")

#SET(PHYS_FEMTO_SRCS ${PHYS_FEMTO_SRCS} ${PHYS_FEMTO_DICTIONARY} dbgstream.cxx)
SET(PHYS_FEMTO_SRCS ${PHYS_FEMTO_SRCS} ${PHYS_FEMTO_DICTIONARY} )

add_library(MpdFemto SHARED ${PHYS_FEMTO_SRCS})
target_link_libraries(MpdFemto ${ROOT_LIBRARIES})
set_target_properties(MpdFemto PROPERTIES VERSION 0.0.1 SOVERSION 0 )

################ install ###################
install(TARGETS MpdFemto DESTINATION ${CMAKE_BINARY_DIR}/lib)
