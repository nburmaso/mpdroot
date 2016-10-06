# Create a library called "libBase" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base 
${CMAKE_SOURCE_DIR}/field 
${CMAKE_SOURCE_DIR}/mcstack 
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/tof
${CMAKE_SOURCE_DIR}/kalman
${CMAKE_SOURCE_DIR}/lhetrack
${CMAKE_SOURCE_DIR}/etof
)

include_directories( ${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories( ${LINK_DIRECTORIES})

set(ETOF_SRCS
MpdEtofGeo.cxx
MpdEtofGeoPar.cxx
MpdEtof.cxx
MpdEtofContFact.cxx
MpdEtofHitProducer.cxx
MpdEtofHitProducerIdeal.cxx
MpdEtofGeoUtils.cxx
MpdEtofMatching.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h ETOF_HEADERS "${ETOF_SRCS}")

set(ETOF_LINKDEF MpdEtofLinkDef.h)
set(ETOF_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdEtofDict.cxx) 

ROOT_GENERATE_DICTIONARY("${ETOF_HEADERS}" "${ETOF_LINKDEF}" "${ETOF_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(ETOF_SRCS ${ETOF_SRCS} ${ETOF_DICTIONARY})


add_library(Etof SHARED ${ETOF_SRCS})
target_link_libraries(Etof ${ROOT_LIBRARIES} Kalman Tof)
set_target_properties(Etof PROPERTIES ${FAIRROOT_LIBRARY_PROPERTIES})

################ install ###################
install(TARGETS Etof DESTINATION ${CMAKE_BINARY_DIR}/lib)
