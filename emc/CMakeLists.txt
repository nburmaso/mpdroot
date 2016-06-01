# Create a library called "libTutorial1" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CMAKE_SOURCE_DIR}/emc 
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base 
${CMAKE_SOURCE_DIR}/mcstack 
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/clustering
${CMAKE_SOURCE_DIR}/emc/hitmaker
${CMAKE_SOURCE_DIR}/emc/clustering
)

include_directories( ${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
) 

link_directories( ${LINK_DIRECTORIES})

set(MPDEMC_SRCS
MpdEmcContFact.cxx
MpdEmc.cxx
MpdEmcGeo.cxx
MpdEmcGeoPar.cxx
MpdEmcPoint.cxx
MpdEmcHit.cxx
MpdEmcHitProducer.cxx

hitmaker/MpdEmcHitA.cxx
hitmaker/MpdEmcHitProducerA.cxx
hitmaker/MpdEmcHitCont.cxx
hitmaker/MpdEmcGeoParWrapper.cxx

clustering/EmcClManager.cxx
clustering/EmcClusterElement.cxx
clustering/EmcCluster.cxx
clustering/EmcClusterCollection.cxx
clustering/EmcClusterInfo.cxx
clustering/EmcCLinfo.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h MPDEMC_HEADERS "${MPDEMC_SRCS}")

set(MPDEMC_LINKDEF MpdEmcLinkDef.h)
set(MPDEMC_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdEmcDict.cxx) 

ROOT_GENERATE_DICTIONARY("${MPDEMC_HEADERS}" "${MPDEMC_LINKDEF}" "${MPDEMC_DICTIONARY}" "${INCLUDE_DIRECTORIES}")

set(MPDEMC_SRCS ${MPDEMC_SRCS} ${MPDEMC_DICTIONARY})

add_library(Emc SHARED ${MPDEMC_SRCS})
target_link_libraries(Emc ${ROOT_LIBRARIES})
set_target_properties(Emc PROPERTIES  ${FAIRROOT_LIBRARY_PROPERTIES})

################ install ###################
install(TARGETS Emc DESTINATION ${CMAKE_BINARY_DIR}/lib)
