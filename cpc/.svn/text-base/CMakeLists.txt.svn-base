# Create a library called "libCpc" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base 
${CMAKE_SOURCE_DIR}/field 
${CMAKE_SOURCE_DIR}/cbmbase 
${CMAKE_SOURCE_DIR}/cpc
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/fairtools
)

include_directories( ${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories( ${LINK_DIRECTORIES})

set(CPC_SRCS
MpdCpcGeo.cxx 
MpdCpcGeoPar.cxx 
MpdCpc.cxx 
MpdCpcContFact.cxx 
MpdCpcHit.cxx 
MpdCpcHitProducer.cxx 
MpdCpcPoint.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h CPC_HEADERS "${CPC_SRCS}")

set(CPC_LINKDEF MpdCpcLinkDef.h)
set(CPC_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdCpcDict.cxx) 

ROOT_GENERATE_DICTIONARY("${CPC_HEADERS}" "${CPC_LINKDEF}" "${CPC_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(CPC_SRCS ${CPC_SRCS} ${CPC_DICTIONARY})


add_library(Cpc SHARED ${CPC_SRCS})
target_link_libraries(Cpc ${ROOT_LIBRARIES})
set_target_properties(Cpc PROPERTIES ${FAIRROOT_LIBRARY_PROPERTIES})

################ install ###################
install(TARGETS Cpc DESTINATION ${CMAKE_BINARY_DIR}/lib)
