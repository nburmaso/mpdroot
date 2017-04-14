# Create a library called "libSts" which includes the source files given in
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
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/sts
)

include_directories(${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

set(STS_SRCS
MpdStsGeo.cxx 
MpdStsGeoPar.cxx 
MpdSts.cxx 
MpdStsNew2.cxx 
MpdStsContFact.cxx 
MpdStsHit.cxx 
MpdStsHitProducer.cxx 
MpdStsHitProducerV1.cxx 
MpdStsHitProducerNew2.cxx 
MpdStsPoint.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h STS_HEADERS "${STS_SRCS}")

set(STS_LINKDEF MpdStsLinkDef.h)
set(STS_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdStsDict.cxx) 

ROOT_GENERATE_DICTIONARY("${STS_HEADERS}" "${STS_LINKDEF}" "${STS_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(STS_SRCS ${STS_SRCS} ${STS_DICTIONARY})


add_library(Sts SHARED ${STS_SRCS})
target_link_libraries(Sts ${ROOT_LIBRARIES})
set_target_properties(Sts PROPERTIES ${FAIRROOT_LIBRARY_PROPERTIES})

################ install ###################
install(TARGETS Sts DESTINATION ${CMAKE_BINARY_DIR}/lib)
