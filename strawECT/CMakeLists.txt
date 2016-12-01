# Create a library called "libBase" which includes the source files given in
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
${CMAKE_SOURCE_DIR}/cbmbase
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/strawECT

)

include_directories(${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)

link_directories(${LINK_DIRECTORIES})

set(StrawECT_SRCS
MpdStrawECTGeo.cxx
MpdStrawECTGeoPar.cxx
MpdStrawECT.cxx
MpdStrawECTContFact.cxx
MpdStrawECTHit.cxx
#MpdStrawECTHitProducer.cxx
MpdStrawECTPoint.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h StrawECT_HEADERS "${StrawECT_SRCS}")

set(StrawECT_LINKDEF MpdStrawECTLinkDef.h)
set(StrawECT_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdStrawECTDict.cxx)

ROOT_GENERATE_DICTIONARY("${StrawECT_HEADERS}" "${StrawECT_LINKDEF}" "${StrawECT_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(StrawECT_SRCS ${StrawECT_SRCS} ${StrawECT_DICTIONARY})


add_library(StrawECT SHARED ${StrawECT_SRCS})
target_link_libraries(StrawECT ${ROOT_LIBRARIES})
set_target_properties(StrawECT PROPERTIES VERSION 0.0.1 SOVERSION 0)

################ install ###################
install(TARGETS StrawECT DESTINATION ${CMAKE_BINARY_DIR}/lib)
