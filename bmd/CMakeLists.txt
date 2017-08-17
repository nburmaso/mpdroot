# Create a library called "" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CLHEP_INCLUDE_DIR}
${Boost_INCLUDE_DIRS}
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base/sim
${CMAKE_SOURCE_DIR}/base/source
${CMAKE_SOURCE_DIR}/base/steer
${CMAKE_SOURCE_DIR}/base/event
${CMAKE_SOURCE_DIR}/base/field
${CMAKE_SOURCE_DIR}/mpdfield
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/passive
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/bmd
${CMAKE_SOURCE_DIR}/lhetrack
${CMAKE_SOURCE_DIR}/kalman
)

include_directories(${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

set(BMD_SRCS
BmdDetector.cxx
BmdPoint.cxx
BmdGeo.cxx
BmdGeoPar.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h BMD_HEADERS "${BMD_SRCS}")

set(BMD_LINKDEF  bmdLinkDef.h)
set(BMD_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/bmdDict.cxx) 

ROOT_GENERATE_DICTIONARY("${BMD_HEADERS}" "${BMD_LINKDEF}" "${BMD_DICTIONARY}" "${INCLUDE_DIRECTORIES}")

#SET(TPC_SRCS ${TPC_SRCS} ${TPC_DICTIONARY} dbgstream.cxx)
SET(BMD_SRCS ${BMD_SRCS} ${BMD_DICTIONARY} )

add_library(bmd SHARED ${BMD_SRCS})
target_link_libraries(bmd ${ROOT_LIBRARIES})
set_target_properties(bmd PROPERTIES VERSION 0.0.1 SOVERSION 0 )

################ install ###################
install(TARGETS bmd DESTINATION ${CMAKE_BINARY_DIR}/lib)
