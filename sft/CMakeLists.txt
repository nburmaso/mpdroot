# Create a library called "libBase" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base
${CMAKE_SOURCE_DIR}/cbmbase 
${CMAKE_SOURCE_DIR}/sft
${CMAKE_SOURCE_DIR}/kalman
${CMAKE_SOURCE_DIR}/lhetrack
${CMAKE_SOURCE_DIR}/strawendcap
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/fairtools
)

include_directories(${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

set(SFT_SRCS
MpdSftGeo.cxx 
MpdSftGeoPar.cxx 
MpdSft.cxx 
MpdSftContFact.cxx 
MpdSftHit.cxx 
MpdSftHitProducer.cxx 
MpdSftPoint.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h SFT_HEADERS "${SFT_SRCS}")

set(SFT_LINKDEF MpdSftLinkDef.h)
set(SFT_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdSftDict.cxx) 

ROOT_GENERATE_DICTIONARY("${SFT_HEADERS}" "${SFT_LINKDEF}" "${SFT_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(SFT_SRCS ${SFT_SRCS} ${SFT_DICTIONARY})


add_library(Sft SHARED ${SFT_SRCS})
target_link_libraries(Sft ${ROOT_LIBRARIES})
set_target_properties(Sft PROPERTIES VERSION 0.0.1 SOVERSION 0)

################ install ###################
install(TARGETS Sft DESTINATION ${CMAKE_BINARY_DIR}/lib)
