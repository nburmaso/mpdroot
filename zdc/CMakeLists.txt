# Create a library called "libBbc" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base 
${CMAKE_SOURCE_DIR}/field 
${CMAKE_SOURCE_DIR}/mcstack 
${CMAKE_SOURCE_DIR}/tpc
${CMAKE_SOURCE_DIR}/zdc
${CMAKE_SOURCE_DIR}/fairtools
)

include_directories( ${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories( ${LINK_DIRECTORIES})

set(ZDC_SRCS
MpdZdcGeo.cxx 
MpdZdcGeoPar.cxx 
MpdZdc.cxx 
MpdZdcContFact.cxx 
MpdZdcDigi.cxx 
MpdZdcDigiProducer.cxx 
MpdZdcPoint.cxx
MpdZdcTstSim.cxx
MpdZdcDigiPar.cxx
MpdZdcDigiScheme.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h ZDC_HEADERS "${ZDC_SRCS}")

set(ZDC_LINKDEF MpdZdcLinkDef.h)
set(ZDC_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdZdcDict.cxx) 

ROOT_GENERATE_DICTIONARY("${ZDC_HEADERS}" "${ZDC_LINKDEF}" "${ZDC_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(ZDC_SRCS ${ZDC_SRCS} ${ZDC_DICTIONARY})


add_library(Zdc SHARED ${ZDC_SRCS})
target_link_libraries(Zdc ${ROOT_LIBRARIES})
set_target_properties(Zdc PROPERTIES ${FAIRROOT_LIBRARY_PROPERTIES})

################ install ###################
install(TARGETS Zdc DESTINATION ${CMAKE_BINARY_DIR}/lib)
