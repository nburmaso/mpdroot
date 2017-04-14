# Create a library called "libBase" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/ndet
${CMAKE_SOURCE_DIR}/fairtools
)

include_directories(${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

set(NDET_SRCS
MpdNDetPoint.cxx
MpdNDetPointLite.cxx
MpdNDetParam.cxx
MpdNDet.cxx
MpdNDetAnalysis.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h NDET_HEADERS "${NDET_SRCS}")

set(NDET_LINKDEF NDetLinkDef.h)
set(NDET_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdNDetDict.cxx) 

ROOT_GENERATE_DICTIONARY("${NDET_HEADERS}" "${NDET_LINKDEF}" "${NDET_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(NDET_SRCS ${NDET_SRCS} ${NDET_DICTIONARY})


add_library(NDet SHARED ${NDET_SRCS})
target_link_libraries(NDet ${ROOT_LIBRARIES})
set_target_properties(NDet PROPERTIES ${FAIRROOT_LIBRARY_PROPERTIES})

################ install ###################
install(TARGETS NDet DESTINATION ${CMAKE_BINARY_DIR}/lib)
