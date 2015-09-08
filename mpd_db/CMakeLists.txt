# Create a library called "libMpdDb" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR}
#${CMAKE_SOURCE_DIR}/geobase
#${CMAKE_SOURCE_DIR}/parbase
#${CMAKE_SOURCE_DIR}/base
#${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/mpd_db
#${CMAKE_SOURCE_DIR}/kalman
#${CMAKE_SOURCE_DIR}/lhetrack
#${CMAKE_SOURCE_DIR}/tof
#${CMAKE_SOURCE_DIR}/tpc
#${CMAKE_SOURCE_DIR}/etof
#${CMAKE_SOURCE_DIR}/field
#${CMAKE_SOURCE_DIR}/fairtools
)

include_directories( ${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories( ${LINK_DIRECTORIES})

set(MPDDB_SRCS
MpdDbConnection.cxx
MpdDbGeoConverter.cxx
MpdDbGenerateClasses.cxx
MpdDbRunData.cxx
db_classes/MpdDbExperimentSession.cxx
db_classes/MpdDbComponent.cxx
db_classes/MpdDbSessionComponent.cxx
db_classes/MpdDbComponentMap.cxx
db_classes/MpdDbMap1dim.cxx
db_classes/MpdDbMap2dim.cxx
db_classes/MpdDbRun.cxx
db_classes/MpdDbRunGeometry.cxx
db_classes/MpdDbRunParameter.cxx
db_classes/MpdDbComponentParameter.cxx
db_classes/MpdDbParameterValue.cxx
db_classes/MpdDbIntValue.cxx
db_classes/MpdDbDoubleValue.cxx
db_classes/MpdDbStringValue.cxx
db_classes/MpdDbShift.cxx
db_classes/MpdDbSimulationFile.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h MPDDB_HEADERS "${MPDDB_SRCS}")

set(MPDDB_LINKDEF MpdDbLinkDef.h)
set(MPDDB_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MpdDbDict.cxx) 

ROOT_GENERATE_DICTIONARY("${MPDDB_HEADERS}" "${MPDDB_LINKDEF}" "${MPDDB_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(MPDDB_SRCS ${MPDDB_SRCS} ${MPDDB_DICTIONARY})


add_library(MpdDb SHARED ${MPDDB_SRCS})
target_link_libraries(MpdDb ${ROOT_LIBRARIES})
set_target_properties(MpdDb PROPERTIES VERSION 0.0.1 SOVERSION 0)

add_executable (get_mpd_prod get_mpd_prod.cxx)
target_link_libraries(get_mpd_prod ${ROOT_LIBRARIES})

################ install ###################
install(TARGETS MpdDb DESTINATION ${CMAKE_BINARY_DIR}/lib)
