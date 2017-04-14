# Create a library called "libKalman" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CMAKE_SOURCE_DIR}/base/source
${CMAKE_SOURCE_DIR}/base/steer
${CMAKE_SOURCE_DIR}/base/event
${CMAKE_SOURCE_DIR}/base/field 
${CMAKE_SOURCE_DIR}/mpdfield 
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/kalman
${CMAKE_SOURCE_DIR}/mpdbase
)

include_directories( ${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories( ${LINK_DIRECTORIES})

set(Kalman_SRCS
MpdKalmanFilter.cxx
MpdKalmanTrack.cxx
MpdKalmanHit.cxx
MpdKalmanGeoScheme.cxx
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h Kalman_HEADERS "${Kalman_SRCS}")

set(Kalman_LINKDEF  KalmanLinkDef.h)
set(Kalman_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MyKalmanDict.cxx) 

ROOT_GENERATE_DICTIONARY("${Kalman_HEADERS}" "${Kalman_LINKDEF}" "${Kalman_DICTIONARY}" "${INCLUDE_DIRECTORIES}")

SET(Kalman_SRCS ${Kalman_SRCS} ${Kalman_DICTIONARY})

add_library(Kalman SHARED ${Kalman_SRCS})
target_link_libraries(Kalman ${ROOT_LIBRARIES})
set_target_properties(Kalman PROPERTIES VERSION 0.0.1 SOVERSION 0 )

################ install ###################
install(TARGETS Kalman DESTINATION ${CMAKE_BINARY_DIR}/lib)
