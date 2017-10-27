# Create a library called "libKalman" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${CMAKE_SOURCE_DIR}/base/source
${CMAKE_SOURCE_DIR}/base/steer
${CMAKE_SOURCE_DIR}/base/event
${CMAKE_SOURCE_DIR}/base/field 
${CMAKE_SOURCE_DIR}/mpdfield 
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/kalman
${CMAKE_SOURCE_DIR}/mpdbase
)

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR}
)

include_directories(${INCLUDE_DIRECTORIES})
Include_Directories(SYSTEM ${SYSTEM_INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
 ${ROOT_LIBRARY_DIR}
)
 
link_directories( ${LINK_DIRECTORIES})

# List of source files
set(SRCS
MpdKalmanFilter.cxx
MpdKalmanTrack.cxx
MpdKalmanHit.cxx
MpdKalmanGeoScheme.cxx
)

Set(HEADERS)
Set(LINKDEF KalmanLinkDef.h)
Set(LIBRARY_NAME Kalman)
Set(DEPENDENCIES Core Base)

GENERATE_LIBRARY()
