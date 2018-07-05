# Create a library called "" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
Set(INCLUDE_DIRECTORIES
${BASE_INCLUDE_DIRECTORIES}
${CMAKE_SOURCE_FAIRMCSTACK}
${CMAKE_SOURCE_DIR}/tpc
${CMAKE_SOURCE_DIR}/mpddata
${CMAKE_SOURCE_DIR}/mpdbase
${CMAKE_SOURCE_DIR}/kalman
${CMAKE_SOURCE_DIR}/lhetrack
${CMAKE_SOURCE_DIR}/base/event
${CMAKE_SOURCE_DIR}/base/field 
${CMAKE_SOURCE_DIR}/base/steer
${CMAKE_SOURCE_DIR}/base/sim
${CMAKE_SOURCE_DIR}/base/source
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/mpdbase
${CMAKE_SOURCE_DIR}/nicaroot/features
${CMAKE_SOURCE_DIR}/nicaroot/unigen
${CMAKE_SOURCE_DIR}/nicaroot/dataformat
${CMAKE_SOURCE_DIR}/nicaroot/dataformat/compound
${CMAKE_SOURCE_DIR}/nicaroot/dataformat/formats
${CMAKE_SOURCE_DIR}/nicaroot/dataformat/formats/fair
${CMAKE_SOURCE_DIR}/nicaroot/dataformat/detector
${CMAKE_SOURCE_DIR}/nica_mpd/format
${CMAKE_SOURCE_DIR}/nica_mpd/format/combined
${CMAKE_SOURCE_DIR}/nica_mpd/format/kalman
${CMAKE_SOURCE_DIR}/nica_mpd/format/kalmandst
${CMAKE_SOURCE_DIR}/nica_mpd/format/mpd
${CMAKE_SOURCE_DIR}/nica_mpd/format/mpdevent
)

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR} 
)

Set(LINK_DIRECTORIES
  ${ROOT_LIBRARY_DIR}
  ${FAIRROOT_LIBRARY_DIR}
)

Include_Directories(${INCLUDE_DIRECTORIES})
Include_Directories(SYSTEM ${SYSTEM_INCLUDE_DIRECTORIES})
 
Link_Directories(${LINK_DIRECTORIES})

# List of source files
set(SRCS
combined/NicaMpdDstMCEvent.cxx
combined/NicaMpdFullEvent.cxx
combined/NicaMpdDstMCEventTpcPads.cxx
kalman/NicaMpdKalmanTrackInterface.cxx
kalman/NicaMpdKalmanEvent.cxx
kalman/NicaMpdKalmanEventInterface.cxx
kalman/NicaMpdKalmanTrack.cxx
kalmandst/NicaMpdDstKalmanEvent.cxx
kalmandst/NicaMpdDstKalmanEventInterface.cxx
kalmandst/NicaMpdDstKalmanTrack.cxx
kalmandst/NicaMpdDstKalmanTrackInterface.cxx
mpdevent/NicaMpdEvent.cxx
mpdevent/NicaMpdEventInterface.cxx
mpdevent/NicaMpdTrackInterface.cxx
mpdevent/NicaMpdTrack.cxx
mpdevent/NicaMpdTrackTpcPads.cxx
mpdevent/NicaMpdEventTpcPads.cxx
mpdevent/NicaTpcSectorGeo.cxx
)

Set(HEADERS )
Set(LINKDEF NicaMpdInterfaceLinkDef.h)
Set(LIBRARY_NAME NicaMpdFormat)
Set(DEPENDENCIES NicaFeatures NicaDataFormat MpdData tpc)

GENERATE_LIBRARY()


