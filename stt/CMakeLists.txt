# Create a library called "libStt" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base 
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/stt
)

include_directories(${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

set(STT_SRCS
CbmSttMatchTracks.cxx
CbmGeoStt.cxx
CbmSttPoint.cxx
CbmSttTrack.cxx
CbmStt.cxx
CbmGeoSttPar.cxx
CbmSttContFact.cxx
CbmSttDigiPar.cxx
CbmSttParRootFileIo.cxx
CbmSttParAsciiFileIo.cxx
CbmSttTrackFinder.cxx
CbmSttFindTracks.cxx
CbmSttTrackFinderIdeal.cxx
CbmSttTrackFitter.cxx
CbmSttFitTracks.cxx
CbmSttTrackMatch.cxx
CbmSttHit.cxx
CbmSttHitInfo.cxx
CbmSttHitProducerIdeal.cxx
TStraw.cxx
CbmSttHitProducerReal.cxx
CbmSttHelixTrackFitter.cxx            
)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h STT_HEADERS "${STT_SRCS}")

set(STT_LINKDEF  SttLinkDef.h)
set(STT_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/MySttDict.cxx) 

ROOT_GENERATE_DICTIONARY("${STT_HEADERS}" "${STT_LINKDEF}" "${STT_DICTIONARY}" "${INCLUDE_DIRECTORIES}")

SET(STT_SRCS ${STT_SRCS} ${STT_DICTIONARY})

add_library(Stt SHARED ${STT_SRCS})
target_link_libraries(Stt ${ROOT_LIBRARIES})
set_target_properties(Stt PROPERTIES VERSION 0.0.1 SOVERSION 0 )

################ install ###################
install(TARGETS Stt DESTINATION ${CMAKE_BINARY_DIR}/lib)
