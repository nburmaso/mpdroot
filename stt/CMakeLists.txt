# Create a library called "libStt" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base 
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/stt
)

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR}
)

include_directories(${INCLUDE_DIRECTORIES})
Include_Directories(SYSTEM ${SYSTEM_INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
 ${ROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

# List of source files
set(SRCS
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

Set(HEADERS)
Set(LINKDEF SttLinkDef.h)
Set(LIBRARY_NAME Stt)
Set(DEPENDENCIES Core Base)

GENERATE_LIBRARY()
