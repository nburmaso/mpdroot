# Create a library called "libLHETrack" 

set(INCLUDE_DIRECTORIES
${ROOT_INCLUDE_DIR} 
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base 
${CMAKE_SOURCE_DIR}/mpdfield 
${CMAKE_SOURCE_DIR}/trackbase 
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/passive
${CMAKE_SOURCE_DIR}/tpc
${CMAKE_SOURCE_DIR}/lhetrack
${CMAKE_SOURCE_DIR}/kalman
${CMAKE_SOURCE_DIR}/etof
${CMAKE_SOURCE_DIR}/tof
${CMAKE_SOURCE_DIR}/strawendcap
${CMAKE_SOURCE_DIR}/mpddata
${CMAKE_SOURCE_DIR}/sts
${CMAKE_SOURCE_DIR}/mpdbase
${CMAKE_SOURCE_DIR}/tgem
${CMAKE_SOURCE_DIR}/cpc
${CMAKE_SOURCE_DIR}/dch
${CMAKE_SOURCE_DIR}/fairtools

)

include_directories( ${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
${ROOT_LIBRARY_DIR}
)
 
link_directories( ${LINK_DIRECTORIES})

set(LHETrack_SRCS
TpcLheTrackFinder.cxx
TpcLheSegments.cxx
TpcLheHit.cxx
TpcLheTrackCuts.cxx      
TpcLheCMPoint.cxx
TpcLheTrack.cxx          
TpcLheCMTrack.cxx        
TpcLheTrackFitter.cxx
TpcLheHitsMaker.cxx      
TpcLhePoint.cxx 
#TpcLheKalmanFilter.cxx
#TpcLheKalmanTrack.cxx
MpdTpcKalmanFilter.cxx
MpdTpcKalmanTrack.cxx
MpdItsKalmanTrack.cxx

MpdEctKalmanTrack.cxx
#MpdEctTrackFinderTof.cxx
#MpdEctTrackFinderTofTpc.cxx
MpdEctTrackFinderTpc.cxx
#MpdEctTrackFollow2Tpc.cxx
MpdSftTrackFinderTpc.cxx
#MpdTpcTrackFollow2Sft.cxx
MpdKfPrimaryVertexFinder.cxx
MpdTrackFinderIts.cxx
MpdVertexZfinder.cxx
MpdTpcDedxTask.cxx
MpdKfV0Fitter.cxx
MpdEctTrackFinderCpc.cxx
MpdEctTrackFinderTof.cxx
MpdMotherFitterPart.cxx
#MpdMotherFitterTrac.cxx
MpdCellAutomat.cxx
MpdCellTrack.cxx

)

# fill list of header files from list of source files
# by exchanging the file extension
CHANGE_FILE_EXTENSION(*.cxx *.h LHETrack_HEADERS "${LHETrack_SRCS}")


set(LHETrack_LINKDEF LHETrackLinkDef.h)
set(LHETrack_DICTIONARY ${CMAKE_CURRENT_BINARY_DIR}/LHETrackDict.cxx) 

ROOT_GENERATE_DICTIONARY("${LHETrack_HEADERS}" "${LHETrack_LINKDEF}" "${LHETrack_DICTIONARY}" "${INCLUDE_DIRECTORIES}")


set(LHETrack_SRCS ${LHETrack_SRCS} ${LHETrack_DICTIONARY})

add_library(LHETrack SHARED ${LHETrack_SRCS})
target_link_libraries(LHETrack ${ROOT_LIBRARIES})
set_target_properties(LHETrack PROPERTIES VERSION 0.0.1 SOVERSION 0 )

################ install ###################
install(TARGETS LHETrack DESTINATION ${CMAKE_BINARY_DIR}/lib)
