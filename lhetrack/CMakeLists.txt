# Create a library called "libLHETrack" 

set(INCLUDE_DIRECTORIES
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase
${CMAKE_SOURCE_DIR}/base/sim
${CMAKE_SOURCE_DIR}/base/source
${CMAKE_SOURCE_DIR}/base/steer
${CMAKE_SOURCE_DIR}/base/event
${CMAKE_SOURCE_DIR}/base/field 
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

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR}
 ${Boost_INCLUDE_DIRS}
)

include_directories(${INCLUDE_DIRECTORIES})
Include_Directories(SYSTEM ${SYSTEM_INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
 ${ROOT_LIBRARY_DIR}
)

link_directories( ${LINK_DIRECTORIES})

# List of source files
set(SRCS
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

# List of header files
set(HEADERS
TpcLheTrackFinder.h
TpcLheSegments.h
TpcLheHit.h
TpcLheTrackCuts.h      
TpcLheCMPoint.h
TpcLheTrack.h          
TpcLheCMTrack.h        
TpcLheTrackFitter.h
TpcLheHitsMaker.h      
TpcLhePoint.h 
#TpcLheKalmanFilter.h
#TpcLheKalmanTrack.h
MpdTpcKalmanFilter.h
MpdTpcKalmanTrack.h
MpdItsKalmanTrack.h

MpdEctKalmanTrack.h
#MpdEctTrackFinderTof.h
#MpdEctTrackFinderTofTpc.h
MpdEctTrackFinderTpc.h
#MpdEctTrackFollow2Tpc.h
MpdSftTrackFinderTpc.h
#MpdTpcTrackFollow2Sft.h
MpdKfPrimaryVertexFinder.h
MpdTrackFinderIts.h
MpdVertexZfinder.h
MpdTpcDedxTask.h
MpdKfV0Fitter.h
MpdEctTrackFinderCpc.h
MpdEctTrackFinderTof.h
MpdMotherFitterPart.h
#MpdMotherFitterTrac.h
MpdCellAutomat.h
MpdCellTrack.h
lhe.h
)

Set(LINKDEF LHETrackLinkDef.h)
Set(LIBRARY_NAME LHETrack)
Set(DEPENDENCIES Core Base Kalman Tof)

GENERATE_LIBRARY()
