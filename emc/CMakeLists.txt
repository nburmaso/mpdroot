# Create a library called "libEmc" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${CMAKE_SOURCE_DIR}/emc 
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base/sim
${CMAKE_SOURCE_DIR}/base/source
${CMAKE_SOURCE_DIR}/base/steer
${CMAKE_SOURCE_DIR}/base/event
${CMAKE_SOURCE_DIR}/mcstack 
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/clustering
${CMAKE_SOURCE_DIR}/emc/hitmaker
${CMAKE_SOURCE_DIR}/emc/clustering
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

link_directories(${LINK_DIRECTORIES})

# List of source files
set(SRCS
MpdEmcContFact.cxx
MpdEmc.cxx
MpdEmcGeo.cxx
MpdEmcGeoPar.cxx
MpdEmcPoint.cxx
MpdEmcHit.cxx
MpdEmcHitProducer.cxx

hitmaker/MpdEmcHitA.cxx
hitmaker/MpdEmcHitProducerA.cxx
hitmaker/MpdEmcHitCont.cxx
hitmaker/MpdEmcGeoParWrapper.cxx

clustering/EmcClManager.cxx
clustering/EmcClusterElement.cxx
clustering/EmcCluster.cxx
clustering/EmcClusterCollection.cxx
clustering/EmcClusterInfo.cxx
clustering/EmcCLinfo.cxx
)

Set(HEADERS)
Set(LINKDEF MpdEmcLinkDef.h)
Set(LIBRARY_NAME Emc)
Set(DEPENDENCIES Cluster Core Base)

GENERATE_LIBRARY()
