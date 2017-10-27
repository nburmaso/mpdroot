# Create a library called "libCpc" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base/sim
${CMAKE_SOURCE_DIR}/base/source
${CMAKE_SOURCE_DIR}/base/steer
${CMAKE_SOURCE_DIR}/base/event
${CMAKE_SOURCE_DIR}/cbmbase 
${CMAKE_SOURCE_DIR}/cpc
${CMAKE_SOURCE_DIR}/mcstack
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
 
link_directories(${LINK_DIRECTORIES})

# List of source files
set(SRCS
MpdCpcGeo.cxx 
MpdCpcGeoPar.cxx 
MpdCpc.cxx 
MpdCpcContFact.cxx 
MpdCpcHit.cxx 
MpdCpcHitProducer.cxx 
MpdCpcPoint.cxx
)

Set(HEADERS)
Set(LINKDEF MpdCpcLinkDef.h)
Set(LIBRARY_NAME Cpc)
Set(DEPENDENCIES Core Base)

GENERATE_LIBRARY()
