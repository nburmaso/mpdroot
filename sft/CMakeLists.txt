# Create a library called "libSft" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base
${CMAKE_SOURCE_DIR}/cbmbase 
${CMAKE_SOURCE_DIR}/sft
${CMAKE_SOURCE_DIR}/kalman
${CMAKE_SOURCE_DIR}/lhetrack
${CMAKE_SOURCE_DIR}/strawendcap
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/fairtools
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
MpdSftGeo.cxx 
MpdSftGeoPar.cxx 
MpdSft.cxx 
MpdSftContFact.cxx 
MpdSftHit.cxx 
MpdSftHitProducer.cxx 
MpdSftPoint.cxx
)

Set(HEADERS)
Set(LINKDEF MpdSftLinkDef.h)
Set(LIBRARY_NAME Sft)
Set(DEPENDENCIES Core Base)

GENERATE_LIBRARY()
