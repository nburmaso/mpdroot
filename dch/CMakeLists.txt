# Create a library called "libDch" which includes the source files given in
# the array.
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base
${CMAKE_SOURCE_DIR}/cbmbase 
${CMAKE_SOURCE_DIR}/mcstack 
${CMAKE_SOURCE_DIR}/dch
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
MpdDchGeo.cxx 
MpdDchGeoPar.cxx 
MpdDch.cxx 
MpdDchContFact.cxx 
MpdDchPoint.cxx
MpdDchHit.cxx 
MpdDchHitProducer.cxx 
)

Set(HEADERS)
Set(LINKDEF MpdDchLinkDef.h)
Set(LIBRARY_NAME Dch)
Set(DEPENDENCIES Core Base)

GENERATE_LIBRARY()
