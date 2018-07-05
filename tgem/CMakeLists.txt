# Create a library called "libTgem" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES 
${CMAKE_SOURCE_DIR}/geobase 
${CMAKE_SOURCE_DIR}/parbase 
${CMAKE_SOURCE_DIR}/base
${CMAKE_SOURCE_DIR}/cbmbase 
${CMAKE_SOURCE_DIR}/mcstack 
${CMAKE_SOURCE_DIR}/tgem
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
MpdTgemGeo.cxx 
MpdTgemGeoPar.cxx 
MpdTgem.cxx 
MpdTgemContFact.cxx 
MpdTgemHit.cxx 
MpdTgemHitProducer.cxx 
MpdTgemPoint.cxx
)

Set(HEADERS)
Set(LINKDEF MpdTgemLinkDef.h)
Set(LIBRARY_NAME Tgem)
Set(DEPENDENCIES Core Base)

GENERATE_LIBRARY()
