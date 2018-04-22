# Create a library called "libStrawendcap" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
 ${BASE_INCLUDE_DIRECTORIES}
 ${CMAKE_SOURCE_DIR}/mcstack 
 ${CMAKE_SOURCE_DIR}/strawendcap
)

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR}
 ${Boost_INCLUDE_DIRS}
)

include_directories(${INCLUDE_DIRECTORIES})
Include_Directories(SYSTEM ${SYSTEM_INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
 ${ROOT_LIBRARY_DIR}
 ${FAIRROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

# List of source files
set(SRCS
MpdStrawendcapGeo.cxx 
MpdStrawendcapGeoPar.cxx 
MpdStrawendcap.cxx 
MpdStrawendcapContFact.cxx 
MpdStrawendcapHit.cxx 
MpdStrawendcapHitProducer.cxx 
MpdStrawendcapPoint.cxx
)

Set(HEADERS)
Set(LINKDEF MpdStrawendcapLinkDef.h)
Set(LIBRARY_NAME Strawendcap)
Set(DEPENDENCIES Core Base)

GENERATE_LIBRARY()
