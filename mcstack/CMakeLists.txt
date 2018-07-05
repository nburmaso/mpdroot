# Create a library called "libMCStack" which includes the source files given in
# the array.
# The extension is already found.  Any number of sources could be listed here.

set(INCLUDE_DIRECTORIES
  ${ROOT_INCLUDE_DIR}
  ${Boost_INCLUDE_DIRS}
  ${BASE_INCLUDE_DIRECTORIES}
  ${CMAKE_SOURCE_DIR}/mcstack 
)

include_directories( ${INCLUDE_DIRECTORIES})

set(LINK_DIRECTORIES
  ${ROOT_LIBRARY_DIR}
  ${FAIRROOT_LIBRARY_DIR}
)
 
link_directories(${LINK_DIRECTORIES})

set(SRCS
FairStack.cxx 
FairMCTrack.cxx
)

set(HEADERS
FairStack.h 
FairMCTrack.h
MpdDetectorList.h
)

Set(LINKDEF MCStackLinkDef.h)
Set(LIBRARY_NAME MCStack)
Set(DEPENDENCIES Base EG Physics Core)

GENERATE_LIBRARY()

