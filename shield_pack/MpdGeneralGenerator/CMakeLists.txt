# Create a library called "libMpdGeneralGenerator"
enable_language (CXX)

set(INCLUDE_DIRECTORIES
  ${BASE_INCLUDE_DIRECTORIES}
  ${CMAKE_SOURCE_DIR}/generators
  ${CMAKE_SOURCE_DIR}/gconfig
  ${CMAKE_CURRENT_SOURCE_DIR}/inc
)

Set(SYSTEM_INCLUDE_DIRECTORIES
  ${ROOT_INCLUDE_DIR}
)
if(PLUTO_FOUND)
 set(SYSTEM_INCLUDE_DIRECTORIES ${SYSTEM_INCLUDE_DIRECTORIES} ${PLUTO_INCLUDE_DIR})
EndIf(PLUTO_FOUND)

include_directories(${INCLUDE_DIRECTORIES})
Include_Directories(SYSTEM ${SYSTEM_INCLUDE_DIRECTORIES})

Set(LINK_DIRECTORIES
 ${ROOT_LIBRARY_DIR}
 ${FAIRROOT_LIBRARY_DIR}
)

if(PLUTO_FOUND)
 set(LINK_DIRECTORIES ${LINK_DIRECTORIES} ${PLUTO_LIBRARY_DIR})
EndIf(PLUTO_FOUND)
 
Link_Directories(${LINK_DIRECTORIES})

configure_file(inc/MpdGeneralGenerator.h inc/MpdGeneralGenerator.h COPYONLY)
configure_file(inc/LinkDef.h inc/LinkDef.h COPYONLY)

set(SRCS
 src/MpdGeneralGenerator.cxx
)

set(HEADERS
 inc/MpdGeneralGenerator.h
)

Set(LINKDEF inc/LinkDef.h)
Set(LIBRARY_NAME MpdGeneralGenerator)
Set(DEPENDENCIES Core Base)

GENERATE_LIBRARY()

