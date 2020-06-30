set(INCLUDE_DIRECTORIES
${BASE_INCLUDE_DIRECTORIES}
 ${CMAKE_SOURCE_DIR}/generators
 ${CMAKE_SOURCE_DIR}/generators/genFactory
 ${CMAKE_SOURCE_DIR}/shield_pack/THadgen/inc
 ${CMAKE_SOURCE_DIR}/shield_pack/hadgen/inc
 ${CMAKE_SOURCE_DIR}/macro/mpd
 ${CMAKE_SOURCE_DIR}
)

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR}
 ${FAIRROOT_LIBRARY_DIR}
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
MpdGeneratorsFactory.cxx
)

Set(HEADERS)
Set(LINKDEF LinkDefFactory.h)
Set(LIBRARY_NAME MpdGenFactory)
Set(DEPENDENCIES Core Base MpdGen)

GENERATE_LIBRARY()
